# See bottom of file for license and copyright info
#
# Virtual file system layered over a Foswiki data store.
# As far as possible this interface only uses the published methods of
# Foswiki::Func. However the current implementation *assumes* an RCS-type
# physical filestore.
#
# The Filesys::Virtual::Plain interface is extended with FUSE-compliant
# methods for handling extended attributes.
#
# Return values are based on the return values from Filesys::Virtual::Plain
# with enhancements (much better use of $!)
#
# Note that files uploaded through the Foswiki 'upload' script may not
# have the original name of the uploaded file. This is due to the name
# 'sanitization' applied by the 'upload' script. This module does not,
# however, limit the characters used in filenames.
#
# A note on character set encodings. It is assumed that all filenames etc
# passed to the API are encoded using perl logical characters i.e. they are
# unicode strings of (potentially) wide-byte characters. Thus when Foswiki
# is called to deal with these names, they must first be encoded in the
# {Site}{CharSet}. This is done as early as possible.
#
# Two external databases (besides the Foswiki system) are used; an extended
# attributes database, implemented using Storable, and a lock database,
# implemented in Filesys::Virtual::Locks.

package Filesys::Virtual::Foswiki;

# Base class not strictly needed
use Filesys::Virtual ();
our @ISA = ('Filesys::Virtual');

use strict;

use File::Path ();
use POSIX ':errno_h';
use Encode                  ();
use Filesys::Virtual::Locks ();
use IO::String              ();
use IO::File                ();
use Storable                ();

# This uses the first occurence of these modules on the path, so the path
# has to have been set up before we get here
use Foswiki          ();    # for constructor
use Foswiki::Plugins ();    # for $SESSION - namespace for compatibility
use Foswiki::Func    ();    # for API
use Foswiki::Meta    ();    # _ONLY_ to get the comment for an attachment :(

our $VERSION   = '1.6.2';
our $RELEASE   = '%$TRACKINGCODE%';
our $FILES_EXT = '_files';
our @views;
our $extensionsRE;
our $excludeRE;

=pod

=head1 NAME

Filesys::Virtual::Foswiki - A virtual filesystem for Foswiki (and Foswiki)

=head1 SYNOPSIS

	use Filesys::Virtual::Foswiki;

	my $fs = Filesys::Virtual::Foswiki->new();

	print foreach ($fs->list('/Sandbox'));

=head1 DESCRIPTION

This module is used by other modules to provide a pluggable filesystem
that sites on top of a Foswiki (or Foswiki) data store.

=head1 CONSTRUCTOR

=head2 new(\%args)

You can set validateLogin => 0 in the args. This will allow
the login method to be used to authenticate a user without checking their
password.

=head1 METHODS

=cut

sub new {
    my $class = shift;
    my $args  = shift;

    unless ( scalar(@views) ) {
        my @v =
          split( /\s*,\s*/,
            $Foswiki::cfg{Plugins}{FilesysVirtualPlugin}{Views} || 'txt' );
        foreach my $view (@v) {
            my $vc = 'Foswiki::Plugins::FilesysVirtualPlugin::Views::' . $view;
            eval "require $vc" || die $@;
            push( @views, $vc );
        }

        $extensionsRE =
          join( '|', ( $FILES_EXT, map { $_->extension() } @views ) );
        $excludeRE = $Foswiki::cfg{Plugins}{FilesysVirtualPlugin}{Exclude};
    }

    # root_path is the location
    # cwd is the full path to the resource (ignore)

    my $this = bless(
        {
            path          => '/',
            session       => undef,
            validateLogin => 1,
        },
        $class
    );
    foreach my $field ( keys %$args ) {
        if ( $this->can($field) ) {
            $this->$field( $args->{$field} );
        }
        else {
            $this->{$field} = $args->{$field};
        }
    }

    return $this;
}

sub DESTROY {
    my $this = shift;

    # Clean up the Foswiki session
    $this->{session}->finish() if $this->{session};
}

sub _locks {
    my $this = shift;
    unless ( $this->{locks} ) {
        my $lockdb =
          Foswiki::Func::getWorkArea('FilesysVirtualPlugin') . '/lockdb';
        $this->{locks} = new Filesys::Virtual::Locks($lockdb);
    }
    return $this->{locks};
}

sub validateLogin {
    my ( $this, $val ) = @_;
    $this->{validateLogin} = $val;
}

sub _initSession {
    my $this = shift;

    return $this->{session} if defined $this->{session};

    # meyer@modell-aachen.de
    # Add support for virtual hosting.
    # See package VirtualHostingContrib for further details.
    eval {
        my $request = $this->{session}->{request};
        my $host = $request->virtual_host();
        my $port = $request->virtual_port();

        if (scalar %{Foswiki::Contrib::VirtualHostingContrib::VirtualHost::}) {
            require Foswiki::Contrib::VirtualHostingContrib::VirtualHost;
            my $vhost = Foswiki::Contrib::VirtualHostingContrib::VirtualHost->find($host, $port);
            my $vconfig = $vhost->run(sub {
                return {
                    PubDir => $Foswiki::cfg{PubDir},
                    WorkingDir => $Foswiki::cfg{WorkingDir},
                    DataDir => $Foswiki::cfg{DataDir}
                };
            });

            $Foswiki::cfg{PubDir} = $vconfig->{PubDir};
            $Foswiki::cfg{WorkingDir} = $vconfig->{WorkingDir};
            $Foswiki::cfg{DataDir} = $vconfig->{DataDir};
	}
    };
    if ( $@ ) {
        # nothing...
    }

    return $this->{session};
}

# Convert one or more strings from perl logical characters to the site encoding
sub _logical2site {
    return
      map { Encode::encode( $Foswiki::cfg{Site}{CharSet} || 'iso-8859-1', $_ ) }
      @_;
}

# Convert one or more strings from perl logical characters to the site encoding
sub _site2logical {
    return
      map { Encode::decode( $Foswiki::cfg{Site}{CharSet} || 'iso-8859-1', $_ ) }
      @_;
}

sub _readAttrs {
    my $this = shift;

    return $this->{attrs_db} if defined $this->{attrs_db};

    $this->{attrs_db} ||= {};

    my $f = Foswiki::Func::getWorkArea('FilesysVirtualPlugin') . '/attrs.db';
    if ( -e $f ) {
        eval {

            # Can't use retrieve of a file; doesn't work on Windows
            my $fd;
            open( $fd, '<', $f );
            $this->{attrs_db} = Storable::fd_retrieve($fd);
            close($fd);
        };
        print STDERR "ERROR LOADING XATTRS DB: $@\n" if $@;

        # Ignore the error. There is no other sensible route
        # to reporting it.
    }

    return 1;
}

sub _lockAttrs {
    my $this = shift;

    my $f = Foswiki::Func::getWorkArea('FilesysVirtualPlugin') . '/attrs.db';
    if ( -e $f ) {
        open( $this->{adb_handle}, '<', $f );
        flock( $this->{adb_handle}, 1 );    # LOCK_SH
        $this->{attrs_db} = Storable::fd_retrieve( $this->{adb_handle} );
    }

    # (re)open and take an exclusive lock
    open( $this->{adb_handle}, '>', $f );
    flock( $this->{adb_handle}, 2 );        # LOCK_EX
}

sub _unlockAttrs {
    my $this = shift;
    Storable::store_fd( $this->{attrs_db}, $this->{adb_handle} );
    flock( $this->{adb_handle}, 8 );        # LOCK_UN
    close( $this->{adb_handle} );
    $this->{adb_handle} = undef;
}

=pod

=head2 login($loginName [, $password])

If the validateLogin option is set, the password will be ignored. Only
use this when you have some other way of authenticating users.

=cut

sub login {
    my $this = shift;
    return 0 unless $this->_initSession();
    my ( $loginName, $loginPass ) = _logical2site(@_);

    # SMELL: violations of core encapsulation
    my $users = $Foswiki::Plugins::SESSION->{users};
    if ( $this->{validateLogin} ) {
        my $validation = $users->checkPassword( $loginName, $loginPass );
        if ( !$validation ) {
            return 0;
        }
    }

    # Map the login name through the transformation rules
    # c.f. LdapContrib's RewriteWikiNames option
    if ( $Foswiki::cfg{Plugins}{FilesysVirtualPlugin}{RewriteLoginNames} ) {
        while ( my ( $pattern, $subst ) =
            each
            %{ $Foswiki::cfg{Plugins}{FilesysVirtualPlugin}{RewriteLoginNames} }
          )
        {

            # Does not validate the pattern nor the subst, so get them wrong
            # at your own risk!
            eval "\$loginName =~ s#$pattern#$subst#g";
        }
    }

    # TODO: use Foswiki::Contrib::LdapContrib;
    # $loginName = Foswiki::Contrib::LdapContrib::normalizeLoginName(
    #      {}, $loginName);

    # Tell the login manager that the new user is logged in
    # Code copied from Foswiki::UI::Rest
    my $cUID     = $users->getCanonicalUserID($loginName);
    my $wikiName = Foswiki::Func::getWikiName($cUID);
    $users->{loginManager}->userLoggedIn( $loginName, $wikiName );

    # Work around TWiki bug
    $cUID ||= $loginName;

    $Foswiki::Plugins::SESSION->{user} = $cUID;
    return 1;
}

# Break a resource into its component parts, web, topic, attachment.
# The return value is an array which may have up to 3 entries:
# [0] is always the full web path name
# [1] is always the topic name with no suffix
# [2] is the attachment name
# if the array is empty, that indicates the root (/)
# The rules for encoding web, topic and attachment names are automatically
# applied.
sub _parseResource {
    my ( $this, $resource ) = @_;

    if ( defined $this->{location} && $resource =~ s/^$this->{location}// ) {

        # Absolute path; must be, cos it has a location
    }
    elsif ( $resource !~ /^\// ) {

        # relative path
        $resource = $this->{path} . '/' . $resource;
    }
    $resource =~ s/\/\/+/\//g;    # normalise // -> /
    $resource =~ s/^\/+//;        # remove leading /

    # Resolve the path into it's components
    my @path;
    foreach ( split( /\//, $resource ) ) {
        if ( $_ eq '..' ) {
            if ($#path) {
                pop(@path);
            }
        }
        elsif ( $_ eq '.' ) {
            next;
        }
        elsif ( $_ eq '~' ) {
            @path = ( $Foswiki::cfg{UsersWebName} );
        }
        else {
            push( @path, $_ );
        }
    }

    # Greedily descend through webs to views
    my $web = shift(@path);
    while ( scalar(@path) && $path[0] !~ /($extensionsRE)$/ ) {
        $web .= '/' . shift(@path);
    }
    my @result = ($web);
    if ( scalar(@path) ) {
        push( @result, shift(@path) );                     # view
        push( @result, shift(@path) ) if scalar(@path);    # attachment
        return undef if scalar(@path);
    }

    return \@result;
}

# Because of different semantics at different levels in the Foswiki store
# hierarchy, there may be up to five different versions of each function.
# This are indicated by the prefixes:
# _R_ - root (/)
# _W_ - web
# _T_ - topic (view)
# _D_ - topic (attachments dir)
# _A_ - attachment (attachment data file)
# This function determines which level is applicable from the path, and
# redirects to the appropriate version.
sub _dispatch {
    my $this     = shift;
    my $function = shift;
    my $resource = shift;

    return 0 unless $this->_initSession();
    my $path = $this->_parseResource($resource);

    # Note: all strings in \@path are encoded using {Site}{CharSet}
    unless ($path) {
        return $this->_fail( POSIX::EBADF, $resource );
    }
    my $view;
    if ( scalar(@$path) > 1 ) {
        if ( $path->[1] =~ s/$FILES_EXT// ) {
            $view = 'D';
        }
        else {
            foreach my $v (@views) {
                my $e = $v->extension();
                if ( $path->[1] =~ s/$e// ) {
                    $view = $v;
                    last;
                }
            }
        }
        if ( !$view ) {
            die "Internal error - no topic view for $resource";
        }
    }
    my $type = 'R';
    if ( $path->[2] && $view eq 'D' ) {
        $type = 'A';
    }
    elsif ( $path->[1] ) {
        if ( $view eq 'D' ) {
            $type = 'D';
        }
        else {
            $type = 'T';
        }
    }
    elsif ( $path->[0] ) {
	$type = 'W'; # Web
    }
    $function = "_${type}_$function";

    #print STDERR "Call $function for $resource type $type ",join(',',@_),"\n";
    if ( $type eq 'T' ) {
        return $this->$function( $view, @$path, @_ );
    }
    else {
        return $this->$function( @$path, @_ );
    }
}

# test if a topic has an attachments dir
sub _hasAttachments {
    my ( $web, $topic ) = @_;

    if ( defined &Foswiki::Func::getAttachmentList ) {
        my @l = Foswiki::Func::getAttachmentList( $web, $topic );
        return scalar(@l) > 0;
    }
    else {

        # Probably TWiki. Have to viloate Store encapsulation
        return -d "$Foswiki::cfg{PubDir}/$web/$topic";
    }
}

# Test if the current user has access to the given resource
sub _haveAccess {
    my ( $type, $web, $topic ) = @_;
    if ( !$web || $web eq '/' || !length($web) ) {
        $type  = "ROOT$type";
        $web   = $topic;
        $topic = undef;
    }

    # SMELL: Foswiki::Func::checkAccessPermission *does not work* on
    # the root, so we have to use a lower-level interface (Foswiki::Meta)
    my $meta = Foswiki::Meta->new( $Foswiki::Plugins::SESSION, $web, $topic );
    return $meta->haveAccess( $type, $Foswiki::Plugins::SESSION->{user} );
}

sub _checkLock {
    my ( $this, $w, $t, $a ) = @_;
    my @junk = Foswiki::Func::checkTopicEditLock( $w, $t );
    if (   $junk[1]
        && $junk[1] ne
        Foswiki::Func::wikiToUserName( Foswiki::Func::getCanonicalUserID() ) )
    {
        return $this->_fail( POSIX::ENOLCK, $junk[1], $w, $t, $a );
    }
    return 1;
}

# Check that a path represents a full valid web/topic name
sub _checkName {
    my ( $this, $w, $t, $a ) = @_;
    if ($w) {
        foreach my $bit ( split( '/', $w ) ) {
            if ( defined &Foswiki::isValidWebName ) {

                #return 0
                die "ARSE $bit" unless Foswiki::isValidWebName($bit);
            }
            elsif ( $bit !~ m/^$Foswiki::regex{webNameRegex}$/o ) {
                return 0;
            }
        }
    }
    return 1 unless defined $t;
    if ( defined &Foswiki::isValidTopicName ) {

        #return 0
        die "CUNT $t" unless Foswiki::isValidTopicName( $t, 1 );
    }
    elsif ( $t !~ m/^$Foswiki::regex{wikiWordRegex}$/o ) {

        # TWiki doesn't know the difference between wikiwords and topic names.
        return 0;
    }
    return 1 unless defined $a;

    # SMELL: should really do this, but Foswiki is totally lackadasical
    # about checking the legality of attachment names. It allows a
    # saveAttachment of an illegal name, so we have to as well.
    #my ($sa) = Foswiki::Func::sanitizeAttachmentName($a);
    #if ($sa ne $a) {
    #    return $this->_fail(POSIX::EBADF, $w, $t, $a);
    #}
    return 1;
}

# Get the parent path of a path
sub _parent {
    my $web = shift;
    if ( $web =~ /(.*)\/[^\/]*\/*$/ ) {
        return $1 || '/';
    }
    else {
        return '/';
    }
}

# Work out a file mode for the given resource
sub _getMode {
    my ( $web, $topic ) = @_;
    my $mode = 0;    # ----------
    if ( !$topic ) {
        if ( !$web || Foswiki::Func::webExists($web) ) {

            # No access unless web exists or root
            $mode |= 01111;    # d--x--x--x
            if ( _haveAccess( 'VIEW', $web ) ) {
                $mode |= 0444;    # -r--r--r--
                                  # No change without view
                if ( _haveAccess( 'CHANGE', $web ) ) {
                    $mode |= 0222;    # --w--w--w-
                }
            }
        }
    }
    elsif ( _haveAccess( 'VIEW', $web, $topic ) ) {
        $mode |= 0444;                # -r--r--r--
                                      # No change without view
        if ( _haveAccess( 'CHANGE', $web, $topic ) ) {
            $mode |= 0222;            # --w--w--w-
        }
    }

    # print STDERR "MODE /".($web||'')."/".($topic||'')."=$mode\n";
    return $mode;
}

sub cwd {
    my ( $this, $path ) = @_;

    # Ignore this. Use chdir to navigate.
}

=pod

=head2 root_path($path)

Get or set the root path. This is the location that should be on the front
of all paths passed to methods of this class.

For example, if you define a WebDAV handler that works on the location
/dav, then this path will be set to /dav. Requests to files under that
location will have the /dav prefix removed before processing.

=cut

sub root_path {
    my ( $this, $path ) = @_;
    if ( defined $path ) {
        ( $this->{location} ) = _logical2site($path);
    }
    return $this->{location};
}

sub _fail {
    my $this = shift;
    my $code = shift;
    if ( $this->{trace} & 1 ) {
        my @c    = caller(1);
        my $op   = $c[3];
        my $path = join( '/', map { $_ || '?' } @_ );
        my $mess;
        if ( $code == POSIX::EPERM ) {
            $mess = shift || 'operation not permitted';
            $path = join( '/', map { $_ || '?' } @_ );
        }
        elsif ( $code == POSIX::EOPNOTSUPP ) {
            $mess = shift || 'operation not supported';
            $path = join( '/', map { $_ || '?' } @_ );
        }
        elsif ( $code == POSIX::EACCES ) {
            $mess = "access denied";
        }
        elsif ( $code == POSIX::ENOENT ) {
            $mess = "no such entity";
        }
        elsif ( $code == POSIX::ENOTEMPTY ) {
            $mess = "not empty";
        }
        elsif ( $code == POSIX::ENOLCK ) {
            my $who = shift;
            $path = join( '/', @_ );
            $mess = "locked by $who";
        }
        elsif ( $code == POSIX::EEXIST ) {
            $mess = "already exists";
        }
        elsif ( $code == POSIX::EBADF ) {
            $mess = "bad name";
        }
        else {
            die 'UNKNOWN ERROR CODE';
        }
        print STDERR "$op $path failed; $mess\n";
    }
    $! = $code;
    return undef;
}

# Not supported for Foswiki
sub chmod {
    return shift->_fail( POSIX::EOPNOTSUPP, undef, @_ );
}

=pod

=head2 modtime($file)

Gets the modification time of a file in YYYYMMDDHHMMSS format.

=cut

sub modtime {
    my $this = shift;
    return ( 0, 0 ) unless $this->_initSession();

    my @stat = $this->stat( @_ );
    return ( 0, '' ) unless scalar(@stat);
    my ( $sec, $min, $hr, $dd, $mm, $yy, $wd, $yd, $isdst ) =
      localtime( $stat[9] );
    $yy += 1900;
    $mm++;
    return ( 1, "$yy$mm$dd$hr$min$sec" );
}

=pod

=head2 size($file)

Gets the size of a file in bytes.

=cut

sub size {
    my $this = shift;
    return 0 unless $this->_initSession();

    my @stat = $this->stat( @_ );
    return $stat[7];
}

=pod

=head2 delete($file)

Deletes a file, returns 1 or 0 on success or failure. ($! is set)

=cut

sub delete {
    my $this = shift;
    return $this->_dispatch( 'delete', _logical2site(@_) );
}

# Delete root - always denied
sub _R_delete {
    return shift->_fail( POSIX::EPERM, undef, '/' );
}

# Delete attachment - by renaming it to Trash/TrashAttachment
sub _A_delete {
    my ( $this, $web, $topic, $attachment ) = @_;

    my $n = '';
    while (
        Foswiki::Func::attachmentExists(
            $Foswiki::cfg{TrashWebName},
            'TrashAttachment', "$attachment$n"
        )
      )
    {
        $n++;
    }
    return $this->_A_rename( $web, $topic, $attachment,
        "/$Foswiki::cfg{TrashWebName}/TrashAttachment$FILES_EXT/$attachment$n"
    );
}

# Delete topic - by renaming it to Trash
sub _T_delete {
    my ( $this, $view, $web, $topic ) = @_;
    my $n = '';
    while (
        Foswiki::Func::topicExists( $Foswiki::cfg{TrashWebName}, "$topic$n" ) )
    {
        $n++;
    }
    return $this->_T_rename( $view, $web, $topic,
        "$Foswiki::cfg{TrashWebName}/$topic$n" . $view->extension() );
}

# Delete attachment directory - always denied
sub _D_delete {
    my ( $this, $web, $topic ) = @_;
    return $this->_fail( POSIX::EPERM, undef, $web, $topic );
}

# Delete web - always denied
sub _W_delete {
    my ( $this, $web ) = @_;
    return $this->_fail( POSIX::EPERM, undef, $web );
}

=pod

=head2 rename($source, $destination)

Renames a file, returns 1 or 0 on success or failure. ($! is set)

=cut

sub rename {
    my $this = shift;
    return $this->_dispatch( 'rename', _logical2site(@_) );
}

# Rename root - always denied
sub _R_rename {
    return shift->_fail( POSIX::EPERM, undef, '/' );
}

# Rename attachment
sub _A_rename {
    my ( $this, $src_web, $src_topic, $src_att, $destination ) = @_;
    return 0 unless $this->_checkLock( $src_web, $src_topic, $src_att );

    unless ( Foswiki::Func::attachmentExists( $src_web, $src_topic, $src_att ) )
    {
        return $this->_fail( POSIX::ENOENT, $src_web, $src_topic, $src_att );
    }
    if ( !_haveAccess( 'CHANGE', $src_web, $src_topic ) ) {
        return $this->_fail( POSIX::EACCES, $src_web, $src_topic, $src_att );
    }

    my $dst_path = $this->_parseResource($destination);
    if ( scalar(@$dst_path) != 3 ) {
        return $this->_fail( POSIX::EPERM,
            'Can only rename an attachment to another attachment',
            $src_web, $src_topic, $src_att );
    }
    my ( $dst_web, $dst_topic, $dst_att ) = @$dst_path;
    $dst_topic =~ s/$FILES_EXT$//o;
    unless ( $this->_checkName( $dst_web, $dst_topic, $dst_att ) ) {
        return $this->_fail( POSIX::EBADF, $dst_web, $dst_topic, $dst_att );
    }
    if ( Foswiki::Func::attachmentExists( $dst_web, $dst_topic, $dst_att ) ) {
        return $this->_fail( POSIX::EEXIST, $dst_web, $dst_topic, $dst_att );
    }
    return 0 unless $this->_checkLock( $dst_web, $dst_topic, $dst_att );

    eval {
        Foswiki::Func::moveAttachment( $src_web, $src_topic, $src_att, $dst_web,
            $dst_topic, $dst_att );
    };
    if ($@) {
        return $this->_fail( POSIX::EPERM, $@, $src_web, $src_topic, $src_att );
    }
    return 1;
}

# Rename topic
sub _T_rename {
    my ( $this, $view, $src_web, $src_topic, $destination ) = @_;
    return 0 unless $this->_checkLock( $src_web, $src_topic );
    if ( !_haveAccess( 'CHANGE', $src_web, $src_topic ) ) {
        return $this->_fail( POSIX::EACCES, $src_web, $src_topic );
    }
    my $dst_path = $this->_parseResource($destination);
    if ( scalar(@$dst_path) != 2 ) {
        return $this->_fail( POSIX::EPERM,
            'Can only rename a topic to another topic',
            $src_web, $src_topic );
    }
    my ( $dst_web, $dst_topic ) = @$dst_path;
    my $ext = $view->extension();
    $dst_topic =~ s/$ext$//;
    unless ( $this->_checkName( $dst_web, $dst_topic ) ) {
        return $this->_fail( POSIX::EBADF, $dst_web, $dst_topic );
    }
    if ( Foswiki::Func::topicExists( $dst_web, $dst_topic ) ) {
        return $this->_fail( POSIX::EEXIST, $dst_web, $dst_topic );
    }
    eval {
        Foswiki::Func::moveTopic( $src_web, $src_topic, $dst_web, $dst_topic );
    };
    if ($@) {
        return $this->_fail( POSIX::EPERM, $@, $src_web, $src_topic );
    }
    return 1;
}

sub _M_rename {
    my ( $this, $web, $topic ) = @_;
    return $this->_fail( POSIX::EPERM, undef, $web, $topic );
}

sub _D_rename {
    my ( $this, $src_web, $src_topic ) = @_;
    return $this->_fail( POSIX::EPERM, undef, $src_web, $src_topic );
}

sub _W_rename {
    my ( $this, $src_web, $destination ) = @_;
    if ( !_haveAccess( 'CHANGE', $src_web ) ) {
        return $this->_fail( POSIX::EACCES, $src_web );
    }
    my $dst_path = $this->_parseResource($destination);
    if ( scalar(@$dst_path) != 1 ) {
        return $this->_fail( POSIX::EPERM,
            'Can only rename a web to another web', $src_web );
    }
    my ($dst_web) = @$dst_path;
    unless ( $this->_checkName($dst_web) ) {
        return $this->_fail( POSIX::EBADF, $dst_web );
    }
    eval { Foswiki::Func::moveWeb( $src_web, $dst_web ); };
    if ($@) {
        return $this->_fail( POSIX::EPERM, $@, $src_web );
    }
    return 1;
}

=pod

=head2 chdir($dir)

Changes the cwd.
Returns undef on failure or the new path on success.

=cut

sub chdir {
    my $this = shift;
    return $this->_dispatch( 'chdir', _logical2site(@_) );
}

sub _R_chdir {
    my ($this) = @_;
    $this->{path} = '';
    return $this->{path};
}

sub _W_chdir {
    my ( $this, $web ) = @_;
    if ( Foswiki::Func::webExists($web) ) {
        $this->{path} = $web;
        return $this->{path};
    }
    return undef;
}

sub _T_chdir {
    my ( $this, $view, $web, $topic ) = @_;
    return $this->_fail( POSIX::EPERM, undef, $web, $topic );
}

sub _D_chdir {
    my ( $this, $web, $topic ) = @_;
    if ( _hasAttachments( $web, $topic ) ) {
        $this->{path} = "$web/$topic";
        return $this->{path};
    }
    return undef;
}

sub _A_chdir {
    return undef;
}

=pod

=head2 mkdir($dir)

Creates a 'directory' (web of attachments dir). Returns 0 (and sets $!)
on failure. Returns 1 otherwise (directory created or already exists)

=cut

sub mkdir {
    my $this = shift;
    return $this->_dispatch( 'mkdir', _logical2site(@_) );
}

sub _R_mkdir {
    return shift->_fail( POSIX::EPERM, undef, '/' );
}

sub _A_mkdir {
    my ( $this, $web, $topic, $attachment ) = @_;

    # Can't mkdir in an attachments dir
    return $this->_fail( POSIX::EPERM, undef, $web, $topic, $attachment );
}

# Create an attachments dir.
sub _D_mkdir {
    my ( $this, $web, $topic ) = @_;
    return 0 unless $this->_checkLock( $web, $topic );
    if ( _hasAttachments( $web, $topic ) ) {
        return 1;
    }
    if ( !_haveAccess( 'CHANGE', _parent($web), $topic ) ) {
        return $this->_fail( POSIX::EACCES, $web, $topic );
    }
    unless ( $this->_checkName( $web, $topic ) ) {
        return $this->_fail( POSIX::EBADF, $web, $topic );
    }

    # Create an attachments dir.
    eval {

        # SMELL: violating store encapsulation. We can't do this
        # through the Func API because we would have to create a
        # "fake attachment" and then delete it, which would be brutally
        # inefficient.
        File::Path::mkpath(
            "$Foswiki::cfg{PubDir}/$web/$topic",
            { mode => $Foswiki::cfg{RCS}{dirPermission} }
        );
    };
    if ($@) {
        return $this->_fail( POSIX::EPERM, $@, $web, $topic );
    }
    return 1;
}

sub _T_mkdir {
    my ( $this, $view, $web, $topic ) = @_;
    return $this->_fail( POSIX::EPERM, undef, $web, $topic );
}

sub _W_mkdir {
    my ( $this, $web ) = @_;

    # Called on an existing web?
    if ( Foswiki::Func::webExists($web) ) {
        return 1;
    }

    # Check change access on parent
    if ( !_haveAccess( 'CHANGE', _parent($web) ) ) {
        return $this->_fail( POSIX::EACCES, $web );
    }
    unless ( $this->_checkName($web) ) {
        return $this->_fail( POSIX::EBADF, $web );
    }
    my $result = 0;
    eval { Foswiki::Func::createWeb( $web, "_default" ); };
    if ($@) {
        return $this->_fail( POSIX::EPERM, $@, $web );
    }
    return 1;
}

=pod

=head2 rmdir($dir)

Deletes a directory or file if -d test fails. Returns 1 on success or 0 on
failure (sets $!).

=cut

sub rmdir {
    my $this = shift;
    return $this->_dispatch( 'rmdir', _logical2site(@_) );
}

sub _R_rmdir {
    return shift->_fail( POSIX::EPERM, undef, '/' );
}

sub _W_rmdir {
    my ( $this, $web ) = @_;
    unless ( Foswiki::Func::webExists($web) ) {
        return $this->_fail( POSIX::ENOENT, $web );
    }
    if (   !_haveAccess( 'CHANGE', $web )
        || !_haveAccess( 'CHANGE', $Foswiki::cfg{TrashWebName} ) )
    {
        return $this->_fail( POSIX::EACCES, $web );
    }
    my @topics = Foswiki::Func::getTopicList($web);
    if ( scalar(@topics) > 1 ) {
        return $this->_fail( POSIX::ENOTEMPTY, $web );
    }
    my $n = '';
    while ( Foswiki::Func::webExists("$Foswiki::cfg{TrashWebName}/$web$n") ) {
        $n++;
    }
    my $newWeb = "$Foswiki::cfg{TrashWebName}/$web$n";
    eval { Foswiki::Func::moveWeb( $web, $newWeb ); };
    if ($@) {
        return $this->_fail( POSIX::EPERM, $@, $web );
    }
    return 1;
}

sub _A_rmdir {
    my ( $this, $web, $topic, $attachment ) = @_;
    return $this->_A_delete( $web, $topic, $attachment );
}

sub _T_rmdir {
    my ( $this, $view, $web, $topic ) = @_;
    return $this->_T_delete( $view, $web, $topic );
}

# Rmdir attachments dir.
# SMELL: This *could* be a NOP if the attachments dir is empty.
# SMELL: Handle error on on-empty dir.
sub _D_rmdir {
    my ( $this, $web, $topic ) = @_;
    return 0 unless $this->_checkLock( $web, $topic );
    if ( !_haveAccess( 'CHANGE', $web, $topic ) ) {
        return $this->_fail( POSIX::EACCES, $web, $topic );
    }

    # SMELL: violate store encapsulation by deleting the empty directory
    return CORE::rmdir("$Foswiki::cfg{PubDir}/$web/$topic");
}

=pod

=head2 list($dir)

Returns an array of the files in a directory. Returns undef and sets $!
on failure.

=cut

sub list {
    my $this = shift;
    my $list = $this->_dispatch( 'list', _logical2site(@_) );
    return () unless $list;
    return () unless scalar(@$list);
    return grep( $_ !~ /$excludeRE/, @$list ) if defined $excludeRE;
    return @$list;
}

sub _R_list {
    my ( $this, $web ) = @_;
    my @list = grep { !/\// } Foswiki::Func::getListOfWebs('user,public');
    @list = map { Encode::decode( $Foswiki::cfg{Site}{CharSet}, $_ ) } @list
      if defined $Foswiki::cfg{Site}{CharSet};
    unshift( @list, '.' );
    return \@list;
}

sub _W_list {
    my ( $this, $web ) = @_;
    my @list = ();
    if ( !_haveAccess( 'VIEW', $web ) ) {
        return $this->_fail( POSIX::EACCES, $web );
    }
    if ( !Foswiki::Func::webExists($web) ) {
        return $this->_fail( POSIX::ENOENT, $web );
    }
    foreach my $f ( Foswiki::Func::getTopicList($web) ) {

        # Always list an _files for the topic even if it doesn't exist
        # on disk. Otherwise we have no drag-drop target.
        #if ( _hasAttachments( $web, $f ) ) {
        push( @list, $f . $FILES_EXT );

        #}
        foreach my $v (@views) {
            push( @list, $f . $v->extension() );
        }
    }
    foreach my $sweb ( Foswiki::Func::getListOfWebs('user,public') ) {
        next if $sweb eq $web;
        next unless $sweb =~ s/^$web\/+//;
        next if $sweb =~ m#/#;
        push( @list, $sweb );
    }
    push( @list, '.' );
    push( @list, '..' );
    @list = map { Encode::decode( $Foswiki::cfg{Site}{CharSet}, $_ ) } @list
      if defined $Foswiki::cfg{Site}{CharSet};
    return \@list;
}

sub _D_list {
    my ( $this, $web, $topic ) = @_;
    if ( !_haveAccess( 'VIEW', $web, $topic ) ) {
        return $this->_fail( POSIX::EACCES, $web, $topic );
    }

    # list attachments
    my @list = ();
    if ( defined &Foswiki::Func::getAttachmentList ) {
        @list = Foswiki::Func::getAttachmentList( $web, $topic );
        # Have to include '.' and '..' to make it look like a dir
        unshift( @list, '.', '..' );
    }
    else {

        # Probably TWiki. Have to violate Store encapsulation
        my $dir = "$Foswiki::cfg{PubDir}/$web/$topic";
        if ( opendir( D, $dir ) ) {
            foreach my $e ( grep { !/,v$/ } readdir(D) ) {
                $e =~ /^(.*)$/;
                push( @list, $1 );
            }
        }
    }
    @list = _site2logical(@list);
    return \@list;
}

sub _T_list {
    my ( $this, $view, $web, $topic ) = @_;
    if ( !Foswiki::Func::topicExists( $web, $topic ) ) {
        return $this->_fail( POSIX::ENOENT, $web, $topic );
    }
    if ( !_haveAccess( 'VIEW', $web, $topic ) ) {
        return $this->_fail( POSIX::EACCES, $web, $topic );
    }
    return [ $topic . $view->extension() ];
}

sub _A_list {
    my ( $this, $web, $topic, $attachment ) = @_;
    if ( !Foswiki::Func::topicExists( $web, $topic ) ) {
        return $this->_fail( POSIX::ENOENT, $web, $topic, $attachment );
    }
    if ( !_haveAccess( 'VIEW', $web, $topic ) ) {
        return $this->_fail( POSIX::EACCES, $web, $topic );
    }
    return [$attachment];
}

=pod

=head2 list_details($file)

Returns the files list formatted as an HTML page.

=cut

sub list_details {
    my ( $this, $path ) = @_;

    my $body = Foswiki::Func::loadTemplate('webdav_folder') ||
	'<html><body>%TITLE%<p>%ENTRIES%</p></body></html>';
    my $title = $path;
    if ( $title =~ /^.*\/([^\/]+)$FILES_EXT\/?$/ ) {
	$title = "$1 - %MAKETEXT{\"Attachments\"}%";
    }
    else {
	$title =~ s/^.*\/([^\/]+)\/?$/$1/;
    }

    my @entries;
    foreach my $file ($this->list($path)) {
	next if $file eq '.';
	my $url = $path;
	$url .= '/' unless $path =~ /\/$/;
	$url .= $file;
	my $entry;
	if ( $file eq '..' ) {
	    $entry = Foswiki::Func::expandTemplate('webdav-updir')
		|| '<a href="%URL%">..</a><br />';
	} elsif ( $file =~ s/$FILES_EXT$// ) {
	    $entry = Foswiki::Func::expandTemplate('webdav-dir')
		|| '<a href="%URL%">%FILE%/</a><br />';
	} else {
	    $entry = Foswiki::Func::expandTemplate('webdav-file')
		|| '<a href="%URL%">%FILE%</a><br />';
	}
	$entry =~ s/%URL%/$url/g;
	$entry =~ s/%FILE%/$file/g;
	push( @entries, $entry );
    }
    $body =~ s/%TITLE%/$title/g;
    $body =~ s/%ENTRIES%/join('', @entries)/e;
    $body = Foswiki::Func::expandCommonVariables( $body );
    $body = Foswiki::Func::renderText( $body );
    $body = $Foswiki::Plugins::SESSION->_renderZones($body);
    return $body;
}

=pod

=head2 stat($file)

Does a normal stat() on a file or directory

=cut

# SMELL: this is a major violation of store encapsulation. Should the
# Foswiki store attempt to provide this sort of info? Really, Filesys::Virtual
# should be a low-level interface provided by that store. It is tolerated
# because it is required for WebDAV to find properties.
sub stat {
    my $this = shift;
    return $this->_dispatch( 'stat', _logical2site(@_) );
}

sub _R_stat {
    my ($this) = @_;
    return () unless -e $Foswiki::cfg{DataDir};
    my @stat = CORE::stat( $Foswiki::cfg{DataDir} );
    $stat[2] = _getMode();
    return @stat;
}

sub _W_stat {
    my ( $this, $web ) = @_;
    return () unless -e "$Foswiki::cfg{DataDir}/$web";
    my @stat = CORE::stat("$Foswiki::cfg{DataDir}/$web");
    $stat[2] = _getMode($web);
    return @stat;
}

sub _D_stat {
    my ( $this, $web, $topic ) = @_;
    return () unless -e "$Foswiki::cfg{PubDir}/$web/$topic";
    my @stat = CORE::stat("$Foswiki::cfg{PubDir}/$web/$topic");
    $stat[2] = _getMode( $web, $topic ) | 01111;
    return @stat;
}

sub _T_stat {
    my ( $this, $view, $web, $topic ) = @_;
    # SMELL: should META:TOPICINFO override what stat() says? It would
    # be very slow :-(
    return () unless -e "$Foswiki::cfg{DataDir}/$web/$topic.txt";
    my @stat = CORE::stat("$Foswiki::cfg{DataDir}/$web/$topic.txt");
    $stat[2] = _getMode( $web, $topic );
    return @stat;
}

sub _A_stat {
    my ( $this, $web, $topic, $attachment ) = @_;
    return ()
      unless Foswiki::Func::attachmentExists( $web, $topic, $attachment );

    # SMELL: using filesystem
    # SMELL: should META:FILEATTACHMENT override what stat() says? It would
    # be very slow :-(
    my @stat = CORE::stat("$Foswiki::cfg{PubDir}/$web/$topic/$attachment");
    $stat[2] = _getMode( $web, $topic );
    return @stat;
}

=pod

=head2 test($test,$file)

Perform a perl type test on a file and returns the results. Some of the tests
don't make sense on Foswiki database data; these will return false.

For example to perform a -d on a directory.

	$self->test('d','/testdir');

-r  File is readable by effective uid/gid
-w  File is writable by effective uid/gid.
-x  File is executable by effective uid/gid.
-o  File is owned by effective uid.

-R  File is readable by real uid/gid.
-W  File is writable by real uid/gid.
-X  File is executable by real uid/gid.
-O  File is owned by real uid.

-e  File exists.
-z  File has zero size.
-s  File has nonzero size (returns size).

-f  File is a plain file.
-d  File is a directory.
-l  File is a symbolic link.
-p  File is a named pipe (FIFO), or Filehandle is a pipe.
-S  File is a socket.
-b  File is a block special file.
-c  File is a character special file.
-t  Filehandle is opened to a tty.

-u  File has setuid bit set.
-g  File has setgid bit set.
-k  File has sticky bit set.

-T  File is a text file.
-B  File is a binary file (opposite of -T).

-M  Age of file in days when script started.
-A  Same for access time.
-C  Same for inode change time.

=cut

sub test {
    my ( $this, $test, $path ) = @_;
    return $this->_dispatch( 'test', _logical2site($path), $test );
}

sub _R_test {
    my ( $this, $undef, $type ) = @_;
    if ( $type =~ /r/i ) {

        # File is readable by effective/real uid/gid.
        return 1;    # No way to limit this, AFAIK
    }
    elsif ( $type =~ /w/i ) {

        # File is writable by effective/real uid/gid.
        return _haveAccess('CHANGE');
    }
    elsif ( $type =~ /[de]/ ) {
        return 1;
    }
    else {

        # SMELL: violating Store encapsulation
        return eval "-$type '$Foswiki::cfg{DataDir}'";
    }
}

sub _A_test {
    my ( $this, $web, $topic, $attachment, $type ) = @_;

    if ( $type =~ /r/i ) {

        # File is readable by effective/real uid/gid.
        return _haveAccess( 'VIEW', $web, $topic );
    }
    elsif ( $type =~ /w/i ) {

        # File is writable by effective/real uid/gid.
        return _haveAccess( 'CHANGE', $web, $topic );
    }
    elsif ( $type =~ /x/i ) {

        # File is executable by effective/real uid/gid.
        return 0;
    }
    elsif ( $type =~ /o/i ) {

        # File is owned by effective/real uid.
        return 1;    # might as well be, for all the difference it makes
    }
    elsif ( $type eq 'e' ) {

        # File exists.
        return Foswiki::Func::attachmentExists( $web, $topic, $attachment );
    }
    elsif ( $type eq 'f' ) {

        # File is a plain file (always)
        return 1;
    }
    elsif ( $type eq 'd' ) {

        # File is a directory (never)
        return 0;
    }

    # All other ops, kick down to the filesystem
    # SMELL: violating Store encapsulation
    # lpSbctugkTBzsMAC
    my $file = "$Foswiki::cfg{PubDir}/$web/$topic/$attachment";
    return eval "-$type $file";
}

sub _D_test {
    my ( $this, $web, $topic, $type ) = @_;
    if ( $type =~ /r/i ) {

        # File is readable by effective/real uid/gid.
        return _haveAccess( 'VIEW', $web, $topic );
    }
    elsif ( $type =~ /w/i ) {

        # File is writable by effective/real uid/gid.
        return _haveAccess( 'CHANGE', $web, $topic );
    }
    elsif ( $type =~ /x/i ) {

        # File is executable by effective/real uid/gid.
        return 1;
    }
    elsif ( $type =~ /o/i ) {

        # File is owned by effective/real uid.
        return 1;    # might as well be, for all the difference it makes
    }
    elsif ( $type eq 'e' ) {

        # File exists.
        # Referring to the attachments subdir, which will be created on demand
        # as long as the topic exists.
        return Foswiki::Func::topicExists( $web, $topic );
    }
    elsif ( $type eq 'f' ) {

        # File is a plain file.
        return 0;
    }
    elsif ( $type eq 'd' ) {

        # File is a directory.
        return 1;
    }

    # All other ops, kick down to the filesystem
    # SMELL: violating Store encapsulation
    # lpSbctugkTBzsMAC
    return eval "-$type $Foswiki::cfg{PubDir}/$web/$topic";
}

sub _T_test {
    my ( $this, $view, $web, $topic, $type ) = @_;
    if ( $type =~ /r/i ) {

        # File is readable by effective/real uid/gid.
        return _haveAccess( 'VIEW', $web, $topic );
    }
    elsif ( $type =~ /w/i ) {

        # File is writable by effective/real uid/gid.
        return _haveAccess( 'CHANGE', $web, $topic );
    }
    elsif ( $type =~ /x/i ) {

        # File is executable by effective/real uid/gid.
        return 0;
    }
    elsif ( $type =~ /o/i ) {

        # File is owned by effective/real uid.
        return 1;    # might as well be, for all the difference it makes
    }
    elsif ( $type eq 'e' || $type eq 'f' ) {

        # File exists.
        return Foswiki::Func::topicExists( $web, $topic );
    }
    elsif ( $type eq 'd' ) {

        # File is a directory.
        return 0;
    }

    # All other ops, kick down to the filesystem
    # SMELL: violating Store encapsulation
    # lpSbctugkTBzsMAC
    return eval "-$type $Foswiki::cfg{DataDir}/$web/$topic.txt";
}

sub _W_test {
    my ( $this, $web, $type ) = @_;
    if ( $type =~ /r/i ) {

        # File is readable by effective/real uid/gid.
        return _haveAccess( 'VIEW', $web );
    }
    elsif ( $type =~ /w/i ) {

        # File is writable by effective/real uid/gid.
        return _haveAccess( 'CHANGE', $web );
    }
    elsif ( $type =~ /o/i ) {

        # File is owned by effective/real uid.
        return 1;    # might as well be, for all the difference it makes
    }
    elsif ( $type =~ /[edXx]/ ) {

        # File exists.
        return Foswiki::Func::webExists($web);
    }
    elsif ( $type eq 'f' ) {

        # File is a plain file.
        return 0;
    }

    # All other ops, kick down to the filesystem
    # SMELL: violating Store encapsulation
    # lpSbctugkTBzsMAC
    my $file = "$Foswiki::cfg{DataDir}/$web";
    return eval "-$type $file";
}

=pod

=head2 open_read($file,[params])

Opens a file with L<IO::File>. Params are passed to open() of IO::File.
It returns the file handle on success or undef on failure. See L<IO::File>'s
open method.

When used to open topics, the content read from the file contains all the
meta-data associated with the topic.

=cut

sub open_read {
    my $this = shift;
    return $this->_dispatch( 'open_read', _logical2site(@_) );
}

sub _R_open_read {
    return shift->_fail( POSIX::EPERM, undef, '/' );
}

sub _W_open_read {
    return shift->_fail( POSIX::EPERM, undef, @_ );
}

sub _D_open_read {
    return shift->_fail( POSIX::EPERM, undef, @_ );
}

sub _T_open_read {
    my ( $this, $view, $web, $topic ) = @_;
    if ( !_haveAccess( 'VIEW', $web, $topic ) ) {
        return $this->_fail( POSIX::EACCES, $web, $topic );
    }

    return $view->read( $web, $topic );
}

sub _A_open_read {
    my ( $this, $web, $topic, $attachment ) = @_;
    if ( !_haveAccess( 'VIEW', $web, $topic ) ) {
        return $this->_fail( POSIX::EACCES, $web, $topic, $attachment );
    }

    my $data = Foswiki::Func::readAttachment( $web, $topic, $attachment );
    return IO::String->new($data);
}

=pod

=head2 close_read($fh)

Performs a $fh->close() on a read handle

=cut

sub close_read {
    my ( $this, $fh ) = @_;
    return $fh->close();
}

=pod

=head2 open_write($file, $append) -> $fh

Performs an open(">$file") or open(">>$file") if $append is defined.
Returns the filehandle on success or undef on failure.

=cut

sub open_write {
    my $this = shift;
    return $this->_dispatch( 'open_write', _logical2site(@_) );
}

sub _R_open_write {
    return shift->_fail( POSIX::EPERM, undef, '/' );
}

sub _W_open_write {
    return shift->_fail( POSIX::EPERM, undef, @_ );
}

sub _D_open_write {
    return shift->_fail( POSIX::EPERM, undef, @_ );
}

# File handle for writing an attachment
sub _makeWriteHandle {
    my $this = shift;
    my %opts = @_;

    my $name =
      Foswiki::Func::getWorkArea('FilesysVirtualPlugin')
      . join( '_', @{ $opts{path} } );
    my $fh = new IO::File( $name, 'w' );
    $this->{_filehandles}->{$fh} = \%opts;
    return $fh;
}

# Close attachment write handle
sub _A_closeHandle {
    my ( $this, $fh, $fn, $rec ) = @_;
    my $result;
    my @stats    = CORE::stat($fh);
    my $fileSize = $stats[7];

    # meyer@modell-aachen.de
    # In case something went wrong and we don't get a valid handle
    # we return here and let WebDAVContrib know to send a HTTP_BAD_REQUEST response.
    return 1 unless $fileSize;

    eval {

        #WORKAROUND to retain attachment comment
        my ( $web, $topic, $attachment ) = @{ $rec->{path} };
        my ( $meta, $text ) = Foswiki::Func::readTopic( $web, $topic );
        my $args = $meta->get( 'FILEATTACHMENT', $attachment );
        my $comment = $args->{comment} || '';
        my $isHideChecked = 0;
        if ( defined( $args->{attr} ) and ( $args->{attr} =~ /h/o ) ) {
            $isHideChecked = 1;
        }

        $result = Foswiki::Func::saveAttachment(
            @{ $rec->{path} },
            {
                stream      => $fh,
                filesize    => $fileSize,
                file	    => $fn,
                filedate    => time(),
                comment     => $comment,
                hide        => $isHideChecked
            }
        );
    };
    if ($@) {

        # In case it dies
        $result = $@;
    }
    if ($result) {

        # Emit the full message to STDERR
        print STDERR $result;
        return EACCES;
    }
    return 0;
}

# Close topic write handle
sub _T_closeHandle {
    my ( $this, $fh, $fn, $rec ) = @_;

    local $/;
    my $text = <$fh>;
    close($fh);
    return $rec->{view}->write( @{ $rec->{path} }, $text );
}

sub _T_open_write {
    my ( $this, $view, $web, $topic, $append ) = @_;
    return 0 unless $this->_checkLock( $web, $topic );
    if ( !_haveAccess( 'CHANGE', $web, $topic ) ) {
        return $this->_fail( POSIX::EACCES, $web, $topic );
    }

    return $this->_makeWriteHandle(
        type   => 'T',
        path   => [ $web, $topic ],
        append => $append,
        view   => $view
    );
}

sub _A_open_write {
    my ( $this, $web, $topic, $attachment, $append ) = @_;
    return 0 unless $this->_checkLock( $web, $topic );
    if ( !_haveAccess( 'CHANGE', $web, $topic ) ) {
        return $this->_fail( POSIX::EACCES, $web, $topic );
    }

    # meyer: fix for subwebs
    if ( $web =~ /(.+)\/(.+)/ ) {
        $web = "$1.$2";
    }

    return $this->_makeWriteHandle(
        type   => 'A',
        path   => [ $web, $topic, $attachment ],
        append => $append
    );
}

=pod

=head2 close_write($fh) -> $status

Performs a $fh->close() on a write handle. 0 return for no error.

=cut

sub close_write {
    my ( $this, $fh ) = @_;

    # meyer@modell-aachen
    # In case _initSession() failes we should return a non-zero value.
    # (see description above: Apache/WebDAV.pm relays on this)
    return POSIX::EACCES unless $this->_initSession();

    $fh->close();
    my $rec = $this->{_filehandles}->{$fh};
    my $result;
    if ($rec) {
        my $path = $rec->{path};
        my $tmpfile =
          Foswiki::Func::getWorkArea('FilesysVirtualPlugin')
          . join( '_', @$path );

	my $tfh;
	open( $tfh, '<', $tmpfile )
          or die "Failed to open temporary file $tmpfile";
        my $fn = '_' . $rec->{type} . '_closeHandle';

        #print STDERR "Call $fn to close write\n";
        $result = $this->$fn( $tfh, $tmpfile, $rec );

        unlink($tmpfile);
        delete( $this->{_filehandles}->{$fh} );
    }
    return $result;
}

=pod

=head2 seek($fh, $pos, $wence)

Performs a $fh->seek($pos, $wence). See L<IO::Seekable>.

=cut

sub seek {
    my ( $this, $fh, $pos, $wence ) = @_;
    return $fh->seek( $pos, $wence );
}

sub XATTR_CREATE {

    # See <sys/xattr.h>.
    return 1;
}

sub XATTR_REPLACE {

    # See <sys/xattr.h>.
    return 2;
}

=pod

=head2 setxattr($file, $name, $val, $flags)

setxattr

Arguments: Pathname, extended attribute's name, extended attribute's value, numeric flags (which is an OR-ing of XATTR_CREATE and XATTR_REPLACE.
Returns an errno or 0 on success.

Called to set the value of the named extended attribute.

If you wish to reject setting of a particular form of extended attribute name
(e.g.: regexps matching user\..* or security\..*), then return -EOPNOTSUPP.

If flags is set to XATTR_CREATE and the extended attribute already exists,
this should fail with -EEXIST. If flags is set to XATTR_REPLACE and the
extended attribute doesn't exist, this should fail with -ENOATTR.

XATTR_CREATE and XATTR_REPLACE are provided by this module.

=cut

sub setxattr {
    my $this = shift;

    # Note: does not use Foswiki, so names are kept as logical perl strings
    my ( $file, $name, $val, $flags ) = @_;
    $flags ||= 0;

    return POSIX::EOPNOTSUPP unless $this->_initSession();
    my $path = $this->_parseResource($file);
    unless ($path) {
        return $this->_fail( POSIX::EBADF, $file );
    }

    return POSIX::EOPNOTSUPP unless $this->_lockAttrs();
    my $pathkey = join( "\0", @$path );
    my $valukey = "$pathkey\1$name";

    if ( ( $flags & XATTR_CREATE ) ) {
        if ( defined $this->{attrs_db}->{$valukey} ) {
            $this->_unlockAttrs();
            $this->_fail( POSIX::EEXIST, @$path );
            return -$!;
        }
    }
    if ( ( $flags & XATTR_REPLACE ) ) {
        if ( !( defined $this->{attrs_db}->{$valukey} ) ) {
            $this->_unlockAttrs();
            $this->_fail( POSIX::EPERM, @$path );
            return -$!;
        }
    }

    $this->{attrs_db}->{$pathkey} ||= '';
    my %names = map { $_ => 1 } split( /\0/, $this->{attrs_db}->{$pathkey} );
    $names{$name} = 1;
    $this->{attrs_db}->{$valukey} = $val;
    $this->{attrs_db}->{$pathkey} = join( "\0", keys %names );
    $this->_unlockAttrs();

    return 0;
}

=pod

=head2 getxattr($file, $name) -> $value

Arguments: Pathname, extended attribute's name. Returns 0 if there was
no value, or the extended attribute's value.

Called to get the value of the named extended attribute.

=cut

sub getxattr {
    my $this = shift;

    # Note: does not use Foswiki, so names are kept as logical perl strings
    my ( $file, $name ) = @_;
    return 0 unless $this->_initSession();
    my $path = $this->_parseResource($file);
    unless ($path) {
        return $this->_fail( POSIX::EBADF, $file );
    }
    my $pathkey = join( "\0", @$path );
    my $valukey = "$pathkey\1$name";

    return 0 unless $this->_readAttrs();
    return $this->{attrs_db}->{$valukey};
}

=pod

=head2 listxattr($file) -> @names

Arguments: Pathname.
Returns a list: 0 or more text strings (the extended attribute names), followed by a numeric errno (usually 0).

Called to get the value of the named extended attribute.

=cut

sub listxattr {
    my ( $this, $file ) = @_;

    # Note: does not use Foswiki, so names are kept as logical perl strings
    return () unless $this->_initSession();
    my $path = $this->_parseResource($file);
    my $pathkey = join( "\0", @$path );
    return (POSIX::EOPNOTSUPP) unless $this->_readAttrs();
    return ( split( /\0/, $this->{attrs_db}->{$pathkey} || '' ), 0 );
}

=pod

=head2 removexattr($file, $name)

Arguments: Pathname, extended attribute's name.
Returns an errno or 0 on success.

=cut

sub removexattr {
    my $this = shift;

    # Note: does not use Foswiki, so names are kept as logical perl strings
    my ( $file, $name ) = @_;
    return POSIX::EOPNOTSUPP unless $this->_initSession();
    my $path    = $this->_parseResource($file);
    my $pathkey = join( "\0", @$path );
    my $valukey = "$pathkey\1$name";
    return POSIX::EOPNOTSUPP unless $this->_lockAttrs();
    $this->{attrs_db}->{$pathkey} ||= '';
    my %names = map { $_ => 1 } split( /\0/, $this->{attrs_db}->{$pathkey} );
    delete $names{$name};
    delete $this->{attrs_db}->{$valukey};
    $this->{attrs_db}->{$pathkey} = join( "\0", keys %names );
    delete $this->{attrs_db}->{$pathkey} unless $this->{attrs_db}->{$pathkey};
    $this->_unlockAttrs();
    return 0;
}

=pod

=head2 lock_types($path)

Return a bitmask of the lock types supported for this resource
(1 => exclusive, 2 => shared)

=cut

sub lock_types {
    return 3;    # exclusive and shared (advisory) locks supported
}

=pod

=head2 add_lock($path, %lock_data)

Add a lock for the given resource. The follwing lock field names are reserved:
   * token - unique token identifying the lock
   * path - resource path
   * depth - depth of the lock, 0 = single level
   * taken - epoch time the lock was taken
Other fields will be preserved in the lock.

=cut

sub add_lock {
    my ( $this, %lockstat ) = @_;
    return unless $this->_initSession();
    $lockstat{taken} ||= time();

    #$Foswiki::Plugins::SESSION->{store}
    #  ->setLease( $web, $topic, $locktoken, $Foswiki::cfg{LeaseLength} );
    $this->_locks->addLock(%lockstat);
}

=pod

=head2 refresh_lock($token)

Refresh (set the time to now) the lock with the given identifying token.

=cut

sub refresh_lock {
    my ( $this, $locktoken ) = @_;
    return unless $this->_initSession();

    #$Foswiki::Plugins::SESSION->{store}
    #  ->setLease( $web, $topic, $locktoken, $Foswiki::cfg{LeaseLength} );
    my $lock = $this->_locks->getLock($locktoken);
    $lock->{taken} = time() if $lock;
}

=pod

=head2 has_lock($token)

Returns a boolean indicating whether a lock exists for the given token.

=cut

# meyer@modell-aachen.de:
# Helper method used by WebDAVContrib.
# See method UNLOCK.
sub has_lock {
    my ( $this, $locktoken ) = @_;
    return unless $this->_initSession();
    my $lock = $this->_locks->getLock( $locktoken );
    return ( $lock ? 1 : 0 );
}

=pod

=head2 remove_lock($token) -> $boolean

Return true if it succeeded

=cut

sub remove_lock {
    my ( $this, $locktoken ) = @_;
    return unless $this->_initSession();

    #$Foswiki::Plugins::SESSION->{store}->clearLease( $web, $topic );
    return $this->_locks->removeLock($locktoken);
}

=pod

=head2 get_locks($path, $recurse) -> @locks

Get the locks on all resources above the given path which have deoth != 0.
If $recurse is true, also get locks in the subtree under the path. Returns
an array of hashes, each of which contains a lock.

=cut

sub get_locks {
    my ( $this, $path, $recurse ) = @_;

    # Note: does not use Foswiki, so names are kept as logical perl strings
    return () unless $this->_initSession();
    my @locks = $this->_locks->getLocks( $path, $recurse );

    # reap timed-out locks on this resource
    my $i = scalar(@locks) - 1;
    while ( $i >= 0 ) {
        my $lock = $locks[$i];
        if ( !$lock->{token} ) {

            #Carp::confess - this should never happen
            splice( @locks, $i, 1 );
        }
        elsif ($lock->{timeout} >= 0
            && $lock->{taken} + $lock->{timeout} < time() )
        {
            $this->_locks->removeLock( $lock->{token} );
            splice( @locks, $i, 1 );
        }
        $i--;
    }
    return @locks;
}

1;

__END__

Copyright (C) 2008 KontextWork.de
Copyright (C) 2011 WikiRing http://wikiring.com
Copyright (C) 2008-2013 Crawford Currie http://c-dot.co.uk

This program is licensed to you under the terms of the GNU General
Public License, version 2. It is distributed in the hope that it will
be useful, but WITHOUT ANY WARRANTY; without even the implied warranty
of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

As per the GPL, removal of this notice is prohibited.

This software cost a lot in blood, sweat and tears to develop, and
you are respectfully requested not to distribute it without purchasing
support from the authors (available from webdav@c-dot.co.uk). By working
with us you not only gain direct access to the support of some of the
most experienced Foswiki developers working on the project, but you are
also helping to make the further development of open-source Foswiki
possible.

Author: Crawford Currie http://c-dot.co.uk

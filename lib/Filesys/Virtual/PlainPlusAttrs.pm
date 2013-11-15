# See bottom of file for license and copyright info
#
# This is an extension of Filesys::Virtual::Plain that adds support for
# FUSE-compliant xattrs, and simple locks. The main purpose of the module
# is for testing WebDAVContrib.
#
package Filesys::Virtual::PlainPlusAttrs;
use Filesys::Virtual::Plain;
push( @ISA, 'Filesys::Virtual::Plain' );

use strict;
use POSIX ':errno_h';

use Data::Dumper;
use Filesys::Virtual::Locks;

our $VAR1;
our $VERSION = '1.6.1';
our $RELEASE = '%$TRACKINGCODE%';

sub new {
    my ( $class, $args ) = @_;
    $args->{root_path} = '/home/crawford/litmus_test/';
    my $this = $class->SUPER::new($args);
    my $f    = $this->_path_from_root('/chav/');
    $this->{locks} = new Filesys::Virtual::Locks("/tmp/plainfilelocks");
    return $this;
}

sub login {
    return 1;
}

sub getxattr {
    my ( $this, $path, $name ) = @_;
    my $attrs = $this->_readAttrs($path);
    $! = POSIX::EBADF unless defined $attrs->{$name};
    return $attrs->{$name};
}

sub setxattr {
    my ( $this, $path, $name, $val, $flags ) = @_;
    my $attrs = $this->_readAttrs($path);
    $attrs->{$name} = $val;
    return $this->_writeAttrs( $path, $attrs );
}

sub removexattr {
    my ( $this, $path, $name, $val, $flags ) = @_;
    my $attrs = $this->_readAttrs($path);
    delete $attrs->{$name};
    return $this->_writeAttrs( $path, $attrs );
}

sub listxattr {
    my ( $this, $path ) = @_;
    my $attrs = $this->_readAttrs($path);
    return ( keys %$attrs, 0 );
}

sub lock_types {
    my ( $this, $path ) = @_;
    return 3;    # exclusive and shared (advisory) locks supported
}

sub add_lock {
    my ( $this, %lockstat ) = @_;
    $lockstat{taken} ||= time();
    $this->{locks}->addLock( taken => time(), %lockstat );
}

sub refresh_lock {
    my ( $this, $locktoken ) = @_;
    Carp::confess unless $locktoken;
    my $lock = $this->{locks}->getLock($locktoken);
    $lock->{taken} = time();
}

# Boolean true if it succeeded
sub remove_lock {
    my ( $this, $locktoken ) = @_;
    Carp::confess unless $locktoken;
    $this->{locks}->removeLock($locktoken);
}

# Get the locks active on the given path
sub get_locks {
    my ( $this, $path, $recurse ) = @_;
    my @locks = $this->{locks}->getLocks( $path, $recurse );

    # reap timed-out locks on this resource
    my $i = scalar(@locks) - 1;
    while ( $i >= 0 ) {
        my $lock = $locks[$i];
        Carp::confess unless $lock->{token};
        if (   $lock->{timeout} >= 0
            && $lock->{taken} + $lock->{timeout} < time() )
        {
            $this->{locks}->removeLock( $lock->{token} );
            splice( @locks, $i, 1 );
        }
        else {
            $i--;
        }
    }
    return @locks;
}

sub _readAttrs {
    my ( $this, $path ) = @_;
    my $f = $this->_attrsFile($path);
    if ( open( F, "<", $f ) ) {
        local $/;
        eval(<F>);
        close(F);
        return $VAR1;
    }
    return {};
}

sub _writeAttrs {
    my ( $this, $path, $attrs ) = @_;
    my $f = $this->_attrsFile($path);
    open( F, ">", $f ) || return $!;
    print F Data::Dumper->Dump( [$attrs] );
    close(F);
}

sub _attrsFile {
    my ( $this, $path ) = @_;
    my $f = $this->_path_from_root($path);
    if ( -d $f ) {
        return "$f/.PlainPlusAttrs";
    }
    elsif ( $f =~ m#(.*)\/(.*?)# ) {
        return "$1/.PlainPlusAttrs_$2";
    }
    else {
        die "COCKUP $f";
    }
}

1;

__END__

Copyright (C) 2009-2012 WikiRing http://wikiring.com

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

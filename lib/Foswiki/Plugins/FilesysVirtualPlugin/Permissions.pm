# See bottom of file for license and copyright information
use strict;
use Fcntl;
use BerkeleyDB;

use Foswiki;
use Foswiki::Func;

# Permissions DB object. Processes protections info out of topic text
# and maintains a database of protections, using BerkelyDB.
package Foswiki::Plugins::FilesysVirtualPlugin::Permissions;

our $VERSION = '$Rev: 1208 $';
our $RELEASE = '1.6.1-/jidQrcaozxnxTDSHEh3qA';

my $setWebRE   = qr/^\s+\*\s+Set\s+(ALLOW|DENY)WEB([A-Z]+?)\s+=\s+(.+?)\s*$/;
my $setTopicRE = qr/^\s+\*\s+Set\s+(ALLOW|DENY)TOPIC([A-Z]+?)\s+=\s+(.+?)\s*$/;

# Constructor for a DB. Does not connect to the DB until actually required.
sub new {
    my ( $class, $dbdir ) = @_;
    my $this = {};

    $dbdir ||= $Foswiki::cfg{Plugins}{FilesysVirtualPlugin}{PermissionsDB};

    $this->{dbfile} = $dbdir . '/DB';
    $this->{db}     = undef;

    return bless( $this, $class );
}

# Refresh permissions everywhere in a installation. Call from cron.
sub recache {
    my ( $this, $web, $topic ) = @_;
    if ( !$web ) {
        my @webs = Foswiki::Func::getListOfWebs('user');

        foreach my $web (@webs) {
            $this->_processWeb( $web, undef );
        }
    }
    else {
        $this->_processWeb( $web, $topic );
    }
}

# Extract and store permissions settings in a single web
# We should really clean out old entries for this web before we
# start, but because the keys are topic specific and not web
# specific this is tricky and would be slow. However the memory
# leakage that results from _not_ doing it is so small that it's
# really not worth bothering about.
sub _processWeb {
    my ( $this, $web, $topic ) = @_;
    my $npr = 0;
    my @topics = $topic ? ($topic) : Foswiki::Func::getTopicList($web);
    foreach my $topic (@topics) {
        $this->_processTopic( $web, $topic );
        $npr++;
    }
    my @webs = $Foswiki::Plugins::SESSION->{store}->getListOfWebs( 'user', $web );
    foreach my $subweb (@webs) {
        $this->_processWeb( "$web/$subweb", undef );
    }

    print "Processed $npr topics from $web\n";
}

# Extract and store permissions settings in a single topic
sub _processTopic {
    my ( $this, $web, $topic ) = @_;

    return unless $topic;

    my ( $meta, $text ) = Foswiki::Func::readTopic( $web, $topic );
    print "Processing topic $web.$topic\n";

    my @lines = split /[\r\n]+/, $text;

    my $path = '';
    if (   $topic eq $Foswiki::cfg{DefaultPrefsTopicName}
        && $web   eq $Foswiki::cfg{SystemWebName}
        || $topic eq $Foswiki::cfg{SitePrefsTopicName}
        && $web   eq Foswiki::Func::getMainWebname() )
    {
        $path = '/';
    }
    elsif ( $topic eq $Foswiki::cfg{WebPrefsTopicName} ) {
        $path = "/$web/";
    }

    if ($path) {

        # first handle (ALLOW|DENY)WEB... (only if it's a XXXPreference topic)
        $this->_clearPath($path);
        foreach (@lines) {
            next unless /$setWebRE/;
            $this->_defineAccessRights( $path, $1, $2, $3 );
        }

        # TODO: process meta-data
    }

    $path = "/$web/$topic";

    # then handle (ALLOW|DENY)TOPIC...
    $this->_clearPath($path);
    foreach (@lines) {
        next unless /$setTopicRE/;
        $this->_defineAccessRights( $path, $1, $2, $3 );
    }

    # TODO: process meta-data
}

# Define access rights for a list of wikinames and groups.
sub _defineAccessRights {
    my ( $this, $path, $ad, $action, $names ) = @_;

    # ALLOW => A, DENY => D
    $ad =~ s/^(\w).*$/$1/o;

    # CHANGE => C, VIEW => V, RENAME => R
    $action =~ s/^(\w).*$/$1/o;

    $this->_setAccessRights( '|' . join( '|', split( /[,\s]+/, $names ) ) . '|',
        $path, $ad, $action );
}

# Clear the database entries for a path; we are about to re-define them.
sub _clearPath {
    my ( $this, $path ) = @_;

    my %db;
    $this->_tieDB( \%db );

    #print STDERR "Clear P:$path\n";
    delete( $db{"P:$path:V:D"} );
    delete( $db{"P:$path:V:A"} );
    delete( $db{"P:$path:C:D"} );
    delete( $db{"P:$path:C:A"} );
    delete( $db{"P:$path:R:D"} );
    delete( $db{"P:$path:R:A"} );
    untie(%db);
}

# Set access rights for a list of usernames.
# $path is the path to the resource i.e. /$web/$topic
# $mode is ALLOW or DENY depending on whether this is an allow or a deny
# $action is the controlled action e.g. VIEW, CHANGE
sub _setAccessRights {
    my ( $this, $users, $path, $allow, $action ) = @_;
    my $key = "P:${path}:${action}:${allow}";
    my %db;
    $this->_tieDB( \%db );
    $db{$key} = $users;

    #print STDERR "Stored $key => $users\n";
    untie(%db);
}

# Define a new group (or redefine an existing one)
sub _defineGroup {
    my ( $this, $group, $members ) = @_;
    my $key = "G:$group";
    my %db;
    $this->_tieDB( \%db );
    $db{$key} = $members;

    #print STDERR "Stored $key => $members\n";
    untie(%db);
}

# PRIVATE get the DB, opening it if required. The DB will persist
# until all references to this Permissions object have been lost.
# We tie and untie on a per-access basis.
sub _tieDB {
    my ( $this, $hash ) = @_;
    tie(
        %hash, 'BerkeleyDB::Hash',
        -Filename => $this->{dbfile},
        -Flags    => BerkeleyDB::DB_CREATE
    );
}

1;
__END__

Copyright (C) 2008-2010 WikiRing http://wikiring.com
Copyright (C) 2004 WindRiver Inc.
Copyright (C) 2008-2012 Crawford Currie http://c-dot.co.uk

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

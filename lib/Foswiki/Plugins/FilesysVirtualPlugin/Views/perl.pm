# See bottom of file for license and copyright information
package Foswiki::Plugins::FilesysVirtualPlugin::Views::perl;

use strict;
use IO::String    ();
use Data::Dumper  ();
use Foswiki::Func ();

our $VERSION = '1.6.1';
our $RELEASE = '%$TRACKINGCODE%';
our $data;

sub extension { '.perl' }

sub read {
    my ( $this, $web, $topic ) = @_;

    my ( $meta, $text ) = Foswiki::Func::readTopic( $web, $topic );

    # Trim back the meta to a useable form
    my %wdata;
    foreach my $k ( keys %$meta ) {
        if ( $k !~ /^_/ ) {
            $wdata{$k} = $meta->{$k};
        }
    }
    unless ( defined $wdata{_text} ) {
        $wdata{_text} = $text;
    }
    return IO::String->new( Data::Dumper->Dump( [ \%wdata ], ['data'] ) );
}

sub write {
    my ( $this, $web, $topic, $perl ) = @_;

    my ( $meta, $text ) = Foswiki::Func::readTopic( $web, $topic );

    # SMELL: untaint?
    local $data;
    eval($perl);
    foreach my $k ( keys %$data ) {
        if ( $k !~ /^_/ ) {
            $meta->{$k} = $data->{$k};
        }
    }
    $text = $data->{_text} if defined $data->{_text};

    eval { Foswiki::Func::saveTopic( $web, $topic, $meta, $text ); };
    return $@;
}

1;

__END__

Copyright (C) 2010-2012 WikiRing http://wikiring.com

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

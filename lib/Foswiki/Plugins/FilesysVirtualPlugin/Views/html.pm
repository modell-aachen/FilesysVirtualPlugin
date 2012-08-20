# See bottom of file for license and copyright information
package Foswiki::Plugins::FilesysVirtualPlugin::Views::html;

use strict;
use IO::String ();
use Foswiki::Func ();
use Foswiki::Plugins::WysiwygPlugin::Handlers ();

our $VERSION = '$Rev: 1208 $';
our $RELEASE = '1.6.1-/jidQrcaozxnxTDSHEh3qA';

sub extension { '.html' };

sub read {
    my ($this, $web, $topic) = @_;

    my ($meta, $text) = Foswiki::Func::readTopic($web, $topic);
    $text = Foswiki::Plugins::WysiwygPlugin::Handlers::TranslateTML2HTML(
        $text, $web, $topic );
    return IO::String->new("<html><body>$text</body></html>");
}

sub write {
    my ($this, $web, $topic, $text) = @_;

    my ($meta, $dummy) = Foswiki::Func::readTopic($web, $topic);

    $text =~ s/^.*?<body[^>]*>\s*//si;
    $text =~ s/<\/body>.*//si;
    $text = Foswiki::Plugins::WysiwygPlugin::Handlers::TranslateHTML2TML(
        $text, $topic, $web );

    eval {
        Foswiki::Func::saveTopic( $web, $topic, $meta, $text );
    };
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

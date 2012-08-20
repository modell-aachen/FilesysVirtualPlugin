# See bottom of file for license and copyright info
#
# WebDAV lock database implementation.
#
# Locks are per resource (depth = 0), or per resource subtree (depth = -1)
# Each lock is identified by a lock token
# Each resource may have multiple locks.
#
# Other than that, the caller can define whatever other fields in a lock
# record they want.
#
package Filesys::Virtual::Locks;

use strict;
use Assert;

use Storable ();
use Fcntl;

our $VERSION = '$Rev: 1208 $';
our $RELEASE = '1.6.1-/jidQrcaozxnxTDSHEh3qA';
our $INFINITY = -1;

# Each node in the tree has two fields;
#    * locks - an array of locks at this level
#    * subnodes - a map of subnodes, indexed by name
# The root node additional has
#    * locktokens - a map of locks, indexed by token
# A lock is a map, which has the following fields:
#    * path - path (/ separated list) from the root to this lock
#    * whatever additional fields are passed in when the lock is added
sub new {
    my ($class, $file) = @_;
    my $this = bless({ file => $file }, $class);
    if (-e $file) {
        eval {
            # Can't use a simple file retrieve; doesn't work on win32
            my $fd;
            open($fd, '<', $file) || die "Failed to open locks DB $file: $!";
	    flock($fd, Fcntl::LOCK_SH) || die "Failed to LOCK_SH: $!";
            $this->{db} = Storable::fd_retrieve($fd);
            close($fd) || die "Failed to close $file";
        };
        return $this unless $@;
        print STDERR "ERROR LOADING LOCKS DB: $@";
        # Ignore the error. There is no other sensible route
        # to reporting it.
    }
    $this->{db} = {
        locktokens => {},
        tree => { locks => [], subnodes => {} },
    };
    return $this;
}

# flock is used to control write access to the Storable file
sub _lock {
    my $this = shift;
    ASSERT(!$this->{handle}) if DEBUG;
    if (-e $this->{file}) {
	# Load the absolute latest
	my $h;
	open($h, '<', $this->{file}) || die "Failed to open locks DB $this->{file}: $!";
	flock($h, Fcntl::LOCK_SH) || die "Failed to LOCK_SH $this->{file}: $!";
        $this->{db} = Storable::fd_retrieve($h);
	close($h) || die "Failed to close $this->{file}";
    }
    # (re)open for write and take an exclusive lock
    open($this->{handle}, '>', $this->{file}) || die "Failed to open locks DB $this->{file}: $!";
    flock($this->{handle}, Fcntl::LOCK_EX) || die "Failed to LOCK_EX $this->{file}: $!";
}

sub _unlock {
    my $this = shift;
    Storable::store_fd($this->{db}, $this->{handle}) || die "Can't store locks '$@' '$!'";
    flock($this->{handle}, Fcntl::LOCK_UN) || die "Failed to LOCK_UN: $!";
    close($this->{handle}) || die "Failed to close: $!";
    undef $this->{handle};
}

# Get the lock with the given token
sub getLock {
    my ($this, $token) = @_;
    my $lock = $this->{db}->{locktokens}->{$token};
    return $lock;
}

# Remove the lock with the given token from the DB
sub removeLock {
    my ($this, $token) = @_;

    my $unlocked = 0;

    # Clean up all traces of the lock. There are two references to the lock; the one in the
    # top level hash, and the one in the tree. We need to remove both. If one or other is
    # missing, then there's something wrong with the DB, but we'll try to recover anyway.

    # Shelter in an eval, because we *must* unlock.
    eval {
	$this->_lock(); # lock the DB
	my $lock = $this->{db}->{locktokens}->{$token};

	if ($lock) {
	    delete $this->{db}->{locktokens}->{$token};
	    $unlocked = 1;
	    my $path = $lock->{path};
	    my $node = $this->{db}->{tree};
	    foreach my $pel (split('/', $path)) {
		unless ($node->{subnodes} && $node->{subnodes}->{$pel}) {
		    $node = undef; # not there; corrupt lock db?
		    last;
		}
		$node = $node->{subnodes}->{$pel};
	    }
	    if ($node && $node->{locks}) {
		for (my $i = 0; $i < scalar(@{$node->{locks}}); $i++) {
		    if ($node->{locks}->[$i]->{token} eq $token) {
			splice(@{$node->{locks}}, $i, 1);
			last;
		    }
		}
	    }
	}
    };
    $this->_unlock();

    return $unlocked;
}

# Add a new lock. %args *must* include {path} and
# *may* include {depth}. depth defaults to 0 if not
# supplied.
# A depth of 0 locks the node
# A depth of $INFINITY indicates infinity (everything below
# the locked node is locked)
# Other values of depth are errors.
sub addLock {
    my ($this, %args) = @_;
    Carp::confess unless defined $args{path};
    $args{depth} ||= 0;
    Carp::confess unless $args{depth} ==0 || $args{depth} == $INFINITY;

    if ($this->{db}->{locktokens}->{$args{token}}) {
        die "Lock $args{token} already exists";
    }

    my $lock = {};

    eval {
	$this->_lock();
	my $node = $this->{db}->{tree};
	foreach my $pel (split('/', $args{path})) {
	    $node->{subnodes} = {} unless $node->{subnodes};
	    my $child = $node->{subnodes}->{$pel};
	    if ( $child ) {
		$node = $child;
	    } else {
		my $subnode = {};
		$subnode->{subnodes} = {};
		$node->{subnodes}->{$pel} = $subnode;
		$subnode->{locks} = [];
		$node = $subnode;
	    }
	}
	while (my ($k, $v) = each %args) {
	    $lock->{$k} = $v;
	}
	$node->{locks} = [] unless $node->{locks};
	push(@{$node->{locks}}, $lock);
	$this->{db}->{locktokens}->{$args{token}} = $lock;
    };
    $this->_unlock();

    return $lock;
}

# Get locks on all resources above the given path which have depth != 0,
# and (if $recurse is true) below the given path
sub getLocks {
    my ($this, $path, $recurse) = @_;

    my @locks = ();
    my $node = $this->{db}->{tree};
    return @locks unless $node;
    # Check the tree above the node for deep locks
    my @path = split('/', $path);
    my $n = $#path;
    foreach my $pel (@path) {
        return @locks unless $node->{subnodes};
        my $subnode = $node->{subnodes}->{$pel};
        return @locks unless $subnode;
        if ($subnode->{locks}) {
            foreach my $plock (@{$subnode->{locks}}) {
                # The lock applies if this is the leaf of the path
                # or the lock is infinite
                if ($plock && $plock->{depth} == $INFINITY || !$n) {
                    push(@locks, $plock);
                }
            }
        }
        $node = $subnode;
        $n--;
    }
    return @locks unless $recurse && $node->{subnodes};
    # All locks below $node are on resources below $path
    my @queue;
    foreach my $subnode (values %{$node->{subnodes}}) {
        push(@queue, $subnode);
    }
    while ($node = shift(@queue)) {
        if ($node->{locks} && scalar(@{$node->{locks}})) {
            push(@locks, @{$node->{locks}});
        }
        next unless $node->{subnodes};
        foreach my $subnode (values %{$node->{subnodes}}) {
            push(@queue, $subnode);
        }
    }
    return @locks;
}

1;
__END__

Copyright (C) 2008-2012 WikiRing http://wikiring.com

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

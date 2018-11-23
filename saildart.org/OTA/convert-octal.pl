#!/usr/bin/perl

# Usage: convert-octal.pl [<file>]
#   Filter stdin or filename converting 36-bit words printed in octal into
#   five 7-bit ASCII characters.  NUL characters at the end of (each) word
#   are removed.

while (<>) {
    my @w = split;
    for my $w (@w) {
	$w =~ /([0-7]{12})/ or next;
	my $x=oct($1);
	$x /= 2;			# discard LSB
	my $bytes = 5;
	my @X = ();
	for my $c (1..5) {
	    push(@X, chr($x & 0x7f)), $x >>= 7;
	}
	shift @X while @X and $X[0] eq "\0"; # prune "trailing" NULs
	print reverse(@X)
    }
}


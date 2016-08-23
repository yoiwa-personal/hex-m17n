#!/usr/bin/perl
#original copyright:
#hex version 2.04 - hexadecimal dumping tool for Japanese
#copyright (c) TAGA Nayuta <nayuta@is.s.u-tokyo.ac.jp>
#  usage: hex [options ...] [filename]
#  options:
# * show non-letter-code by ...
#    -c, --color                    : color (tty-output default)
#    -b, --bold                     : bold
#    -d, --dot                      : `.' (non-tty-output default)
#    -t, --text                     : code + '@' (not useful)
#    -u, --underline                : underline
# * output coding system is ...
#    -e, -oe, --oeuc                : *euc-japan* (default)
#    -s, -os, --osjis               : *sjis*
#    -j, -oj, --ojis                : *iso-2022-jp*
# * assume input coding system to be ...
#    -E, -ie, --ieuc                : *euc-japan* or *iso-2022-jp* (default)
#    -S, -is, --isjis               : *sjis* or *iso-2022-jp*
#    -U, -iu, --iunknown            : unknown
# * other ...
#    -cs1 <cs>, --colorstring1 <cs> : color-string1 is <cs> (ex. -cs1 '7;33')
#    -cs2 <cs>, --colorstring2 <cs> : color-string2 is <cs>
#    -siso, --enablesiso            : enable ^N/^O (SO/SI) KATAKANA (default)
#    -dsiso, --disablesiso, +siso   : disable ^N/^O (SO/SI) KATAKANA
#    -r, --restore                  : "hex hoe | hex -r > hoge" is "cp hoe hoge"
#... and all options can be set to HEX_OPTIONS environment.

use Encode;
use utf8;
use strict;

our $enc_SJ = find_encoding("Shift_JIS") // die;
our $enc_932 = find_encoding("CP932") // die;
our $enc_EJ = find_encoding("EUC-JP") // die;
our $enc_U8 = find_encoding("UTF8") // die;

our $borrow = 0;
our $addr = 0;
our $buf = "";
our $bufstart = 0;
our $eof = undef;

our $binbuf = "";
our $chrbuf = "";

our $chreaten = 0; # number of characters already processed
our $chrforward = 0; # number of extra characters already emitted to output
our $charbased = 0;

our $widthmethod = 0;

use constant { BUFSIZE => 1024 };
use Getopt::Long qw(:config bundling permute);

our $incode = 0;
our $decorate = (-t STDOUT);
our $outenc = undef;

GetOptions ("shift-jis|s" => sub { $incode = 1 },
	    "euc-jp|e" => sub { $incode = 2 },
	    "utf-8|u" => sub { $incode = 3 },
	    "decorate|d!" => \$decorate,
	    "char|c:i" => \$charbased,
	    "charwidth=i" => \$widthmethod,
	   ) or die;

open IN, $ARGV[0] or die "open: $!";
binmode(STDOUT, ":utf8");

while (1) {
    my $o;
    $binbuf = "";
    $chrbuf = "";
    for ($o = 0; $o < 16; $o++) {
	my $code = get($addr + $o);
	last if !defined $code;
	unless ($charbased) {
	    $binbuf .= sprintf (" %02x", $code);
	    $binbuf .= " -" if $o == 7;
	}
	process_char($addr + $o, $code);
    }
    last if $o == 0;
    unless ($charbased) {
	$binbuf = substr($binbuf . (" "x48), 0, 50) if length $binbuf < 50;
    }
    put_normal("", 0);
    printf "%08x:%s  %s\n", $addr, $binbuf, $chrbuf;
    $chrforward = 0;
    last if $o != 16;
    $addr += 16;
}
exit 0;

{
    my $curdecorate = 0;
    sub put_decorate {
	my ($dec, $smpl, $n) = @_;
	$smpl //= ("." x length $_[0]);
	$n //= length($smpl);
	
	if ($decorate) {
	    if ($curdecorate != 1) {
		$chrbuf .= "\e[0;7;34;46m";
		$curdecorate = 1;
	    }
	}
	if ($charbased) {
	    my $ld = length($dec);
	    $ld++ if $dec =~ /\p{Ea: W}|\p{Ea: F}/;
	    my $l = int((length($dec) + 2) / 3);
	    my $sp = $l * 3 - length($dec);
	    $chrbuf .= $dec;
	    if ($sp) {
		$chrbuf .= " " x ($sp - 1);
		put_normal("", 0);
		$chrbuf .= " ";
	    }
	    $chreaten = $n - 1;
	    $chrforward = $l - 1;
#	    print "decorate: $dec for spc $n, strlen $ld, used_blocks $l, added_spaces $sp -> etn $chreaten fwd $chrforward\n";
	    $chrforward = 0 if $chrforward < 0; #for safety
	} else {
	    $chrbuf .= ($decorate ? $dec : $smpl);
	    $chreaten = $chrforward = $n - 1;
	}
    }
    sub put_fill {
	if ($decorate) {
	    if ($curdecorate != 2) {
		$chrbuf .= "\e[0;36m";
		$curdecorate = 2;
	    }
	    $chrbuf .= ($charbased ? "__ " : "_") x $_[0];
	} else {
	    $chrbuf .= ($charbased ? "   " : " ") x $_[0];
	}
    }
    sub put_normal {
	my ($_, $n) = @_;
	if ($curdecorate != 0) {
	    $chrbuf .= "\e[0m";
	    $curdecorate = 0;
	}
	return if $_ eq '';
	die if $n == 0;
	# $chrforward = length($sj) - 1;
	my $width = wcwidth($_);
	if ($width == 2) {
	    $chrforward = 1;
	    $chrforward = 0 if $n == 0;
	} elsif ($width == 0) {
	    $chrforward = 0;
	    $_ = " $_";
	} else {
	    $chrforward = 0;
	}
	if (/\p{Bidi_class: R}/) {
	    $_ = "\x{202d}$_\x{2069}";
	}
	$chreaten = $n - 1;
	if ($charbased) {
	    $chrbuf .= $_ . (" " x (2-$chrforward));
	    $chrforward = 0;
	} else {
	    $chrbuf .= $_;
	}
    }
}

sub process_char {
    my ($ofs, $code) = (@_);

    if ($chreaten) {
	$chreaten--;
	if ($chrforward) {
	    $chrforward--;
	} else {
	    put_fill (1);
	}
	return;
    }
    
    if ($code > 0x20 && $code <= 0x7e) {
	put_normal(chr($code), 1);
	return;
    }
    if ($code == 0x20 && $charbased <= 1) {
	put_normal(chr($code), 1);
	return;
    }
  SKIP_HICODE:
    {
	if ($incode == 1) {
	    if ($code >= 0xa1 && $code <= 0xdf) {
		put_normal($enc_SJ->decode(chr($code)), 1);
		return;
	    }
	    if ($code >= 0x80 && $code <= 0x9f || $code >= 0xe0 && $code <= 0xef) {
		my $next = get($ofs + 1);
		if ($next >= 0x40 && $next <= 0x7e || $next >= 0x80 && $next <= 0xfd) {
		    my $s = $enc_932->decode(chr($code) . chr($next));
		    if (length $s > 1 || substr($s, 1) eq "\x{fffd}") {
			put_decorate("〓", "..", 2);
		    } else {
			put_normal($s, 2);
		    }
		    return;
		}
	    }
	}
	if ($incode == 2) {
	    if ($code == 0x8e) {
		my $next = get($ofs + 1);
		if ($next >= 0xa1 && $next <= 0xdf) {
		    put_normal($enc_EJ->decode(chr($code) . chr($next)), 2);
		    return;
		}
	    }
	    if ($code >= 0xa1 && $code <= 0xfe) {
		my $next = get($ofs + 1);
		if ($next >= 0xa1 && $next <= 0xfe) {
		    my $s = $enc_EJ->decode(chr($code) . chr($next));
		    if (length $s > 1 || substr($s, 1) eq "\x{fffd}") {
			put_decorate("〓", "..", 2);
		    } else {
			put_normal($s, 2);
		    }
		    return;
		}
	    }
	}
	if ($incode == 3) {
	    if ($code >= 0xc0 && $code <= 0xfd) {
		my $n = (($code < 0xe0) ? 2 :
			 ($code < 0xf0) ? 3 :
			 ($code < 0xf8) ? 4 :
			 ($code < 0xfc) ? 5 : 6);
		my $c = $code & (0x7f >> $n);
		for (my $i = 1; $i < $n; $i++) {
		    my $next = get($ofs + $i);

		    # invalid UTF-8 sequence (out of range)
		    last SKIP_HICODE if ($next < 0x80 | $next > 0xbf);

		    $c = ($c << 6) | ($next & 0x3f);
		}
		  (0x40 << (($n - 2) * 5)), (0x40 << (($n - 1) * 5));

		last SKIP_HICODE if $c < (0x40 << (($n - 2) * 5));  # redundant
		last SKIP_HICODE if ($c >= 0xd800 && $c <= 0xdfff); # lone surrogate
		last SKIP_HICODE if ($c >= 0x110000);               # over the range

		my $s = pack("U", $c);
		# check if it is Japanese characters (TODO: "printable")
		my $ok;
		if (0) {
		    my $sj = $enc_932->encode($s);
		    $ok = $enc_932->decode($sj) eq $s;
		} else {
		    $ok = $s =~ /^\p{Print}$/;
		}
		if ($ok) {
		    my $outs = $s;
		    put_normal($s, $n);
		} else {
		    if ($charbased) {
			put_decorate(sprintf("U+%X", $c), undef, $n);
		    } else {
			put_decorate("." x $n);
		    }
		}
		return;
	    }
	}
    }
    if ($charbased) {
	if ($code == 0x0a) {
	    put_decorate('\n', undef, 1);
	} elsif ($code == 0x09) {
	    put_decorate('\t', undef, 1);
	} elsif ($code == 0x0d) {
	    put_decorate('\r', undef, 1);
	} elsif ($code < 0x20) {
	    put_decorate("^" . chr(0x40 + $code), undef, 1);
	} elsif ($code == 0x20) {
	    put_decorate('sp', undef, 1);
	} else {
	    put_decorate(sprintf("%02x", $code), undef, 1);
	}
    } else {
	if ($code < 0x20) {
	    put_decorate(chr(0x40 + $code), ".");
	} elsif ($code == 0x7f) {
	    put_decorate("~", ".");
	} else {
	    put_decorate(".", ".");
	}
    }
}

sub get ($) {
    my ($pos) = (@_);
    my $bufofs = $pos - $bufstart;
    die "oops" if $bufofs < 0;
    if ($bufofs > BUFSIZE + 16 && length $buf > BUFSIZE + 16) {
	$buf = substr($buf, BUFSIZE);
	$bufstart += BUFSIZE;
	$bufofs -= BUFSIZE;
    }
    if ($bufofs >= length $buf) {
	my $s;
	my $n = read(IN, $s, BUFSIZE);
	if (!defined $n) {
	    die "read error: $!";
	} else {
	    $buf .= $s;
	}
    }
    my $r = substr($buf, $bufofs, 1);
    if ($r eq '') {
	$eof = 0;
	return undef;
    }
    return ord($r);
}

sub wcwidth {
    return 0 if $_[0] eq "";
    return 1 if ord($_[0]) < 0x80;
    my $_ = substr($_[0], 0, 1);
    if ($widthmethod == 0) {
	use Text::CharWidth;
	my $r = Text::CharWidth::mbwidth($_);
	return $r if ($r >= 0);
    }
    
    if (/\p{Canonical_Combining_Class: 8}/) {
	return '00';
    }
    if (/\p{Block: Combining_Diacritical_Marks}
     |\p{Block: Combining_Diacritical_Marks_For_Symbols}
     |\p{Block: Combining_Half_Marks}
     |\p{Block: Combining_Marks_For_Symbols}
     |\p{Block: Combining_Diacritical_Marks_For_Symbols}/x) {
	return 0;
    }
    if (/\p{Ea: W}|\p{Ea: F}/) {
	return 2;
    }
    if ($widthmethod >= 2 && /\p{Ea: A}/) {
	return 2;
    }
    return 1;
}

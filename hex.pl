#!/usr/bin/perl
# hexja.pl version 0.01 - hexadecimal dump tool for Japanese
# (c) 2016 Yutaka OIWA.

use Encode;
use utf8;
use strict;

use v5.10.0;
use feature "switch";

our $enc_SJ = find_encoding("Shift_JIS") // die;
our $enc_932 = find_encoding("CP932") // die;
our $enc_EJ = find_encoding("EUC-JP") // die;
our $enc_U8 = find_encoding("UTF8") // die;

use constant { BUFSIZE => 1024 };
use Getopt::Long qw(:config bundling permute);

# input byte handling
our $addr = 0; # currently processing address in input
our $buf = ""; # processing data buffer
our $bufstart = 0; # address of the buffer top
our $eof = undef;  # 1 if read function returns 0

# multi byte sequences status
our $chreaten = 0;   # number of characters already processed
our $chrforward = 0; # number of extra characters already emitted to output

# output
our $binbuf = "";  # accumulator for binary dump
our $chrbuf = "";  # accumulator for character dump

# configuration
our $charbased = 0;
our $textbased = 0;
our $widthmethod = 0;
our $is_tty = (-t STDOUT);
our $incode = 'detect';
our $decorate = 2;
our $outenc = undef;
our $reset_2022_status = 1;

our %codings = 
  ( 'binary'    => [ \&process_GL_ASCII,  \&process_GR_none,   \&process_C0_ASCII ],
    'iso88591'  => [ \&process_GL_ASCII,  \&process_GR_8859_1, \&process_C0_ASCII ],
    'shiftjis'  => [ \&process_GL_ASCII,  \&process_GR_SJIS,   \&process_C0_ASCII ],
    'eucjp'     => [ \&process_GL_ASCII,  \&process_GR_EucJP,  \&process_C0_ASCII ],
    'utf8'      => [ \&process_GL_ASCII,  \&process_GR_UTF8,   \&process_C0_ASCII ],
    'iso2022'   => [ \&process_GLR_2022, \&process_GLR_2022,   \&process_C0_2022,  \&process_init_junet ],
    'detect'    => [],
  );

our %coding_aliases = 
  ( 'ascii' => 'binary',
    'junet' => 'iso2022',
    'jis'   => 'iso2022',
    'iso2022jp' => 'iso2022',
    'guess' => 'detect' );

GetOptions ("ascii|a"     => sub { set_input_coding('binary') },
	    "shift-jis|s" => sub { set_input_coding('shiftjis') },
	    "euc-jp|e"    => sub { set_input_coding('eucjp') },
	    "utf-8|u"     => sub { set_input_coding('utf8') },
	    "junet|jis|iso-2022-jp|j" => sub { set_input_coding('jis') },
	    "input-coding|i=s" => \&set_input_coding,
	    "decorate|d!" => \$decorate,
	    "plaintext|p" => sub { $decorate = 0 },
	    "char|c:1"    => \$charbased,
	    "text|t"      => \$textbased,
	    "charwidth=i" => \$widthmethod,
	    "reset-status!" => \$reset_2022_status,
	    "help|h|?"    => sub { &usage() },
	   ) or usage();

usage("-c and -t cannot be used together") if $charbased && $textbased;

sub usage {
    require File::Basename;
    print "Error: $_[0]\n" if defined $_[0];

    printf <<'EOS', File::Basename::basename($0);
Usage: %s [options] [--] [input file]

Options:
  --ascii       -a : input is ASCII or binary
                     (no special treatment for high codes)
  --shift-jis   -s : input is Shift-JIS
  --euc-jp      -e : input is EUC-JP
  --utf-8       -u : input is UTF-8
  --iso-2022-jp -j : input is ISO-2022-JP (also --jis, --junet)
                   (default : to guess from above four candidates)
  --input-coding=<coding> -i : set input coding

  --decorate  -d : use colorized output (default if output is tty)
  --no-decorate  : use plain-text output

  --char      -c : put printable characters in binary dump (a-la "od -c")
  --char=2   -c2 :   show space as "sp"
  --text      -t : print as plain text, quoting non-printable as hex dump

  --charwidth=n  : use internal character-width calculation
                     "n" (1..2) specifies width of "ambiguous" characters
                   (default: use locale settings (also when n = 0))
  --help      -h : show this help
EOS
    exit 1;
}

if (defined $ARGV[1]) {
    die "too many files specified.";
}
if (!defined $ARGV[0] || $ARGV[0] eq '-') {
    *IN = *STDIN{IO};
} else {
    open IN, $ARGV[0] or die "open: $!";
}

if ($decorate == 2 && $is_tty) {
    $decorate = 1;
}
binmode(STDOUT, ":utf8");

if ($incode eq 'detect') {
    # input encoding guess (detection)
    read(IN, $buf, BUFSIZE * 8) // die "read error: $!"; # 0 is OK, undef is error

    my $inc = detect_coding($buf);
    printf "# Detected coding: %s\n", $inc if (($buf ne '') && (!$textbased));
    set_input_coding($inc);
}

my ($process_GL, $process_GR, $process_C0, $process_init) = @{$codings{$incode}};

&$process_init() if defined $process_init;

while (1) {
    my $o;
    $binbuf = "";
    $chrbuf = "";
    for ($o = 0; $o < 16; $o++) {
	my $code = get($addr + $o);
	last if !defined $code;
	unless ($charbased | $textbased) {
	    $binbuf .= sprintf (" %02x", $code);
	    $binbuf .= " -" if $o == 7;
	}
	process_char($addr + $o, $code);
    }
    last if $o == 0;
    unless ($charbased | $textbased) {
	$binbuf = substr($binbuf . (" "x48), 0, 50) if length $binbuf < 50;
    }
    put_normal("", 0) unless $textbased;
    if ($textbased) {
	print $chrbuf;
    } else {
	printf "%08x:%s  %s\n", $addr, $binbuf, $chrbuf;
    }
    $chrforward = 0;
    last if $o != 16;
    $addr += 16;
}
put_normal("", 0); # if $textbased
exit 0;

### input coding parameter handlings

sub set_input_coding {
    my $coding = $_[-1];

    my $c = lc $coding;
    $c =~ s/[_-]//g;
    require Data::Dumper;
    Data::Dumper::Dumper(%coding_aliases);
    $c = $coding_aliases{$c} if exists $coding_aliases{$c};
    usage("unknown coding: $coding ($c)") unless exists $codings{$c};
    $incode = $c;
}

sub detect_coding {
    # A few final bytes might be a partial sequences of
    # multibyte sequences.  So, the patterns below contains
    # .? or similar at the end.  Flag "s" allows \n to be
    # matched with "." there.

    given ($_[0]) {
	when (m#\e\$?[\$(-/][A-z\@][\x0\x0f\x1b\x20-\x7e\x8e\x8f\xa0-\xff]+(?:\e\([B\@]|\n)#sx) {
	    'ISO-2022'
	}
	when (/\A(?:[\x00-\x7f])*\z/sx) {
	    'ASCII'
	      # internally "OK" with EUC-JP below, but for displaying
	}
	when (/\A(?:[\x00-\x7f]
	       |\x8e[\xa1-\xdf]
	       |[\xa1-\xfe][\xa1-\xfe]
	       |\x8f[\xa1-\xfe][\xa1-\xfe]
	       )*.?\z/sx) {
	    'EUC-JP'
	}
	when (/\A(?:[\x00-\x7f]|
		   [\xc2-\xdf][\x0x80-\xbf]|
		   [\xe0-\xef][\x0x80-\xbf]{2}|
		   [\xf0-\xf7][\x0x80-\xbf]{3}|
		   [\xf8-\xfb][\x0x80-\xbf]{4}|
		   [\xf8-\xfb][\x0x80-\xbf]{5}
	       )*.{0,5}\z/sx) {
	    'UTF-8';
	}
	when (/\A(?:[\x00-\x7f\xa1-\xdf]
	       |[\x80-\x9f\xe0-\xef][\x40-\x7e\x80-\xfd]
	       )*.?\z/sx) {
	    'Shift_JIS'; # SJIS
	}
	default {
	    'binary'; # unknown : assume binary (ASCII)
	}
    }
}

### output control

our $chareaten = 0;
our $charforward = 0;

sub uline {
    my $_ = $_[0];
    return $_ if $decorate != 2;
    return s/(.)/_\b$1\b$1/ugr;
}

{
    my $curdecorate = 0;
    sub put_decorate {
	my ($dec, $smpl, $n) = @_;
	$smpl //= ("." x length $_[0]);
	$n //= length($smpl);
	
	if ($decorate == 1) {
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
	    $chrbuf .= uline($dec);
	    if ($sp) {
		$chrbuf .= uline(" " x ($sp - 1));
		put_normal("", 0);
		$chrbuf .= " ";
	    }
	    $chreaten = $n - 1;
	    $chrforward = $l - 1;
#	    print "decorate: $dec for spc $n, strlen $ld, used_blocks $l, added_spaces $sp -> etn $chreaten fwd $chrforward\n";
	    $chrforward = 0 if $chrforward < 0; #for safety
	} else {
	    $chrbuf .= ($decorate ? uline($dec) : $smpl);
	    $chreaten = $chrforward = $n - 1;
	}
    }
    sub put_fill {
	return if $textbased;
	if ($decorate == 1) {
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
	$chrforward = $chareaten if $charforward > $chareaten;
	# output wider than available:
	# happens when ISO-8859-1 (or similar) is dumped to
	# CJK-width terminals.
    }
}

### input processing: general

sub process_char {
    my ($ofs, $code) = (@_);

    if ($chreaten) {
#	printf "ofs %x: eaten %d, forward %d\n", $ofs, $chreaten, $chrforward;
	$chreaten--;
	if ($chrforward) {
	    $chrforward--;
	} else {
	    put_fill (1);
	}
	return;
    }
    
    if (($code >= 0x20 && $code <= 0x7f)) {
	&$process_GL(@_);
    } elsif ($code < 0x20) {
	&$process_C0(@_);
    } elsif ($code >= $0x80) {
	&$process_GR(@_);
    }
}

sub process_C0_ASCII {
    my ($ofs, $code) = (@_);

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
	    if ($charbased > 1) {
		put_decorate('sp', undef, 1);
	    } else {
		put_normal(" ", 1);
	    }
	} else {
	    put_decorate(sprintf("%02x", $code), undef, 1);
	}
    } else {
	if ($code == 0x20) {
	    put_normal(" ", 1);
	} elsif ($textbased && $code == 0x0a) {
	    put_normal("\n", 1);
	} elsif ($code < 0x20) {
	    put_decorate(chr(0x40 + $code), ".");
	} elsif ($code == 0x7f) {
	    put_decorate("~", ".");
	} else {
	    put_decorate(".", ".");
	}
    }
}

sub process_GL_ASCII {
    my ($ofs, $code) = (@_);

    if ($code == 0x20 || $code == 0x7f) {
	process_C0_ASCII(@_);
    } else {
	put_normal(chr($code), 1);
    }
}

sub process_GR_none {
    my ($ofs, $code) = (@_);
    put_decorate(".", ".");
}

### input processing: wide char supports

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

sub put_maybe_fullwidth {
    my ($s, $n) = @_;
    if (length $s > 1 || substr($s, 1) eq "\x{fffd}") {
	put_decorate("〓" . (" " x ($n - 2)), "." x $n, $n);
    } else {
	put_normal($s, $n);
    }
}

### input processing: japanese specific

sub process_GR_SJIS {
    my ($ofs, $code) = (@_);
    if ($code >= 0xa1 && $code <= 0xdf) {
	put_normal($enc_SJ->decode(chr($code)), 1);
	return;
    }
    if ($code >= 0x80 && $code <= 0x9f || $code >= 0xe0 && $code <= 0xef) {
	my $next = get($ofs + 1);
	if ($next >= 0x40 && $next <= 0x7e || $next >= 0x80 && $next <= 0xfd) {
	    put_maybe_fullwidth($enc_932->decode(chr($code) . chr($next)), 2);
	    return;
	}
    }
    process_GR_none(@_);
}

sub process_GR_EucJP {
    my ($ofs, $code) = (@_);
    if ($code >= 0xa1 && $code <= 0xfe) {
	my $next = get($ofs + 1);
	if ($next >= 0xa1 && $next <= 0xfe) {
	    put_maybe_fullwidth($enc_EJ->decode(chr($code) . chr($next)), 2);
	    return;
	}
    }
    if ($code == 0x8e) {
	my $next = get($ofs + 1);
	if ($next >= 0xa1 && $next <= 0xdf) {
	    put_normal($enc_EJ->decode(chr($code) . chr($next)), 2);
	    return;
	}
    }
    if ($code == 0x8f) {
	my $next = get($ofs + 1);
	my $next2 = get($ofs + 2);
	put_maybe_fullwidth($enc_EJ->decode(chr($code) . chr($next). chr($next2)), 3);
	return;
    }
    process_GR_none(@_);
}

### input processing: utf-8 (with wide-char support)

sub process_GR_8859_1 {
    my ($ofs, $code) = (@_);
    if ($code >= 0xa1 && $code <= 0xfe) {
	put_normal(pack("U", $code), 1);
	return;
    }
    process_GR_none(@_);
}

sub process_GR_UTF8 {
    my ($ofs, $code) = (@_);

    SKIP_HICODE:
    {
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
	    my $ok = $s =~ /^\p{Print}$/;
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
    process_GR_none(@_);
}

### input processing: ISO-2022 handling

my @GLR;
my @G_init;
my @G;
my $SS;

sub process_init_junet {
    @GLR = (0, 1);
    @G_init = ('B', 'I', '', '');
    @G = @G_init;
    $SS = 0;
}

my %encode_cache;
my %I646table;

BEGIN {
    %encode_cache = ( '$A' => 'GB2312',
		      '$C' => 'EUC-KR',
#		      '$G' => 'EUC-TW', # only Big5 is included in pure Perl distribution
		      ',B' => 'ISO-8859-2',
		      ',C' => 'ISO-8859-3',
		      ',D' => 'ISO-8859-4',
		      ',F' => 'ISO-8859-7',
		      ',G' => 'ISO-8859-6',
		      ',H' => 'ISO-8859-8',
		      ',L' => 'ISO-8859-5',
		      ',M' => 'ISO-8859-9',
		      ',T' => 'ISO-8859-11',
		      ',V' => 'ISO-8859-10',
		      ',Y' => 'ISO-8859-13',
		      ',_' => 'ISO-8859-14',
		      ',b' => 'ISO-8859-15',
		      ',f' => 'ISO-8859-16',
		    );
    %I646table = 
      ('@' => q(#¤@[\]^_`{|}~), # 4/0  IRV old
       'A' => q(£$@[\]^_`{|}~), # 4/1  UK
       'B' => q(#$@[\]^_`{|}~), # 4/2  US-ASCII
       'G' => q(#¤@ÄÖÅ^_`äöå~), # 4/7  SE-B
       'H' => q(#¤ÉÄÖÅÜ_éäöåü), # 4/8  SE-C
       'J' => q(#$@[¥]^_`{|}~), # 4/10 JP
       'K' => q(#$§ÄÖÜ^_`ÄÖÜß), # 4/11 DE
       'L' => q(#¤§ÃÇÕ^_`ãçõ°), # 4/12 PT-o
       'R' => q(#£à°ç§^_µéùè¨), # 5/2  FR old
       'T' => q(#¥@[\]^_`{|}~), # 5/4  CN
       'Y' => q(#£§°çé^_ùàòèì), # 5/9  IT
       '`' => q(#$@ÆØÅ^_`ÆØÅ~), # 6/0  NO
       'a' => q(#§@ÆØÅ^_`ÆØÅ|), # 6/1  NO-2 old
       'f' => q(#£à°ç§^_µéùè¨), # 6/6  FR
       'g' => q(#¤´ÃÇÕ^_`ãçõ~), # 6/7  PT-i
       'h' => q(#$·¡ÑÇ¿_`´ñç¨), # 6/8  ES
       'i' => q(#¤ÁÉÖÜ^_áéöü˝), # 6/9  HU
       'w' => q(#$àâçêî_ôéùèû), # 7/7  CA1
       'x' => q(#$àâçêÉ_ôéùèû), # 7/8  CA2
       'z' => q(#$ŽŠĐĆČ_žšđćč), # 7/10 YU
       '!A'=> q(#¤@¡Ñ]¿_`´ñ[¨), # 2/1 4/1  CU
       '!C'=> q(£¤ÓÉÍÚÁ_óéíúá), # 2/1 4/3  IE
       );
}

sub get_encode_cache {
    my $key = $_[0];
    return undef unless exists $encode_cache{$key};
    my $e = $encode_cache{$key};
    unless (ref($e)) {
	my $enc = find_encoding($e) // die "Error: Can't load encoding $e";
#	print "Info: Loaded encoding $e\n";
	$e = $encode_cache{$key} = $enc;
    }
    return $e;
}

sub process_GLR_2022 {
    my ($ofs, $code) = (@_);

    my $is_GR = ($code >= 0x80 ? 1 : 0);

    if ($code == 0x8e || $code == 0x8f) {
	# SS2
	$SS = $code - 0x8c;
	put_fill (1);
	return;
    }
    if ($code >= 0x80 && $code < 0xA0) {
	# C1
	$SS = 0;
	process_GR_none(@_);
	return;
    }

    my $plane = $SS ? $SS : $GLR[($code > 0x80 ? 1 : 0)];
    $SS = 0;

    my $G = $G[$plane];
    my $c = $code & 0x7f;
    
#    printf "ofs %04x: plane %d, assign %s, code %02x (%02x)\n", $ofs, $plane, $G, $c, $code;
 #   $chrbuf .= sprintf("<%04x>>", $ofs);

    given ($G) {
	when (['B']) {
	    process_GL_ASCII($ofs, $c);
	}
	when (',A') {
	    # ISO-8859-1 GR
	    my $s = pack("U", 0x80 | $code);
	    put_normal($s, 1);
	}
	when ('I') {
	    if ($c == 0x20 && $is_GR == 0) {
		process_GL_ASCII($ofs, 0x20);
	    } elsif ($c >= 0x21 && $c <= 0x5f) {
		my $s = $enc_SJ->decode(chr(0x80 | $c));
#		printf "JIS_KANA: decode result is %s\n", join(".", unpack("U*", $s));
#		$chrbuf .= sprintf("[JK[");
		put_normal($s, 1);
#		$chrbuf .= sprintf("]]");
	    } else {
#		printf "JIS_KANA: out-of-bounds (%02x)\n", $c;
#		$chrbuf .= sprintf("[JKO[");
		put_decorate(".");
#		$chrbuf .= sprintf("]]");
	    }
	}
	when (['$B', '$@']) {
	    my $next = get($ofs + 1) & 0x7f;
	    if ($next >= 0x21 && $next <= 0x7e) {
		put_maybe_fullwidth($enc_EJ->decode(chr(0x80 | $c) . chr(0x80 | $next)), 2);
		$chreaten = $chrforward = 1;
	    } else {
		put_decorate(".");
	    }
	}
	when ('$D') {
	    my $next = get($ofs + 1) & 0x7f;
	    if ($next >= 0x21 && $next <= 0x7e) {
		put_maybe_fullwidth($enc_EJ->decode(chr(0x8f) . chr(0x80 | $c) . chr(0x80 | $next)), 2);
		$chreaten = $chrforward = 1;
		return;
	    } else {
		put_decorate(".");
	    }
	}
	when (/^!?[\x40-\x7f]$/) {
	    # ISO-646 or other 94-char set
	    my $table = $I646table{$_};
	    if (defined $table) {
		my $key = $I646table{B}; # US ASCII
		my $i = index($key, chr($code));
		if ($i != -1) {
		    put_normal(substr($table, $i, 1), 1);
		} else {
		    process_GL_ASCII($ofs, $c);
		}
	    } else {
		# TODO: other 94char codes
		put_decorate("x");
	    }
	}
	when (/^,[B-z]$/) {
	    # ISO-8859-? GR or other 96-char set
	    my $enc = get_encode_cache($G);
	    if ($enc) {
		my $s = $enc->decode(chr(0x80 | $code));
		if (length $s == 1 && $s ne '\x{fffd}') {
		    put_normal($s, 1);
		} else {
		    put_decorate(".");
		}
	    } else {
		put_decorate("x");
	    }
	}
	when (/^\$[AC]$/) { # G dropped
	    my $next = get($ofs + 1) & 0x7f;
	    if ($next >= 0x21 && $next <= 0x7e) {
		my $enc = get_encode_cache($G);
		put_maybe_fullwidth($enc->decode(chr(0x80 | $c) . chr(0x80 | $next)), 2);
		$chreaten = $chrforward = 1;
	    } else {
		put_decorate(".");
	    }
	}
#	when (/^\$[H-M]$/) {
#	    my $next = get($ofs + 1) & 0x7f;
#	    my $next2 = get($ofs + 2) & 0x7f;
#	    if ($next >= 0x21 && $next <= 0x7e) {
#		my $enc = get_encode_cache('$G'); # EUC-TW
#		put_maybe_fullwidth($enc->decode(chr(0xa1 + ord(substr($G, 2, 1)) - 0x48) . chr(0x80 | $c) . chr(0x80 | $next)), 2);
#		$chreaten = 2;
#		$chrforward = 1;
#	    } else {
#		put_decorate(".");
#	    }
#	}
	
	default {
	    put_decorate("x"); # "x"
	}
    }

#    $chrbuf .= sprintf("<<%04x,%d,%d>", $ofs, $chreaten, $chrforward);
}

sub process_C0_2022 {
    my ($ofs, $code) = (@_);

    $SS = 0;

    given ($code) {
	when ([0x10, 0x13, 0x11, 0x00]) {
	    if ($reset_2022_status) {
		# workaround for half-binary data:
		# reset shift statuses at each line-beginning
		@G = @G_init;
		@GLR = (0, 1);
	    }
	    process_C0_ASCII(@_);
	}
	when ([0x0e, 0x0f]) {
	    # LS0 (SO), LS1 (SI)
	    $GLR[0] = $code ^ 0x0f;
	    put_fill(1);
	}
	when (0x1b) {
	    my $n = 0;
	    my $targetC = get($ofs + 1);

	    given ($targetC) {
		when ([0x6e, 0x6f]) {
		    # LS2, LS3
		    $GLR[0] = $targetC & 3;
		    put_fill(1);
		    $chreaten = 1; $chrforward = 0;
		    return;
		}
		when ([0x7e, 0x7d, 0x7c]) {
		    # LS[123]R
		    $GLR[1] = 0x7f - $targetC;
		    put_fill(1);
		    $chreaten = 1; $chrforward = 0;
		    return;
		}
		when ([0x4e, 0x4f]) {
		    # SS2, SS3
		    $SS = $targetC & 3;
		    put_fill(1);
		    $chreaten = 1; $chrforward = 0;
		    return;
		}
	    }

	    my $target = -1;
	    my $assign = "";

	    if ($targetC >= 0x28 && $targetC <= 0x2f) {
	    
		$target = $targetC & 3;
		my $is_96char = ($targetC & 4) ? "," : "";

		my $setC = get($ofs + 2);
		if ($setC == 0x21) {
		    $setC = get($ofs + 3);
		    if ($setC >= 0x40 && $setC <= 0x7e) {
			$assign = $is_96char . '!' . chr($setC);
			$n = 4;
		    }
		} elsif ($setC >= 0x40 && $setC <= 0x7e) {
		    $assign = $is_96char . chr($setC);
		    $n = 3;
		}
	    } elsif ($targetC == 0x24) {
		my $targetC = get($ofs + 2);
		if ($targetC >= 0x40 && $targetC <= 0x42) {
		    $target = 0;
		    $assign = '$' . chr($targetC);
		    $n = 3;
		} elsif ($targetC >= 0x28 && $targetC <= 0x2b) {
		    my $setC = get($ofs + 3);
		    if ($setC >= 0x40 && $setC <= 0x7e) {
			$target = $targetC & 3;
			$assign = '$' . chr($setC);
			$n = 4;
		    }
		}
	    }
	    if ($n) {
		# printf "ISO-2022 SEQUENCE %d: ofs %x, target G%d, assign %s\n", $n, $ofs, $target, $assign;
		$G[$target] = $assign;
		put_fill(1);
		$chreaten = $n - 1;
		$chrforward = 0;
		return;
	    }
	    continue
	}
	default {
	    process_C0_ASCII(@_);
	}
    }
}

### input buffer

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


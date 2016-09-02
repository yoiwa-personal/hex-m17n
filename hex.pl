#!/usr/bin/perl -C0
# -*- coding: utf-8 -*-
# hexja.pl version 0.10 - hexadecimal dump tool for Japanese
# (c) 2016 Yutaka OIWA.

use Encode;
use utf8;
use strict;

use v5.10.0;
use feature qw( switch unicode_strings );
no if ($] >= 5.018), 'warnings', qw( experimental::smartmatch );

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
our $use_control_pictures = 0;
our $iso2022init = undef;

our %codings = 
  ( 'binary'    => [ "Binary input (ASCII only)",
		     \&process_GL_ASCII,  \&process_GR_none,   \&process_C0_ASCII ],
    'iso88591'  => [ "ISO 8859-1 (Latin-1)", # (2022: :fixed B ,A) (mb: latin1 80-ff)
		     \&process_GL_ASCII,  \&process_GR_8859_1, \&process_C0_ASCII ],
    'shiftjis'  => [ "Shift_JIS (Japanese)",
		     \&process_GL_ASCII,  \&process_GR_SJIS,   \&process_C0_ASCII ],
    'eucjp'     => [ "EUC-JP (Japanese)",                  # (2022: :fixed B $B I $D)
		     \&process_GL_ASCII,  \&process_GR_EucJP,  \&process_C0_ASCII ],
    'utf8'      => [ "UTF-8 encoding of ISO 10646-1",
		     \&process_GL_ASCII,  \&process_GR_UTF8,   \&process_C0_ASCII ],

    'iso2022'     => [ "ISO 2022 (neutral: GR default to ISO 8859-1)",
		     qw(2022: B ,A) ],
    'iso2022jp' => [ "ISO-2022-JP (other ISO-2022-KR, CN is also accepted)",
		     qw(2022: B I) ],
    'japaneseiso8' => [ "EUC-JP with ISO-2022 handling", qw(2022: B $B I $D) ],
    'iso88592' => [ "ISO 8859-2 (Latin-2)", qw(2022: :fixed B ,B) ],
    'iso88593' => [ "ISO 8859-3 (Latin-3)", qw(2022: :fixed B ,C) ],
    'iso88594' => [ "ISO 8859-4 (Latin-4)", qw(2022: :fixed B ,D) ],
    'iso88595' => [ "ISO 8859-5", qw(2022: :fixed B ,L) ],
#    'iso88596' => [ "ISO 8859-6", qw(2022: :fixed B ,G) ],
    'iso88597' => [ "ISO 8859-7", qw(2022: :fixed B ,F) ],
    'iso88598' => [ "ISO 8859-7", qw(2022: :fixed B ,H) ],
    'iso88599' => [ "ISO 8859-9 (Latin-5)", qw(2022: :fixed B ,M) ],
    'iso885910' => [ "ISO 8859-10 (Latin-6)", qw(2022: :fixed B ,V) ],
    'iso885913' => [ "ISO 8859-13 (Latin-7)", qw(2022: :fixed B ,Y) ],
    'iso885914' => [ "ISO 8859-14 (Latin-8)", qw(2022: :fixed B ,_) ],
    'iso885915' => [ "ISO 8859-15 (Latin-9)", qw(2022: :fixed B ,b) ],
    'iso885916' => [ "ISO 8859-16 (Latin-10)", qw(2022: :fixed B ,f) ],
    'euccn' => [ "EUC-CN (China (PRC), GB 2312)",
		  qw(2022: :fixed B $A) ],              # (mb: euc-cn a1-fe a1-fe)
    'euckr' => [ "EUC-KR (South Korean, KS X 1001)",
		  qw(2022: :fixed B $C) ],

    'big5' => [ "Big-5 (Taiwan - ROC)",
		qw(mb: big5 a1-c6,c9-f9 40-7e,a1-fe) ],

    'detect'    => [ "Automatic detection" ],
  );

our %coding_aliases = 
  ( 'ascii' => 'binary',
    'junet' => 'iso2022jp',
    'jis'   => 'iso2022jp',
    'ujis'  => 'eucjp',
    'sjis'  => 'shiftjis',
    'guess' => 'detect' );

GetOptions ("ascii|a"     => sub { set_input_coding('binary') },
	    "shift-jis|s" => sub { set_input_coding('shiftjis') },
	    "euc-jp|e"    => sub { set_input_coding('eucjp') },
	    "utf-8|u"     => sub { set_input_coding('utf8') },
	    "junet|jis|iso-2022-jp|j" => sub { set_input_coding('jis') },
	    "input-coding|i=s" => \&set_input_coding,
	    "list-coding" => \&list_input_coding,
	    "decorate|d!" => \$decorate,
	    "plaintext|p" => sub { $decorate = 0 },
	    "char|c:1"    => \$charbased,
	    "text|t"      => \$textbased,
	    "charwidth=i" => \$widthmethod,
	    "reset-status!" => \$reset_2022_status, # use with --no-reset-status
	    "help|h|?"    => sub { &usage() },
	    "use-control-pictures:1" => \$use_control_pictures,
	   ) or usage();

usage("-c and -t cannot be used together") if $charbased && $textbased;

sub usage {
    require FindBin;
    print "Error: $_[0]\n" if defined $_[0];

    printf <<'EOS', $FindBin::Script;
Usage: %s [options] [--] [input file]

Options:
  --ascii       -a  : input is ASCII or binary
                      (no special treatment for high codes)
  --shift-jis   -s  : input is Shift-JIS
  --euc-jp      -e  : input is EUC-JP
  --utf-8       -u  : input is UTF-8
  --iso-2022-jp -j  : input is ISO-2022-JP (also --jis, --junet)
                      (default : to guess from above four candidates)

  --input-coding=<coding> -i : set input coding
  --list-coding              : show the list of accepted input codings

  --decorate    -d  : use colorized output (default if output is tty)
  --no-decorate -p  : use plain-text output

  --char        -c  : put printable characters in binary dump (like "od -c")
  --char=2      -c2 :   show space as "sp"
  --text        -t  : print as plain text, quoting non-printable as hex dump

  --charwidth=n     : use internal character-width calculation
                      "n" (1..2) specifies width of "ambiguous" characters
                      (default: use locale settings (also when n = 0))
  --help        -h  : show this help
EOS
    exit 1;
}

sub list_input_coding {
    my %r = ();
    for (sort keys %coding_aliases) {
	$r{$coding_aliases{$_}} //= [];
	push @{$r{$coding_aliases{$_}}}, $_;
    }
    
    print "List of supported input codings:\n";

    for (sort(keys(%codings))) {
	printf("  %-13s: %s\n", $_, $codings{$_}->[0]);
	printf("        %s\n", 
	       "(" . 
	       (@{$r{$_}} >= 2 ? "aliases" : "alias") .
	       " : " .
	       join(", ", @{$r{$_}}) . ")") if $r{$_};
    }
    print "\n(cases, spaces and hyphens are ignored: e.g. ISO-8859-1 is accepted)\n";
    exit 0;
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

my ($process_GL, $process_GR, $process_C0, $process_init, @process_init_args);

{
    my @a;

    if (defined $iso2022init) {
	@a = ('2022:', split(/ +/, $iso2022init));
    } else {
	if ($incode eq 'detect') {
	    # input encoding guess (detection)
	    read(IN, $buf, BUFSIZE * 8) // die "read error: $!"; # 0 is OK, undef is error

	    my $inc = detect_coding($buf);
	    printf "# Detected coding: %s\n", $inc if (($buf ne '') && (!$textbased));
	    set_input_coding($inc);
	}
	@a = @{$codings{$incode}};
	shift @a;
    }

    if (ref $a[0]) {
	($process_GL, $process_GR, $process_C0, $process_init, @process_init_args) = @a;
    } elsif ($a[0] eq '2022:') {
	shift @a;
	($process_GL, $process_GR, $process_C0, $process_init, @process_init_args) = 
	  (\&process_GLR_2022, \&process_GLR_2022, \&process_C0_2022, \&process_init_2022, @a);
    } elsif ($a[0] eq 'mb:') {
	shift @a;
	($process_GL, $process_GR, $process_C0, $process_init, @process_init_args) = 
	  (\&process_GL_ASCII, \&process_GR_mb, \&process_C0_ASCII, \&process_init_mb, @a);
    } else {
	die "internal error: unknown coding system definition";
    }
}

&$process_init(@process_init_args) if defined $process_init;

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
    unless ($charbased || $textbased) {
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

    if ($coding =~ /\A([iI][sS][oO]-?2022)?\((.+)\)\z/) {
	$incode = 'iso2022';
	$iso2022init = $2;
	return;
    }
    my $c = lc $coding;
    $c =~ s/[_-]//g;
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
	when (m#\e\$?[\$(-/][A-~\@]
		[\x0e\x0f\x1b\x20-\x7e\x8e\x8f\xa0-\xff]+
		(?:\e\([ABGHJ-LRTY`afghiwx\@]|\n)#sx) {
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

sub uline_decorate {
    local $_ = $_[0];
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
	    $chrbuf .= uline_decorate($dec);
	    if ($sp) {
		$chrbuf .= uline_decorate(" " x ($sp - 1));
		put_normal("", 0);
		$chrbuf .= " ";
	    }
	    $chreaten = $n - 1;
	    $chrforward = $l - 1;
#	    print "decorate: $dec for spc $n, strlen $ld, used_blocks $l, added_spaces $sp -> etn $chreaten fwd $chrforward\n";
	    $chrforward = 0 if $chrforward < 0; #for safety
	} else {
	    $chrbuf .= ($decorate ? uline_decorate($dec) : $smpl);
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
	my ($s, $n) = @_;
	if ($curdecorate != 0) {
	    $chrbuf .= "\e[0m";
	    $curdecorate = 0;
	}
	return if $s eq '';
	die if $n == 0;
	# $chrforward = length($sj) - 1;
	my $width = wcwidth($s);
	if ($width == 2) {
	    $chrforward = 1;
	} elsif ($width == 0) {
	    $chrforward = 0;
	    $s = " $s";
	} else {
	    $chrforward = 0;
	}
	if ($s =~ /\p{Bidi_class: R}/ && !$textbased) {
	    $s = "\x{202d}$s\x{2069}"; # force LTR direction in char dump.
	}
	$chreaten = $n - 1;
	if ($charbased) {
	    $chrbuf .= $s . (" " x (2-$chrforward));
	    $chrforward = 0;
	} else {
	    $chrbuf .= $s;
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
	#printf "ofs %x: eaten %d, forward %d\n", $ofs, $chreaten, $chrforward;
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

my @C0_abbrs;
BEGIN {
    @C0_abbrs = 
      ('\0^A^B^C^D^E^F\a\b\t\n\v\f\r^N^O^P^Q^R^S^T^U^U^V^W^X^Y^Z^[^\^]^^sp^?',
       '                BSHTLFVTFFCRSOSI                  EM    FSGSRSUSSP  ',
       'NUSHSXEXETEQAKBLBSHTLFVTFFCRSOSIDED1D2D3D4NKSNEBCNEMSBECFSGSRSUSSPDL') ; # non-standard
}

sub process_C0_ASCII {
    my ($ofs, $code) = (@_);

    if ($charbased) {
	if ($code < 0x20 || 
	    ($code == 0x20 && $charbased > 1) ||
	    $code == 0x7f) {
	    my $c = $code == 0x7f ? 33 : $code;
	    my $s = substr($C0_abbrs[$use_control_pictures], $c * 2, 2);
	    if ($s eq '  ') {
		put_decorate(chr(0x2400 + $code), undef, 1);
	    } else {
		put_decorate($s, undef, 1);
	    }
	} elsif ($code == 0x20) {
	    put_normal(" ", 1);
	} else {
	    put_decorate(sprintf("%02x", $code), undef, 1);
	}
    } else {
	if ($code == 0x20) {
	    put_normal(" ", 1);
	} elsif ($textbased && ($code == 0x0a || $code == 0x09)) {
	    put_normal(chr($code), 1);
	} elsif ($code < 0x20) {
	    if ($use_control_pictures) {
		put_decorate(chr(0x2400 + $code), chr(0x2400 + $code));
	    } else {
		put_decorate(chr(0x40 + $code), ".");
	    }
	} elsif ($code == 0x7f) {
	    put_decorate("?", ".");
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

    if ($charbased) {
	put_decorate(sprintf("%02x", $code), undef, 1);
    } else {
	put_decorate(".", ".");
    }
}

sub process_GR_8859_1 {
    my ($ofs, $code) = (@_);
    if ($code >= 0xa0 && $code <= 0xff) {
	put_normal(pack("U", $code), 1);
	return;
    }
    process_GR_none(@_);
}

### input processing: wide char supports

{
    use Text::CharWidth;

    my $chrattr_cache = "";
    # 0x80 -> checked
    # 0x40 -> char printable class
    # 0x20 -> mbwidth available
    # 0x03 -> char width (0-2)

    sub check_char_attr {
	my $char = substr($_[0], 0, 1);
	my $code = ord($char);
	my $r;
	if ($code < 0x10000) {
	    my $r = vec($chrattr_cache, $code, 8);
	    return $r if $r;
	}
	$r = 0x80; # checked
	$r |= 0x40 if $char =~ /^\p{Print}$/;
	
	my $skip_classcheck = 0;
	
	if ($widthmethod == 0) {
	    my $v = Text::CharWidth::mbwidth($char);
	    if ($v >= 0 && $v < 4) {
		$r |= (0x20 | $v);
		$skip_classcheck = 1;
	    }
	} 
	unless ($skip_classcheck) {
	    $r |= 0x20 if ($widthmethod != 0) && ($r & 0x40 != 0);
	    
	    if (/\p{Block: Combining_Diacritical_Marks}
	     |\p{Block: Combining_Diacritical_Marks_For_Symbols}
	     |\p{Block: Combining_Half_Marks}
	     |\p{Block: Combining_Marks_For_Symbols}
	     |\p{Block: Combining_Diacritical_Marks_For_Symbols}/x) {
	    } elsif (/\p{Ea: W}|\p{Ea: F}/) {
		$r |= 2;
	    } elsif ($widthmethod >= 2 && /\p{Ea: A}/) {
		$r |= 2;
	    } else {
		$r |= 1;
	    }
	}
	if ($code < 0x10000) {
	    vec($chrattr_cache, $code, 8) = $r;
	}
	#printf "DEBUG: checked code U+%x -> %b\n", $code, $r;
	return $r;
    }
}

sub wcwidth {
    return 0 if $_[0] eq "";
    return 1 if ord($_[0]) < 0x80;
    return check_char_attr($_[0]) & 3;
}

sub is_printable_to_terminal {
    return (check_char_attr($_[0]) & 0x60) == 0x60;
}

sub put_maybe_fullwidth {
    my ($s, $n) = @_;
    if (length $s > 1 || substr($s, 0, 1) eq "\x{fffd}") {
	put_decorate("〓" . (" " x ($n - 2)), "." x $n, $n);
    } else {
	put_normal($s, $n);
    }
}

### input processing: Japanese specific

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
	    if (is_printable_to_terminal($s)) {
#		printf "debug: char %x is printable\n", $c;
		my $outs = $s;
		put_normal($s, $n);
	    } else {
#		printf "debug: char %x is NOT printable\n", $c;
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

### input processing: ISO/IEC 2022 and compliant encodings

{
    my %encode_cache;
    my %I646table;


    my @GLR_init;
    my @G_init;
    my @GLR;
    my @G;
    my $SS;
    my ($allow_LS, $allow_SS, $allow_designate);

    BEGIN {
	%encode_cache = ( '$A' => 'GB2312',
			  '$B' => 'EUC-JP',     # not used: directly implemented
			  '$C' => 'EUC-KR',
			  # '$G' => 'EUC-TW', # only Big5 is included in pure Perl distribution
			  ',A' => 'ISO-8859-1', # not used: directly implemented
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
	# Note: Technically, $B corresponds to jisx0208-raw, $C to KSC5601-raw, etc.
	# We need to use FULLWIDTH variants for ASCII-conflicting characters, and
	# current Perl implementation of *-raw encodings _actually_ does that,
	# against the mapping definitions of Unicode Consortium.
	# However, to make this doubly-sure for any future,
	# we use EUC-encoded tables here.
	
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

    sub process_init_2022 {
	my @p = @_;
	
	$allow_SS = $allow_LS = $allow_designate = 1;
	
	while ($p[0] =~ /^:/) {
	    given(shift @p) {
		when (':noshift') {
		    $allow_SS = $allow_LS = 0;
		}
		when (':noSS') {
		    $allow_SS = 0;
		}
		when (':noLS') {
		    $allow_LS = 0;
		}
		when (':nodesignate') {
		    $allow_designate = 0;
		}
		when (':fixed') {
		    $allow_SS = $allow_LS = $allow_designate = 0;
		}
		when (':noreset') {
		$reset_2022_status = 0; # regardless of cmdline options
	    }
		default {
		    die "Error: unknown flag for ISO-2022 initialization: $_\n";
		}
	    }
	}
	@GLR_init = (0, 1);
	if ($p[0] =~ /^[0123]$/) {
	    $GLR_init[0] = shift @p;
	    if ($p[0] =~ /^[0123]$/) {
		$GLR_init[1] = shift @p;
	    }
	}
	die "Error: too many codeset for ISO-2022 initialization\n" if (@p > 4);
    
	@G_init = ('B', '', '', '');
	for (0 .. 3) {
	    my $g = $p[$_];
	    last unless defined $g;
	    die "Error: bad ISO2022 specifier for G$_: $g\n" unless $g =~ /^[\,\$]?[\@-\~]$/s;
	    $G_init[$_] = $g;
	}

	@GLR = @GLR_init;
	@G = @G_init;
	$SS = 0;
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

	if (($code == 0x8e || $code == 0x8f) && $allow_SS) {
	    # SS2/3
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
	    when ('B') {
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
		} else {
		    put_decorate(".");
		}
	    }
	    when ('$D') {
		my $next = get($ofs + 1) & 0x7f;
		if ($next >= 0x21 && $next <= 0x7e) {
		    put_maybe_fullwidth($enc_EJ->decode(chr(0x8f) . chr(0x80 | $c) . chr(0x80 | $next)), 2);
		    return;
		} else {
		    put_decorate(".");
		}
	    }
	    when (/^[\x21-\x23]?[\x40-\x7e]$/) {
		# ISO-646 or other 94-char set
		my $table = $I646table{$_};
		if (defined $table) {
		    my $key = $I646table{B}; # US ASCII
		    my $i = index($key, chr($c));
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
	    when (/^,[\x42-\x7e]$/) {
		# ISO-8859-? GR or other 96-char set
		my $enc = get_encode_cache($G);
		if ($enc) {
		    my $s = $enc->decode(chr(0x80 | $code));
		    if (length $s == 1 && $s ne "\x{fffd}") {
			put_normal($s, 1);
		    } else {
			put_decorate(".");
		    }
		} else {
		    put_decorate("x");
		}
	    }
	    when (/^\$[AC]$/) { # G (EUC-TW) dropped as optional in Perl
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
		    @GLR = @GLR_init;
		}
		process_C0_ASCII(@_);
	    }
	    when ([0x0e, 0x0f]) {
		# LS0 (SO), LS1 (SI)
		continue unless $allow_LS;
		$GLR[0] = $code ^ 0x0f;
		put_fill(1);
	    }
	    when (0x1b) {
		my $n = 0;
		my $targetC = get($ofs + 1);

		given ($targetC) {
		    when ([0x6e, 0x6f]) {
			# LS2, LS3
			break if !$allow_LS;
			$GLR[0] = $targetC & 3;
			put_fill(1);
			$chreaten = 1; $chrforward = 0;
			return;
		    }
		    when ([0x7e, 0x7d, 0x7c]) {
			# LS[123]R
			break if !$allow_LS;
			$GLR[1] = 0x7f - $targetC;
			put_fill(1);
			$chreaten = 1; $chrforward = 0;
			return;
		    }
		    when ([0x4e, 0x4f]) {
			# SS2, SS3
			break if !$allow_SS;
			$SS = $targetC & 3;
			put_fill(1);
			$chreaten = 1; $chrforward = 0;
			return;
		    }
		    when ([0x20]) {
			# announcer
			break if !$allow_designate;
			my $codeA = get($ofs + 2);
			if ($codeA >= 0x41 && $codeA <= 0x5C) {
			    put_fill(1);
			    $chreaten = 2; $chrforward = 0;
			    return;
			}
			break;
		    }
		}

		if ($allow_designate) {
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
			#printf "ISO-2022 SEQUENCE %d: ofs %x, target G%d, assign %s\n", $n, $ofs, $target, $assign;
			$G[$target] = $assign;
			put_fill(1);
			$chreaten = $n - 1;
			$chrforward = 0;
			return;
		    }
		}
		continue
	    }
	    default {
		process_C0_ASCII(@_);
	    }
	}
    }
}

### input processing: non ISO/IEC 2022 encodings with fixed-length multi-byte block

{
    my @mb_input_range;
    my $mb_input_enc;

    sub process_init_mb (@) {
	@mb_input_range = ();
	my $enc = shift @_;

	$mb_input_enc = find_encoding($enc) // die "internal error: can't find encoding $enc";

	for (@_) {
	    my @a = split(",", $_);
	    my @o = ();
	    for (@a) {
		m/^([0-9a-fA-F]{2})-([0-9a-fA-F]{2})\z/s or die "internal error: bad code spec";
		my ($f, $t) = (hex($1), hex($2));
#		printf "%d-%d: %02x-%02x\n", scalar(@mb_input_range), scalar(@o) / 2, $f, $t;
		die "internal error: bad code spec" if ($f > $t);
		die "internal error: bad code spec" if (scalar(@mb_input_range) == 0 && $f < 0x80);
		push @o, hex($1), hex($2);
	    }
	    die "internal error: bad code spec" unless @o;
	    push @mb_input_range, \@o;
	}
	die "internal error: bad code spec" unless @mb_input_range;
    }

    sub process_GR_mb ($$) {
	my ($ofs, $code) = (@_);
	my $s = '';

	OUT_OF_RANGE: {
	    SCAN:
	      for my $n (0 .. (@mb_input_range - 1)) {
		  my $c = get($ofs + $n) // last OUT_OF_RANGE;

#		  printf "mb: ofs %x+%x, char %02x\n", $ofs, $n, $c;
		  my @r = @{$mb_input_range[$n]};
#		  printf "mb: ofs %x+%x, spec %s\n", $ofs, $n, join("-", @r);
		  while (@r) {
		      my ($f, $t) = (shift(@r), shift(@r));
#		      printf "mb: ofs %x+%x, char %02x, range %02x-%02x\n", $ofs, $n, $c, $f, $t;
		      if ($f <= $c && $c <= $t) {
#			  printf "mb: ofs %x+%x, char %02x OK\n", $ofs, $n, $c;
			  $s .= chr($c);
			  next SCAN;
		      }
		  }
		  last OUT_OF_RANGE;
	      }
	      die if length $s != scalar @mb_input_range;

	      put_maybe_fullwidth($mb_input_enc->decode($s), length $s);
	      return;
	  }
#	printf "mb: ofs %x considered out-of-range\n", $ofs;
	process_GR_none(@_);
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


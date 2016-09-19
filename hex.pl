#!/usr/bin/perl -C0
# -*- coding: utf-8 -*-
# hex.pl version 0.30 - multi-locale hexadecimal dump tool
# (c) 2016 Yutaka OIWA.
# $Id: hex.pl,v 1.17 2016/09/11 01:03:51 yutaka Exp yutaka $

use Encode qw(:default FB_QUIET);
use utf8;
use strict;

use v5.10.0;
use feature qw( switch unicode_strings );
no if ($] >= 5.018), 'warnings', qw( experimental::smartmatch );

use constant ("is_DOSish", $^O =~ /^(?:MSWin32|cygwin|dos)\z/s);

# debugging
my $dlevel = 0;

sub debug ();

BEGIN {
    if (scalar(grep{ m/^--debug(?:=.+)?$/ } @ARGV)) {
	*debug = sub () { $dlevel };
	eval 'use Data::Dumper;
              $Data::Dumper::Useqq = $Data::Dumper::Terse = 1;
              $Data::Dumper::Quotekeys = 0;'; die "$@" if "$@";
    } else {
        *debug = sub () { 0 };
	*Dumper = sub (@) { die 'Dumper is not loaded (no debugging)' }
    }
    # usage: debug&n&& printf(...);
    # allocation of the bit flag <n>:
    #   2: encoding loading
    #   4: locale
    #   8: charwidth
    #  16: terminfo/termcap

    # Without HEXDEBUG or --debug, the whole debugging code will be optimized out.
}

our $enc_SJ = find_encoding("Shift_JIS") // die;
our $enc_932 = find_encoding("CP932") // die;
our $enc_EJ = find_encoding("EUC-JP") // die;
our $enc_U8 = find_encoding("UTF8") // die;

our ($enc_locale, $locale_coding);
our ($cjk_region);

use constant { BUFSIZE => 1024 };
use Getopt::Long qw(:config bundling require_order);

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
our $outcode = 'detect';
our $output_utf8 = 0;
our $output_cp932 = 0;
our $decorate = 2;
our $outenc = undef;
our $reset_2022_status = 1;
our $use_control_pictures = 0;

# terminal attributes
our ($color_sequence_d, $color_sequence_f, $color_sequence_n, $color_colorful);

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

    'iso2022'     => [ "ISO 2022 (neutral: useful for -KR, -CN as well)",
		     qw(2022: B) ],
    'iso2022u'     => [ "ISO 2022 with transition to UTF-8",
		     qw(2022: :utf_ok :noreset B ,A) ],
    'iso2022jp' => [ "ISO-2022-JP (GR is JIS x0201 Kana)",
		     qw(2022: B I) ],
    'japaneseiso8' => [ "EUC-JP with ISO-2022 handling", qw(2022: B $B I $D) ],
    'iso88592' => [ "ISO 8859-2 (Latin-2)", qw(2022: :fixed B ,B) ],
    'iso88593' => [ "ISO 8859-3 (Latin-3)", qw(2022: :fixed B ,C) ],
    'iso88594' => [ "ISO 8859-4 (Latin-4)", qw(2022: :fixed B ,D) ],
    'iso88595' => [ "ISO 8859-5", qw(2022: :fixed B ,L) ],
#    'iso88596' => [ "ISO 8859-6", qw(2022: :fixed B ,G) ],
    'iso88597' => [ "ISO 8859-7", qw(2022: :fixed B ,F) ],
    'iso88598' => [ "ISO 8859-8", qw(2022: :fixed B ,H) ],
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
    'euctw' => [ "EUC-TW (Taiwan -ROC, CNS-11643)",
		 \&process_GL_ASCII,  \&process_GR_EucTW, \&process_C0_ASCII, \&process_init_eucTW ],

    'detect'    => [ "Automatic detection" ],
  );

our %coding_aliases =
  (
   # altenative keywords
   'ascii' => 'binary',
   'junet' => 'iso2022jp',
   'jis'   => 'iso2022jp',
   'ujis'  => 'eucjp',
   'sjis'  => 'shiftjis',
   'guess' => 'detect',
   'gb2312' => 'euccn',
   
   # mnemonics
   'j'      => 'iso2022jp',
   'ij'     => 'iso2022jp',
   'ej'     => 'eucjp',
   'sj'     => 'shiftjis',
   'ek'     => 'euckr',
   'ec'     => 'euccn',
   'b5'     => 'big5',
   'u'      => 'utf8',
   'l1'     => 'iso88591',
   'latin1' => 'iso88591',
   '2022'   => 'iso2022',
   'ik'     => 'iso2022',
   'ic'     => 'iso2022',
  );

our ($process_GL, $process_GR, $process_C0, $process_init, @process_init_args);


### Main routine

sub main () {
    # initialize
    GetOptions ("ascii|a"          => sub { set_input_coding('binary') },
		"shift-jis|s"      => sub { set_input_coding('shiftjis') },
		"euc-jp|ujis|e"    => sub { set_input_coding('eucjp') },
		"utf-8|u"          => sub { set_input_coding('utf8') },
		"iso-2022-jp|jis|junet|j" => sub { set_input_coding('jis') },
		"input-coding|i=s" => \&set_input_coding,
		"output-coding=s"  => \$outcode,
		"list-coding"      => \&list_input_coding,
		"decorate|d!"      => \$decorate,
		"plaintext|p"      => sub { $decorate = 0 },
		"char|c:1"         => \$charbased,
		"text|t"           => \$textbased,
		"charwidth=i"      => \$widthmethod,
		"reset-status=i"   => \$reset_2022_status,
		"no-reset-status"  => sub { $reset_2022_status = 0 },
		"help|h|?"         => sub { &usage() },
		"use-control-pictures:1" => \$use_control_pictures,
		"cjk-region=s"     => \$cjk_region,
		"debug=i"          => \$dlevel,
	       ) or usage('');

    usage("-c and -t cannot be used together") if $charbased && $textbased;

    if (defined $ARGV[1]) {
	die "too many files specified.";
    }
    if (!defined $ARGV[0] || $ARGV[0] eq '-') {
	*IN = *STDIN{IO};
    } else {
	open IN, "<:raw", $ARGV[0] or die "open: $!";
    }
    binmode(IN);

    set_output_coding($outcode);

    setup_color_sequences();

    if ($decorate == 2 && $is_tty) {
	$decorate = 1;
    }
    if ($decorate == 2 && !$is_tty && $color_sequence_n eq '') {
	$decorate = 0;
    }

    {
	my @a;

	if (ref($incode) eq 'ARRAY' && $incode->[0] eq 'iso2022') {
	    @a = ('2022:', split(/ +/, $incode->[1]));
	} else {
	    if ($incode eq 'detect') {
		# input encoding guess (detection)
		read(IN, $buf, BUFSIZE * 8) // die "read error: $!"; # 0 is OK, undef is error

		my $inc = detect_coding($buf);
		printf "# Detected coding: %s\n", $inc if (($buf ne '') && (!$textbased || !!($dlevel & 4)));
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

    die "Write failed: $!" if STDOUT->error;

    # main loop

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
	    print $chrbuf                                       or last;
	} else {
	    printf "%08x:%s  %s\n", $addr, $binbuf, $chrbuf     or last;
	}
	$chrforward = 0;
	last if $o != 16;
	$addr += 16;
    }
    die "Write failed: $!" if STDOUT->error;

    # cleaning up

    $chrbuf = "";
    put_normal("", 0); # if $textbased
    print "$chrbuf";

    close STDOUT or die "Write failed: $!"; # catch any errors
    exit 0;
}

sub usage {
    require FindBin;
    my $ret = 1;

    local *OUTPUT;

    if (defined $_[0]) {
	*OUTPUT = *STDERR;
	print OUTPUT "Error: $_[0]\n\n" if $_[0] ne '';
    } else {
	*OUTPUT = *STDOUT;
	$ret = 0;
    }
    printf OUTPUT <<'EOS', $FindBin::Script;
Usage: %s [options] [--] [input file]

Options:
  --ascii       -a  : input is ASCII or binary
                      (no special treatment for high codes)
  --shift-jis   -s  : input is Shift-JIS
  --euc-jp      -e  : input is EUC-JP
  --utf-8       -u  : input is UTF-8
  --iso-2022-jp -j  : input is ISO-2022-JP (also --jis, --junet)
                      (default : to guess)

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
    exit $ret;
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

### input coding parameter handlings

sub set_input_coding {
    my $coding = $_[-1]; # called both from internally and from GetOptions

    if ($coding =~ /\A([iI][sS][oO]-?2022)?\((.+)\)\z/) {
	$incode = ['iso2022', $2];
	return;
    }
    my $c = lc $coding;
    $c =~ s/[_-]//g;
    $c = $coding_aliases{$c} if exists $coding_aliases{$c};
    unless (exists $codings{$c}) {
	usage("unknown coding: $coding ($c)" .
	      (@_ == 2 ? "\n  (try --list-coding for accepted values)" : ""));
    }
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
		(?:\e\([ABGHJ-LRTY\`afghiwx\@]|\n)#sx) {
	    if (m|\e%G| && m|\e%@|) {
		'ISO-2022-U'
	    } else {
		'ISO-2022'
	    }
	}
	when (/\A(?:[\x00-\x7f])*\z/sx) {
	    'ASCII'
	      # internally "OK" with EUC-* below, but for better displaying
	}
	when ($cjk_region ~~ ['KR','CN'] &&
	      /\A(?:[\x00-\x7f]
	       |[\xa1-\xfe][\xa1-\xfe]
	       )*.?\z/sx) {
	    "EUC-$cjk_region"
	}
	when ($cjk_region eq 'TW' &&
	      /\A(?:[\x00-\x7f]
	       |[\xa1-\xfe][\xa1-\xfe]
	       |\x8e[\xa2-\xaf][\xa1-\xfe][\xa1-\xfe]
	       )*.{0-3}\z/sx) {
	    'EUC-TW'
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
	when ($cjk_region eq 'TW' &&
	      /\A(?:[\x00-\x7f\xa1-\xdf]
	       |[\xa1-\xc6\xc9-\xf9][\x40-\x7e\xa1-\xfe]
	       )*.?\z/sx) {
	    'big5';
	}
	when (/\A(?:[\x00-\x7f\xa1-\xdf]
	       |[\x80-\x9f\xe0-\xef][\x40-\x7e\x80-\xfd]
	       )*.?\z/sx) {
	    'Shift_JIS';
	}
	when (/\A[\x00-\x7f\xa0-\xff]+\z/sx) {
	    'ISO-8859-1';
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
		$chrbuf .= $color_sequence_d;
		$curdecorate = 1;
	    }
	}
	if ($charbased) {
	    my $ld = length($dec);
	    $ld++ if $dec =~ /\p{Ea: W}|\p{Ea: F}/;
	    my $l = int((length($dec) + 2) / 3);
	    my $sp = $l * 3 - $ld;
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
	    my $s = ($decorate ? $dec : $smpl);
	    my $ld = length($s);
	    $ld++ if $dec =~ /\p{Ea: W}|\p{Ea: F}/;
	    $chrbuf .= ($decorate ? uline_decorate($dec) : $smpl);
	    $chreaten = $n - 1;
	    $chrforward = $ld - 1;
	    #print "decorate: $dec for spc $n, strlen $ld -> etn $chreaten fwd $chrforward\n";
	}
    }

    sub put_fill {
	return if $textbased;
	if ($decorate == 1) {
	    if ($curdecorate != 2) {
		$chrbuf .= $color_sequence_f;
		$curdecorate = 2;
	    }
	    if ($color_colorful) {
		$chrbuf .= ($charbased ? "__ " : "_") x $_[0];
	    } else {
		$chrbuf .= ($charbased ? "   " : " ") x $_[0];
	    }
	} else {
	    $chrbuf .= ($charbased ? "   " : " ") x $_[0];
	}
    }

    sub put_ucs_nonprintable {
	my ($c, $n) = @_;
	if ($charbased) {
	    put_decorate(sprintf("U+%X", $c), undef, $n);
	} else {
	    put_decorate("." x $n);
	}
    }

    sub put_normal {
	my ($s, $n) = @_;
	my $o = $s;
	if ($curdecorate != 0) {
	    $chrbuf .= $color_sequence_n;
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
	    $o = " $o";
	} else {
	    $chrforward = 0;
	}
	if (ord($s) >= 0x20 && !is_printable_to_terminal($s)) {
	    put_ucs_nonprintable(ord($s), $n);
	    return;
	}
	if ($s =~ /\p{Bidi_class: R}/ && !$textbased) {
	    $o = "\x{202d}$o\x{2069}"; # force LTR direction in char dump.
	}
	$chreaten = $n - 1;
	if ($charbased) {
	    $chrbuf .= $o . (" " x (2 - $chrforward));
	    $chrforward = 0;
	} else {
	    $chrbuf .= $o;
	}
	$chrforward = $chareaten if $charforward > $chareaten;
	# output wider than available:
	# happens when ISO-8859-1 (or similar) is dumped to
	# CJK-width terminals.
    }
}

### output: coding

{
    my $inited = 0;

    sub determine_locale_encoding () {
	if (!$inited) {
	    $enc_locale = undef;
	    $locale_coding = undef;
	    my $err = "";
	    {
		my $l = undef;
		eval {
		    require I18N::Langinfo;
		    $l = I18N::Langinfo::langinfo (I18N::Langinfo::CODESET());
		};
		if ($@ || !$l) {
		    $err = "langinfo() does not return coding system.";
		} else {
		    $locale_coding = $l;
		}
	    }
	    if (!$locale_coding && $^O eq 'MSWin32') {
		require Win32;
		my $CP = Win32::GetConsoleOutputCP();
		if ($CP) {
		    $locale_coding = "cp$CP";
		} else {
		    $err = "Console codepage cannot be acquired";
		}
	    }
	    if ($locale_coding) {
		my $l = $locale_coding;
		if ($enc_locale = find_encoding($locale_coding)) {
		    $locale_coding = $enc_locale->name();
		    $locale_coding = 'utf-8' if $locale_coding == 'utf-8-strict';
		    debug&4 and printf "LOCALE ENCODING: %s (%s)\n", $locale_coding, $l;
		    detemine_cjk_region($locale_coding);
		    debug&4 and printf "LOCALE ENCODING: %s (%s), REGION %s\n", $locale_coding, $l, $cjk_region;
		} else {
		    warn "Encoding of locale ($l) is not available\n";
		    $locale_coding = undef;
		    $enc_locale = undef;
		    debug&4 and printf "LOCALE ENCODING: -- (%s)\n", $l;
		}
	    } else {
		warn $err;
		debug&4 and printf "LOCALE ENCODING: --\n";
	    }
	    $inited = 1;
	}
	return ($enc_locale, $locale_coding);
    }

    sub detemine_cjk_region {
	return $cjk_region if $cjk_region;

	my $encname = $_[0];
	my $locale = $ENV{LC_CTYPE} || $ENV{LC_ALL} || $ENV{LANG} || '-';

	$cjk_region = do { given ("$encname\0$locale") {
	    when (m@^euc-jp\0|^cp932\0|^shiftjis\0|\0ja(_.*)$@si) {
		"JP";
	    }
	    when (m@^euc-kr\0|^cp949\0|\0ko(_.*)$@si) {
		"KR";
	    }
	    when (m@^euc-cn\0|^cp936\0|\0zh_cn(\..*)$|\0zh_hk(\..*)$@si) {
		"CN";
	    }
	    when (m@^euc-tw\0|^big5\0|^cp950\0|\0zh_TW(\..*)$@si) {
		"TW";
	    }
	    default {
		"-";
	    }
	}};

	return $cjk_region;
    }
}

sub set_output_coding ($) {
    my ($coding) = @_;

    my ($enc_locale, $locale_coding) = determine_locale_encoding();

    if ($coding ne 'detect') {
	debug&4 and print "requested output coding: $coding\n";
	$outenc = find_encoding($coding) // die "Error: cannot find output coding $coding\n";
	$coding = $outenc->name(); # canonify for possible coding-dependent tweaks
    } elsif ($locale_coding) {
	$outenc = $enc_locale;
	$coding = $locale_coding;
    } else {
	$outenc = $enc_U8;
	$coding = "UTF-8";
    }
    $coding = 'utf-8' if $coding eq 'utf-8-strict';
    
    binmode (STDOUT);
    binmode (STDOUT, ":crlf") if is_DOSish;
    if ($coding =~ /\Autf-8\z/i) {
	binmode (STDOUT, ":utf8");
	$output_utf8 = 1;
	$output_cp932 = 0;
    } else {
	binmode (STDOUT, ":encoding($coding)") // return undef;
	$output_utf8 = 0;
	$output_cp932 = $coding eq 'cp932';
    }
    debug&4 and printf "OUTPUT ENCODING: $coding\n";
    return;
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
    $chrforward = 0; # for safety; space overrun is silently ignored

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
    my $charwidth_available = undef;

    my $chrattr_cache = "";
    # 0x80 -> checked
    # 0x40 -> char printable class
    # 0x20 -> mbwidth available
    # 0x10 -> available in current output coding
    # 0x03 -> char width (0-2)

    sub check_char_attr {
	my $char = substr($_[0], 0, 1);
	my $code = ord($char);
	my $r;
	if ($code < 0x10000) {
	    my $r = vec($chrattr_cache, $code, 8);
	    return $r if $r;
	}
	if ($code >= 0x20 && $code <= 0x7e) {
	    return 0xf1; # ASCII shortcut
	}
	$r = 0x80; # checked

	if (!defined $charwidth_available) {
	    debug&8 and printf("DEBUG: loading CharWidth\n");
	    determine_locale_encoding();
	    if (!$enc_locale) {
		$charwidth_available = 0;
	    } else {
		eval {
		    require Text::CharWidth;
		};
		$charwidth_available = ! $@;
	    }
	    debug&8 and printf("DEBUG: loading CharWidth: result: %d\n", $charwidth_available);
	}

	my $printable = ($char =~ /^\p{Print}$/);
	$r |= 0x40 if $printable;

	my $skip_classcheck = 0;

	if ($widthmethod == 0 && $charwidth_available) {
	    debug&8 and printf("DEBUG: trying charwidth for U+%04x\n", $code);
	    my $cc = "$char";
	    my $s = $enc_locale->encode($cc, FB_QUIET);
	    my $v = -1;
	    if ($cc eq '' && length($s) >= 1) {
		$v = Text::CharWidth::mbwidth($s);
		debug&8 and printf("DEBUG: calling charwidth for %s -> %d\n", unpack("H*",$s), $v);
	    } else {
		debug&8 and printf("DEBUG: U+%04x does not encode: skipping\n", $code);
	    }
	    if ($v >= 0 && $v < 4) {
		$r |= (0x20 | $v);
		$skip_classcheck = 1;
	    }
	}
	unless ($skip_classcheck) {
	    $r |= 0x20 if ($widthmethod != 0 || !$charwidth_available) && ($r & 0x40 != 0);

	    if ($char =~ /\p{Block: Combining_Diacritical_Marks}
	     |\p{Block: Combining_Diacritical_Marks_For_Symbols}
	     |\p{Block: Combining_Half_Marks}
	     |\p{Block: Combining_Marks_For_Symbols}
	     |\p{Block: Combining_Diacritical_Marks_For_Symbols}/x) {
		$r |= 0;
	    } elsif ($char =~ /\p{Ea: W}|\p{Ea: F}/) {
		$r |= 2;
	    } elsif ($widthmethod >= 2 && $char =~ /\p{Ea: A}/) {
		$r |= 2;
	    } else {
		$r |= 1;
	    }
	}
	if ($output_utf8) {
	    $r |= 0x10 if ($printable && (($r & 0x20) != 0));
	} else {
	    my $s = $char;
	    my $t = $outenc->encode($s, FB_QUIET);
	    $r |= 0x10 if $printable && $s eq '' && length($t) >= 1;
	    debug&8 and printf("check U+%04X -> %d %d %d\n", $code, $printable, $s eq '', length($t) >= 1);
	}

	if ($code < 0x10000) {
	    vec($chrattr_cache, $code, 8) = $r;
	}
	debug&8 and printf("DEBUG: checked code U+%x -> %b\n", $code, $r);
	return $r;
    }
}

sub wcwidth {
    return 0 if $_[0] eq "";
    return 1 if ord($_[0]) < 0x80;
    return (check_char_attr($_[0]) & 3);
}

sub is_printable_to_terminal {
    return ((check_char_attr($_[0]) & 0x50) == 0x50);
}

my $fw_fill_char;

sub put_invalid_fullwidth {
    my ($n) = @_;
    if (!$fw_fill_char) {
	$fw_fill_char = is_printable_to_terminal("\x{3013}") ? "\x{3013}" : "..";
    }
#    put_decorate($fw_fill_char . (" " x ($n - 2)), "." x $n, $n);
    put_decorate($fw_fill_char, "..", $n);
}

sub put_maybe_fullwidth {
    # Filters output of "decode", assuming input is "full-width".
    # 2+ chars (decode may produce 2+ characters for "bad" inputs),
    # U+FFFD substitution character,
    # output-dependent unprintable characters.
    my ($s, $n) = @_;
    if (length $s > 1 || substr($s, 0, 1) eq "\x{fffd}") {
	put_invalid_fullwidth($n);
    } else {
	put_normal($s, $n);
    }
}

sub decode_char_maybe ($$) {
    my ($enc, $_s) = @_;
    my $s = "$_s";
    my $o = $enc->decode($s, FB_QUIET);
    ($s ne '' or length($o) != 1 or $o eq "\x{fffd}") ? undef : $o;
}

sub put_decode_maybe_fullwidth ($$$) {
    my ($enc, $s, $n) = @_;
    my $t = decode_char_maybe($enc, $s);
    if ($t) {
	put_normal($t, $n);
    } else {
	put_invalid_fullwidth($n);
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
	    my $s = chr($code) . chr($next);
	    my $o;
	    $o =   decode_char_maybe($enc_SJ, $s) unless $output_cp932;
	    $o //= decode_char_maybe($enc_932, $s);
	    defined $o ? put_normal($o, 2) : put_invalid_fullwidth(2);
	}
    }
    process_GR_none(@_);
}

sub process_GR_EucJP {
    my ($ofs, $code) = (@_);
    if ($code >= 0xa1 && $code <= 0xfe) {
	my $next = get($ofs + 1);
	if ($next >= 0xa1 && $next <= 0xfe) {
	    if (! $output_cp932) {
		put_decode_maybe_fullwidth($enc_EJ, chr($code) . chr($next), 2);
	    } else {
		# requireing EUCJPMS module is too much: we do convert by our own
		my $a = jis2sj_code($code, $next);
		put_decode_maybe_fullwidth($enc_932, $a, 2);
	    }
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
	if ($next >= 0xa1 && $next <= 0xfe
	    && $next2 >= 0xa1 && $next2 <= 0xfe) {
	    put_decode_maybe_fullwidth($enc_EJ, chr($code) . chr($next). chr($next2), 3);
	    return;
	}
    }
    process_GR_none(@_);
}

sub jis2sj_code (@) {
    my ($h, $l) = @_;
    if (@_ == 1) {
	$l = $h & 0x7f;
	$h = ($h >> 8) & 0x7f;
    } else {
	$l &= 0x7f;
	$h &= 0x7f;
    }
    $h -= 0x20;
    $l -= 0x20;
    my $s1 = int(($h + 1) / 2) + 0x80;
    $s1 += 0x40 if $s1 >= 0xa0;
    my $s2 = $l + 0x3f;
    $s2 += 94 unless $h & 1;
    $s2++ if $s2 >= 0x7f;
    return chr($s1) . chr($s2);
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
		put_ucs_nonprintable($c, $n);
	    }
	    return;
	}
    }
    process_GR_none(@_);
}

### input processing: ISO/IEC 2022 and compliant encodings

{
    my %encode_cache;
    my $broken_euc_tw;
    my %I646table;

    my @GLR_init;
    my @G_init;
    my @GLR;
    my @G;

    my $SS;
    my ($allow_LS, $allow_SS, $allow_designate, $allow_UTF);

    sub process_init_2022 {
	my @p = @_;

	$allow_SS = $allow_LS = $allow_designate = 1;
	$allow_UTF = 0;

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
		when (':utf_ok') {
		    $allow_UTF = 1;
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
	    if (exists $encode_cache{$g}) {
		get_encode_cache($g, 1);
	    }
	    $G_init[$_] = $g;
	}

	@GLR = @GLR_init;
	@G = @G_init;
	$SS = 0;
    }

    BEGIN {
	%encode_cache = ( '$A' => 'GB2312',
			  '$B' => 'EUC-JP',     # not used: directly implemented
			  '$C' => 'EUC-KR',
			  '$G' => ['EUC-TW', \&load_euc_tw, 'CNS 11643'],
			                        # only Big5 is included in pure Perl distribution
			  '$O' => ['euc-jisx0213', 'JIS2K', 'JIS X 0213'],
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
          ('@' => "\#\N{U+a4}\@\[\\\]\^_\`\{\|\}\~", # 4/0  IRV old
           'A' => "\N{U+a3}\$\@\[\\\]\^_\`\{\|\}\~", # 4/1  UK
           'B' => "\#\$\@\[\\\]\^_\`\{\|\}\~", # 4/2  US-ASCII
           'G' => "\#\N{U+a4}\@\N{U+c4}\N{U+d6}\N{U+c5}\^_\`\N{U+e4}\N{U+f6}\N{U+e5}\~", # 4/7  SE-B
           'H' => "\#\N{U+a4}\N{U+c9}\N{U+c4}\N{U+d6}\N{U+c5}\N{U+dc}_\N{U+e9}\N{U+e4}\N{U+f6}\N{U+e5}\N{U+fc}", # 4/8  SE-C
           'J' => "\#\$\@\[\N{U+a5}\]\^_\`\{\|\}\~", # 4/10 JP
           'K' => "\#\$\N{U+a7}\N{U+c4}\N{U+d6}\N{U+dc}\^_\`\N{U+c4}\N{U+d6}\N{U+dc}\N{U+df}", # 4/11 DE
           'L' => "\#\N{U+a4}\N{U+a7}\N{U+c3}\N{U+c7}\N{U+d5}\^_\`\N{U+e3}\N{U+e7}\N{U+f5}\N{U+b0}", # 4/12 PT-o
           'R' => "\#\N{U+a3}\N{U+e0}\N{U+b0}\N{U+e7}\N{U+a7}\^_\N{U+b5}\N{U+e9}\N{U+f9}\N{U+e8}\N{U+a8}", # 5/2  FR old
           'T' => "\#\N{U+a5}\@\[\\\]\^_\`\{\|\}\~", # 5/4  CN
           'Y' => "\#\N{U+a3}\N{U+a7}\N{U+b0}\N{U+e7}\N{U+e9}\^_\N{U+f9}\N{U+e0}\N{U+f2}\N{U+e8}\N{U+ec}", # 5/9  IT
           '`' => "\#\$\@\N{U+c6}\N{U+d8}\N{U+c5}\^_\`\N{U+c6}\N{U+d8}\N{U+c5}\~", # 6/0  NO
           'a' => "\#\N{U+a7}\@\N{U+c6}\N{U+d8}\N{U+c5}\^_\`\N{U+c6}\N{U+d8}\N{U+c5}\|", # 6/1  NO-2 old
           'f' => "\#\N{U+a3}\N{U+e0}\N{U+b0}\N{U+e7}\N{U+a7}\^_\N{U+b5}\N{U+e9}\N{U+f9}\N{U+e8}\N{U+a8}", # 6/6  FR
           'g' => "\#\N{U+a4}\N{U+b4}\N{U+c3}\N{U+c7}\N{U+d5}\^_\`\N{U+e3}\N{U+e7}\N{U+f5}\~", # 6/7  PT-i
           'h' => "\#\$\N{U+b7}\N{U+a1}\N{U+d1}\N{U+c7}\N{U+bf}_\`\N{U+b4}\N{U+f1}\N{U+e7}\N{U+a8}", # 6/8  ES
           'i' => "\#\N{U+a4}\N{U+c1}\N{U+c9}\N{U+d6}\N{U+dc}\^_\N{U+e1}\N{U+e9}\N{U+f6}\N{U+fc}\N{U+2dd}", # 6/9  HU
           'w' => "\#\$\N{U+e0}\N{U+e2}\N{U+e7}\N{U+ea}\N{U+ee}_\N{U+f4}\N{U+e9}\N{U+f9}\N{U+e8}\N{U+fb}", # 7/7  CA1
           'x' => "\#\$\N{U+e0}\N{U+e2}\N{U+e7}\N{U+ea}\N{U+c9}_\N{U+f4}\N{U+e9}\N{U+f9}\N{U+e8}\N{U+fb}", # 7/8  CA2
           'z' => "\#\$\N{U+17d}\N{U+160}\N{U+110}\N{U+106}\N{U+10c}_\N{U+17e}\N{U+161}\N{U+111}\N{U+107}\N{U+10d}", # 7/10 YU
           '!A'=> "\#\N{U+a4}\@\N{U+a1}\N{U+d1}\]\N{U+bf}_\`\N{U+b4}\N{U+f1}\[\N{U+a8}", # 2/1 4/1  CU
           '!C'=> "\N{U+a3}\N{U+a4}\N{U+d3}\N{U+c9}\N{U+cd}\N{U+da}\N{U+c1}_\N{U+f3}\N{U+e9}\N{U+ed}\N{U+fa}\N{U+e1}", # 2/1 4/3  IE
	  );
    }

    sub get_encode_cache {
	my ($key, $forced) = @_;
	my $enc = undef;

	return undef unless defined $encode_cache{$key};

	my $e = $encode_cache{$key};
	my $name = $e;
	my $r = ref($e);

	if ($r eq 'ARRAY') {
	    $name = $e->[0];
	    my $module = $e->[1];
	    my $printname = $e->[2] || $name;

	    if (ref $module eq 'CODE') {
		debug&2 and	print "Info: Loading encoding '$name' (via func)\n";
		eval { &$module() };
	    } else {
		die unless $module =~ /\A[A-Za-z0-9-_:]+\z/;
		debug&2 and	print "Info: Loadng extra encoding module $module\n";
		eval "require Encode::$module";
	    }
	    if ($@) {
		if ($@ =~ m|^Can't locate Encode\/(\w+)\.pm in \@INC|) {
		    $@ = "module Encode::$1 is required to support $printname\n";
		}
		if ($forced) {
		    die "Error: can't load Encoding $name: $@";
		}
		else {
		    warn "Warning: can't load Encoding $name: $@";
		    $enc = undef;
		    $encode_cache{$key} = undef;
		    return undef;
		}
	    }
	    $e = $name;
	    $r = '';
	    # fall through
	}
	if ($r) {
	    return $e;
	} else {
	    $enc = find_encoding($e) // die "Error: Can't load encoding $e";
	    debug&2 and	print "Info: Loaded encoding $e\n";
	    $encode_cache{$key} = $enc;
	    return $enc;
	}
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

	my $plane = $SS ? $SS : $GLR[$is_GR];
	$SS = 0;

	my $G = $G[$plane];

	my $c = $code & 0x7f;

    #    printf "ofs %04x: plane %d, assign %s, code %02x (%02x)\n", $ofs, $plane, $G, $c, $code;
     #   $chrbuf .= sprintf("<%04x>>", $ofs);

	if (($c == 0x20 || $c == 0x7f) && substr($G, 0, 1) ne ',') {
	    $is_GR ? process_GR_none($ofs, $code) : process_C0_ASCII($ofs, $code);
	    return;
	}

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
		    put_normal($s, 1);
		} else {
		    put_decorate(".");
		}
	    }
	    when (['$B', '$@']) {
		my $next = get($ofs + 1) & 0x7f;
		if ($next >= 0x21 && $next <= 0x7e) {
		    if ($output_cp932) {
			put_decode_maybe_fullwidth($enc_932, jis2sj_code($c, $next), 2);
		    } else {
			put_decode_maybe_fullwidth($enc_EJ, chr(0x80 | $c) . chr(0x80 | $next), 2);
		    }
		} else {
		    put_decorate(".");
		}
	    }
	    when ('$D') {
		my $next = get($ofs + 1) & 0x7f;
		if ($next >= 0x21 && $next <= 0x7e) {
		    put_decode_maybe_fullwidth($enc_EJ, chr(0x8f) . chr(0x80 | $c) . chr(0x80 | $next), 2);
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
	    when (/^\$[ACO]$/ or ($_ eq '$G' and !$broken_euc_tw)) {
		# G (EUC-TW) is optional AND broken in Perl
		my $next = get($ofs + 1) & 0x7f;
		if ($next >= 0x21 && $next <= 0x7e) {
		    my $enc = get_encode_cache($G);
		    if ($enc) {
			put_decode_maybe_fullwidth($enc, chr(0x80 | $c) . chr(0x80 | $next), 2);
		    } else {
			put_invalid_fullwidth(2);
		    }
		    $chreaten = $chrforward = 1;
		} else {
		    put_decorate(".");
		}
	    }
	    when (/^\$[G-M]$/) {
		# $G comes here when $broken_euc_tw is true
		my $next = get($ofs + 1) & 0x7f;
		if ($next >= 0x21 && $next <= 0x7e) {
		    my $enc = get_encode_cache('$G'); # EUC-TW
		    if ($enc) {
			my $s = chr(0x8e) . chr(0xa1 + ord(substr($G, 1, 1)) - 0x47) .
			  chr(0x80 | $c) . chr(0x80 | $next);
			put_decode_maybe_fullwidth($enc, $s, 2);
		    } else {
			put_invalid_fullwidth(2);
		    }
		    $chreaten = $chrforward = 1;
		} else {
		    put_decorate(".");
		}
	    }
	    when ('$P') {
		# EUC-JP2013, 2nd plane
		my $next = get($ofs + 1) & 0x7f;
		if ($next >= 0x21 && $next <= 0x7e) {
		    my $enc = get_encode_cache('$O');
		    if ($enc && 
			($c == 0x21
			 || $c >= 0x23 && $c <= 0x25
			 || $c == 0x28
			 || $c >= 0x2d && $c <= 0x2f
			 || $c >= 0x6e)) {
			put_decode_maybe_fullwidth($enc, chr(0x8f) . chr(0x80 | $c) . chr(0x80 | $next), 2);
			return;
		    } else {
			put_invalid_fullwidth(2);
		    }
		} else {
		    put_decorate(".");
		}
	    }
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
	    when ([0x0a, 0x0d, 0x0b, 0x00]) {
		debug&4&& print "before: @GLR @G  tobe: @GLR_init @G_init reset=$reset_2022_status\n";
		if ($reset_2022_status == 1) {
		    # workaround for half-binary data:
		    # reset shift status of GL at each line-beginning
		    $GLR[0] = @GLR_init[0];
		    $G[$GLR[0]] = $G_init[$GLR[0]] if $G_init[$GLR[0]] ne '';
		} elsif ($reset_2022_status >= 2) {
		    @G = @G_init;
		    @GLR = @GLR_init;
		}
		debug&4&& print "after: @GLR @G  tobe: @GLR_init @G_init reset=$reset_2022_status\n";
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
		    when (0x25) {
			# code transition
			break if !$allow_UTF;
			my $codeA = get($ofs + 2);
			if ($codeA == 0x47) {
			    #printf "ISO-2022 SEQUENCE at %x: go to UTF\n", $ofs;
			    put_fill(1);
			    $chreaten = 2; $chrforward = 0;
			    # switch to UTF (with special return)
			    ($process_GL, $process_GR, $process_C0) = 
			      (\&process_GL_ASCII, \&process_GR_UTF8, \&process_C0_UTF8_in_2022);
			    return;
			} elsif ($codeA == 0x40) {
			    # ISO-2022 calling: no-op
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
			continue if $target == 0 && $is_96char; # 96-char set cannot be load into G0

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

    sub process_C0_UTF8_in_2022 ($$) {
	my ($ofs, $code) = (@_);
	if ($code == 0x1b) {
	    if (get($ofs + 1) == 0x25) {
		my $t = get($ofs + 2);
		if ($t == 0x40) {
		    # return to ISO-2022
		    put_fill(1);
		    $chreaten = 2; $chrforward = 0;
		    # switch to UTF (with special return)
		    ($process_GL, $process_GR, $process_C0) = 
		      (\&process_GLR_2022, \&process_GLR_2022, \&process_C0_2022);
		    @GLR = @GLR_init;
		    @G = @G_init;
		    return;
		} elsif ($t == 0x47) {
		    # UTF-8 calling: no-op
		    put_fill(1);
		    $chreaten = 2; $chrforward = 0;
		    return;
		}
	    }
	}
	process_C0_ASCII(@_);
    }

    sub load_euc_tw {
	# EUC-TW is broken in current HanExtra;
	# check it.
	require Encode::HanExtra;

	my $enc = find_encoding('euc-tw');
	given (unpack("H*", $enc->encode("1 \x{4E00}"))) {
	    when ('3120c4a1') {
		# CNS-11643-1 is loaded to GR/G1.  OK.
		$broken_euc_tw = 0;
	    }
	    when ('31208ea1c4a1') {
		# CNS-11643 is encoded as combined G2.  NG.
		$broken_euc_tw = 1;
	    }
	    default {
		die "Internal error: module Encode::HanExtra is deeply broken ($_);"
	    }
	}
	return;
    }
    
    # put here because we need $broken_euc_tw
    sub process_GR_EucTW {
	my ($ofs, $code) = (@_);
	my $enc = get_encode_cache('$G');

	if ($code >= 0xa1 && $code <= 0xfe) {
	    my $next = get($ofs + 1);
	    if ($next >= 0xa1 && $next <= 0xfe) {
		my $s = chr($code) . chr($next);
		$s = "\x8e\xa1$s" if $broken_euc_tw;
		put_decode_maybe_fullwidth($enc, $s, 2);
		return;
	    }
	}
	if ($code == 0x8e) {
	    my $next = get($ofs + 1);
	    my $next1 = get($ofs + 2);
	    my $next2 = get($ofs + 3);
	    if ($next >= 0xa2 && $next <= 0xaf
		&& $next1 >= 0xa1 && $next1 <= 0xfe
		&& $next2 >= 0xa1 && $next2 <= 0xfe) {
		put_decode_maybe_fullwidth($enc, chr($code) . chr($next) . chr($next1) . chr($next2), 4);
		return;
	    }
	}
	process_GR_none(@_);
    }

    sub process_init_eucTW {
	get_encode_cache('$G', 1);
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

	      put_decode_maybe_fullwidth($mb_input_enc, $s, length $s);
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
	    die "Error: read error: $!\n";
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

### terminal_handling

{
    sub setup_color_sequences {
	my ($ansi_OK, $term);
	my $reESC = qr/(?:\\[eE]|\\033|\\[xX]1[bB])/;

      TESTING: {
	    if (is_DOSish) {
		$ansi_OK = 1;
		debug&16&& print "ANSI sequence is assumed supported on DOSish platforms\n";
		last TESTING;
	    }
	    if ($ENV{LS_COLORS}) {
		debug&16&& print "LS_COLORS exists.\n";
		# check from LS_COLORS (trusting dircolors(1))
		local $_ = ":$ENV{LS_COLORS}:";
		if ((/:lc=/ && !/:lc=${reESC}\[:/) ||
		    (/:rc=/ && !/:rc=m:/) ||
		    (/:ec=/ && !/:ec=${reESC}\[0?m:/)) {
		    debug&16 && print "non-ANSI code definition is found on LS_COLORS: skipping\n";
		} else {
		    if (/:([^:=]+=(([34]|[0-2][0-9]*);)*[34][0-9](;([34]|[0-2][0-9]*))*):/) {
			debug&16 && print "LS_COLORS entry $1 seems to use ANSI sequence. trusting\n";
			$ansi_OK = 1;
			last TESTING;
		    };
		}
	    }
	    # terminfo testing
	    if (-x "/usr/bin/infocmp") {
		debug&16&& do { print "trying terminfo.\n" };
		local $_ = qx(/usr/bin/infocmp -I 2>/dev/null); # -CTr
		if (defined $_) {
		    s/^#.*\n//gm;
		    s/\n[ \t]+//gs;
		    s/\A[^,]+, *//s;
		    chomp $_;
		    my %e = map { / *+([^=#]+)[=#](.*)\z/ ? ($1, $2) : ($_, '') } (split /, */s, $_);
		    if (($e{AF} // $e{setaf}) =~ /^${reESC}\[3\%p1\%dm\z/s
			&& ($e{AB} // $e{setab}) =~ /^${reESC}\[4\%p1\%dm\z/s) {
			debug&16&& print "setaf/setab found as expected: ANSI OK\n";
			$ansi_OK = 1;
			last TESTING;
		    } elsif ($e{setaf} ne '' && $e{setab} ne '') {
			debug&16&& print "setaf/setab found is not expected\n";
		    } else {
			debug&16&& print "setaf/setab not found\n";
		    }
		}
	    };
	  TERMCAP_TESTING:
	    {
		debug&16&& print "trying termcap\n";
		eval {
		    require Term::Cap;
		};
		if ($@) {
		    debug&16&& print "Term::Cap is not available: $@\n";
		    $term = undef;
		    last TESTING;
		}
		    
		eval {
		    local $ENV{PATH} = "/usr/bin"; # only to use /usr/bin/infocmp
		    $term = Tgetent Term::Cap { OSPEED => 9600 };
		};
		if ($@) {
		    debug&16&& print "tgetent is failed: $@\n";
		    $term = '';
		    last TERMCAP_TESTING;
		}
		my $af = $term->Tputs('AF');
		my $ab = $term->Tputs('AB');
		if ($af =~ /^\e\[3\%dm\z/s
		    && $ab =~ /^\e\[4\%dm\z/s) {
		    debug&16&& print "AF/AB found as expected: ANSI OK\n";
		    $ansi_OK = 1;
		    last TESTING;
		}
	    }
	}
	if ($ansi_OK) {
	    $color_sequence_d = "\e[0;7;34;46m";
	    $color_sequence_f = "\e[0;36m";
	    $color_sequence_n = "\e[0m";
	    $color_colorful = 1;
	    return 1;
	}
	if ($term) {
	    my $b = $term->Tputs('md');
	    my $u = $term->Tputs('us');
	    my $e = $term->Tputs('me');
	    if ($b && $u && $e) {
		debug&16&& print "bold, underline, reset found\n";
		$e =~ s/\e\(B//;
		$color_sequence_d = "$e$b$u";
		$color_sequence_f = "$e$u";
		$color_sequence_n = "$e";
		$color_colorful = 0;
		return 1;
	    }
	}
	debug&16&& print "bold, underline, reset not found ... dumb terminal?\n";
	$color_sequence_d = '';
	$color_sequence_f = '';
	$color_sequence_n = '';
	$color_colorful = 0;
	return 0;
    }
}

sub load {
    determine_locale_encoding();
    set_output_coding('detect');
    1;
}

caller() ? load() : main();


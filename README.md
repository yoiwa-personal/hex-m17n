[-]: # " -*- mode: gfm; coding: utf-8 -*- "

hex : multi-locale hexadecimal dump tool
========================================

(c) 2016-2021 Yutaka OIWA.

## 1. Overview

"Hex" is a hexadecimal dump tool for binary/text files
supporting multi-locale strings and texts.
It provides the following features:

 - Hexadecimal dump, along with character code dumps

 - Support several character encoding for character dumps,
   including many multi-byte and shift-sequence encoding.

 - Automatic detection of several encoding schemes for the same
   languages.

 - Rigid dump for multi-byte and full-width characters.

 - Colored/decorated character dumps for easy recognition of
   control sequences.

 - Decorated dumps for "less" text previewer families.

 - Character-based dumps (like od -c but in hexadecimal)

 - "text dump" for safely previewing "mostly-text" files with a
   small number of control characters.

## 2. Usage

For a simple binary dump, just call it like `hex filename`.  (We
assume, for the rest of document, that the script is installed as
a command "hex".)  If the filename is omitted, input is taken from
the standard input (stdin).  It takes several additional options
between the command name and input file, described below.


### 2.1 Input encoding - automatic detection and manual setting

If the input is a text file, its coding schemes are detected by
using heuristics and the current locale settings.  If you know the
coding scheme of the input beforehand, or if automatic detection
is not satisfactory, you can specify the input coding using the `-i`
(`--input-coding`) option.

The complete list of the supported encoding is available by
calling the script as ``hex --list-codings``.  The list below is a
short summary of (a part of) currently supported encoding schemes.


 * For Latin-based languages and language-neutral binaries:

         -iascii, -ibinary                     ASCII      (ISO-646-US)
         -il1, -iISO8859-1                     ISO-8859-1 (also known as Latin-1)
         -iu, -u, -iutf-8                      UTF-8      (Unicode)

 * For Japanese

         -iej, -e, -iEUC-JP                    EUC-JP
         -isj, -s, -ishift_JIS                 Shift_JIS
         -iij, -ij, -j, -ijis, -iISO-2822-JP   ISO-2022-JP (aka "jis" or "junet")
         -iu, -u, -iutf-8                      UTF-8      (Unicode)

 * For Korean

         -iek, -iEUC-KR                        EUC-KR     (KS X 1001)
         -iik, -i2022, -iISO-2022              ISO-2022-KR
         -iu, -u, -iutf-8                      UTF-8      (Unicode)

 * For Simplified Chinese (Mainland China)

         -iec, -igb2312, -iEUC-CN              EUC-CN
         -iic, -i2022, -iISO-2022              ISO-2022-CN
         -iu, -u, -iutf-8                      UTF-8      (Unicode)

 * For Traditional Chinese (Taiwan)

         -ib5, -ibig5                          Big5-ETEN
         -iEUC-TW                              EUC-TW (CNS 11643 based)
                             [Perl module Encode::HanExtra needed]
         -iu, -u, -iutf-8                      UTF-8      (Unicode)

 * For multilingual texts

         -iu, -u, -iutf-8                      UTF-8      (Unicode)
         -iutf-16-be                           UTF-16-BE  (Big-endian Unicode)
         -iutf-16-le                           UTF-16-LE  (Little-endian Unicode)
         -iutf-32-be                           UTF-32-BE  (Big-endian Unicode)
         -iutf-32-le                           UTF-32-LE  (Little-endian Unicode)
         -i2022, -iISO-2022                    ISO-2022   (shift-sequence based generic encoding)
         -iISO-2022-U                          ISO-2022/UTF-8 (switched by special sequence)

Note: if you use this script from another programs automatically,
please specify the full encoding name shown as last one in the
above list.  Short mnemonics are more likely to be changed in
future.


### 2.2 Output style

#### 2.2.1 The default "hexadecimal" output

By default, the output consists of three components like following:

````````````````````````````````````````````````````````````````
# Detected coding: binary
00000000: 1a 01 29 00 26 00 0f 00 - 9d 01 5a 05 78 74 65 72  ..).&.....Z.xter
00000010: 6d 7c 78 74 65 72 6d 2d - 64 65 62 69 61 6e 7c 58  m|xterm-debian|X
00000020: 31 31 20 74 65 72 6d 69 - 6e 61 6c 20 65 6d 75 6c  11 terminal emul
00000030: 61 74 6f 72 00 00 01 00 - 00 01 00 00 00 01 00 00  ator............
````````````````````````````````````````````````````````````````

The leftmost component shows a relative byte address of the input
from the beginning.  The middle part shows the hexadecimal dump of
bytes in the input.  Each line of the output correspond to 16
bytes of input.  The rightmost part shows the character correspond
to each byte of the input, shown in the middle. For example, the
ampersand in the fifth character in the first line correspond to
the byte "26" shown left.

Bytes not corresponding to printable characters, for example "1a"
in address 0, is replaced by a filling character, ".", in the
character dump.  If the terminal supports the coloring output, the
fillers are replaced with more descriptive "marks":

````````````````````````````````````````````````````````````
# Detected coding: binary

00000000: 1a 01 29 00 26 00 0f 00 - 9d 01 5a 05 78 74 65 72  ZA)@&@O@.AZExter
                                                             ^^ ^ ^^^^^ ^
00000010: 6d 7c 78 74 65 72 6d 2d - 64 65 62 69 61 6e 7c 58  m|xterm-debian|X

00000020: 31 31 20 74 65 72 6d 69 - 6e 61 6c 20 65 6d 75 6c  11 terminal emul

00000030: 61 74 6f 72 00 00 01 00 - 00 01 00 00 00 01 00 00  ator@@A@@A@@@A@@
                                                                 ^^^^^^^^^^^^

````````````````````````````````````````````````````````````

On the colored terminal, the characters marked by ``^`` is shown as
cyan-on-blue decorated mark.  Here, the mark ``Z`` correspond to the
code of "control-Z" = 1a (hexadecimal).  Similarly, ``A``
corresponds to 01, ``@`` to 00, respectively.  If the script cannot
detect that terminal supports coloring, or the output is
redirected to non-terminal (assuming "less" or something there),
the mark is shown in bold-underlined, instead.  The option ``-d``
forces decorated output to a non-terminal output, and the option
``-p`` forces "plain", non-decorated output.  (Hint: option ``-d`` can
be useful with pagers supporting coloring sequences, e.g.
``less -R`` or ``lv -c``.)

If the input contains wide characters, or input contains a
multi-byte sequence which corresponds to a single character, the
character dump output is adjusted to reflect that.  For example,
if the input is ``Japanese 日本語-Nihongo is supported.`` in UTF-8,
the output becomes as follows:

````````````````````````````````````````````````````````````````
# Detected coding: UTF-8
00000000: 4a 61 70 61 6e 65 73 65 - 20 e6 97 a5 e6 9c ac e8  Japanese 日 本 語
00000010: aa 9e 2d 4e 69 68 6f 6e - 67 6f 20 69 73 20 73 75    -Nihongo is su
00000020: 70 70 6f 72 74 65 64 0a -                          pported.
````````````````````````````````````````````````````````````````

   * The character 日 has full-width (2-spaces), and its
     corresponding input is 3 bytes, "e6 97 a5".  A filler space is
     inserted after the character to adjust it.

   * The character 語 (e8 aa 9e) appears at the very end of the line.
     The half of the character is hanged out to the right of
     character dump (see the position of last character ``u`` in the
     second line), and the location of the immediately following
     character ``-`` is also adjusted in the beginning of the next
     line.

If the output is colored, a cyan "space-filler line" is put to
each of those spaces to show that there is no corresponding
"white-space characters" in input.

````````````````````````````````````````````````````````````````
# Detected coding: UTF-8
00000000: 4a 61 70 61 6e 65 73 65 - 20 e6 97 a5 e6 9c ac e8  Japanese 日_本_語
00000010: aa 9e 2d 4e 69 68 6f 6e - 67 6f 20 69 73 20 73 75  __-Nihongo is su
00000020: 70 70 6f 72 74 65 64 0a -                          pported[J]
````````````````````````````````````````````````````````````````
([J] is a colored marker, taking single column)

Such space-fillers are also inserted when some input byte sequence
corresponds to an encoding function, not to a character.  See the
following example, that is the same text as above within
ISO-2022-JP encoding.  The space-filler is inserted to the
location corresponding to the byte sequences "1b 24 42" and "1b 28
42" (escape sequences switching between JIS Kanji and ASCII).

````````````````````````````````````````````````````````````````
# Detected coding: ISO-2022
00000000: 4a 61 70 61 6e 65 73 65 - 20 1b 24 42 46 7c 4b 5c  Japanese ___日本
00000010: 38 6c 1b 28 42 2d 4e 69 - 68 6f 6e 67 6f 20 69 73  語___-Nihongo is
00000020: 20 73 75 70 70 6f 72 74 - 65 64 0a                  supported[J]
````````````````````````````````````````````````````````````````

#### 2.2.2 Character dump output

If you like the ``-c`` option of ``od``, the ``-c`` option of ours might
also be useful.  Of course, we use hexadecimal numbers, instead of
octal.

The above two examples will be shown as follows with the ``-c`` option:

````````````````````````````````````````````````````````````````
# Detected coding: binary
00000000:  ^Y ^A )  \0 &  \0 ^O \0 9d ^A Z  ^E x  t  e  r  
00000010:  m  |  x  t  e  r  m  -  d  e  b  i  a  n  |  X  
00000020:  1  1     t  e  r  m  i  n  a  l     e  m  u  l  
00000030:  a  t  o  r  \0 \0 ^A \0 \0 ^A \0 \0 \0 ^A \0 \0 
````````````````````````````````````````````````````````````````

````````````````````````````````````````````````````````````````
# Detected coding: UTF-8
00000000:  J  a  p  a  n  e  s  e     日 __ __ 本 __ __ 語 
00000010:  __ __ -  N  i  h  o  n  g  o     i  s     s  u  
00000020:  p  p  o  r  t  e  d  \n 
````````````````````````````````````````````````````````````````

The sequences with backslashes correspond to the equivalent
ones used in C, Perl etc.  For example, ``\0`` corresponds to
binary 00, ``\n`` to binary 0a, etc.
The sequence ``^Y`` corresponds to control-Y, etc.
If such "readable" output is not possible, two hexadecimal
digits are just shown (see ``9d`` in the first line.)
If wide characters are found, fillers are also put in
the character dump format, as shown in the second example
(in non-colored output, fillers will be invisible.)

The option `-c2` shows ASCII spaces as `sp`.

Note: outputs have "reversible" or "lossless" property
only when the input coding system is set to "ascii" (binary).


#### 2.2.3 "Text dump" output

If you put ``-t`` options, the output is "text"...
If that options is used with pure text file, it behaves
almost like a ``cat`` with a coding detection and conversion
facilities.
However, it might be mostly useful with a "broken" half-text
file or to see the embedded texts in a binary:
any unprintable characters are replaced by filling
characters, in the same way as the right-hand-side of the
hexadecimal dump.

````````````````````````````````````````````````````````````````
..).&.....Z.xterm|xterm-debian|X11 terminal emulator.......
.................................P.........................

(omitted)

..[%i%p1%d;%p2%dr..[3g..[H.[2J..[K..[J..[%i%p1%dG..[%i%p1%d
;%p2%dH.
..[H..[?25l....[?12l.[?25h..[C..[A..[?12;25h..[P..[M..(0..[
5m..[1m..[?1049h..[4h..[8m..[7m..[7m..[4m..[%p1%dX..(B..(B.
[m..[?1049l..[4l..[27m..[24m..[?5h$<100/>.[?5l..[!p.[?3;4l.
````````````````````````````````````````````````````````````````


## 3. Internals and tweaking options

The option ``--output-coding`` specifies the coding system of the
output text.  Usually, the output coding is detected from a locale
setting (or the "code page" setting on Windows platform).  If it
is not satisfactory, you can manually specify its coding.  The
parameter value is a coding name used in Perl, listed in the
``Encode::Supported`` document.  This setting also affects which
characters are considered "printable" by this tool.

The option ``--charwidth`` works on more deeply inside nits: to
determine which characters are wide characters.  Since Unicode is
a mixture of several sources of characters, there are several
characters whose width is "ambiguous".  For example, on almost any
character coding ``A`` (U+41) takes a "halfwidth" or single-column.
``日`` or any other "Kanji" takes double-column, as well as ``Ａ``
(U+FF21), which is an explicit full-width variant of ``A``.
However, ``£`` (U+00A3) is halfwidth on Latin-based terminals but
is fullwidth on East-Asian terminals.  The ``hex`` script, by
default, tries to get its information from the underlying
operating system as much as possible: as long as some character
can be encoded in the current output coding, it will ask the
operating system to calculate its width.  However, that method is
not always useful or reliable.  If the result is not satisfactory,
you can specify either ``--charwidth=1`` or ``--charwidth=2`` to use
only Perl-internal Unicode database to determine the character
width, not relying on the operating system.  The value specified
on the option will determine the width of "ambiguous" characters
specified in Unicode standard.  (So, ``--charwidth=1`` might be
useful in Western environment, and so ``--charwidth=2`` in most
East-Asian environment.)

## Acknowledgement:

This tool is fully inspired by the original idea of
"hex - hexadecimal dumping tool for Japanese" by Taga Nayuta,
especially the ideas of color-decorated dumps, filler characters
and "less"-friendly output format (using backspaces).


## Appendix A:  the brief description of "undocumented" options.

Please do not use these options in general usage, as it might be
changed, extended or removed in future releases.

 *  ``--no-reset-status``:

    "Hex" treats ISO-2022 input as slightly deviated from the
    standard.  To avoid never-ending "mojibakes" caused by
    unintended, unlucky occurrences of some escape sequences, our
    decoder forcibly resets part of ISO-2022 shift status on some
    control characters which seem to mean end-of-data, such as CR, LF or NUL.
    The ``--no-reset-status`` option inhibits this non-standard behavior.

    It may be useful to handle "correct" input of ISO-2022 which uses shift
    status over multiple lines.
    However, many encoding schemes named "ISO-2022-..." *do not* require
    this option, because those are define in the way that each lines of input
    can be decoded separately.  Also, enabling this option makes input 
    handling quite unstable with badly-encoded inputs, especially
    against a single-byte SI (0x0e) in the input.

 *  ``--use-control-pictures``:

    Unicode standard includes a "printable symbols for control
    characters", e.g. "NUL" or "DC1" combined into a single character
    space.  This options let "hex" use these characters for "character
    dumps".  However, be notified that many fonts does not implement
    these characters and make its output may become less useful than
    before.  in ``-c`` mode, control characters with two-character
    mnemonics are displayed by ASCII characters, not using the
    combined symbols.  ``--use-control-pictures=2`` uses
    two-character variant (NU and D1 for example) for all ASCII
    control characters in ``-c`` output,
    based on an ancient ISO 2047 abbreviations.

    (For those who know memory of traditional Japanese PC-9801
    environment, the two-character mnemonics have been seen there.)

 *  ``--cjk-region``:

    Automatic detection algorithms for input encoding picks possible
    candidates depending on the current locale language.  It will be
    taken from the current locale name, name of terminal character
    sets in the locale, and Windows code pages.  This option overrides
    the detected language environment.  The valid values are currently
    ``JP``, ``KR``, ``CN``, ``TW``, and ``-`` (neutral).

 *  ``-i'iso2022(spec)'``:

    Detailed ISO-2022-based encoding can be specified by
	extended ``-i`` option.
	The spec is a space-separated sequence of the following elements.
	
	First, there may be zero or more of following flags:

	 * ``:noLS``: Ignore locking shifts.
	 * ``:noSS``: Ignore single shifts.
	 * ``:nodesignate``: ignore any character-set designations.
	 * ``:noshift``: Ignore both locking shifts and single shifts.
	 * ``:fixed``: Ignore all shifts and designations.
	 * ``:noreset``: Designations will not be reset on every line starts.
	   Same as ``--no-reset-status`` option.
	 * ``utf_ok``: Shift sequence to UTF-8 coding is understood.

    Next, there may be optionally one or two specifiers for initial shifts:
	each digits from ``0`` to ``3`` means G0 to G3, respectively, is
	shifted initially to GL / GR.
	If omitted, ``0 1`` (G0 to GL, G1 to GR) is assumed.

    Finally, there must be one to four four character set designations,
	which are corresponding to G0 to G3 respectively.
	Each character set designation means:

     * a single character between `@` and `~`: a 94-character set,
	   whose "final byte" is corresponding ascii character code,
	   is selected.
	 * `$` + a single character: a 94²-character set is selected.
	 * `,` + a single character: a 96-character set is selected.

    Some examples are as follows:

     * `-i'iso-2022(:fixed B $A)'` is the same as `EUC-CN`.
     * `-i'iso-2022(:fixed B ,A)'` is the same as `ISO-8859-1`.
     * `-i'iso-2022(:fixed @)'` is obsolete ISO-646-IRV.
     * `-i'iso-2022(:nodesignate :noLS B $B I $D)'` is the same as `EUC-JP`.
     * `-i'iso-2022(:fixed J I)'` is JIS X 0201.

    Single quotes around the spec is required by POSIX (Unix-like) shells.
    Other operating environments may require another style of quoting.

 *  ``--debug``:

    It's a debug option.  It will not be documented even in this
    appendix, but one note: To make it easy to find correspondence
    between debug messages and binary inputs (and its corresponding
    output), the debug messages of this "hex" script is intentionally
    directed to standard output (stdout), not to standard error output
    (stderr).  This means that, ``--debug`` must not be used when output
    is used by some other programs for automated processing.

## Appendix B: known issues (including "by-design" non-issues)

 * ISO-2022 based encodings do not preserve locale "origins" of
   input characters: the similar characters in different codesets
   (for example, ``Á`` in Latin-1 and Latin-2, or ``、`` in
   JIS X0208, KSC 5601, GB2312 and CNS11643).
   These are mapped to the same internal coding points based on
   ISO 10646-1 (Unicode).

 * Paddings displayed in the "character" dump section is not
   identical between EUC-JP and japanese-iso8.
   The former treats the byte sequence "8e a1" as a two-byte
   character, where the latter treats the same sequence as
   a SS2 control character followed by an one-byte character.
   A similar thing happens for three-byte sequence "8f xx xx".

 * ISO-2022 "announcers" are parsed but disregarded and discarded.
   Regardless of the announced capabilities, the ISO-2022 parser
   accepts all classes of designation sequences, as long as
   these are not disabled by coding systems.

#!/usr/bin/perl -w
# libguestfs
# Copyright (C) 2016 Red Hat Inc.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

# Reformat the code in libguestfs (currently only C code).

use warnings;
use strict;

use YAML qw(DumpFile);

# Check we are run from the top level directory.
die "$0: you must run this script from the top level source directory\n"
    unless -f "BUGS";

# Make sure we have the clang-format program.
system ("clang-format --help >/dev/null 2>&1") == 0
    or die "$0: 'clang-format' program (from Clang) must be installed\n";

# Which files to process.  Use ./scripts/code-format.pl to process all
# files in the project, else select which files to process on the
# command line.

my @files;
if (0 == @ARGV) {
    @files = `git ls-files '*.[ch]'`;
    chomp @files;
} else {
    @files = @ARGV;
}

# http://clang.llvm.org/docs/ClangFormatStyleOptions.html
my $clang_stylesheet = {
    Language => "Cpp",
    AccessModifierOffset => -2,
    AlignAfterOpenBracket => "true",
    AlignConsecutiveAssignments => "false",
    AlignEscapedNewlinesLeft => "true",
    AlignOperands => "true",
    AlignTrailingComments => "true",
    AllowAllParametersOfDeclarationOnNextLine => "true",
    AllowShortBlocksOnASingleLine => "false",
    AllowShortCaseLabelsOnASingleLine => "false",
    AllowShortFunctionsOnASingleLine => "All",
    AllowShortIfStatementsOnASingleLine => "false",
    AllowShortLoopsOnASingleLine => "false",
    AlwaysBreakAfterDefinitionReturnType => "TopLevel",
    AlwaysBreakBeforeMultilineStrings => "false",
    AlwaysBreakTemplateDeclarations => "false",
    BinPackArguments => "true",
    BinPackParameters => "true",
    BraceWrapping => {
        AfterClass => "false",
        AfterControlStatement => "false",
        AfterEnum => "false",
        AfterFunction => "true",
        AfterNamespace => "true",
        AfterObjCDeclaration => "false",
        AfterStruct => "false",
        AfterUnion => "true",
        BeforeCatch => "false",
        BeforeElse => "true",
        #AfterIndentBraces => "false",  -- only in clang > 3.8.0
    },
    BreakBeforeBinaryOperators => "None",
    BreakBeforeBraces => "Custom",
    BreakBeforeTernaryOperators => "true",
    BreakConstructorInitializersBeforeComma => "false",
    #BreakStringLiterals => "false",  -- only in clang > 3.8.0
    ColumnLimit => 76,
    CommentPragmas => "51 Franklin Street",
    ConstructorInitializerAllOnOneLineOrOnePerLine => "false",
    ConstructorInitializerIndentWidth => 2,
    ContinuationIndentWidth => 2,
    Cpp11BracedListStyle => "false",
    DerivePointerAlignment => "false",
    DisableFormat => "false",
    ExperimentalAutoDetectBinPacking => "false",
    ForEachMacros => [],
    IndentCaseLabels => "false",
    IndentWidth => 2,
    IndentWrappedFunctionNames => "false",
    KeepEmptyLinesAtTheStartOfBlocks => "true",
    MacroBlockBegin => "",
    MacroBlockEnd => "",
    MaxEmptyLinesToKeep => 1,
    NamespaceIndentation => "None",
    ObjCBlockIndentWidth => 2,
    ObjCSpaceAfterProperty => "false",
    ObjCSpaceBeforeProtocolList => "true",
    PenaltyBreakBeforeFirstCallParameter => 19,
    PenaltyBreakComment => 300,
    PenaltyBreakFirstLessLess => 120,
    PenaltyBreakString => 1000,
    PenaltyExcessCharacter => 1000000,
    PenaltyReturnTypeOnItsOwnLine => 60,
    PointerAlignment => "Right",
    SortIncludes => "false",
    SpaceAfterCStyleCast => "false",
    SpaceBeforeAssignmentOperators => "true",
    SpaceBeforeParens => "Always",
    SpaceInEmptyParentheses => "false",
    SpacesBeforeTrailingComments => 1,
    SpacesInAngles => "false",
    SpacesInContainerLiterals => "true",
    SpacesInCStyleCastParentheses => "false",
    SpacesInParentheses => "false",
    SpacesInSquareBrackets => "false",
    Standard => "Cpp11",
    TabWidth => 8,
    UseTab => "Never",
};
DumpFile (".clang-format", $clang_stylesheet);

# Run clang-format on each file, then post-process the output further.
foreach my $input (@files) {
    my @o = ();

    open INPUT, "clang-format $input |"
        or die "clang-format: $input: $!";
    push @o, $_ while <INPUT>;
    close INPUT or die;

    # Post-process the output.
    my ($i, $j);
    for ($i = 0; $i < @o; ++$i) {
        # clang-format puts a space after every function, but we
        # don't want one after the _ (gettext) function:
        if (($o[$i] =~ s{_ \("}{_("}g) == 1) {
            # If the following lines are strings, assume a continuation
            # string and reduce its indent by 1 character.
            for ($j = $i+1; $j < @o; ++$j) {
                last if ($o[$j] =~ s{^(\s+)\s"}{$1"}) == 0;
            }
        }

        # As above, for __attribute__:
        $o[$i] =~ s{__attribute__\s+}{__attribute__}g;

        # clang-format doesn't format __attribute__((noreturn)) on
        # function decls correctly.
        $o[$i] =~ s{(void\s+__attribute__\(\(noreturn\)\))\s+}{$1\n};

        # If we have #define PREFIX_... on several adjacent lines,
        # align the right hand side.  clang-format has a setting
        # AlignConsecutiveAssignments but it invasively applies
        # to every assignment in the code.
        # XXX This doesn't align comments properly.
        my $def = qr/^#define (([A-Z90-9]+)_[A-Z0-9_]+)\s(?=\S)/;
        if ($o[$i] =~ /$def/) {
            my $maxlen = length $1;
            my $prefix = $2;
            my $nr_lines = 1;
            my $len;
            my $spaces;

            for ($j = $i+1; $j < @o; ++$j) {
                last unless $o[$j] =~ /$def/;
                last unless $prefix eq $2;
                $nr_lines++;
                $len = length $1;
                $maxlen = $len if $maxlen < $len;
            }
            if ($nr_lines > 1) { # only if multiple #define PREFIX_ found
                for (; $nr_lines >= 1; $nr_lines--, $i++) {
                    die unless $o[$i] =~ /$def/;
                    $len = length $1;
                    $spaces = ' ' x ($maxlen - $len + 1);
                    $o[$i] =~ s/$def/#define $1$spaces/;
                }
            }
        }
    }

    # Overwrite the original file with the output.
    open OUTPUT, "> $input" or die "$input: $!";
    print OUTPUT $_ foreach @o;
    close OUTPUT;
}

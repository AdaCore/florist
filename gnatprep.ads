------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             G N A T P R E P                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$                              --
--                                                                          --
--          Copyright (C) 1992-1997, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

--  This program provides a simple preprocessing capability for Ada programs.
--  It is designed for use with GNAT, but is not dependent on any special
--  features of GNAT.

--    To call gnatprep use

--      gnatprep infile outfile deffile [-c] [-b] [-r] [-s] [-v]

--    where

--      infile is the full name of the input file, which is an Ada source
--      file containing preprocessor directives.

--      outfile is the full name of the output file, which is an Ada source
--      in standard Ada form. When used with GNAT, this file name will
--      normally have an ads or adb suffix.

--      deffile is the full name of a text file containing definitions of
--      symbols to be referenced by the preprocessor.

--      The -c switch, causes both preprocessor lines and the lines deleted
--      by preprocessing to be retained in the output source as comments marked
--      with the special string "--! ". This option will result in line numbers
--      being preserved in the output file.

--      The -b switch causes both preprocessor lines and the lines deleted by
--      preprocessing to be replaced by blank lines in the output source file,
--      thus preserving line numbers in the output file.

--      The -r switch causes a Source_Reference pragma to be generated that
--      references the original input file, so that error messages will use
--      the file name of this original file.

--      The -v switch causes gnatprep to output a copyright notice including
--      the version number of gnatprep.

--      The -s switch causes a sorted list of symbol names and values to be
--      listed on the standard output file.

--      Note: if neither -b nor -c is present, then preprocessor lines and
--      deleted lines are completely removed from the output, unless -r is
--      specified, in which case -b is assumed.

--   The definitions file contains lines of the form

--      symbol := value

--   where symbol is an identifier, following normal Ada (case-insensitive)
--   rules for its syntax, and value is one of the following:

--      Empty, corresponding to a null substitution

--      A string literal using normal Ada syntax

--      Any sequence of characters from the set
--        (letters, digits, period, underline)

--   Comment lines may also appear in the definitions file, starting with
--   the usual --, and comments may be added to the definitions lines.

--   The input text may contain preprocessor conditional inclusion lines,
--   and also general symbol substitution sequences.

--   The preprocessor conditional inclusion commands have the form

--      #if [not] symbol [then]
--         lines
--      #elsif [not] symbol [then]
--         lines
--      #elsif [not] symbol [then]
--         lines
--      ...
--      #else
--         lines
--      #end if;

--     For these Boolean tests, the symbol must have either the value True or
--     False. If the value is True, then the corresponding lines are included,
--     and if the value is False, they are excluded. It is an error to
--     reference a symbol not defined in the symbol definitions file, or
--     to reference a symbol that has a value other than True or False.

--     The use of the not operator inverts the sense of this logical test, so
--     that the lines are included only if the symbol is not defined.

--     The THEN keyword is optional as shown

--     The # must be in column one, but otherwise the format is free form.
--     Spaces or tabs may appear between the # and the keyword. The keywords
--     and the symbols are case insensitive as in normal Ada code. Comments
--     may be used on a preprocessor line, but other than that, no other
--     tokens may appear on a preprocessor line.

--     Any number of #elsif clauses can be present, including none at all.

--     The #else is optional, as in Ada.

--     The # marking the start of a preprocessor line must be the first
--     non-blank character on the line, i.e. it must be preceded only by
--     spaces or horizontal tabs.

--   Symbol substitution is obtained by using the sequence

--     $symbol

--   anywhere within a source line, except in a comment. The identifier
--   following the $ must match one of the symbols defined in the symbol
--   definition file, and the result is to substitute the value of the
--   symbol in place of $symbol in the output file.

procedure GNATprep;

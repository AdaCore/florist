------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             G N A T P R E P                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$                              --
--                                                                          --
--          Copyright (C) 1996-1997, Free Software Foundation, Inc.         --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Command_Line;        use Ada.Command_Line;
with Ada.Text_IO;             use Ada.Text_IO;

with GNAT.Heap_Sort_G;

procedure GNATprep is

   type Strptr is access String;

   Usage_Error : exception;
   --  Raised if a usage error is detected, causes termination of processing
   --  with an appropriate error message and error exit status set.

   Fatal_Error : exception;
   --  Exception raised if fatal error detected

   ------------------------
   -- Argument Line Data --
   ------------------------

   Infile_Name  : Strptr;
   Outfile_Name : Strptr;
   Deffile_Name : Strptr;
   --  Names of files

   Infile  : File_Type;
   Outfile : File_Type;
   Deffile : File_Type;

   Opt_Comment_Deleted_Lines : Boolean := False;  -- Set if -c switch set
   Blank_Deleted_Lines       : Boolean := False;  -- Set if -b switch set
   List_Symbols              : Boolean := False;  -- Set if -s switch set
   Source_Ref_Pragma         : Boolean := False;  -- Set if -r switch set
   Verbose_Mode              : Boolean := False;  -- Set if -v switch set
   --  Record command line options

   Arg : Strptr;
   --  One argument from command line

   Argnum : Natural;
   --  Count arguments on command line

   ---------------------------
   -- Definitions File Data --
   ---------------------------

   Num_Syms : Natural := 0;
   --  Number of symbols defined in definitions file

   Symbols : array (0 .. 10_000) of Strptr;
   Values  : array (0 .. 10_000) of Strptr;
   --  Symbol names and values. Note that the zero'th element is used only
   --  during the call to Sort (to hold a temporary value, as required by
   --  the GNAT.Heap_Sort_G interface).

   ---------------------
   -- Input File Data --
   ---------------------

   Current_File_Name : Strptr;
   --  Holds name of file being read (definitions file or input file)

   Line_Buffer : String (1 .. 20_000);
   --  Hold one line

   Line_Length : Natural;
   --  Length of line in Line_Buffer

   Line_Num : Natural;
   --  Current input file line number

   Ptr : Natural;
   --  Input scan pointer for line in Line_Buffer

   type Keyword is (K_Not, K_Then, K_If, K_Else, K_End, K_Elsif, K_None);
   --  Keywords that are recognized on preprocessor lines. K_None indicates
   --  that no keyword was present.

   K : Keyword;
   --  Scanned keyword

   Kptr : Positive;
   --  Pointer to start of scanned keyword

   Start_Sym, End_Sym : Natural;
   --  First and last positions of scanned symbol

   Num_Errors : Natural := 0;
   --  Number of errors detected

   -----------------------
   -- Preprocessor Data --
   -----------------------

   --  The following record represents the state of an #if structure:

   type PP_Rec is record
      If_Line : Positive;
      --  Line number for #if line

      Else_Line : Natural;
      --  Line number for #else line, zero = no else seen yet

      Deleting : Boolean;
      --  True if lines currently being deleted

      Match_Seen : Boolean;
      --  True if either the #if condition or one of the previously seen
      --  #elsif lines was true, meaning that any future #elsif sections
      --  or the #else section, is to be deleted.
   end record;

   PP_Depth : Natural;
   --  Preprocessor #if nesting level. A value of zero means that we are
   --  outside any #if structure.

   PP : array (0 .. 100) of PP_Rec;
   --  Stack of records showing state of #if structures. PP (1) is the
   --  outer level entry, and PP (PP_Depth) is the active entry. PP (0)
   --  contains a dummy entry whose Deleting flag is always set to False.

   Not_Seen : Boolean;
   --  Set True if NOT keyword present on if or elsif line

   Symbol_Defined : Boolean;
   --  Set True if symbol on #if or #elsif matches one of the defined symbols

   Symbol_Is_True : Boolean;
   --  Set True if symbol on #if or #elsif matches one of the defined symbols
   --  and the value is true. Otherwise set to False.

   -----------------
   -- Subprograms --
   -----------------

   function At_End_Of_Line return Boolean;
   --  First advances Ptr using Skip_Spaces. Then returns True if Ptr is
   --  either at the end of the line, or at a -- comment sequence.

   procedure Error (Msg : String);
   --  Post error message with given text. The line number is taken from
   --  Line_Num, and the column number from Ptr.

   function Is_Preprocessor_Line return Boolean;
   --  Tests if current line is a preprocessor line, i.e. that its first
   --  non-blank character is a # character. If so, then a result of True
   --  is returned, and Ptr is set to point to the character following the
   --  # character. If not, False is returned and Ptr is undefined.

   procedure No_Junk;
   --  Make sure no junk is present on a preprocessor line. Ptr points past
   --  the scanned preprocessor syntax.

   function OK_Identifier (S : String) return Boolean;
   --  Tests if given referenced string is valid Ada identifier

   function Matching_Strings (S1, S2 : String) return Boolean;
   --  Check if S1 and S2 are the same string (this is a case independent
   --  comparison, lower and upper case letters are considered to match).

   function Scan_Keyword return Keyword;
   --  Advances Ptr to end of line or next non-blank using Skip_Spaces. Then
   --  attempts to scan out a recognized keyword. if a recognized keyword is
   --  found, sets Ptr past it, and returns the code for the keyword, if not,
   --  then Ptr is left unchanged pointing to a non-blank character or to the
   --  end of the line. If a keyword is scanned Kptr is set to the first
   --  character of the keyword, otherwise Kptr is undefined.

   function Symbol_Scanned return Boolean;
   --  On entry, Start_Sym is set to the first character of an identifier
   --  symbol to be scanned out. On return, End_Sym is set to the last
   --  character of the identifier, and the result indicates if the scanned
   --  symbol is a valid identifier (True = valid). Ptr is not changed.

   procedure Skip_Spaces;
   --  Skips Ptr past tabs and spaces to next non-blank, or one character
   --  past the end of line.

   --------------------
   -- At_End_Of_Line --
   --------------------

   function At_End_Of_Line return Boolean is
   begin
      Skip_Spaces;

      return Ptr > Line_Length
        or else
          (Ptr < Line_Length and then Line_Buffer (Ptr .. Ptr + 1) = "--");
   end At_End_Of_Line;

   -----------
   -- Error --
   -----------

   procedure Error (Msg : String) is
      L : constant String := Natural'Image (Line_Num);
      C : constant String := Natural'Image (Ptr);

   begin
      Put (Standard_Error, Current_File_Name.all);
      Put (Standard_Error, ':');
      Put (Standard_Error, L (2 .. L'Length));
      Put (Standard_Error, ':');
      Put (Standard_Error, C (2 .. C'Length));
      Put (Standard_Error, ": ");

      Put_Line (Standard_Error, Msg);
      Num_Errors := Num_Errors + 1;
   end Error;

   --------------------------
   -- Is_Preprocessor_Line --
   --------------------------

   function Is_Preprocessor_Line return Boolean is
   begin
      Ptr := 1;

      while Ptr <= Line_Length loop
         if Line_Buffer (Ptr) = '#' then
            Ptr := Ptr + 1;
            return True;

         elsif Line_Buffer (Ptr) > ' ' then
            return False;

         else
            Ptr := Ptr + 1;
         end if;
      end loop;

      return False;
   end Is_Preprocessor_Line;

   ----------------------
   -- Matching_Strings --
   ----------------------

   function Matching_Strings (S1, S2 : String) return Boolean is
   begin
      if S1'Length /= S2'Length then
         return False;

      else
         for J in S1'Range loop
            if To_Upper (S1 (J)) /=
               To_Upper (S2 (J - S1'First + S2'First))
            then
               return False;
            end if;
         end loop;

         return True;
      end if;
   end Matching_Strings;

   -------------
   -- No_Junk --
   -------------

   procedure No_Junk is
   begin
      Skip_Spaces;

      if Ptr = Line_Length
        or else (Ptr < Line_Length
                   and then Line_Buffer (Ptr .. Ptr + 1) /= "--")
      then
         Error ("extraneous text on preprocessor line ignored");
      end if;
   end No_Junk;

   -------------------
   -- OK_Identifier --
   -------------------

   function OK_Identifier (S : String) return Boolean is
   begin
      if S'Length = 0
        or else not Is_Letter (S (S'First))
      then
         return False;

      else
         for P in S'Range loop
            if Is_Letter (S (P)) or Is_Digit (S (P)) then
               null;

            elsif S (P) = '_'
              and then P < S'Last
              and then S (P + 1) /= '_'
            then
               null;

            else
               return False;
            end if;
         end loop;

         return True;
      end if;
   end OK_Identifier;

   ------------------
   -- Scan_Keyword --
   ------------------

   function Scan_Keyword return Keyword is
   begin
      Skip_Spaces;
      Start_Sym := Ptr;

      if Symbol_Scanned then
         Kptr := Start_Sym;
         Ptr  := End_Sym + 1;

         declare
            Sym : constant String := Line_Buffer (Start_Sym .. End_Sym);

         begin
            if    Matching_Strings (Sym, "not")   then return K_Not;
            elsif Matching_Strings (Sym, "then")  then return K_Then;
            elsif Matching_Strings (Sym, "if")    then return K_If;
            elsif Matching_Strings (Sym, "else")  then return K_Else;
            elsif Matching_Strings (Sym, "end")   then return K_End;
            elsif Matching_Strings (Sym, "elsif") then return K_Elsif;
            end if;
         end;
      end if;

      Ptr := Start_Sym;
      return K_None;
   end Scan_Keyword;

   -----------------
   -- Skip_Spaces --
   -----------------

   procedure Skip_Spaces is
   begin
      while Ptr <= Line_Length loop
         if Line_Buffer (Ptr) /= ' '
           and then Line_Buffer (Ptr) /= Ascii.HT
         then
            return;
         else
            Ptr := Ptr + 1;
         end if;
      end loop;
   end Skip_Spaces;

   --------------------
   -- Symbol_Scanned --
   --------------------

   function Symbol_Scanned return Boolean is
   begin
      End_Sym := Start_Sym - 1;

      while End_Sym < Line_Length
        and then (Is_Alphanumeric (Line_Buffer (End_Sym + 1))
                    or else
                  Line_Buffer (End_Sym + 1) = '_')
      loop
         End_Sym := End_Sym + 1;
      end loop;

      return OK_Identifier (Line_Buffer (Start_Sym .. End_Sym));
   end Symbol_Scanned;

--  Start of processing for GNATprep

begin
   Argnum := 1;
   while Argnum <= Argument_Count loop

      Arg := new String'(Argument (Argnum));

      if Arg (1) /= '-' then
         if Infile_Name = null then
            Infile_Name := Arg;
         elsif Outfile_Name = null then
            Outfile_Name := Arg;
         elsif Deffile_Name = null then
            Deffile_Name := Arg;
         else
            raise Usage_Error;
         end if;

      --  Switch processing

      else -- Arg (1) = '-'

         if Arg'Length = 1
           or else (Arg'Length > 2 and then (Arg (2) /= 'D'
                                               and then
                                             Arg (2) /= 'd'))
         then
            raise Usage_Error;

         elsif Arg (2) = 'B' or else Arg (2) = 'b' then
            Blank_Deleted_Lines := True;

         elsif Arg (2) = 'C' or else Arg (2) = 'c' then
            Opt_Comment_Deleted_Lines := True;

         elsif Arg (2) = 'R' or else Arg (2) = 'r' then
            Source_Ref_Pragma := True;

         elsif Arg (2) = 'S' or else Arg (2) = 's' then
            List_Symbols := True;

         elsif Arg (2) = 'V' or else Arg (2) = 'v' then
            Verbose_Mode := True;

         else
            raise Usage_Error;
         end if;
      end if;

      Argnum := Argnum + 1;
   end loop;

   if Verbose_Mode then
      Put_Line (Standard_Error,
       "GNAT Preprocessor Version 1.03 " &
       "Copyright 1996 Free Software Foundation, Inc.");
   end if;

   if Deffile_Name = null then
      raise Usage_Error;
   end if;

   if Source_Ref_Pragma and (not Opt_Comment_Deleted_Lines) then
      Blank_Deleted_Lines := True;
   end if;

   --  Get symbol definitions

   begin
      Open (Deffile, In_File, Deffile_Name.all);

   exception
      when Name_Error =>
         Put_Line (Standard_Error, "cannot open " & Deffile_Name.all);
         raise Fatal_Error;
   end;

   Line_Num := 0;
   Current_File_Name := Deffile_Name;

   --  Loop through lines in symbol definitions file

   while not End_Of_File (Deffile) loop
      Get_Line (Deffile, Line_Buffer, Line_Length);
      Line_Num := Line_Num + 1;

      Ptr := 1;
      Skip_Spaces;

      if Ptr > Line_Length
        or else (Ptr < Line_Length
                   and then
                 Line_Buffer (Ptr .. Ptr + 1) = "--")
      then
         goto Continue;
      end if;

      Start_Sym := Ptr;

      if not Symbol_Scanned then
         Error ("invalid symbol identifier """ &
                Line_Buffer (Start_Sym .. End_Sym) &
                '"');
         goto Continue;
      end if;

      Ptr := End_Sym + 1;
      Skip_Spaces;

      if Ptr >= Line_Length
        or else Line_Buffer (Ptr .. Ptr + 1) /= ":="
      then
         Error ("missing "":="" in symbol definition line");
         goto Continue;
      end if;

      Ptr := Ptr + 2;
      Skip_Spaces;

      Num_Syms := Num_Syms + 1;
      Symbols (Num_Syms) := new String'(Line_Buffer (Start_Sym .. End_Sym));

      Start_Sym := Ptr;
      End_Sym := Ptr - 1;

      if At_End_Of_Line then
         null;

      elsif Line_Buffer (Start_Sym) = '"' then
         loop
            End_Sym := End_Sym + 2;

            if End_Sym > Line_Length then
               Error ("no closing quote for string constant");
               goto Continue;

            elsif End_Sym < Line_Length
              and then Line_Buffer (End_Sym .. End_Sym + 1) = """"""
            then
               End_Sym := End_Sym + 1;

            elsif Line_Buffer (End_Sym) = '"' then
               exit;
            end if;
         end loop;

      else
         End_Sym := Ptr - 1;

         while End_Sym < Line_Length
           and then (Is_Alphanumeric (Line_Buffer (End_Sym + 1))
                       or else
                     Line_Buffer (End_Sym + 1) = '_'
                       or else
                     Line_Buffer (End_Sym + 1) = '.')
         loop
            End_Sym := End_Sym + 1;
         end loop;

         Ptr := End_Sym + 1;

         if not At_End_Of_Line then
            Error ("incorrect symbol value syntax");
            goto Continue;
         end if;
      end if;

      Values (Num_Syms) := new String'(Line_Buffer (Start_Sym .. End_Sym));

      <<Continue>>
         null;
   end loop;

   if Num_Errors > 0 then
      raise Fatal_Error;

   elsif List_Symbols then
      declare
         procedure Move (From : Natural; To : Natural);
         --  Move routine for sort call

         function Lt (Op1, Op2 : Natural) return Boolean;
         --  Comparison routine for sort call

         procedure Move (From : Natural; To : Natural) is
         begin
            Symbols (To) := Symbols (From);
            Values  (To) := Values  (From);
         end Move;

         function Lt (Op1, Op2 : Natural) return Boolean is
            L1   : constant Natural := Symbols (Op1)'Length;
            L2   : constant Natural := Symbols (Op2)'Length;
            MinL : constant Natural := Natural'Min (L1, L2);

            C1, C2 : Character;

         begin
            for J in 1 .. MinL loop
               C1 := To_Upper (Symbols (Op1).all (J));
               C2 := To_Upper (Symbols (Op2).all (J));

               if C1 < C2 then
                  return True;

               elsif C1 > C2 then
                  return False;
               end if;
            end loop;

            return L1 < L2;
         end Lt;

         package Sort_Syms is new GNAT.Heap_Sort_G (Move, Lt);

         Max_L : Natural;
         --  Maximum length of any symbol

      begin
         Sort_Syms.Sort (Num_Syms);

         Max_L := 7;
         for J in 1 .. Num_Syms loop
            Max_L := Natural'Max (Max_L, Symbols (J)'Length);
         end loop;

         New_Line;
         Put ("Symbol");

         for J in 1 .. Max_L - 5 loop
            Put (' ');
         end loop;

         Put_Line ("Value");

         Put ("------");

         for J in 1 .. Max_L - 5 loop
            Put (' ');
         end loop;

         Put_Line ("------");

         for J in 1 .. Num_Syms loop
            Put (Symbols (J).all);

            for K in 1 .. Max_L - Symbols (J)'Length + 1 loop
               Put (' ');
            end loop;

            Put_Line (Values (J).all);
         end loop;

         New_Line;
      end;
   end if;

   --  Open files and initialize preprocessing

   begin
      Open (Infile,  In_File,  Infile_Name.all);

   exception
      when Name_Error =>
         Put_Line (Standard_Error, "cannot open " & Infile_Name.all);
         raise Fatal_Error;
   end;

   begin
      Create (Outfile, Out_File, Outfile_Name.all);

   exception
      when Name_Error =>
         Put_Line (Standard_Error, "cannot create " & Outfile_Name.all);
         raise Fatal_Error;
   end;

   if Source_Ref_Pragma then
      Put_Line
        (Outfile, "pragma Source_Reference (1, """ & Infile_Name.all & """);");
   end if;

   Line_Num := 0;
   Current_File_Name := Infile_Name;

   PP_Depth := 0;
   PP (0).Deleting := False;

   --  Loop through lines in input file

   while not End_Of_File (Infile) loop
      Get_Line (Infile, Line_Buffer, Line_Length);
      Line_Num := Line_Num + 1;

      --  Handle preprocessor line

      if Is_Preprocessor_Line then
         K := Scan_Keyword;

         case K is

            when K_None | K_Not | K_Then =>
               Error ("invalid preprocessor keyword syntax");

            --  If/Elsif processing

            when K_If | K_Elsif =>

               --  If differs from elsif only in that an initial stack entry
               --  must be made for the new if range. We set the match seen
               --  entry to a copy of the deleting status in the range above
               --  us. If we are deleting in the range above us, then we want
               --  all the branches of the nested #if to delete.

               if K = K_If then
                  PP_Depth := PP_Depth + 1;
                  PP (PP_Depth) :=
                    (If_Line    => Line_Num,
                     Else_Line  => 0,
                     Deleting   => False,
                     Match_Seen => PP (PP_Depth - 1).Deleting);

               elsif PP_Depth = 0 then
                  Error ("no matching #if for this #elsif");
                  goto Output;

               end if;

               --  Common processing for if and elsif

               Skip_Spaces;

               K := Scan_Keyword;

               if K = K_None then
                  Not_Seen := False;

               elsif K = K_Not then
                  Not_Seen := True;

               else
                  Ptr := Kptr;
                  Error ("invalid syntax in preprocessor line");
                  goto Output;
               end if;

               Skip_Spaces;
               Start_Sym := Ptr;

               if not Symbol_Scanned then
                  Error ("invalid symbol name");
                  goto Output;
               end if;

               Ptr := End_Sym + 1;

               Symbol_Defined := False;

               declare
                  Sym : constant String := Line_Buffer (Start_Sym .. End_Sym);

               begin
                  for J in 1 .. Num_Syms loop
                     if Matching_Strings (Symbols (J).all, Sym) then
                        Symbol_Defined := True;

                        if Matching_Strings (Values (J).all, "True") then
                           Symbol_Is_True := True;

                        elsif Matching_Strings (Values (J).all, "False") then
                           Symbol_Is_True := False;

                        else
                           Error ("symbol value is not True or False");
                           Symbol_Is_True := False;
                        end if;

                        exit;
                     end if;
                  end loop;

                  if not Symbol_Defined then
                     Error
                       ("symbol name """ &
                        Sym &
                        """ is not defined in definitions file");
                  end if;
               end;

               if Scan_Keyword = K_Then then
                  null;
               end if;

               if Symbol_Is_True xor Not_Seen then

                  --  Case of match and no match yet in this #if

                  if not PP (PP_Depth).Match_Seen then
                     PP (PP_Depth).Deleting := False;
                     PP (PP_Depth).Match_Seen := True;

                  --  Case of match, but we matched already in this #if

                  else
                     PP (PP_Depth).Deleting := True;
                  end if;

               --  Case of no match

               else
                  PP (PP_Depth).Deleting := True;
               end if;

               No_Junk;

            --  Processing for #else

            when K_Else =>

               if PP_Depth = 0 then
                  Error ("no matching #if for this #else");

               elsif PP (PP_Depth).Else_Line /= 0 then
                  Error ("duplicate #else line (previous was on line" &
                          Natural'Image (PP (PP_Depth).Else_Line)     &
                          ")");

               else
                  PP (PP_Depth).Else_Line := Line_Num;
                  PP (PP_Depth).Deleting := PP (PP_Depth).Match_Seen;
               end if;

               No_Junk;

            --  Process for #end

            when K_End =>

               if PP_Depth = 0 then
                  Error ("no matching #if for this #end");

               else
                  Skip_Spaces;

                  if Scan_Keyword /= K_If then
                     Error ("expected if after #end");
                     Ptr := Line_Length + 1;
                  end if;

                  Skip_Spaces;

                  if Ptr > Line_Length
                    or else Line_Buffer (Ptr) /= ';'
                  then
                     Error ("missing semicolon after #end if");
                  else
                     Ptr := Ptr + 1;
                  end if;

                  No_Junk;

                  PP_Depth := PP_Depth - 1;
               end if;

         end case;

      --  Handle symbol substitution

      else
         Ptr := 1;

         while Ptr < Line_Length loop
            exit when At_End_Of_Line;

            if Line_Buffer (Ptr) = '$' then

               --  $ found, so scan out possible following symbol

               Start_Sym := Ptr + 1;

               if Symbol_Scanned then

                  --  Look up symbol in table and if found do replacement

                  for J in 1 .. Num_Syms loop
                     if Matching_Strings
                          (Symbols (J).all, Line_Buffer (Start_Sym .. End_Sym))
                     then
                        declare
                           OldL : constant Positive := End_Sym - Start_Sym + 2;
                           NewL : constant Positive := Values (J)'Length;
                           AdjL : constant Integer  := NewL - OldL;
                           NewP : constant Positive := Ptr + NewL - 1;

                        begin
                           Line_Buffer (NewP + 1 .. Line_Length + AdjL) :=
                             Line_Buffer (End_Sym + 1 .. Line_Length);
                           Line_Buffer (Ptr .. NewP) := Values (J).all;

                           Ptr := NewP;
                           Line_Length := Line_Length + AdjL;
                        end;

                        exit;
                     end if;
                  end loop;
               end if;
            end if;

            Ptr := Ptr + 1;
         end loop;
      end if;

      --  Here after dealing with preprocessor line, output current line

      <<Output>>

      if (Line_Length > 0 and then Line_Buffer (1) = '#')
         or else PP (PP_Depth).Deleting
      then
         if Blank_Deleted_Lines then
            New_Line (Outfile);

         elsif Opt_Comment_Deleted_Lines then
            Put (Outfile, "--! ");
            Put_Line (Outfile, Line_Buffer (1 .. Line_Length));
         end if;

      else
         Put_Line (Outfile, Line_Buffer (1 .. Line_Length));
      end if;
   end loop;

   for J in 1 .. PP_Depth loop
      Error ("no matching #end for #if at line" &
             Natural'Image (PP (J).If_Line));
   end loop;

   if Num_Errors = 0 then
      Close (Outfile);
      Set_Exit_Status (0);
   else
      Delete (Outfile);
      Set_Exit_Status (1);
   end if;


exception
   when Usage_Error =>
      Put_Line
        (Standard_Error,
         "usage: gnatprep infile outfile deffile [-c] [-b] [-r] [-s] [-v]");
      Set_Exit_Status (1);

   when Fatal_Error =>
      Set_Exit_Status (1);

end GNATprep;

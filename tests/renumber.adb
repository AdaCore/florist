------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                             R E N U M B E R                              --
--                                                                          --
--                                B o d y                                   --
--                                                                          --
--                                                                          --
--  Copyright (c) 1998      Florida  State  University  (FSU).  All Rights  --
--  Reserved.                                                               --
--                                                                          --
--  This is free software;  you can redistribute it and/or modify it under  --
--  terms of the  GNU  General  Public  License  as published by the  Free  --
--  Software Foundation;  either version 2, or (at your option) any  later  --
--  version.  This  software  is distributed  in the hope that it  will be  --
--  useful, but WITHOUT ANY WARRANTY; without even the implied warranty of  --
--  MERCHANTABILITY   or  FITNESS FOR A PARTICULAR PURPOSE.   See the  GNU  --
--  General Public License for more details.  You  should have  received a  --
--  copy of the GNU General Public License  distributed  with  GNARL;  see  --
--  file  COPYING.  If not,  write to  the  Free  Software  Foundation, 59  --
--  Temple Place - Suite 330, Boston, MA 02111-1307, USA.                   --
--                                                                          --
--  Under contract  GS-35F-4506G, the U. S. Government obtained  unlimited  --
--  rights in the software and documentation contained herein.   Unlimited  --
--  rights are defined in DFAR 252,227-7013(a)(19).  By making this public  --
--  release,   the  Government  intends  to  confer  upon  all  recipients  --
--  unlimited  rights equal to those held by the Government.  These rights  --
--  include rights to use,  duplicate,  release  or  disclose the released  --
--  data an computer software  in whole or in part,  in any manner and for  --
--  any purpose whatsoever, and to have or permit others to do so.          --
--                                                                          --
--  DISCLAIMER   --   ALL MATERIALS OR INFORMATION HEREIN RELEASED,   MADE  --
--  AVAILABLE OR DISCLOSED ARE AS IS.   THE GOVERNMENT MAKES NO EXPRESS OR  --
--  IMPLIED WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS  --
--  OF THE SOFTWARE,  DOCUMENTATION  OR  OTHER INFORMATION RELEASED,  MADE  --
--  AVAILABLE OR DISCLOSED, OR THE OWNERSHIP,  MERCHANTABILITY, OR FITNESS  --
--  FOR A PARTICULAR PURPOSE OF SAID MATERIAL.                              --
--                                                                          --
------------------------------------------------------------------------------
--  [$Revision$]

--  Renumbers failure message identifiers in a POSIX.5b test. (a tool)

with Ada.Command_Line,
     Ada.Exceptions,
     Ada.Strings,
     Ada.Strings.Fixed,
     Ada.Text_IO;
procedure Renumber is
   use Ada.Command_Line,
       Ada.Exceptions,
       Ada.Strings,
       Ada.Strings.Fixed,
       Ada.Text_IO;
   Infile, Outfile : File_Type;
   Buf : String (1 .. 128);
   Last, I, J, K, L : Integer;
   Count : Integer := 0;
   Z : constant Integer := Character'Pos ('0');
begin
   if Argument_Count /= 1 then
      Put_Line (Standard_Error, "usage: numbering <filename>");
      return;
   end if;
   begin
      Open (Infile, In_File, Argument (1));
   exception when others =>
      Put_Line (Standard_Error,
      "cannot open input file " & Argument (1));
      return;
   end;
   begin
      Open (Outfile, Out_File, Argument (1) & ".scr");
   exception when others =>
      begin
         Create (Outfile, Out_File, Argument (1) & ".scr");
      exception
      when others =>
         Put_Line (Standard_Error,
         "cannot open or create output file " & Argument (1) & ".scr");
         return;
      end;
   end;
   begin
      loop
         Get_Line (Infile, Buf, Last);
         I := 1; J := I + 4;
         while J <= Last loop
            if Buf (I) = '"'
              and then Buf (I + 1) = 'A'
              and then Buf (I + 2) in '0' .. '9'
              and then Buf (I + 3) in '0' .. '9'
              and then Buf (I + 4) in '0' .. '9'
            then
               Count := Count + 1;
               Put (Outfile, """A"); K := Count;
               L := K / 100;
               if L /= 0 then
                  Put (Outfile, Character'Val (Z + L)); K:= K - L * 100;
               else Put (Outfile, '0');
               end if;
               L := K / 10;
               if L /= 0 then
                  Put (Outfile, Character'Val (Z + L)); K:= K - L * 10;
               else Put (Outfile, '0');
               end if;
               if K /= 0 then
                  Put (Outfile, Character'Val (Z + K));
               else Put (Outfile, '0');
               end if;
               if Buf (I + 5) in 'a' .. 'z' then
                  I := I + 6; J := J + 6;
               else
                  I := I + 5; J := J + 5;
               end if;
            else
               Put (Outfile, Buf (I)); I:= I + 1; J := J + 1;
            end if;
         end loop;
         for M in I .. Last loop
            Put (Outfile, Buf (M));
         end loop;
         New_Line (Outfile);
      end loop;
   exception
   when others => null;
   end;
   Close (Infile);
   Close (Outfile);
   begin
      Open (Infile, In_File, Argument (1) & ".scr");
   exception when others =>
      Put_Line (Standard_Error,
      "cannot open input file " & Argument (1) & ".scr");
      return;
   end;
   begin
      Open (Outfile, Out_File, Argument (1));
   exception when others =>
      Put_Line (Standard_Error,
      "cannot open output file " & Argument (1));
      return;
   end;
   begin
      loop
         Get_Line (Infile, Buf, Last);
         Put_Line (Outfile, Buf (1 .. Last));
      end loop;
   exception when End_Error => null;
   end;
   Close (Infile);
   Close (Outfile);
exception
when others =>
   Close (Infile);
end Renumber;

------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                             P 0 3 0 1 0 0 b                              --
--                                                                          --
--                                B o d y                                   --
--                                                                          --
--                                                                          --
--  Copyright (c) 1995-1998 Florida  State  University  (FSU).  All Rights  --
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
--  AVAILABLE OR DISCLOSED,  OR THE OWNERSHIP,  MERCHANTABILITY, OR FITNESS  --
--  FOR A PARTICULAR PURPOSE OF SAID MATERIAL.                              --
--                                                                          --
------------------------------------------------------------------------------
--  [$Revision$]

with POSIX,
     POSIX_Process_Environment,
     POSIX_Report;

procedure p030100b is

   use POSIX,
       POSIX_Process_Environment,
       POSIX_Report;

   --  Cases for child process:
   Should_Not_Start     : constant := 1;
   Parents_Environment  : constant := 2;
   Explicit_Environment : constant := 3;

   Child_Filename : constant POSIX_String := "p030100b";

   Arg_Count : Integer := 0;
   procedure Print_Arg (S : POSIX_String; Quit : in out Boolean);
   procedure Print_Arg (S : POSIX_String; Quit : in out Boolean) is
   begin
      Arg_Count := Arg_Count + 1;
      Quit := False;
      Comment ("Argument (" & Integer'Image (Arg_Count) & ") = """ &
        To_String (S) & """");
   end Print_Arg;
   procedure Print_Args is new For_Every_Item (Print_Arg);

begin

   Comment ("child process for test p030100");
   Comment ("child =" & Integer'Image (Child));
   Print_Args (Argument_List);

   -------------------------------------------------------------------------

   begin
      if Child = Should_Not_Start then
         --  Fail because process should not have been created
         Assert (False, "A001: P030100b: creation should have failed");
         Done;
      else
         --  Check for bad argument(s)
         Assert (Value (Argument_List, 1) = "p030100b" and
           (Child in Parents_Environment .. Explicit_Environment),
            "A002: P030100b: bad arg: " &
             To_String (Value (Argument_List, 1)));
         if Child = Explicit_Environment then
            --   check environment variables also
            Assert (Environment_Value_Of
              (Child_Filename) = "special",
              "A003: P030100b: wrong env. value");
            if Environment_Value_Of ("WAIT") = "YES" then
               Comment ("waiting for parent to send signal");
               loop
                  delay 10.0;
               end loop;
               --  Fail because parent did not kill child
               Assert (False, "A004: P030100b: not killed");
            end if;
         else
            --  Check environment variable
            Assert (Environment_Value_Of
              (Child_Filename) = "default",
              "A005: P030100b: wrong env. value");
         end if;
      end if;
   exception when E : others =>
      Unexpected_Exception (E, "A006");
   end;

   -----------------------------------------------------------

   Done;
exception
when E : others => Unexpected_Exception (E, "A007");
end p030100b;

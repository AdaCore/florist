------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                             P 0 3 0 1 0 1 b                              --
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

--  Test package POSIX_Process_Primitives,
--  defined by IEEE Std 1003.5b Section 3.1,
--  for consistency with package Ada.Command_Line.

--  ....
--  This test needs more work; it is presently incomplete.

with Ada.Command_Line,
     POSIX,
     POSIX_Process_Environment,
     POSIX_Process_Primitives,
     POSIX_Report;

procedure p030101b is

   use POSIX,
       POSIX_Process_Environment,
       POSIX_Process_Primitives,
       POSIX_Report;

   --  Cases to be tested by child process

   Normal_Completion : constant := 1;
   Normal_Completion_With_Ada_Status : constant := 2;
   Unhandled_Exception : constant := 3;
   POSIX_Exit_Process : constant := 4;

   Arg_Count : Integer := 0;
   procedure Print_Arg (S : POSIX_String; Quit : in out Boolean);
   procedure Print_Arg (S : POSIX_String; Quit : in out Boolean) is
   begin
      Quit := False;
      Comment ("Argument (" & Integer'Image (Arg_Count) &
        " =" & To_String (S));
   end Print_Arg;
   procedure Print_Args is new For_Every_Item (Print_Arg);

begin

   Comment ("child process for test p030101");
   Print_Args (Argument_List);

   -------------------------------------------------------------------------

   --  Check for inconsistent argument counts
   Assert (Ada.Command_Line.Argument_Count =
     Length (Argument_List) - 1, "A001");

   -------------------------------------------------------------------------

   begin
      --  Check first argument
      Assert (Ada.Command_Line.Command_Name =
        To_String (Value (Argument_List, 1)),
        "A002");
      --  Check second argument
      Assert (Ada.Command_Line.Argument (1) =
        To_String (Value (Argument_List, 2)),
        "A003");
   exception when E : others => Unexpected_Exception (E, "A004");
   end;

   -----------------------------------------------------------

   if Child = Normal_Completion then
      return;
   elsif Child = Normal_Completion_With_Ada_Status then
      Ada.Command_Line.Set_Exit_Status (77);
      return;
   elsif Child = Unhandled_Exception then
      raise Program_Error;
   elsif Child = POSIX_Exit_Process then
      Exit_Process (78);
   --  Fail if abnormal -child argument value
   else Fail ("A005");
   end if;

exception when E : others =>
   if Child = Unhandled_Exception then raise;
   else Fatal_Exception (E, "A006");
   end if;
end p030101b;

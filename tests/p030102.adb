------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                             P 0 3 0 1 0 2                                --
--                                                                          --
--                                B o d y                                   --
--                                                                          --
--                                                                          --
--  Copyright (c) 1998-1999 Florida  State  University  (FSU).  All Rights  --
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

--  Test package POSIX_Process_Primitives,
--  defined by IEEE Std 1003.5b Section 3.1,
--  for consistency with package Ada.Command_Line.

--  Setup: The program must be run with the executable file for
--  program p030102b accessible via the pathname "./p030102b".

with Ada.Command_Line,
     POSIX,
     POSIX_Process_Environment,
     POSIX_Process_Identification,
     POSIX_Process_Primitives,
     POSIX_Report,
     POSIX_Signals;

procedure p030102 is

   use POSIX,
       POSIX_Process_Environment,
       POSIX_Process_Identification,
       POSIX_Process_Primitives,
       POSIX_Report,
       POSIX_Signals;

   Child_Program_Pathname : constant POSIX_String := "./p030102";

   --  Cases to be tested by child processes:

   type Test_Cases is
     (Normal_Completion,
      Normal_Completion_With_Ada_Status,
      Unhandled_Exception,
      POSIX_Exit_Process);

   procedure Check_Child_Status
     (Status : Termination_Status;
      Child_ID : Process_ID;
      Expected : Exit_Status);

   procedure Check_Child_Status
     (Status : Termination_Status;
      Child_ID : Process_ID;
      Expected : Exit_Status) is
      E : Exit_Status;
   begin
      Assert (Child_ID /= Null_Process_ID, "null id");
      if not Status_Available (Status) then
         Fail ("A001: status not available");
         return;
      end if;
      Assert (Process_ID_Of (Status) = Child_ID, "child ID");
      if Termination_Cause_Of (Status) /= Exited then
         Fail ("A002: did not exit");
         return;
      end if;
      E := Exit_Status_Of (Status);
      if E > 0 and E < Failed_Creation_Exit then
         --  child process reports errors via exit status
         Increment_Error_Count (Integer (E));
      elsif E /= Expected then
         Fail ("A003: unexpected exit status" & Exit_Status'Image (E));
      end if;
      declare
         Sig : Signal;
      begin
         Sig := Stopping_Signal_Of (Status);
         Assert (False, "Stopping_Signal_Of invalid status");
      exception
      when POSIX_Error =>
         Check_Error_Code (Invalid_Argument, "A004");
      when E : others =>
         Unexpected_Exception (E, "A005");
      end;
      declare
         Sig : Signal;
      begin
         Sig := Termination_Signal_Of (Status);
         Assert (False, "Termination_Signal_Of invalid status");
      exception
      when POSIX_Error =>
         Check_Error_Code (Invalid_Argument, "A006");
      when E : others =>
         Unexpected_Exception (E, "A007");
      end;
   exception when E : others =>
      Unexpected_Exception (E, "checking child status");
   end Check_Child_Status;

   procedure p030102b;

   procedure p030102b is

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

      Comment ("child process for test p030102");
      Print_Args (Argument_List);

      -------------------------------------------------------------------------

      Assert (Ada.Command_Line.Argument_Count =
        Length (Argument_List) - 1, "inconsistent argument counts");

      -------------------------------------------------------------------------

      begin
         Assert (Ada.Command_Line.Command_Name =
           To_String (Value (Argument_List, 1)),
           "inconsistent first argument");
         Assert (Ada.Command_Line.Argument (1) =
           To_String (Value (Argument_List, 2)),
           "inconsistent first argument");
      exception when E : others =>
         Unexpected_Exception (E, "A008");
      end;

      -----------------------------------------------------------

      case Test_Cases'Val (Child) is
      when Normal_Completion =>
         return;
      when Normal_Completion_With_Ada_Status =>
         Ada.Command_Line.Set_Exit_Status (77);
         return;
      when Unhandled_Exception =>
         raise Program_Error;
      when POSIX_Exit_Process =>
         Exit_Process (78);
      when others => Fail ("A009: abnormal -child argument value");
      end case;

   exception when E : others =>
      if Test_Cases'Val (Child) = Unhandled_Exception then raise;
      end if;
      Unexpected_Exception (E, "A010");
   end p030102b;

begin

   if Child /= 0 then
      p030102b;
      Done;
   end if;

   Header ("p030102");

   ---------------------------------------------------------------------

   for I in Test_Cases loop
      declare
         Pid : Process_ID;
         Status : Termination_Status;
         Template : Process_Template;
         Args : POSIX_String_List;
      begin
         Test (Test_Cases'Image (I));
         Open_Template (Template);
         Comment ("Set up argument list");
         POSIX.Append (Args, Child_Program_Pathname);
         POSIX.Append (Args, "-child");
         POSIX.Append (Args,
           To_POSIX_String (Integer'Image (Test_Cases'Pos (I))));
         Start_Process (Pid, Child_Program_Pathname, Template, Args);
         Wait_For_Child_Process (Status, Pid);
         Check_Child_Status (Status, Pid, Normal_Exit);
         Close_Template (Template);
      exception
      when E : others =>
         Unexpected_Exception (E, "A011");
      end;
   end loop;

   --------------------------------------------------------------------

   Done;

exception
when E : others =>
   if Child = Test_Cases'Pos (Unhandled_Exception) then raise;
      --  Allow child process to be terminated by unhandled exception.
   else Unexpected_Exception (E, "A012: child process");
   end if;
end p030102;

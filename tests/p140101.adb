------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                             P 1 4 0 1 0 1                                --
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
--  AVAILABLE OR DISCLOSED,  OR THE OWNERSHIP,  MERCHANTABILITY, OR FITNESS  --
--  FOR A PARTICULAR PURPOSE OF SAID MATERIAL.                              --
--                                                                          --
------------------------------------------------------------------------------
--  [$Revision$]

--  Test for package POSIX_Timers,
--  in IEEE Std 1003.5b Section 14.1.

--  This test covers features that require special privilege.

with POSIX,
     POSIX_Report,
     POSIX_Timers,
     Test_Parameters;

procedure p140101 is

   use POSIX,
       POSIX_Report,
       POSIX_Timers,
       Test_Parameters;

   Time_Restore : Timespec;
   --  Used to restore clock value.

begin

   Header ("p140101", Root_OK => True);

   -----------------------------------------------------------------------
   --  Set_Time can be used to set the clock to a valid
   --  value, unless the calling process lacks sufficient privilege.

   Test ("Set_Time [14.1.4] all valid data");
   begin
      Time_Restore := Get_Time (Clock_Realtime);
      Set_Time (Clock_Realtime, Time_Restore);
      Assert (Time_Restore <= Get_Time (Clock_Realtime), "A001");
      Assert (Get_Seconds (Time_Restore)
        <= Get_Seconds (Get_Time (Clock_Realtime)), "A002");
   exception
   when E1 : POSIX_Error =>
      Privileged (Set_Time_Privilege,
        Timers_Option, Operation_Not_Implemented, E1, "A003");
   when E2 : others => Unexpected_Exception (E2, "A004");
   end;

   -----------------------------------------------------------------------
   --  POSIX_Error is raised with error code Invalid_Argument if
   --  Set_Time is called with invalid clock identifier.

   --  An invalid value of any type may be obtained with high
   --  probability via an uninitialized variable.  To improve
   --  the odds, we can try to make sure the memory contains
   --  an "interesting" value (e.g. not zero) by overlaying
   --  an uninitialized variable on a memory location that
   --  previously contained a value of a different type.

   Test ("Set_Time [14.1.4] Invalid clock ID");
   declare
      T : Timespec;
      Invalid : Clock_ID := Invalid_Clock_ID;
      Invalid_ID_Found : Boolean := False;
   begin
      --  Check that we actually have an invalid Clock_ID value.
      begin
         T := Get_Time (Invalid);
         Comment ("WARNING: garbage clock ID appears to work!");
      exception
      when E1 : POSIX_Error =>
         if Get_Error_Code = Invalid_Argument then
            Invalid_ID_Found := True;
         else
            Optional (Timers_Option,
              Operation_Not_Implemented, E1, "A005");
         end if;
      when E2 : others =>
         Unexpected_Exception (E2, "A006");
      end;
      if Invalid_ID_Found then
         Set_Time (Invalid, To_Timespec (0.1));
         Expect_Exception ("A007");
      end if;
   exception
   when E1 : POSIX_Error =>
      Privileged (Privilege => Set_Time_Privilege,
        Option => Timers_Option,
        Expected_If_Not_Supported => Operation_Not_Implemented,
        Expected_If_Supported => Invalid_Argument,
        E => E1, Message => "A008");
   when E2 : others => Unexpected_Exception (E2, "A009");
   end;

   ------------------------------------------------------------
   --  Testing to see if the Clock_Realtime can support up to
   --  at least Seconds'Last.

   Test ("Clock_Realtime can be set to Seconds'Last [14.1.2]");
   Comment ("This test can be performed only by the system adminstrator");
   declare
      Time_After : Timespec;
   begin
      Time_Restore := Get_Time (Clock_Realtime);
      Set_Time (Clock_Realtime, To_Timespec (Seconds'Last, 0));
      Time_After := Get_Time (Clock_Realtime);
      Assert (Get_Seconds (Time_After) = Seconds'Last,
        "A010: " & Image (Time_After));
      Set_Time (Clock_Realtime, Time_Restore);
   exception
   when E1 : POSIX_Error =>
      Privileged (Set_Time_Privilege,
        Timers_Option, Operation_Not_Implemented,
        E1, "A011");
   end;

   -----------------------------------------------------------------------
   --  POSIX_Error is raised with error code Invalid_Argument if
   --  the Value parameter for Set_Time is outside
   --  the range allowed for the specified Clock.

   Test ("Set_Time [14.1.4] negative Timespec");
   begin
      Set_Time (Clock_Realtime, To_Timespec (-5.0));
      --  This may or may not raise an exception.
      Set_Time (Clock_Realtime, Time_Restore);
   exception
   when Constraint_Error =>
      --  Value might simply be out of range.
      null;
   when E1 : POSIX_Error =>
      Privileged (Privilege => Set_Time_Privilege,
        Option => Timers_Option,
        Expected_If_Not_Supported => Operation_Not_Implemented,
        Expected_If_Supported => Invalid_Argument,
        E => E1, Message => "A012");
   when E2 : others => Unexpected_Exception (E2, "A013");
   end;

   Test ("Set_Time [14.1.4] zero Timespec");
   begin
      Set_Time (Clock_Realtime, To_Timespec (0.0));
      --  This may or may not raise an exception.
      Set_Time (Clock_Realtime, Time_Restore);
   exception
   when Constraint_Error =>
      --  Value might simply be out of range.
      null;
   when E1 : POSIX_Error =>
      Privileged (Privilege => Set_Time_Privilege,
        Option => Timers_Option,
        Expected_If_Not_Supported => Operation_Not_Implemented,
        Expected_If_Supported => Invalid_Argument,
        E => E1, Message => "A014");
   when E2 : others => Unexpected_Exception (E2, "A015");
   end;

   Test ("Set_Time [14.1.4] large positive Timespec");
   begin
      Set_Time (Clock_Realtime, To_Timespec (1000_000_000.0));
      --  This may or may not raise an exception.
      Set_Time (Clock_Realtime, Time_Restore);
   exception
   when Constraint_Error =>
      --  Value might simply be out of range.
      null;
   when E1 : POSIX_Error =>
      Privileged (Privilege => Set_Time_Privilege,
        Option => Timers_Option,
        Expected_If_Not_Supported => Operation_Not_Implemented,
        Expected_If_Supported => Invalid_Argument,
        E => E1, Message => "A016");
   when E2 : others => Unexpected_Exception (E2, "A017");
   end;

   -----------------------------------------------------------------------
   --  POSIX_Error is raised with error code Invalid_Argument if
   --  the argument of type Timespec cannot not be interpreted
   --  as a valid time.

   Test ("Set_Time [14.1.4] Invalid Timespec");
   declare
      Time : Timespec := Invalid_Timespec;
   begin
      Set_Time (Clock_Realtime, Time);
      Expect_Exception ("A018");
      Set_Time (Clock_Realtime, Time_Restore);
   exception
   when Constraint_Error =>
      --  Value might simply be out of range.
      null;
   when E1 : POSIX_Error =>
      Privileged (Privilege => Set_Time_Privilege,
        Option => Timers_Option,
        Expected_If_Not_Supported => Operation_Not_Implemented,
        Expected_If_Supported => Invalid_Argument,
        E => E1, Message => "A019");
   when E2 : others => Unexpected_Exception (E2, "A020");
   end;

   Test ("Set_Time [14.1.4] uninitialized Timespec");
   declare
      Uninitialized_Time : Timespec;
   begin
      Set_Time (Clock_Realtime, Uninitialized_Time);
      --  No exception will be raised if the value happens to
      --  be in range, by accident.
      Set_Time (Clock_Realtime, Time_Restore);
   exception
   when Constraint_Error =>
      --  Value might simply be out of range.
      null;
   when E1 : POSIX_Error =>
      Privileged (Privilege => Set_Time_Privilege,
        Option => Timers_Option,
        Expected_If_Not_Supported => Operation_Not_Implemented,
        Expected_If_Supported => Invalid_Argument,
        E => E1, Message => "A021");
   when E2 : others => Unexpected_Exception (E2, "A022");
   end;

   -----------------------------------------------------------------------
   --  Time values between two consecutive multiples of the resolution
   --  of the specified clock shall be truncated down to the smaller
   --  multiple of the resolution (Set_Time).

   Test ("Set_Time [14.1.4] Time truncated to the smaller resolution");
   declare
      Clock : constant Clock_ID := Clock_Realtime;
      Resolution : Timespec;
      Diff_1, Diff_2 : Timespec;
      Target_1, Target_2 : Timespec;

   begin
      Resolution := Get_Resolution (Clock);
      Time_Restore := Get_Time (Clock_Realtime);
      Target_1 := Time_Restore;
      Target_2 := Time_Restore + Resolution / 2;
      Set_Time (Clock, Target_1);
      Diff_1 := Get_Time (Clock) - Target_1;
      Set_Time (Clock, Target_2);
      Diff_2 := Get_Time (Clock) - Target_2;
      Set_Time (Clock, Time_Restore);
      Comment ("Resolution", Resolution);
      Comment ("Diff_1", Diff_1);
      Comment ("Diff_2", Diff_2);
      Assert (Diff_1 - Diff_2 < Resolution / 4,
        "A023: time apparently not truncated");
   exception
   when E1 : POSIX_Error =>
      Privileged (Privilege => Set_Time_Privilege,
        Option => Timers_Option,
        Expected_If_Not_Supported => Operation_Not_Implemented,
        Expected_If_Supported => Invalid_Argument,
        E => E1, Message => "A024");
   when E2 : others => Unexpected_Exception (E2, "A025");
   end;

   -----------------------------------------------------------------------

   Done;

exception
when E1 : POSIX_Error =>
   Optional (Timers_Option, Operation_Not_Implemented, E1, "A026");
   Done;
when E2 : others => Fatal_Exception (E2, "A027");
end p140101;

------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                             P 1 4 0 1 0 1                                --
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
     POSIX_Signals,
     POSIX_Timers,
     Test_Parameters;

procedure p140101 is

   use POSIX,
       POSIX_Report,
       POSIX_Signals,
       POSIX_Timers,
       Test_Parameters;

begin

   Header ("A001101", Root_OK => True);

   ------------------------------------------------------------
   --  Try to find an invalid Clock_ID value.

   ------------------------------------------------------------
   --  Testing to see if the Clock_Realtime can support up to
   --  at least Seconds'Last.

   Test ("Clock_Realtime can be set to Seconds'Last [14.1.2]");
   Comment ("This test can be performed only by the system adminstrator");
   declare
      Time_Restore : Timespec;
      Time_After : Timespec;
   begin
      Time_Restore := Get_Time (Clock_Realtime);
      Set_Time (Clock_Realtime, To_Timespec (Seconds'Last, 0));
      Time_After := Get_Time (Clock_Realtime);
      Assert (Get_Seconds (Time_After) = Seconds'Last,
        "A000: " & Image (Time_After));
      Set_Time (Clock_Realtime, Time_Restore);
   exception
   when E1 : POSIX_Error =>
      Privileged (Set_Time_Privilege,
        Timers_Option, Operation_Not_Implemented,
        E1, "A000");
   end;

   -----------------------------------------------------------------------
   --  Set_Time can be used to set the clock to a valid
   --  value, unless the calling process lacks sufficient privilege.

   Test ("Set_Time [14.1.4] all valid data");
   declare
      Time : Timespec;
   begin
      Time := Get_Time (Clock_Realtime);
      Set_Time (Clock_Realtime, Time);
      Assert (Time <= Get_Time (Clock_Realtime), "A000");
      Assert (Get_Seconds (Time)
        <= Get_Seconds (Get_Time (Clock_Realtime)), "A000");
   exception
   when E1 : POSIX_Error =>
      Privileged (Set_Time_Privilege,
        Timers_Option, Operation_Not_Implemented, E1, "A000");
   when E2 : others => Unexpected_Exception (E2, "A000");
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
              Operation_Not_Implemented, E1, "A000");
         end if;
      when E2 : others =>
         Unexpected_Exception (E2, "A000");
      end;
      if Invalid_ID_Found then
         Set_Time (Invalid, To_Timespec (0.1));
         Expect_Exception ("A000");
      end if;
   exception
   when E1 : POSIX_Error =>
      Privileged (Privilege => Set_Time_Privilege,
        Option => Timers_Option,
        Expected_If_Not_Supported => Operation_Not_Implemented,
        Expected_If_Supported => Invalid_Argument,
        E => E1, Message => "A000");
   when E2 : others => Unexpected_Exception (E2, "A000");
   end;

   -----------------------------------------------------------------------
   --  POSIX_Error is raised with error code Invalid_Argument if
   --  the Value parameter for Set_Time is outside
   --  the range allowed for the specified Clock.

   Test ("Set_Time [14.1.4] Valid Range (<0)");
   begin
      Set_Time (Clock_Realtime, To_Timespec (-5.0));
         Expect_Exception ("A000");
   exception
   when E1 : POSIX_Error =>
      Privileged (Privilege => Set_Time_Privilege,
        Option => Timers_Option,
        Expected_If_Not_Supported => Operation_Not_Implemented,
        Expected_If_Supported => Invalid_Argument,
        E => E1, Message => "A000");
   when E2 : others => Unexpected_Exception (E2, "A000");
   end;

   Test ("Set_Time [14.1.4] Valid Range (=0)");
   begin
      Set_Time (Clock_Realtime, To_Timespec (-0.0));
         Expect_Exception ("A000");
   exception
   when E1 : POSIX_Error =>
      Privileged (Privilege => Set_Time_Privilege,
        Option => Timers_Option,
        Expected_If_Not_Supported => Operation_Not_Implemented,
        Expected_If_Supported => Invalid_Argument,
        E => E1, Message => "A000");
   when E2 : others => Unexpected_Exception (E2, "A000");
   end;

   Test ("Set_Time [14.1.4] Valid Range (=1000_000_000)");
   begin
      Set_Time (Clock_Realtime, To_Timespec (1000_000_000.0));
         Expect_Exception ("A000");
   exception
   when E1 : POSIX_Error =>
      Privileged (Privilege => Set_Time_Privilege,
        Option => Timers_Option,
        Expected_If_Not_Supported => Operation_Not_Implemented,
        Expected_If_Supported => Invalid_Argument,
        E => E1, Message => "A000");
   when E2 : others => Unexpected_Exception (E2, "A000");
   end;

   Test ("Set_Time [14.1.4] Valid Range (>1000_000_000)");
   begin
      Set_Time (Clock_Realtime, To_Timespec (1000_000_001.0));
         Expect_Exception ("A000");
   exception
   when E1 : POSIX_Error =>
      Privileged (Privilege => Set_Time_Privilege,
        Option => Timers_Option,
        Expected_If_Not_Supported => Operation_Not_Implemented,
        Expected_If_Supported => Invalid_Argument,
        E => E1, Message => "A000");
   when E2 : others => Unexpected_Exception (E2, "A000");
   end;

   -----------------------------------------------------------------------
   --  POSIX_Error is raised with error code Invalid_Argument if
   --  the argument of type Timespec cannot not be interpreted
   --  as a valid time.

   Test ("Set_Time [14.1.4] Valid Time");
   declare
      Time : Timespec;
   begin
      Set_Time (Clock_Realtime, Time);
   exception
   when E1 : POSIX_Error =>
      Privileged (Privilege => Set_Time_Privilege,
        Option => Timers_Option,
        Expected_If_Not_Supported => Operation_Not_Implemented,
        Expected_If_Supported => Invalid_Argument,
        E => E1, Message => "A000");
   when E2 : others => Unexpected_Exception (E2, "A000");
   end;
   -----------------------------------------------------------------------
   --  Time values between two consecutive multiples of the resolution
   --  of the specified clock shall be truncated down to the smaller
   --  multiple of the resolution (Set_Time).

   Test ("Set_Time [14.1.4] Time truncated to the smaller resolution");
   Comment ("This test asserts that a time value between two" &
            " consecutive multiples of the resolution is" &
            " truncated down to the smaller one");
   declare
      Clock : constant Clock_ID := Clock_Realtime;
      Value1 : Timespec;
      Value2 : Timespec;
      Value3 : Timespec;
   begin
      Value3 := Get_Resolution (Clock);
      Set_Time (Clock, Value3 + To_Timespec (100.0));
      Value1 := Get_Time (Clock);
      Set_Time (Clock, (Value3 * 3 / 2) + To_Timespec (100.0));
      Value2 := Get_Time (Clock);
      Set_Time (Clock, (Value3 * 3) + To_Timespec (100.0));
      Value3 := Get_Time (Clock);
      Assert ((Value3 - Value2) > (Value2 - Value1), "A000");
   exception
   when E1 : POSIX_Error =>
      Privileged (Privilege => Set_Time_Privilege,
        Option => Timers_Option,
        Expected_If_Not_Supported => Operation_Not_Implemented,
        Expected_If_Supported => Invalid_Argument,
        E => E1, Message => "A000");
   when E2 : others => Unexpected_Exception (E2, "A000");
   end;
   Comment ("BOBOBO BABA BEEBEE");

   -----------------------------------------------------------------------
   --  Ahmed
   --  Time values between two consecutive multiples of the resolution
   --  of the specified clock shall be rounded up to the next larger
   --  multiple of the resolution (Arm_Timer).

   Test ("Arm_Timer [14.1.7] Time rounded up to the higher resolution");
   declare
      Event : Signal_Event;
      Sample_Timer : Timer_ID;
      New_State : Timer_State;
      Value1 : Timespec;
      Value2 : Timespec;
      Value3 : Timespec;
      Resolution : Timespec;
   begin
      Set_Signal (Event, SIGUSR1);
      Set_Notification (Event, No_Notification);
      Comment ("Passed 1");
      Resolution := Get_Resolution (Clock_Realtime);
      Comment ("Passed 2");
      Sample_Timer := Create_Timer (Clock_Realtime, Event);
      Comment ("Passed 3");
      Set_Initial (New_State, To_Timespec (100.0));
      Set_Interval (New_State, To_Timespec (0.0));
      Value1 := Get_Initial (New_State);
      Comment ("Initial value = ", Value1);
      Arm_Timer (Sample_Timer, Absolute_Timer, New_State);
      Value1 := Get_Initial (New_State);
      Comment ("Value1 = ", Value1);
      Set_Initial (New_State, (Resolution * 3 / 2) + To_Timespec (100.0));
      Arm_Timer (Sample_Timer, Absolute_Timer, New_State);
      Value2 := Get_Initial (New_State);
      Comment ("Value2 = ", Value2);
      Set_Initial (New_State, (Resolution * 3) + To_Timespec (100.0));
      Arm_Timer (Sample_Timer, Absolute_Timer, New_State);
      Value3 := Get_Initial (New_State);
      Comment ("Value3 = ", Value3);
      Assert ((Value3 - Value2) < (Value2 - Value1),
         "A000: Time was not rounded up to" &
         " the higher multiple of resolution");
   end;
   -----------------------------------------------------------------------
   Done;

exception
when E1 : POSIX_Error =>
   Optional (Timers_Option, Operation_Not_Implemented, E1, "A000");
   Done;
when E2 : others => Fatal_Exception (E2, "A000");
end p140101;

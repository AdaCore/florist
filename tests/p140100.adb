------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                             P 1 4 0 1 0 0                                --
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

--  Basic test for package POSIX_Timers,
--  in IEEE Std 1003.5b Section 14.1.

--  This test covers only features that depend only on
--  the package itself and features from other packages
--  that are required to be supported.
--  More detailed tests are required for specific properties
--  of timers, defined in [14.1.7].
--  See other tests for uses of this package in combination
--  with optional features, signals, and tasking.

--  ....We still need to test  periodic timers.

with Calendar,
     POSIX,
     POSIX_Limits,
     POSIX_Report,
     POSIX_Signals,
     POSIX_Timers;

procedure p140100 is

   use POSIX,
       POSIX_Report,
       POSIX_Signals,
       POSIX_Timers;

   Clock_Realtime_Supported : Boolean := False;
   Clock_Realtime_Moves : Boolean := False;
   Interval_Roundup : Boolean := False;

   Uninitialized_Clock : Clock_ID;  --  never initialized
   Uninitialized_Works : Boolean := False;

   Timer : Timer_ID;
   Zero_Timespec : constant Timespec := To_Timespec (0, 0);

   type Timer_Mode is (Absolute, Relative);

   procedure Test_Timer (Mode : Timer_Mode);

   procedure Test_Timer (Mode : Timer_Mode) is
      Time : Timespec;
      Event : Signal_Event;
      State : Timer_State;
   begin
      begin
         Set_Signal (Event, SIGUSR1);
         Set_Notification (Event, No_Notification);
         Timer := Create_Timer (Clock_Realtime, Event);
      exception
      when E1 : POSIX_Error =>
         Optional (Timers_Option, Operation_Not_Implemented, E1, "A001");
      when E2 : others => Unexpected_Exception (E2, "A002");
      end;

      ----------------------------------------------------------
      --  The timer returned by create_Timer should be in the
      --  disarmed state

      State := Get_Timer_State (Timer);
      Assert (Get_Initial (State) = Zero_Timespec, "A005");
      Assert (Get_Interval (State) = Zero_Timespec, "A006");


      ---------------------------------------------------------
      --  It is possible to arm a timer
      --  and then disarm it before it expires.
      --  There should be time left on the timer
      --  and it should be less than or equal to the time requested.

      Time := Get_Time (Clock_Realtime);
      Set_Initial (State, Time + To_Timespec (1, 0));
      Set_Interval (State, Zero_Timespec);
      Arm_Timer (Timer, Absolute_Timer, State);
      Disarm_Timer (Timer);
      State := Get_Timer_State (Timer);
      Assert (Get_Initial (State) = Zero_Timespec,
        "A007: Get_Initial after disarm /= 0");
      Assert (Get_Interval (State) <= To_Timespec (1, 0),
        "A008: Get_Interval after disarm > initial value");

      --------------------------------------------------------------
      --  Arming timer with zero initial value
      --  should be detected with error code Invalid_Argument.

      begin
         Set_Initial (State, Zero_Timespec);
         Set_Interval (State, Zero_Timespec);
         Arm_Timer (Timer, Absolute_Timer, State);
         Assert (False, "A009: zero initial value not detected");
         Disarm_Timer (Timer);
      exception
      when E1 : POSIX_Error =>
         if Get_Error_Code /= Invalid_Argument then
            Optional (Timers_Option, Operation_Not_Supported, E1, "A010");
         end if;
      when E2 : others => Unexpected_Exception (E2, "A011");
      end;

      if Mode = Relative then

         -----------------------------------------------------------
         --  Arming timer with negative initial value
         --  should be detected with error code Invalid_Argument.

         begin
            Set_Initial (State, To_Timespec (-1, 0));
            Set_Interval (State, Zero_Timespec);
            Arm_Timer (Timer, Absolute_Timer, State);
            Assert (False, "A012: negative initial value not detected");
            Disarm_Timer (Timer);
         exception
         when E1 : POSIX_Error =>
            if Get_Error_Code /= Invalid_Argument then
               Optional (Timers_Option, Operation_Not_Supported, E1, "A013");
            end if;
         when E2 : others => Unexpected_Exception (E2, "A014");
         end;
      else -- absolute timer

      --------------------------------------------------------------
      --  Arming timer with present or past time
      --  causes it to go off immediately.

         begin
            Time := Get_Time (Clock_Realtime);
            Set_Initial (State, Time);
            Set_Interval (State, Zero_Timespec);
            Arm_Timer (Timer, Absolute_Timer, State);
            State := Get_Timer_State (Timer);
            Assert (Get_Initial (State) = Zero_Timespec,
              "A015: Get_Initial after expiration /= 0");
            Assert (Get_Interval (State) = Zero_Timespec,
              "A016: Get_Interval after expiration /= 0");
            Assert (Get_Timer_Overruns (Timer) = 0,
              "A017: nonzero overruns");
         exception
         when E1 : POSIX_Error =>
            Optional (Timers_Option, Operation_Not_Implemented, E1, "A018");
         when E2 : others => Unexpected_Exception (E2, "A019");
         end;

      end if;

      -------------------------------------------------------------
      --  Arming timer with future time causes
      --  it to go off after that time has passed.
      --  Test this by requesting delays of several lengths,
      --  then delaying for that amount of time and checking
      --  whether the timer has expired.

      declare
         Initial,
         LastInitial,
         NowInitial,
         Start_Time,
         Stop_Time,
         Temp : Timespec;
         PDelta : Timespec := To_Timespec (0, 1);
         --  Ada times and interval, to be used as backup
         --  in case Clock_Realtime does not work.
         NowState : Timer_State;
         Failed : Boolean;
      begin
         while PDelta < To_Timespec (2, 0) loop
            Failed := False;
            Start_Time := Get_Time (Clock_Realtime);
            if Mode = Relative then
               Initial := PDelta;
            else -- absolute
               Initial := Start_Time + PDelta;
            end if;
            Set_Initial (State, Initial);
            Set_Interval (State, Zero_Timespec);
            Arm_Timer (Timer, Empty_Set, State);
            NowState := Get_Timer_State (Timer);
            NowInitial := Get_Initial (NowState);
            Temp := NowInitial - PDelta;
            if Temp > Zero_Timespec then
               Comment ("observed interval roundup", Temp);
               if Get_Seconds (Temp) > 10 then
                  if not Interval_Roundup then
                     Assert (False,
                       "A020: unbelievable interval roundup: "
                       & Image (Temp));
                  end if;
                  Interval_Roundup := True;
                  Failed := True;
               end if;
            end if;
            while not Failed loop
               LastInitial := NowInitial;
               NowState := Get_Timer_State (Timer);
               NowInitial := Get_Initial (NowState);
               exit when NowInitial = Zero_Timespec;
               if NowInitial >= LastInitial then
                  Assert (False, "A021: timer value nondecreasing");
                  Comment ("request", PDelta);
                  Comment ("current", NowInitial);
                  Failed := True;
               end if;
            end loop;
            if not Failed then
               Comment ("request", PDelta);
               Stop_Time := Get_Time (Clock_Realtime);
               Temp := Stop_Time - Start_Time;
               if Temp < PDelta then
                  Assert (False, "A022: early expiration: "
                    & Image (PDelta - Temp));
                  Comment ("under  ", PDelta - Temp);
               else
                  Comment ("over   ", Temp - PDelta);
               end if;
            end if;
            PDelta := PDelta * 2;
         end loop;
      exception
      when E1 : POSIX_Error =>
         Optional (Timers_Option, Operation_Not_Implemented, E1, "A023");
      when E2 : others => Unexpected_Exception (E2, "A024");
      end;
      Delete_Timer (Timer);
   exception
   when E1 : POSIX_Error =>
      Optional (Timers_Option, Operation_Not_Implemented, E1, "A025");
   when E2 : others => Unexpected_Exception (E2, "A026");
   end Test_Timer;

begin

   Header ("A027100", Root_OK => True);

   -----------------------------------------------------------------------

   --  We first do a (superficial) test of the arithmetic on
   --  type Timespec, since the rest of this test depends on that.

   Test ("Timespec arithmetic accuracy");
   declare
      One_Second : constant Timespec := To_Timespec (1, 0);
      PDelta : Timespec := To_Timespec (0, 1);
      N : Nanoseconds_Base := 1;
   begin
      Assert (To_Timespec (0, 1) > Zero_Timespec, "A028");
      Assert (To_Timespec (-1, 0) < Zero_Timespec, "A029");
      Assert (To_Timespec (0, 0) = Zero_Timespec, "A030");
      Assert (To_Timespec (0, 1) + To_Timespec (0, 1)
        = To_Timespec (0, 2), "A031");
      Assert (To_Timespec (0, Nanoseconds'Last) + To_Timespec (0, 1)
        = To_Timespec (1, 0), "A032");
      Assert (To_Timespec (0, Nanoseconds'Last) - To_Timespec (0, 1)
        = To_Timespec (0, Nanoseconds'Last - 1), "A033");
      while PDelta < To_Timespec (2, 0) loop
         if PDelta < One_Second then
            Assert (PDelta = To_Timespec (0, N), "A034");
            Assert (N = Get_Nanoseconds (PDelta), "A035");
         end if;
         Assert (Get_Seconds (PDelta) < 2, "A036");
         Assert (PDelta > Zero_Timespec, "A037");
         PDelta := PDelta * 2;  N := N * 2;
      end loop;
      Assert (To_Timespec (987_654_321, 87_654_321) +
        To_Timespec (100_000_000, 10_000_000) =
        To_Timespec (1_087_654_321, 97_654_321), "A038");
      Assert (To_Timespec (987_654_321, 87_654_321) -
        To_Timespec (100_000_000, 10_000_000) =
        To_Timespec (887_654_321, 77_654_321), "A039");
      Assert (To_Timespec (0, 955_899) > Zero_Timespec, "A040");
      Assert (To_Timespec (0, 955_899) -
        To_Timespec (0, 955_900) = To_Timespec (-1, 1E9 - 1), "A041");
   exception
   when E : others => Unexpected_Exception (E, "A042");
   end;

   --------------------------------------------------------
   --  Testing the validity of variables of type Timer_ID will be
   --  done with testing sections of [14.1.5] and [14.1.6].

   ---------------------------------------------------------
   --  Testing the resolution of the identifier Clock_Realtime
   --  will be done with testing of section [14.1.4].

   ------------------------------------------------------------
   --  Clock_Realtime can support up to at least Seconds'Last.

   Test ("Clock_Realtime can be set to Seconds'Last [14.1.2]");
   Comment ("This test can be performed only by the system adminstrator");
   declare
      Time_Restore : Timespec;

   begin
      Time_Restore := Get_Time (Clock_Realtime);
      Set_Time (Clock_Realtime, To_Timespec (Seconds'Last, 0));
      Assert (Get_Time (Clock_Realtime) = To_Timespec (
              Seconds'Last, 0), "A003");
      Set_Time (Clock_Realtime, Time_Restore);
   exception
   when E1 : POSIX_Error =>
      Privileged (Set_Time_Privilege,
        Timers_Option, Operation_Not_Implemented,
        E1, "A004");
   end;

   -----------------------------------------------------------------------

   Test ("Timer_State type and operations [14.1.3]");
   declare
      Temp1,
      Temp2 : Timespec;
      State : Timer_State;
   begin

      ------------------------------------------------------------
      --  The effects of the Get_/Set_ operations
      --  on timers are consistent with one another.

      Set_Seconds (Temp1, 999);
      Set_Nanoseconds (Temp1, 1111);

      Set_Initial (State, Temp1);
      Temp2 := Get_Initial (State);
      Assert (Temp1 = Temp2, "Get_/Set_Initial");

      Set_Interval (State, Temp1);
      Temp2 := Get_Interval (State);
      Assert (Temp1 = Temp2, "Get_/Set_Interval");

   exception
   when E1 : POSIX_Error =>
      Optional (Timers_Option, Operation_Not_Implemented, E1, "A043");
   when E2 : others => Unexpected_Exception (E2, "A044");
   end;

   -----------------------------------------------------------------------

   Test ("Timer_Options type and operations [14.1.3]");
   declare
      Options : Timer_Options;
   begin
      Assert (Options = Empty_Set, "Timer_Options default value");

   -----------------------------------------------------------------
   --  The effect of the Absolute_Timer option
   --  is different from specifying no option.

      Options := Absolute_Timer;
      Assert (Options /= Empty_Set, "Absolute_Timer value");
   exception
   when E1 : POSIX_Error =>
      Optional (Timers_Option, Operation_Not_Implemented, E1, "A045");
   when E2 : others => Unexpected_Exception (E2, "A046");
   end;

   -----------------------------------------------------------------------

   Test ("Resolution (Clock_Realtime) [14.1.4]");
   declare
      Resolution : Timespec := To_Timespec (999, 999);
      S          : Seconds;
      NS         : Nanoseconds;
   begin
      Resolution := Get_Resolution (Clock_Realtime);
      Clock_Realtime_Supported := True;

      ----------------------------------------------------------------

      --  The resolution reported by Get_Resolution is consistent
      --  with POSIX_Limits.Portable_Clock_Resolution_Minimum.

      Split (Resolution, S, NS);
      Assert (S = 0
        and NS <= POSIX_Limits.Portable_Clock_Resolution_Minimum, "A047");
      Comment ("Clock_Realtime reported resolution", Resolution);
   exception
   when E1 : POSIX_Error =>
      Optional (Timers_Option, Operation_Not_Implemented, E1, "A048");
   when E2 : others => Unexpected_Exception (E2, "A049");
   end;

   -----------------------------------------------------------------------

   Test ("Get_Time (Clock_Realtime) [14.1.4]");
   declare
      Time : Timespec := To_Timespec (-1, 0);
   begin
      Time := Get_Time (Clock_Realtime);
      Assert (Clock_Realtime_Supported, "inconsistent support");
      Clock_Realtime_Supported := True;
      Assert (Get_Seconds (Time) /= -1, "A050");
      Comment ("current time", Time);
   exception
   when E1 : POSIX_Error =>
      Optional (Timers_Option, Operation_Not_Implemented, E1, "A051");
   when E2 : others => Unexpected_Exception (E2, "A052");
   end;

   -----------------------------------------------------------------------

   Test ("Clock_Realtime behavior [14.1.4]");
   declare
      Resolution : Timespec;
      S     : Seconds;
      NS    : Nanoseconds;
      PTime,                  --  start time according to POSIX clock
      PNow  : Timespec;       --  current time according to POSIX clock
      N     : constant := 100;
      Count : Integer;
      Pmin,                   --  minimum of N Pdif values
      Pdif  : Timespec;       --  apparent POSIX clock resolution
      ATime,                  --  start time according to Ada clock
      ANow  : Calendar.Time;  --  current time according to Ada clock
      Amin,                   --  minimum of N Adif values
      Adif  : Duration;       --  apparent Ada clock resolution
   begin

      ---------------------------------------------------------------------
      --  The maximum allowable value returned by Get_Resolution
      --  for Clock_Realtime is Portable_Clock_Resolution_Minimum.

      Resolution := Get_Resolution (Clock_Realtime);
      Split (Resolution, S, NS);
      Clock_Realtime_Supported := True;

      ---------------------------------------------------------------------
      --  The effective clock resolution should be consistent with
      --  the reported resolution.
      --  This can be partially checked by repeatedly
      --  reading the clock until the value changes,
      --  and then looking at the difference in clock values.

      --  First check, using Ada's Calendar.Clock, that Clock_Realtime
      --  is moving, so we won't get stuck in an infinite loop when
      --  we test the clock later.

      ATime := Calendar.Clock;
      PTime := Get_Time (Clock_Realtime);
      loop
         Adif := Calendar."-" (ATime, Calendar.Clock);
         Pdif := PTime - Get_Time (Clock_Realtime);
         Clock_Realtime_Moves := True;
         if Adif > 2.0 then
            Fail ("A053: Clock_Realtime does not move");
            Clock_Realtime_Moves := False;
            exit;
         end if;
         exit when Pdif /= Zero_Timespec;
      end loop;

      --  Now find an upper bound on the Clock_Realtime resolution.
      --  See how this compares with the Ada Calendar clock.

      if Clock_Realtime_Moves then
         PTime := Get_Time (Clock_Realtime);
         Pmin := To_Timespec (1000, 0);
         Count := 0;
         while Count < N loop
            PNow := Get_Time (Clock_Realtime);
            Pdif := PNow - PTime;
            Split (Pdif, S, NS);
            if Pdif > Zero_Timespec then
               if Pdif < Pmin then Pmin := Pdif;
               end if;
               PTime := PNow; Count := Count + 1;
            end if;
         end loop;
         Split (Pmin, S, NS);
         Assert (S = 0 and
           NS <= POSIX_Limits.Portable_Clock_Resolution_Minimum, "A054");
         Comment ("Clock_Realtime apparent resolution + overhead", Pmin);
      end if;

      --  For comparison, check the resolution of Calendar.Clock.

      ATime := Calendar.Clock;
      Amin := 1000.0;
      Count := 0;
      while Count < N loop
         ANow := Calendar.Clock;
         Adif := Calendar."-" (ANow, ATime);
         if Adif > 0.0 then
            if Adif < Amin then Amin := Adif;
            end if;
            ATime := ANow; Count := Count + 1;
         end if;
      end loop;
      Comment ("Calendar.Clock apparent resolution + overhead =" &
        Integer'Image (Integer (Amin * 1E9)));
   exception
   when E1 : POSIX_Error =>
      Optional (Timers_Option, Operation_Not_Implemented, E1, "A055");
   when E2 : others => Unexpected_Exception (E2, "A056");
   end;

   -----------------------------------------------------------------------
   --  An uninitialized Clock_ID value should be detected as
   --  invalid, or else work as a valid argument.

   Test ("uninitialized Clock_ID value [14.1.4]");
   declare
      Resolution,
      Time : Timespec;
   begin
      begin
         Resolution := Get_Resolution (Uninitialized_Clock);
         Comment ("Uninitialized Clock_ID has Resolution");
         Uninitialized_Works := True;
      exception
      when E1 : POSIX_Error =>
         if Get_Error_Code /= Invalid_Argument then
            Optional (Timers_Option, Operation_Not_Implemented, E1, "A057");
         end if;
      when E2 : others => Unexpected_Exception (E2, "A058");
      end;
      begin
         Time := Get_Time (Uninitialized_Clock);
         Comment ("Uninitialized Clock_ID has Get_Time");
         Assert (Uninitialized_Works, "inconsistent notion of validity");
         Uninitialized_Works := True;
      exception
      when E1 : POSIX_Error =>
         if Get_Error_Code /= Invalid_Argument then
            Optional (Timers_Option, Operation_Not_Implemented, E1, "A059");
         end if;
      when E2 : others => Unexpected_Exception (E2, "A060");
      end;
   end;

   -----------------------------------------------------------------------
   --  Set_Time can be used to set the clock to a valid
   --  value, unless the calling process lacks sufficient privilege.

   Test ("Set_Time [14.1.4]");
   declare
      Time : Timespec;
   begin
      Time := Get_Time (Clock_Realtime);
      Set_Time (Clock_Realtime, Time);
      Assert (Time = Get_Time (Clock_Realtime), "A061");
   exception
   when E1 : POSIX_Error =>
      Privileged (Set_Time_Privilege,
        Timers_Option, Operation_Not_Implemented, E1, "A062");
   when E2 : others => Unexpected_Exception (E2, "A063");
   end;

   -----------------------------------------------------------------------
   --  POSIX_Error is raised with error code Invalid_Argument if
   --  Set_Time is called with invalid clock identifier,
   --  or the Value parameter for Set_Time is outside
   --  the range allowed for the specified Clock, or the argument of type
   --  Timespec cannot not be interpreted as a valid time.
   --  An invalid value of type Timespec may be obtained with high
   --  probability via an uninitialized variable.
   --  These requirements are checked in a separate program, along
   --  with other checks of operations that may require special privilege.

   -----------------------------------------------------------------------

   Test ("Signal_Event variable initialization");
   declare
      Event : Signal_Event;
   begin
      Set_Notification (Event, No_Notification);
      Set_Signal (Event, SIGUSR1);
      Set_Notification (Event, Signal_Notification);
   exception
   when E1 : POSIX_Error =>
      Optional (Timers_Option, Operation_Not_Implemented, E1, "A064");
   when E2 : others => Unexpected_Exception (E2, "A065");
   end;

   -----------------------------------------------------------------------
   --  An attempt to create a timer with an invalid clock ID
   --  results in POSIX_Error with Invalid_Argument,
   --  if Timers_Option is supported.

   Test ("Create_Timer [4.1.5], invalid clock ID");
   declare
      Event : Signal_Event;
   begin
      Set_Notification (Event, No_Notification);
      Timer := Create_Timer (Uninitialized_Clock, Event);
      Assert (Uninitialized_Works, "inconsistent notion of validity");
      Comment ("Uninitialized Clock_ID can create timer");
      Uninitialized_Works := True;
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code /= Invalid_Argument then
         Optional (Timers_Option, Operation_Not_Supported, E1, "A066");
      end if;
   when E2 : others => Unexpected_Exception (E2, "A067");
   end;

   -----------------------------------------------------------------------
   --  An attempt to delete a timer that was not initialized
   --  should either fail, or fail with Operation_Not_Supported.

   Test ("Delete_Timer [4.1.6], invalid timer ID");
   declare
      Timer : Timer_ID;  --  uninitialized
   begin
      Delete_Timer (Timer);
      Comment ("Uninitialized Timer_ID can be deleted");
      Delete_Timer (Timer);
      Fail ("A068: deleted Timer_ID can be deleted");
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code /= Invalid_Argument then
         Optional (Timers_Option, Operation_Not_Supported, E1, "A069");
      end if;
   when E2 : others => Unexpected_Exception (E2, "A070");
   end;

   -----------------------------------------------------------------------
   --  An attemp to arm a timer with invalid timer ID should fail

   Test ("Arm_Timer, Invalid timer ID");
   declare
      State : Timer_State;
   begin
      Set_Initial (State, To_Timespec (1, 0));
      Set_Interval (State, Zero_Timespec);
      Arm_Timer (Timer, Empty_Set, State);
      Disarm_Timer (Timer);
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code /= Invalid_Argument then
         Optional (Timers_Option, Operation_Not_Supported, E1, "A071");
      end if;
   when E2 : others => Unexpected_Exception (E2, "A072");
   end;

   -----------------------------------------------------------------------
   --  An attempt to Get_Timer_State a timer that was not initialized
   --  should either fail, or fail with Operation_Not_Supported.

   --  Test ("Get_Timer_State, invalid timer ID");
   --  declare
   --   Timer : Timer_ID;  --  uninitialized
   --  begin
   --   Get_Timer_State (Timer);
   --   Assert (False,
   --     "A073: can Get_Timer_State from Uninitialized Timer_ID");
   --  exception
   --  when E1 : POSIX_Error =>
   --   if Get_Error_Code /= Invalid_Argument then
   --      Optional (Timers_Option, Operation_Not_Supported, E1, "A074");
   --   end if;
   --  when E2 : others => Unexpected_Exception (E2, "A075");
   --  end;

   -----------------------------------------------------------------------
   --  An attempt to Get_Timer_Overruns a timer that was not initialized
   --  should either fail, or fail with Operation_Not_Supported.

   --  Test ("Get_Timer_Overruns, invalid timer ID");
   --  declare
   --   Timer : Timer_ID;  --  uninitialized
   --  begin
   --   Get_Timer_Overruns (Timer);
   --   Assert (False, "A076: can get overruns from Uninitialized Timer_ID");
   --  exception
   --  when E1 : POSIX_Error =>
   --   if Get_Error_Code /= Invalid_Argument then
   --      Optional (Timers_Option, Operation_Not_Supported, E1, "A077");
   --   end if;
   --  when E2 : others => Unexpected_Exception (E2, "A078");
   --  end;

   -----------------------------------------------------------------------
   --  It should be possible to create a timer.
   --  Since the clock ID is valid and this is the only timer we create
   --  in this process, it should succeed.
   --  This should work both with and without signal notification.

   Test ("Create_Timer [4.1.5], valid clock ID");
   declare
      Event : Signal_Event;
   begin
      Set_Notification (Event, No_Notification);
      Timer := Create_Timer (Clock_Realtime, Event);
      Test ("Delete_Timer [4.1.6], valid timer ID");
      Delete_Timer (Timer);
      Set_Signal (Event, SIGUSR1);
      Set_Notification (Event, Signal_Notification);
      Timer := Create_Timer (Clock_Realtime, Event);
      Delete_Timer (Timer);
   exception
   when E1 : POSIX_Error =>
      Optional (Timers_Option, Operation_Not_Supported, E1, "A079");
   when E2 : others => Unexpected_Exception (E2, "A080");
   end;

   -----------------------------------------------------------------------

   Test ("Operations on one-shot timer [14.1.7]");
   Test_Timer (Absolute);

   -----------------------------------------------------------------------

   Test ("Operations on relative one-shot timer [14.1.7]");
   Test_Timer (Relative);

   Done;

exception
when E1 : POSIX_Error =>
   Optional (Timers_Option, Operation_Not_Implemented, E1, "A081");
   Done;
when E2 : others => Fatal_Exception (E2, "A082");
end p140100;

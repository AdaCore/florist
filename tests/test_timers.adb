--------------------------------------------------------------------------
--  Copyright (C) 1995, 1996 by the Florida State University            --
--                                                                      --
--  This program is free software; you can redistribute it and/or       --
--  modify it under the terms of the GNU General Public License as      --
--  published by the Free Software Foundation; either version 2 of      --
--  the License, or (at your option) any later version.                 --
--------------------------------------------------------------------------
--  [$Revision$]

--  Test package POSIX_Timers.

with POSIX;
with POSIX_Options;
with POSIX_Process_Identification;
with POSIX_Report;
with POSIX_Signals;
with POSIX_Timers;
procedure Test_Timers is

   use  POSIX;
   use  POSIX_Process_Identification;
   use  POSIX_Report;
   use  POSIX_Signals;
   use  POSIX_Timers;

   Signal_Delivered : Boolean := False;
   Event : Signal_Event;
   Tid : Timer_ID;
   Timer_State1,
   New_State : Timer_State;
   Initial,
   Interval : Timespec;

   task Handler is
      entry Done;
      for Done use at Signal_User_2_Ref;
   end Handler;

   task body Handler is
   begin
      loop
         select
            accept Done do
               Signal_Delivered := true;
            end Done;
         or terminate;
         end select;
      end loop;
   end Handler;

begin

   Header ("Test_Timers");

   -----------------------------------------------------------------------

   Signal_Delivered := False;

   Test ("initialize signal event variable");
   Set_Signal (Event, SIGUSR2);
   Set_Notification (Event, Signal_Notification);

   -----------------------------------------------------------------------

   Test ("get clock resolution");
   declare
      T  : Timespec := To_Timespec (999, 999);
      S  : Seconds;
      NS : Nanoseconds;
   begin
      T := Get_Resolution (Clock_Realtime);
      Split (T, S, NS);
      Assert (S /= 999);
      Comment ("real-time clock resolution = " &
        Seconds'Image (S) & "ns + " & Nanoseconds'Image (NS) & "ns");
   exception
   when E1 : POSIX_Error =>
      Optional (Timers_Option, Operation_Not_Implemented, E1);
   when E2 : others => Fail (E2);
   end;

   -----------------------------------------------------------------------

   Test ("get real-time clock value");
   declare
      T  : Timespec := To_Timespec (999, 999);
      S  : Seconds;
      NS : Nanoseconds;
   begin
      T := Get_Time;
      Split (T, S, NS);
      Assert (S /= 999);
      Comment ("current time = " &
        Seconds'Image (S) & "ns + " & Nanoseconds'Image (NS) & "ns");
   exception
   when E1 : POSIX_Error =>
      Optional (Timers_Option, Operation_Not_Implemented, E1);
   when E2 : others => Fail (E2);
   end;

   -----------------------------------------------------------------------

   Test ("set real-time clock");
   declare
      T1, T2 : Timespec := To_Timespec (999, 999);
      S  : Seconds;
      NS : Nanoseconds;
      EC : Error_Code;
   begin
      Set_Time (T1);
      T2 := Get_Time;
      Assert (T1 = T2);
   exception
   when E1 : POSIX_Error =>
      Privileged (Set_Time_Privilege,
        Timers_Option, Operation_Not_Implemented, E1);
   when E2 : others => Fail (E2);
   end;

   -----------------------------------------------------------------------

   Test ("initialize timer state variable");
   POSIX.Set_Seconds (Initial, 1);
   POSIX.Set_Nanoseconds (Initial, 1);
   POSIX.Set_Seconds (Interval, 0);
   POSIX.Set_Nanoseconds (Interval, 0);
   Set_Initial (New_State, Initial);
   Set_Interval (New_State, Interval);

   --  Since Absolute_Timer is specified, timer is set to expire to
   --  Epoch+1 seconds,  so it will generate a signal immediately.
   --  Interval = 0, thus it only generates the signal once.

   -----------------------------------------------------------------------

   Test ("create timer");
   begin
      Tid := Create_Timer (Clock_Realtime, Event);
   exception
   when E1 : POSIX_Error =>
      Optional (Timers_Option, Operation_Not_Supported, E1);
   when E2 : others => Fail (E2);
   end;

   -----------------------------------------------------------------------

   Test ("arm timer for single shot");
   --  .... This test stops by receiving a signal due to the
   --  "sigwaitinfo/timer_settime" malfunction (Solaris). See s_timer.c
   begin
      Arm_Timer (Tid, Absolute_Timer, New_State);
      --  Should generate signal immediately
   exception
   when E1 : POSIX_Error =>
      Optional (Timers_Option, Operation_Not_Implemented, E1);
   when E2 : others => Fail (E2);
   end;

   -----------------------------------------------------------------------

   Test ("get timer state");
   begin
      Timer_State1 := Get_Timer_State (Tid);
      Assert (Get_Seconds (Get_Initial (Timer_State1)) = 0,
        "initial value should be zero");
      Assert (Get_Seconds (Get_Interval (Timer_State1)) = 0,
        "interval should be zero");

   -----------------------------------------------------------------------

      Test ("get timer overruns");
      for I in 1 .. 10 loop
         Assert (Get_Timer_Overruns (Tid) = 0, "overruns");
         exit when Signal_Delivered;
         delay 1.0;
      end loop;

   -----------------------------------------------------------------------

      if not Signal_Delivered then Fail ("timer signal delivery");
      end if;
   exception
   when E1 : POSIX_Error =>
      Optional (Timers_Option, Operation_Not_Implemented, E1);
   when E2 : others => Fail (E2);
   end;

   --  ???? need additional test, for periodic timer

   -----------------------------------------------------------------------

   Test ("disarm timer");
   begin
      Disarm_Timer (Tid);
   exception
   when E1 : POSIX_Error =>
      Optional (Timers_Option, Operation_Not_Implemented, E1);
   when E2 : others => Fail (E2);
   end;

   -----------------------------------------------------------------------

   Test ("delete timer");
   begin
      Delete_Timer (Tid);
   exception
   when E1 : POSIX_Error =>
      Optional (Timers_Option, Operation_Not_Supported, E1);
   when E2 : others => Fail (E2);
   end;

   -----------------------------------------------------------------------

   Done;

exception
when E1 : POSIX.POSIX_Error =>
   Optional (Timers_Option, Operation_Not_Implemented, E1);
   Done;
when E2 : others => Fatal_Exception (E2);
end Test_Timers;

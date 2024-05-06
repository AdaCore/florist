------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                             P 9 9 0 0 x 0                                --
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

--  Simulate mix of periodic jobs with rate monotone priorities.

with Calendar,
     Ada.Real_Time,
     POSIX,
     POSIX_Calendar,
     POSIX_Process_Identification,
     POSIX_Process_Primitives,
     POSIX_Process_Scheduling,
     POSIX_Report,
     POSIX_Timers,
     System,
     P9900doc,
     P990000;
package body P9900x0 is

   use P990000,
       POSIX,
       POSIX_Process_Identification,
       POSIX_Process_Primitives,
       POSIX_Process_Scheduling,
       POSIX_Report;

   Data : Shared_Data_Ptr;
   --  Initialized in each main procedure.

   --  For versions that use multiple processes, we also need to
   --  set the process priority, elsewhere.

   procedure Compute_Loads (Load_Factor : Positive);
   procedure Find_Utilization_Limit;
   procedure Start_Tasks;
   procedure Stop_Tasks;
   procedure Start_Processes;
   procedure Stop_Processes;
   procedure Work (Job : Jobs);
   --  Implements the body of periodic task.
   function Run_Jobs return Boolean;

   procedure Compute_Loads (Load_Factor : Positive) is
   begin
      Data.Input_Load (1) := 1;
      for J in Jobs loop
         Data.Input_Load (J) :=
           Integer (Float (Load_Factor) * Input_Time (J));
         Data.Output_Load (J) :=
           Integer (Float (Load_Factor) * Output_Time (J));
         Data.Computation_Load (J) :=
           Integer (Float (Load_Factor) * Computation_Time (J));
      end loop;
      exception when E : others => Fatal_Exception (E, "A001: P9900x0");
   end Compute_Loads;

   procedure Work (Job : Jobs) is
   begin
      Await_Start;
      loop
         Do_Input (Data.Input_Load (Job));
         Do_Computation (Data.Computation_Load (Job));
         Do_Output (Data.Output_Load (Job));
         exit when not Reschedule (Job);
      end loop;
      Done_Job;
   exception
   when E : others =>
      Done_Job;
      Fatal_Exception (E, "A002: P9900x0");
   end Work;

   function Run_Jobs return Boolean is
   begin
      Data.Missed_Deadlines := False;
      Initialize_Sync;
      if Jobs_Are_Processes then
         Start_Processes;
         Data.Start_POSIX_Time := POSIX_Calendar.Clock;
         Data.Start_Calendar_Time := Calendar.Clock;
         Data.Start_Real_Time := Ada.Real_Time.Clock;
         if Needs_Clock_Realtime then
            Data.Start_Timespec :=
              POSIX_Timers.Get_Time (POSIX_Timers.Clock_Realtime);
         end if;
         Initialize_Scheduling (Data);
         Start_All_Jobs;
         Await_All_Jobs_Done;
         Stop_Processes;
      else
         Start_Tasks;
         Data.Start_POSIX_Time := POSIX_Calendar.Clock;
         Data.Start_Calendar_Time := Calendar.Clock;
         Data.Start_Real_Time := Ada.Real_Time.Clock;
         if Needs_Clock_Realtime then
            Data.Start_Timespec :=
              POSIX_Timers.Get_Time (POSIX_Timers.Clock_Realtime);
         end if;
         Initialize_Scheduling (Data);
         Start_All_Jobs;
         Await_All_Jobs_Done;
         Stop_Tasks;
      end if;
      Finalize_Scheduling;
      return not Data.Missed_Deadlines;
   exception
   when E : others =>
      Finalize_Scheduling;
      Fatal_Exception (E, "A003: P9900x0");
      return not Data.Missed_Deadlines;
   end Run_Jobs;

   procedure Find_Utilization_Limit is
      use Calendar;
      T1, T2 : Time;
      Clock_Resolution_Bound, D, Base : Duration;
      K, Hi, Lo, Load_Factor : Integer;
      Total_Utilization,
      Unit_Work_Execution_Time : Float;
   begin

      --  Compute job periods and estimate total utilization.

      Total_Utilization := 0.0;
      for J in Jobs loop
         Total_Utilization := Total_Utilization +
           (Input_Time (J) + Computation_Time (J) + Output_Time (J))
             * float (Rate (J));
      end loop;

      --  Estimate resolution of Calendar.Clock.

      Clock_Resolution_Bound := 100.0;
      for I in 1 .. 1000 loop
         T1 := Clock;
         loop
            T2 := Clock;
            D := T2 - T1;
            exit when D > 0.0;
            T1 := T2;
         end loop;
         if D < Clock_Resolution_Bound then
            Clock_Resolution_Bound := D;
         end if;
      end loop;
      Comment ("using clock resolution bound of" &
        Integer'Image (Integer (D * 1_000_000)) & "us");

      --  Use Calendar.Clock to measure execution time of
      --  procedure P990000.Do_Unit_Work, to a number of decimal
      --  digits specified by the constant Real_Accuracy.
      --  Use dual-loop benchmark method.

      K := 10000;
      loop
         T1 := Clock;
         for J in 1 .. K loop
            P990000.Do_Unit_Work (J);
         end loop;
         T2 := Clock;
         Base := T2 - T1;
         exit when Base > Real_Accuracy * Clock_Resolution_Bound;
         K := K * 10;
      end loop;
      T1 := Clock;
      for J in 1 .. K loop
         P990000.Do_Unit_Work (J);
         P990000.Do_Unit_Work (J);
      end loop;
      T2 := Clock;
      D := (T2 - T1) - Base;
      Unit_Work_Execution_Time := Float (D) / Float (K);
      Comment ("unit_work computation time",
        To_Timespec (Duration (Unit_Work_Execution_Time)));

      --  Initialize lower and upper bounds on achievable
      --  load factor, before bisection.
      --  Upper bound (Hi) must be high enough to cause failure.

      Lo := 1;
      Comment ("finding a breakdown load factor");
      Hi := 1;
      loop
         Compute_Loads (Hi);
         if Run_Jobs then
            Lo := Hi;
            Comment ("underloaded at " & Integer'Image (Hi));
            Hi := Hi * 16;
         else
            Comment ("overloaded  at " & Integer'Image (Hi));
            exit;
         end if;
      end loop;

      --  Zero in on maximum workable load factor, by bisection.

      Comment ("using bisection to find limiting load factor");
      loop
         Load_Factor := (Lo + Hi) / 2;
         --  Lo <= Load_Factor < Hi
         Compute_Loads (Load_Factor);
         if Run_Jobs then
            Lo := Load_Factor;
            Comment ("underloaded at " & Integer'Image (Load_Factor));
         else
            Hi := Load_Factor;
            Comment ("overloaded  at " & Integer'Image (Load_Factor));
         end if;
         exit when Hi - Lo <= (Load_Factor + Accuracy) / Accuracy;
      end loop;

      Comment ("limiting load factor =" & Integer'Image (Load_Factor));

      --  Compute actual effective utilization.

      Total_Utilization := 0.0;
      for J in Jobs loop
         Total_Utilization := Total_Utilization +
           Float ((Data.Input_Load (J) + Data.Output_Load (J)
             + Data.Computation_Load (J))
             * Rate (J)) * Unit_Work_Execution_Time;
      end loop;
      Comment ("apparent limit utilization =" &
        Integer'Image (Integer (Total_Utilization * 100.0)) & "%");

   exception when E : others => Fatal_Exception (E, "A004: P9900x0");
   end Find_Utilization_Limit;

   task type Periodic_Task
     (Job  : Jobs) is
      pragma Priority (Priority (Job));
   end Periodic_Task;

   type Periodic_Task_Ptr is access all Periodic_Task;

   task body Periodic_Task is
   begin
      Work (Job);
   end Periodic_Task;

   Periodic_Tasks : array (Jobs) of Periodic_Task_Ptr;

   procedure Start_Tasks is
   begin
      for J in Jobs loop
         Periodic_Tasks (J) :=
           new Periodic_Task (J);
      end loop;
   exception
   when E : others => Fatal_Exception (E, "A005: P9900x0: in Start_Jobs");
   end Start_Tasks;

   procedure Stop_Tasks is
   begin
      for J in Jobs loop
         if not Periodic_Tasks (J).all'Terminated then
            abort Periodic_Tasks (J).all;
         end if;
      end loop;
   exception
   when E : others => Fatal_Exception (E, "A006: P9900x0: in Stop_Jobs");
   end Stop_Tasks;

   Status : Termination_Status;
   Periodic_Processes : array (Jobs) of Process_ID;

   procedure Start_Processes is

      Child_Pathname : constant POSIX.Pathname :=
        "p9900" & To_POSIX_String (Version) & "b";
      Template : Process_Template;
      Args     : POSIX_String_List;
      Parms    : Scheduling_Parameters;
      Max_Prio : constant Scheduling_Priority :=
                   Get_Maximum_Priority (Sched_FIFO);
      Min_Prio : constant Scheduling_Priority :=
                   Get_Minimum_Priority (Sched_FIFO);
      Num_Jobs : constant Integer := Jobs'Last - Jobs'First + 1;

      function Process_Prio
        (Prio : System.Priority) return Scheduling_Priority;

      function Process_Prio
        (Prio : System.Priority) return Scheduling_Priority is

         --  In System.Priority, higher numbers are higher priorities;
         --  so also for Scheduling_Priority values.

      begin
         return Scheduling_Priority
           (Max_Prio - (System.Priority'Last - Prio));
      end Process_Prio;

   begin
      --  set main process's priority to the maximum
      Assert (Integer
        (Max_Prio - Min_Prio + 1) >= Num_Jobs + 1, "A007: P9900x0");
      Comment ("min_prio = " & Scheduling_Priority'Image (Min_Prio));
      Comment ("max_prio = " & Scheduling_Priority'Image (Max_Prio));
      Comment ("num_jobs = " & Jobs'Image (Num_Jobs));
      begin
         Comment ("priority = " & Scheduling_Priority'Image
           (Process_Prio (System.Priority'Last)));
      exception when E : others =>
         Unexpected_Exception (E, "A008: P9900x0: in Run_Jobs/Set_Priority");
         raise POSIX_Error;
      end;
      begin
         Comment ("Set_Priority to system.Priority'last");
         Set_Priority (Parms, Process_Prio (System.Priority'Last));
      exception
      when E: others =>
         Unexpected_Exception (E, "A009: P9900x0: in Run_Jobs/Set_Priority");
         raise POSIX_Error;
      end;
      Comment ("setting scheduling policy");
      begin
         null;
         Set_Scheduling_Policy (Process => Get_Process_ID,
           New_Policy => Sched_FIFO,
           Parameters => Parms);
      exception
      when E1 : POSIX_Error =>
         Privileged (Realtime_Process_Priority_Privilege,
           Priority_Process_Scheduling_Option,
           Operation_Not_Permitted, E1, "A010: P9900x0");
      when E2 : others => Unexpected_Exception
           (E2, "A011: P9900x0: in Run_Jobs/Set_Sched_Policy");
         raise POSIX_Error;
      end;
      Open_Template (Template);
      --  create the periodic processes
      for J in Jobs loop
         Make_Empty (Args);
         POSIX.Append (Args, Child_Pathname);
         Pass_Through_Verbosity (Args);
         POSIX.Append (Args, "-child" & To_POSIX_String (Jobs'Image (J)));
         Start_Process (Child => Periodic_Processes (J),
           Pathname => Child_Pathname,
           Template => Template,
           Arg_List => Args);
         Set_Priority (Parms, Process_Prio (Priority (J)));
         Set_Scheduling_Policy (Process => Periodic_Processes (J),
           New_Policy => Sched_FIFO,
           Parameters => Parms);
         Wait_For_Child_Process
           (Status => Status,
            Child => Periodic_Processes (J),
            Block => False);
         Assert (not Status_Available (Status), "A012: P9900x0");
      end loop;
   exception
   when E : others => Fatal_Exception (E, "A013: P9900x0: Start_Jobs");
   end Start_Processes;

   procedure Stop_Processes is
   begin
      for J in Jobs loop
         Wait_For_Child_Process
           (Status => Status,
            Child => Periodic_Processes (J));
         Check_Child_Status
           (Status => Status,
            Child_ID => Periodic_Processes (J),
            Expected => 0,
            Message => "A014: P9900x0");
      end loop;
   exception
   when E : others => Fatal_Exception (E, "A015: P9900x0: Stop_Jobs");
   end Stop_Processes;

   procedure Parent_Main is
      task Main is
         pragma Priority (Main_Priority);
      end Main;
      task body Main is
      begin
         Header ("P9900" & Version, Root_OK => True);
         Optional (Priority_Process_Scheduling_Option, "A016: P9900x0");
         if Needs_Clock_Realtime then
            Optional (Timers_Option, "P9900x0");
         end if;
         Data := Shared_Data;
         Find_Utilization_Limit;
         Done;
      exception when E : others => Fatal_Exception (E, "A017: P9900x0");
      end Main;
   begin
      while not Main'Terminated loop
         delay 1.0;
      end loop;
      Finalize_Sync;
      Finalize_Shared_Data;
   exception when E : others =>
      Finalize_Sync;
      Finalize_Shared_Data;
      Fatal_Exception (E, "A018: P9900x0");
   end Parent_Main;

   procedure Child_Main is
   begin
      Data := Shared_Data;
      Initialize_Scheduling (Data);
      Work (Jobs (Child));
   exception when E : others => Fatal_Exception (E, "A019: P9900x0");
   end Child_Main;

end P9900x0;

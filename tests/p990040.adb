------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                             P 9 9 0 0 4 0                                --
--                                                                          --
--                                B o d y                                   --
--                                                                          --
--                                                                          --
--  Copyright (c) 1998 Florida  State  University  (FSU).       All Rights  --
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
--  This version uses:
--     POSIX processes and process priorities for concurrency and scheduling
--     Ada delay statements and Ada.Calendar.Clock for timing control
--     POSIX semaphores for mutual exclusion

with p990000b,   --  finding maximum achievable utilization
     p990001d,   --  mutual exclusion and synchronization
     p990002d,   --  timing control
     POSIX,
     POSIX_Process_Identification,
     POSIX_Process_Primitives,
     POSIX_Process_Scheduling,
     POSIX_Report,
     System;
procedure p990040 is

   use p990000b,
       p990001d,
       p990002d,
       POSIX,
       POSIX_Process_Identification,
       POSIX_Process_Primitives,
       POSIX_Process_Scheduling,
       POSIX_Report;

   Status : Termination_Status;

   procedure Run_Jobs;

   procedure Run_Jobs is

      function Next_Job return Jobs;
      Next : Jobs := Jobs'First;
      function Next_Job return Jobs is
         Result : Jobs;
      begin
         Result := Next;
         if Next < Jobs'Last then Next := Next + 1;
         end if;
         return Result;
      end Next_Job;

      Periodic : array (Jobs) of Process_ID;
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
        (Max_Prio - Min_Prio + 1) >= Num_Jobs + 1, "A001");
      Comment ("min_prio = " & Scheduling_Priority'Image (min_prio));
      Comment ("max_prio = " & Scheduling_Priority'Image (max_prio));
      Comment ("num_jobs = " & Jobs'Image (Num_Jobs));
      begin
         Comment ("priority = " & Scheduling_Priority'Image
           (Process_Prio (System.Priority'Last)));
      exception when E : others =>
         Unexpected_Exception (E, "A002: in Run_Jobs/Set_Priority");
         raise POSIX_Error;
      end;
      begin
         Comment ("set_Priority to system.Priority'last");
         Set_Priority (Parms, Process_Prio (System.Priority'Last));
      exception when E : others =>
         Unexpected_Exception (E, "A003: in Run_Jobs/Set_Priority");
         raise POSIX_Error;
      end;
      Comment ("setting scheduling policy");
      begin
         Set_Scheduling_Policy (Process => Get_Process_ID,
           New_Policy => Sched_FIFO,
           Parameters => Parms);
      exception when E : others =>
         Unexpected_Exception (E, "A004: in Run_Jobs/Set_Sched_Policy");
         raise POSIX_Error;
      end;
      Comment ("opening template");
      Open_Template (Template);
      --  create the periodic processes
      for I in Jobs loop
         Make_Empty (Args);
         POSIX.Append (Args, "p99004a");
         POSIX.Append (Args, "-child" & To_Posix_String (Jobs'Image (I)));
         Start_Process (Child => Periodic (I),
           Pathname => "./p990040a",
           Template => Template,
           Arg_List => Args);
         Comment ("priority = " & Scheduling_Priority'Image
           (Process_Prio (Priority (I))));
         Set_Priority (Parms, Process_Prio (Priority (I)));
         Set_Scheduling_Policy (Process => Periodic (I),
           New_Policy => Sched_FIFO,
           Parameters => Parms);
         Wait_For_Child_Process
           (Status => Status,
            Child => Periodic (I),
            Block => False);
         Assert (not Status_Available (Status), "A000");         
      end loop;
      p990002d.Initialize_Scheduling;
      Sync.Start_All_Jobs;
      Sync.Await_All_Jobs_Done;
   exception when E : others => Fatal_Exception (E, "A005: in Run_Jobs");
   end Run_Jobs;

   procedure Find_Limit is new Find_Utilization_Limit (Run_Jobs);

begin
   Header ("p990040", Root_OK => True);
   Find_Limit;
   Done;
exception when E : others => Fatal_Exception (E, "A006");
end p990040;

------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                             P 1 2 0 1 0 1                                --
--                                                                          --
--                                B o d y                                   --
--                                                                          --
--                                                                          --
--  Copyright (c) 1995-1999 Florida  State  University  (FSU).  All Rights  --
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
--  AVAILABLE OR DISCLOSED,  OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS  --
--  FOR A PARTICULAR PURPOSE OF SAID MATERIAL.                              --
--                                                                          --
------------------------------------------------------------------------------
--  [$Revision$]

--  This test is on POSIX_Memory_Locking. The test generate memory-bound
--  processes, and get performance results on memory access with lock and
--  without. Thus this test tries to covers the following "Untestable Aspects"
--  in the VSRT:

--  rt.os/mlock/mlockall/Untestable_Aspects/1:
--    A call to mlockall() when flags contains MCL_CURRENT shall cause all of
--    the pages currently mapped by the address space of a process to be
--    memory resident.

--  rt.os/mlock/munlockall/Untestable_Aspects/1
--    A call to munlockall() shall unlock all currently mapped pages of the
--    address space of the process

--  Setup: This program requires that its own executable be accessible
--  via pathname "./p120101", in order create a child process.

--  The test strategy is to measure the difference in performance of
--  the main process doing a memory-intensive computation both with and
--  without memory locking.  If the performance is better with memory
--  locking, that indicates it had some effect.  If the performance is
--  unchanged, the result is inconclusive.

--  For the test to work well you will need a machine with limited
--  real memory and will need to run it with unlimited "ulimit" values.

with Ada.Calendar,
     POSIX,
     POSIX_Limits,
     POSIX_Memory_Locking,
     POSIX_Process_Environment,
     POSIX_Process_Identification,
     POSIX_Process_Primitives,
     POSIX_Signals,
     POSIX_Report,
     Unchecked_Deallocation;

procedure p120101 is

   use Ada.Calendar,
       POSIX,
       POSIX_Memory_Locking,
       POSIX_Report;

   Array_Size : Natural := POSIX_Limits.Page_Size_Range'Last / Natural'Size;
   type Array_Type is array (1 .. Array_Size) of Natural;
   type Record_Type;
   type Pointer_Type is access Record_Type;
   type Record_Type is record
      Item : Integer;
      Pad : Array_Type;
      Next : Pointer_Type;
   end record;

   Head : Pointer_Type;
   Delay_Time : constant Duration := 10.0;
   --  enough time for other processes to access their entire
   --  lists, causing the pages to be brought into local memory
   My_Path : POSIX.POSIX_String := "./p120101";
   Num_Processes : Natural := 5;

   type Process_Record is record
      Child : POSIX_Process_Identification.Process_ID;
      Pathname : POSIX.Pathname (1 .. My_Path'Length) := My_Path;
      Template : POSIX_Process_Primitives.Process_Template;
      Arg_List : POSIX.POSIX_String_List;
   end record;
   Process_Array : array (1 .. Num_Processes) of Process_Record;

   List_Size : Integer := 1024;

   We_Are_The_Child : Boolean := False;

   procedure Free is new Unchecked_Deallocation (Record_Type, Pointer_Type);

   --------------------------
   -- Time_Big_List_Access --
   --------------------------

   function Time_Big_List_Access (Size : Natural) return Duration;

   --  Compute the average amount of time it takes to access a
   --  linked list of Size integers.

   function Time_Big_List_Access (Size : Natural) return Duration is
      Head : Pointer_Type;
      P1 : Pointer_Type;
      Sum : Duration := 0.0;
      Reps : constant Integer := 1;
      Time1, Time2 : Ada.Calendar.Time;
      Dur : Duration;
   begin
      Comment ("Time_Big_List_Access " & Integer'Image (Size));
      Head := new Record_Type;
      P1 := Head;
      for K in 2 .. Size loop
         P1.Next := new Record_Type;
         P1 := P1.Next;
      end loop;
      for J in 1 .. Reps loop
         P1 := Head;
         Time1 := Ada.Calendar.Clock;
         --  This loop should not need to bring the list back from secondary
         --  storage, unless it was too big to fit into primary memory.
         while P1 /= null loop
            P1.Item := 1;
            P1 := P1.Next;
         end loop;
         Time2 := Ada.Calendar.Clock;
         Sum := Sum + (Time2 - Time1);
      end loop;
      Dur := Sum / Reps;
      Comment ("Duration:          "
        & Integer'Image (Integer (Dur * 1_000_000)) & "us");
      while Head /= null loop
         P1 := Head; Head := P1.Next; Free (P1);
      end loop;
      return Dur;
   exception
   when Storage_Error =>
      while Head /= null loop
         P1 := Head; Head := P1.Next; Free (P1);
      end loop;
      raise;
   when E : others  => Unexpected_Exception (E, "Time_Big_List_Access");
      return Dur;
   end Time_Big_List_Access;

   -------------------
   -- Traverse_List --
   -------------------

   procedure Traverse_List;

   procedure Traverse_List is
      P1 : Pointer_Type;
   begin
      P1 := Head;
      while P1 /= null loop
         P1.Item := 1;
         P1 := P1.Next;
      end loop;
   end Traverse_List;

   ---------------------------
   -- Time_Test_List_Access --
   ---------------------------

   --  Compute the average amount of time it takes to traverse a
   --  given linked list of integers.

   function Time_Test_List_Access (Head : Pointer_Type; N : Integer)
      return Duration;

   function Time_Test_List_Access
     (Head : Pointer_Type;
      N    : Integer) return Duration
   is
      Sum  : Duration := 0.0;
      Reps : constant Integer := 1;
      D    : Duration;
      Time1, Time2 : Ada.Calendar.Time;
   begin
      Comment ("Time_Test_List_Access " & Integer'Image (N));
      for J in 1 .. Reps loop
         Time1 := Ada.Calendar.Clock;
         --  The following loop  will access the nodes of this list.  The
         --  nodes should not be in secondary storage, unless the list was
         --  too big to fit into primary memory.
         Traverse_List;
         Time2 := Ada.Calendar.Clock;
         Sum := Sum + (Time2 - Time1);
      end loop;
      D := Sum / Reps;
      Comment ("Duration: " &
        Integer'Image (Integer (D * 1_000_000)) & "us");
      return D;
   end Time_Test_List_Access;

   ----------------
   --  Check_Arg --
   ----------------

   procedure Check_Arg (Item : in POSIX_String; Quit : in out Boolean);

   procedure Check_Arg (Item : in POSIX_String; Quit : in out Boolean) is
   begin
      if Item = "-b" then We_Are_The_Child := True;
         Quit := True;
      end if;
   end Check_Arg;

   procedure Check_Args is new For_Every_Item (Check_Arg);

   -------------------
   -- Set_List_Size --
   -------------------

   procedure Set_List_Size (List_Size : in out Integer);

   procedure Set_List_Size (List_Size : in out Integer) is
      D1, D2 : Duration;
   begin
      if not We_Are_The_Child then
         Comment ("Estimate size needed to force paging behavior");
      end if;
      Outer :
      loop
         begin
            D1 := Time_Big_List_Access (List_Size);
            loop
               D2 := Time_Big_List_Access (List_Size * 3 / 2);
               if D2 > 5.0 then
                  Comment ("Giving up: no slowdown at 5 seconds");
                  return;
               end if;
               if List_Size > Integer'Last / 2 then
                  Comment ("giving up: list reached limit of type Integer");
                  return;
               end if;
               List_Size := List_Size * 3 / 2;
               exit when D2 > D1 * 2;
               D1 := D2;
            end loop;
            return;
         exception
         when Storage_Error =>
            Comment ("Reducing list size due to Storage_Error");
            loop
               begin
                  D1 := Time_Big_List_Access (List_Size);
                  exit Outer;
               exception
               when Storage_Error => null;
                  if List_Size = 0 then
                     Fatal ("A001: Storage_Error for zero list size");
                  end if;
               end;
               List_Size := List_Size * 7 / 8;
            end loop;
         when E : others =>
            Unexpected_Exception  (E, "exception estimating size needed" &
              "for paging");
         end;
      end loop Outer;
   end Set_List_Size;

   -----------------
   -- Create_List --
   -----------------

   procedure Create_List;

   procedure Create_List is
      P : Pointer_Type := Head;
   begin
      if not We_Are_The_Child then
         Comment ("Creating list of size " & Integer'Image (List_Size));
      end if;
      Head := new Record_Type;
      P := Head;
      for I in 2 .. List_Size loop
         P.Next := new Record_Type;
         P := P.Next;
      end loop;
   end Create_List;

   --------------------
   -- Randomize_List --
   --------------------

   procedure Randomize_List;

   procedure Randomize_List is
   --  Try to sufficiently randomize the list so that we minimize the
   --  pre-paging effects of the system.
      P1, P2, Tmp1, Tmp2, Last : Pointer_Type;
   begin
      for K in 1 .. 7 loop
         if not We_Are_The_Child then
            Comment ("Starting shuffle number " & Integer'Image (K) & ".");
         end if;
         P1 := Head;
         P2 := Head;
         for J in 1 .. List_Size / 2 loop
            Last := P2;
            P2 := P2.Next;
         end loop;
         Last.Next := null;
         while P1 /= null and P2 /= null loop
            Tmp1 := P1.Next;
            P1.Next := P2;
            Tmp2 := P2.Next;
            P2.Next := Tmp1;
            P1 := Tmp1;
            P2 := Tmp2;
         end loop;
      end loop;
   end Randomize_List;

begin

   --  Scan for appropriate arguments, and begin doing background work
   --  to keep memory busy if we are not the primary process.

   Check_Args (POSIX_Process_Environment.Argument_List);

   if not We_Are_The_Child then
      Header ("p120101", Root_OK => True);
      Test ("Memory_Locking Performance Test");
   end if;

   Optional (Memory_Locking_Option, "");

   --  Find a list size that seems to provoke paging, if that is
   --  possible within the operative process resource limits.

   Set_List_Size (List_Size);

   --  Create a list of that size.

   Create_List;

   --  Randomize the order of items in the list, to reduce the
   --  benefits of any prepaging that the sytem might do.

   Randomize_List;

   if We_Are_The_Child then
      --  We are a child process, who is supposed to keep the
      --  system busy with memory accesses.
      --  This loop does not need to terminate, because our
      --  parent will kill us when the test is done.
      loop
         Traverse_List;
      end loop;
   end if;

   --  If we get this far, we are the primary process for this test.

   Comment ("Starting other processes.");
   begin
      for P in 1 .. Num_Processes loop
         Append (Process_Array (P).Arg_List, My_Path);
         Append (Process_Array (P).Arg_List, "-b");
         POSIX_Process_Primitives.Open_Template (Process_Array (P).Template);
         POSIX_Process_Primitives.Start_Process
           (Process_Array (P).Child,
            Process_Array (P).Pathname,
            Process_Array (P).Template,
            Process_Array (P).Arg_List);
         Comment ("Process " & Integer'Image (P) & " started.");
      end loop;
   exception when E : POSIX_Error =>
      --  If the system can't allow us to have another process, we will
      --  go on with what we have.
      if Get_Error_Code = Resource_Temporarily_Unavailable or
        Get_Error_Code = Not_Enough_Space or
        Get_Error_Code = Operation_Not_Supported then
         Comment ("Unable to start process.  Continuing.");
      else Unexpected_Exception (E, "A002");
      end if;
   end;

   --  Time the list accesses, both with memory locked and with memory
   --  unlocked.  Compare the results, and report to the user.

   declare
      Locked_Duration, Unlocked_Duration, Ratio : Duration;
   begin
      delay Delay_Time;
      Comment ("Time the list access with memory unlocked.");
      Unlocked_Duration := Time_Test_List_Access (Head, List_Size);
      --  Do it again, because the first time may have had extra
      --  overhead to initialize the pages.
      Unlocked_Duration := Time_Test_List_Access (Head, List_Size);
      Lock_All (Current_Pages);
      delay Delay_Time;
      Comment ("Time the list access with memory locked.");
      Locked_Duration := Time_Test_List_Access (Head, List_Size);
      --  This should take less time if locking memory saved us any
      --  page faults.
      Ratio := Unlocked_Duration / Locked_Duration;
      if Ratio > 4.0 then
         Comment ("Performance indicates that memory was locked.");
      else
         Comment ("Performance does not indicate expected effect.");
      end if;
      Unlock_All;
      delay Delay_Time;
      Comment ("Time the list access again with the memory unlocked.");
      Unlocked_Duration := Time_Test_List_Access (Head, List_Size);
      --  This should take more time if unlocking memory caused us
      --  to have page faults.
      Ratio := Unlocked_Duration / Locked_Duration;
      if Ratio > 4.0 then
         Comment ("Performance indicates that memory was unlocked.");
      else
         Comment ("Performance does not indicate expected effect.");
      end if;
   end;

   --  Kill the child processes.

   declare
      Status : POSIX_Process_Primitives.Termination_Status;
   begin
      for Q in 1 .. Num_Processes loop
         POSIX_Signals.Send_Signal
           (Process_Array (Q).Child, POSIX_Signals.Signal_Kill);
         POSIX_Process_Primitives.Wait_For_Child_Process
           (Status, Process_Array (Q).Child);
      end loop;
   end;
   Done;

exception
when E1 : POSIX.POSIX_Error =>
   --  Since there is only one lock operation, we
   --  assume there is no capacity limit, so any failures can only
   --  be due to lack of support or lack of privilege.
   if Get_Error_Code = Resource_Temporarily_Unavailable then
      Comment ("The system was not able to lock the memory.");
   else
      Privileged (Memory_Locking_Privilege,
        Memory_Locking_Option, Operation_Not_Implemented, E1, "A003");
   end if;
   Done;
when E2 : others => Unexpected_Exception (E2, "A004");
end p120101;

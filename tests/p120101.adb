------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                             P 1 2 0 1 0 1                                --
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
--  AVAILABLE OR DISCLOSED,  OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS  --
--  FOR A PARTICULAR PURPOSE OF SAID MATERIAL.                              --
--                                                                          --
------------------------------------------------------------------------------
--  [$Revision$]

--  This test is on POSIX_Memory_Locking. The test generate memory-bound
--  processes, and get performance results on memory access with lock and
--  without. Thus this test also covers following "Untestable Aspects" in VSRT
--  Assertion set:

--  rt.os/mlock/mlockall/Untestable_Aspects/1:
--    A call to mlockall() when flags contains MCL_CURRENT shall cause all of
--    the pages currently mapped by the address space of a process to be
--    memory resident.

--  rt.os/mlock/munlockall/Untestable_Aspects/1
--    A call to munlockall() shall unlock all currently mapped pages of the
--    address space of the process


with POSIX,
     POSIX_Limits,
     POSIX_Memory_Locking,
     POSIX_Process_Environment,
     POSIX_Process_Identification,
     POSIX_Process_Primitives,
     POSIX_Signals,
     POSIX_Timers,
     POSIX_Report;

procedure p120101 is

   use POSIX,
       POSIX_Memory_Locking,
       POSIX_Timers,
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
      Time1, Time2 : POSIX.Timespec;
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
         Time1 := Get_Time (Clock_Realtime);
         --  This loop should not need to bring the list back from secondary
         --  storage, unless it was too big to fit into primary memory.
         while P1 /= null loop
            P1.Item := 1;
            P1 := P1.Next;
         end loop;
         Time2 := Get_Time (Clock_Realtime);
         Sum := Sum + (To_Duration (Time2 - Time1));
      end loop;
      Dur := Sum / Reps;
      Comment ("Duration: "
        & Integer'Image (Integer (Dur * 1_000_000)) & "us");
      return Dur;
   exception
   when Storage_Error => raise;
   when E : others  => Unexpected_Exception (E, "Time_Big_List_Access");
      return Dur;
   end Time_Big_List_Access;

   ---------------------------
   -- Time_Test_List_Access --
   ---------------------------

   function Time_Test_List_Access (Head : Pointer_Type; N : Integer)
      return Duration;

   --  Compute the average amount of time it takes to access a
   --  given linked list of integers.

   function Time_Test_List_Access
     (Head : Pointer_Type;
      N    : Integer) return Duration
   is
      Sum  : Duration := 0.0;
      Reps : constant Integer := 1;
      D    : Duration;
      P1   : Pointer_Type;
      Time1, Time2 : POSIX.Timespec;
   begin
      Comment ("Time_Test_List_Access " & Integer'Image (N));
      P1 := Head;
      for J in 1 .. Reps loop
         Time1 := Get_Time (Clock_Realtime);
         --  The following loop  will access the nodes of this list.  The
         --  nodes should not be in secondary storage, unless the list was
         --  too big to fit into primary memory.
         while P1 /= null loop
            P1.Item := 1;
            P1 := P1.Next;
         end loop;
         Time2 := Get_Time (Clock_Realtime);
         Sum := Sum + (To_Duration (Time2 - Time1));
      end loop;
      D := Sum / Reps;
      Comment ("Duration: " &
        Integer'Image (Integer (D * 1_000_000)) & "us");
      return D;
   end Time_Test_List_Access;


   Delay_Time : constant Duration := 30.0;
   Head : Pointer_Type;
   My_Path : POSIX.POSIX_String := "test_memory_locking_b";
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
   procedure Check_Arg (Item : in POSIX_String; Quit : in out Boolean);
   procedure Check_Arg (Item : in POSIX_String; Quit : in out Boolean) is
   begin
      if Item = "-b" then We_Are_The_Child := True;
         Quit := True;
      end if;
   end Check_Arg;
   procedure Check_Args is new For_Every_Item (Check_Arg);

   procedure Set_List_Size (List_Size : in out Integer);
   procedure Set_List_Size (List_Size : in out Integer) is
      D1, D2 : Duration;
   begin
      Comment ("Estimate size needed to force paging behavior");
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
               exit when D2 > D1 * 1.5;
               D1 := D2;
            end loop;
            return;
         exception
         when Storage_Error =>
            Comment ("Reducing list size due to Storage_Error");
            loop
               List_Size := List_Size / 2;
               begin
                  D1 := Time_Big_List_Access (List_Size);
                  exit Outer;
               exception
               when Storage_Error => null;
               end;
            end loop;
         when E : others =>
            Unexpected_Exception  (E, "exception estimating size needed" &
              "for paging");
         end;
      end loop Outer;
   end Set_List_Size;

begin

   --  Scan for appropriate arguments, and begin doing background work
   --  to keep memory busy if we are not the primary process.

   Check_Args (POSIX_Process_Environment.Argument_List);
   if We_Are_The_Child then
      --  We are a child process, who is supposed to keep the
      --  system busy with memory accesses.
      --  This loop does not need to terminate, because our
      --  parent will kill us when the test is done.
      Set_List_Size (List_Size);
      declare
         N : Integer := 1024;
         D1 : Duration;
         D2 : Duration := 0.0;
      begin
         D1 := Time_Big_List_Access (N);
         D2 := Time_Big_List_Access (N * 3 / 2);
         loop
            if D2 >  D1 * 1.5 then
               loop
                  D2 := Time_Big_List_Access (N * 3 / 2);
               end loop;
            else N := N * 3 / 2;
            end if;
         end loop;
      exception
      when E : others => Unexpected_Exception (E, "A001");
      end;
   end if;

   --  If we get this far, we are the primary process for this test.

   Header ("p120101", Root_OK => True);
   Test ("Memory_Locking Performance Test");

   Set_List_Size (List_Size);

   Comment ("Creating list of size " & Integer'Image (List_Size));
   Head := new Record_Type;
   declare
      P : Pointer_Type := Head;
      D : Duration;
   begin
      for I in 2 .. List_Size loop
         P.Next := new Record_Type;
         P := P.Next;
      end loop;
      D := Time_Test_List_Access (Head, List_Size);
   end;

   --  Try to sufficiently randomize the list so that we minimize the
   --  pre-paging effects of the system.

   declare
      P1, P2, Tmp1, Tmp2, Last : Pointer_Type;
   begin
      for K in 1 .. 7 loop
         Comment ("Starting shuffle number " & Integer'Image (K) & ".");
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
   end;

   Comment ("Locking all of current memory...");
   Lock_All (Current_Pages);

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

   delay Delay_Time;

   --  Time the list accesses, both with memory locked and with memory
   --  unlocked.  Compare the results, and report to the user.

   declare
      Locked_Duration, Unlocked_Duration, Ratio : Duration;
   begin
      Comment ("Time the list access with memory locked.");
      Locked_Duration := Time_Test_List_Access (Head, List_Size);
      Unlock_All;
      delay Delay_Time;
      Comment ("Time this list access again with the memory unlocked.");
      Unlocked_Duration := Time_Test_List_Access (Head, List_Size);
      Ratio := Unlocked_Duration / Locked_Duration;
      Comment ("With memory unlocked, the test took "
        & Integer'Image (Integer (Ratio))
        & " times longer than with memory locked.");
      if Ratio > 20.0 then
         Comment ("Performance indicates that memory was locked.");
      elsif Ratio > 10.0 then
         Comment ("Performance does not indicate whether memory was locked.");
      else
         Comment ("Performance indicates that memory was not locked.");
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
   --  since this is the first lock operation, we
   --  assume there is no capacity limit
   if Get_Error_Code = Resource_Temporarily_Unavailable then
      Comment ("The system was not able to lock the memory.");
   else
      Privileged (Memory_Locking_Privilege,
        Memory_Locking_Option, Operation_Not_Implemented, E1, "A003");
   end if;
   Done;
when E2 : others => Unexpected_Exception (E2, "A004");
end p120101;

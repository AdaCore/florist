------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                             P 0 3 0 3 0 1                                --
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

--  Test interactions of package POSIX_Signals with other packages,
--  including POSIX_Message_Queues, POSIX_Timers, and
--  POSIX_Asychronous_IO.

with Ada.Streams,
     Ada_Task_Identification,
     POSIX,
     POSIX_Asynchronous_IO,
     POSIX_Files,
     POSIX_IO,
     POSIX_Message_Queues,
     POSIX_Permissions,
     POSIX_Process_Identification,
     POSIX_Process_Primitives,
     POSIX_Report,
     POSIX_Signals,
     POSIX_Timers,
     System,
     System.Storage_Elements,
     Unchecked_Conversion;

procedure p030301 is
use  Ada.Streams,
     Ada_Task_Identification,
     POSIX,
     POSIX_Asynchronous_IO,
     POSIX_Files,
     POSIX_IO,
     POSIX_Message_Queues,
     POSIX_Permissions,
     POSIX_Process_Identification,
     POSIX_Process_Primitives,
     POSIX_Report,
     POSIX_Signals,
     POSIX_Timers,
     System;

   function To_Signal_Data is
     new Unchecked_Conversion (Long_Integer, Signal_Data);
   function To_Integer is
     new Unchecked_Conversion (Signal_Data, Long_Integer);

   Old_Mask : Signal_Set;

   A_Signal : constant Signal := SIGHUP;
   A_Signal_Addr : constant System.Address := Signal_Reference (A_Signal);
   B_Signal  : constant Signal := SIGUSR1;
   C_Signal  : constant Signal := SIGUSR2;

   task My_Handler is
      entry Reset_Count;
      entry Current_Count (X : out integer);
      entry Done;
      for Done use at A_Signal_Addr;
   end My_Handler;

   task body My_Handler is
      Count : integer := 0;
   begin
      loop
         select
            accept Done do
               Count := Count + 1;
            end Done;
         or
            accept Reset_Count do
               Count := 0;
            end Reset_Count;
         or
            accept Current_Count (X : out integer) do
               X := Count;
            end Current_Count;
         or
            terminate;
         end select;
      end loop;
   end My_Handler;

   Valid_MQ_Name : constant POSIX.POSIX_String := "/test_mq";
   Valid_AIO_File_Name : constant POSIX.POSIX_String := "aio_test_file";

begin

   Header ("p030301");

   Test ("Validity of Signals used for this test ");

   declare
      Mask1 : Signal_Set;
   begin
      Delete_All_Signals (Mask1);
      begin
         Add_Signal (Mask1, A_Signal);
      exception
         when E1 : POSIX_Error =>
            Comment (Image (A_Signal) & " is not a valid signal");
            Fatal_Exception (E1, "A001");
         when E2 : others =>
            Unexpected_Exception (E2, "A002");
      end;
      begin
         Add_Signal (Mask1, B_Signal);
      exception
         when E1 : POSIX_Error =>
            Comment (Image (B_Signal) & " is not a valid signal");
            Fatal_Exception (E1, "A003");
         when E2 : others =>
            Unexpected_Exception (E2, "A004");
      end;
      begin
         Add_Signal (Mask1, C_Signal);
      exception
         when E1 : POSIX_Error =>
            Comment (Image (C_Signal) & " is not a valid signal");
            Fatal_Exception (E1, "A005");
         when E2 : others =>
            Unexpected_Exception (E2, "A006");
      end;
   end;

   ---------------------------------------------------------------------
   --  Set_Stopped_Child_Signal shall control the generation of the
   --  Signal_Child signal, if the implementation supports the
   --  Signal_Child signal.

   --  If the default action is to stop the process, the execution of
   --  that process (including all tasks within it) shall be
   --  temporarily suspended.  When a process stops, a Signal_Child
   --  signal shall be generated for its parent process, unless the
   --  parent process has disabled this feature, by calling
   --  Set_Stopped_Child_Signal with parameter Enable set to False.

   --  Stopped_Child_Signal_Enabled shall return True if and only if
   --  the signal specified by Signal_Child will be generated for the
   --  calling process whenever any of its child processes stop.

   Test ("Set_Stopped_Child_Signal [3.3.10]");
   declare

      New_Mask : Signal_Set;
      Sig      : Signal;
      Child_ID : Process_ID;
      Template : Process_Template;
      Args     : POSIX_String_List;
      Status   : Termination_Status;
      Child_Pathname : POSIX_String := "./p030301b";
   begin

      Open_Template (Template);
      Make_Empty (Args);
      POSIX.Append (Args, "p030301b");
      POSIX.Append (Args, "-child");
      Pass_Through_Verbosity (Args);

      ------------------------------------------------------------------
      --  The initial state of the process has the generation of
      --  SIGCHLD enabled for stopped child processes.

      Assert (Stopped_Child_Signal_Enabled, "A007");

      ------------------------------------------------------------------
      --  If the parameter Enable has the value True, the
      --  Signal_Child signal shall be generated for the calling
      --  process whenever any of its child processes stop.

      Comment ("Setting Stopped_Child_Signal to True");
      Set_Stopped_Child_Signal (True);
      Assert (Stopped_Child_Signal_Enabled, "A008");

      Comment ("Deleting all signals");
      Delete_All_Signals  (New_Mask);
      Comment ("Adding in SIGCHLD");
      Add_Signal (New_Mask, SIGCHLD);
      Comment ("Blocking SIGCHLD");
      Block_Signals (New_Mask, Old_Mask);

      Comment ("Starting child process");
      Start_Process (Child_ID, Child_Pathname, Template, Args);
      Wait_For_Child_Process (Status, Child_ID, Block => False);
      Assert (not Status_Available (Status), "A000");

      Comment ("Parent: stopping child");
      Send_Signal (Child_ID, SIGSTOP);

      Comment ("Parent: Awaiting SIGCHLD");
      begin
         Sig := Await_Signal_Or_Timeout (New_Mask, To_Timespec (3.0));
         Assert (Sig = SIGCHLD, "A009");
      exception
      when POSIX_Error =>
         Comment ("Parent: apparently timed out");
         Check_Error_Code (ETIMEDOUT, "A101");
      when E : others =>
         Unexpected_Exception (E, "A102");
      end;

      --  .... A similar test should be done for the case where
      --  the thread awaiting the child is not the initial thread of the
      --  process.

      Comment ("Parent: continuing child");
      Send_Signal (Child_ID, SIGCONT);

      Wait_For_Child_Process (Status, Child_ID);
      Check_Child_Status (Status, Child_ID, 0, "A090");

      ------------------------------------------------------------------
      --  If Enable is False, the implementation shall not generate
      --  the Signal_Child signal in this way.

      Comment ("Setting Stopped_Child_Signal to False");
      Set_Stopped_Child_Signal (False);
      Assert (not Stopped_Child_Signal_Enabled, "A007");

      Comment ("Starting child process");
      Start_Process (Child_ID, Child_Pathname, Template, Args);
      Comment ("Parent: stopping child");
      Send_Signal (Child_ID, SIGSTOP);

      Comment ("Parent: Awaiting SIGCHLD");
      begin
--         Sig := Await_Signal_Or_Timeout
--          (New_Mask, To_Timespec (3.0));
         Expect_Exception ("A100");
      exception
      when POSIX_Error => Check_Error_Code (ETIMEDOUT, "A101");
      when E : others =>
         Unexpected_Exception (E, "A102");
      end;

      Wait_For_Child_Process (Status, Child_ID);
      Check_Child_Status (Status, Child_ID, 0, "A090");

   exception
      when E1 : POSIX_Error =>
         Optional (Job_Control_Option, Invalid_Argument, E1, "A010");
      when E2 : others =>
         Unexpected_Exception (E2, "A011");
   end;

   ---------------------------------------------------------------------

   Test ("Pending_Signals [3.3.11]");
   declare
      New_Mask  : Signal_Set;
      Ret : Integer := 0;
   begin

      My_Handler.Reset_Count;

      Delete_All_Signals (New_Mask);
      Add_Signal (New_Mask, A_Signal);
      Comment ("Blocking A_Signal");
      Block_Signals (New_Mask, Old_Mask);

      Comment ("Sending A_Signal to self");
      Send_Signal (POSIX_Process_Identification.Get_Process_ID, A_Signal);
      Comment ("Delaying");
      delay 0.1;
      Comment ("Checking count");
      My_Handler.Current_Count (Ret);
      Assert (Ret = 0, "A012: Count = " & Integer'Image (Ret));

      Assert (Is_Member (Pending_Signals, A_Signal), "A013");

      Comment ("Unblocking A_Signal");
      Unblock_Signals (New_Mask, Old_Mask);
      Comment ("Delaying");
      delay 0.1;
      Comment ("Checking count");
      My_Handler.Current_Count (Ret);
      Assert (Ret = 1, "A014: " & Integer'Image (Ret));

   exception
      when E1 : others =>
         Unexpected_Exception (E1, "A015");
   end;

   ---------------------------------------------------------------------

   Test ("Await_Signal without info [3.3.15]");
   declare
      Old_Sig   : Signal;
      New_Mask  : Signal_Set;

   begin

      Delete_All_Signals  (New_Mask);
      Add_Signal (New_Mask, A_Signal);
      Block_Signals (New_Mask, Old_Mask);

      Comment ("--1--");

      begin
         --  The signal is alreay bound to an entry
         Old_Sig := Await_Signal (New_Mask);
         Expect_Exception ("A016");
      exception
      when POSIX_Error =>
         Check_Error_Code (Invalid_Argument, "A017");
      when E1 : others => Unexpected_Exception (E1, "A018");
      end;

      Comment ("--2--");

      Delete_All_Signals  (New_Mask);
      Add_Signal (New_Mask, B_Signal);
      Block_Signals (New_Mask, Old_Mask);

      Comment ("--3--");

      Send_Signal (POSIX_Process_Identification.Get_Process_ID, B_Signal);

      Comment ("--4--");

      Old_Sig := Await_Signal (New_Mask);
      Assert (Old_Sig = B_Signal, "A019");
      --  This should return immediately since there is a signal pending

      Send_Signal (POSIX_Process_Identification.Get_Process_ID, B_Signal);

      Comment ("--5--");
      begin
         Old_Sig := Await_Signal_Or_Timeout (New_Mask, To_Timespec (1, 0));
         --  This should return immediately since there is a signal pending
         Assert (Old_Sig = B_Signal, "A020");
      exception
      when E1 : others => Unexpected_Exception (E1, "A021");
      end;

      begin
         Old_Sig := Await_Signal_Or_Timeout (New_Mask, To_Timespec (1, 0));
         --  This should not return with signal catching. Instead it
         --  should return a POSIX_Error when the time expires.
         Expect_Exception ("A022");
      exception
      when POSIX_Error =>
         Check_Error_Code (Resource_Temporarily_Unavailable, "A023");
      when E1 : others => Unexpected_Exception (E1, "A024");
      end;

      Comment ("--6--");

      Unblock_Signals (New_Mask, Old_Mask);
   exception
   when E1 : others => Unexpected_Exception (E1, "A025");
   end;

   ---------------------------------------------------------------------

   Test ("Await_Signal with info [3.3.16]");
   declare
      New_Mask  : Signal_Set;
      Sig_Info  : Signal_Info;
   begin
      Delete_All_Signals  (New_Mask);
      Add_Signal (New_Mask, B_Signal);
      Block_Signals (New_Mask, Old_Mask);
      Send_Signal (POSIX_Process_Identification.Get_Process_ID, B_Signal);

      Comment ("--1--");

      Sig_Info := Await_Signal (New_Mask);

      Comment ("--2--");

      Assert (Get_Signal (Sig_Info) = B_Signal, "A026");
      --  This should return immediately since there is a signal pending

      Send_Signal (POSIX_Process_Identification.Get_Process_ID, B_Signal);
      Comment ("--3--");

      begin
         Sig_Info := Await_Signal_Or_Timeout (New_Mask, To_Timespec (1, 0));
         --  This should return immediately since there is a signal pending
         Assert (Get_Signal (Sig_Info) = B_Signal, "A027");
      exception
         when E1 : others =>
            Unexpected_Exception (E1, "A028");
      end;

      begin
         Sig_Info := Await_Signal_Or_Timeout (New_Mask, To_Timespec (1, 0));
         --  This should not return with signal catching. Instead it
         --  should return a POSIX_Error when the time expires.
         Expect_Exception ("A029");
      exception
      when POSIX_Error =>
         Check_Error_Code (Resource_Temporarily_Unavailable, "A030");
      when E1 : others => Unexpected_Exception (E1, "A031");
      end;

      Comment ("--4--");

      Unblock_Signals (New_Mask, Old_Mask);
   exception
   when E1 : POSIX_Error =>
      Optional (Realtime_Signals_Option,
        Operation_Not_Implemented, E1, "A032");
   when E2 : others => Unexpected_Exception (E2, "A033");
   end;

   ---------------------------------------------------------------------

   Test ("Interrupt_Task [3.3.20]");
   declare

      task Blocked_Task is
         entry Get_ID (ID : out Task_Id);
      end Blocked_Task;

      task body Blocked_Task is
         Buf_1 : POSIX_String (1 .. 4);
         Last : IO_Count;
         MMM : String (1 .. 1);
      begin

         accept Get_ID (ID : out Task_Id) do
            ID := Current_Task;
         end Get_ID;

         POSIX_IO.Read
           (POSIX_IO.Standard_Input, Buf_1, Last);
      exception
         when POSIX_Error =>
            Check_Error_Code (Interrupted_Operation, "A034");
         when E1 : others =>
            Unexpected_Exception (E1, "A035");
      end Blocked_Task;

      T_ID : Task_Id;

   begin
      Blocked_Task.Get_ID (T_ID);
      delay 0.1;
      Interrupt_Task (T_ID);
   end;

   ---------------------------------------------------------------------

   Test ("Queue_Signal [3.3.19]");

   declare
      New_Mask : Signal_Set;
      Sig_Info : Signal_Info;
      Sig_D    : Signal_Data := To_Signal_Data (10);

   begin

      Delete_All_Signals  (New_Mask);
      Add_Signal (New_Mask, B_Signal);
      Block_Signals (New_Mask, Old_Mask);

      begin
         Enable_Queueing (B_Signal);
      exception
         when E1 : POSIX_Error =>
            Optional (Realtime_Signals_Option, Operation_Not_Supported,
                E1, "A036");
         when E2 : others =>
            Unexpected_Exception (E2, "A037");
      end;

      delay 0.1;
      Queue_Signal
        (POSIX_Process_Identification.Get_Process_ID, B_Signal, Sig_D);

      Sig_Info := Await_Signal (New_Mask);

      Assert (Get_Data (Sig_Info) = Sig_D, "A038: signal data = "
        & Long_Integer'Image (To_Integer (Get_Data (Sig_Info))));

      Unblock_Signals (New_Mask, New_Mask);

   exception
      when E1 : POSIX_Error =>
         Optional (Realtime_Signals_Option, Operation_Not_Implemented, E1,
                "A039");
      when E2 : others =>
         Unexpected_Exception (E2, "A040");
   end;

   ---------------------------------------------------------------------

   Test ("Signal notification with message queue");

   declare
      New_Mask : Signal_Set;
      Sig_Info : Signal_Info;
      Sig_D    : Signal_Data := To_Signal_Data (20);
      Sig_E    : Signal_Event;
      MQ : Message_Queue_Descriptor;

      Received_B_Signal : Boolean := False;
      pragma Volatile (Received_B_Signal);

      task Watchdog;

      task body Watchdog is
      begin
         delay 2.0;
         if not Received_B_Signal then
            Send_Signal (Get_Process_ID, B_Signal);
         end if;
      end Watchdog;

   begin

      Comment ("Checking that no residual message queue exists.");
      begin
         Unlink_Message_Queue (Valid_MQ_Name);
      exception
      when E1 : POSIX_Error =>
         if Get_Error_Code /= No_Such_File_Or_Directory then
            Optional (Realtime_Signals_Option,
              Operation_Not_Implemented, E1, "A041");
         end if;
      when E2 : others => Unexpected_Exception (E2, "A042");
      end;

      Comment ("Creating message queue");
      MQ := Open_Or_Create (Valid_MQ_Name, Read_Write, Owner_Permission_Set);

      Set_Signal (Sig_E, B_Signal);
      Set_Notification (Sig_E, Signal_Notification);
      Set_Data (Sig_E, Sig_D);

      Delete_All_Signals  (New_Mask);
      Add_Signal (New_Mask, B_Signal);
      Comment ("Blocking signals");
      Block_Signals (New_Mask, Old_Mask);

      begin
         Comment ("Enabling queueing");
         Enable_Queueing (B_Signal);
      exception
         when E1 : POSIX_Error =>
            Optional (Realtime_Signals_Option, Operation_Not_Supported, E1,
                "A043");
         when E2 : others =>
            Unexpected_Exception (E2, "A044");
      end;

      Comment ("Sending message");
      Send (MQ, To_Stream_Element_Array ("Hello....."), 1);
--  This doesnt appear to send any signals.
      Comment ("Delaying");
      delay 0.1;

      Assert (Is_Member (New_Mask, B_Signal), "A045");
      Assert (Is_Member (Pending_Signals, B_Signal), "A046");
      Assert (not Is_Member (New_Mask, C_Signal), "A047");
      Assert (not Is_Member (Pending_Signals, C_Signal), "A048");

      Comment ("Awaiting signal");
      Sig_Info := Await_Signal (New_Mask);
      Received_B_Signal := True;

      Assert (Get_Signal (Sig_Info) = B_Signal, "A049: signal = "
        & Signal'Image (Get_Signal (Sig_Info)));

      Assert (Get_Data (Sig_Info) = Sig_D, "A050: signal data = "
        & Long_Integer'Image (To_Integer (Get_Data (Sig_Info))));

      Assert (not Is_Member (Pending_Signals, C_Signal), "A051");

      Comment ("Unblocking signals");
      Unblock_Signals (New_Mask, New_Mask);

      Comment ("Closing MQ");
      Close (MQ);

      Comment ("Unlinking MQ");
      Unlink_Message_Queue (Valid_MQ_Name);

   exception
      when E1 : POSIX_Error =>
         Optional (Realtime_Signals_Option, Message_Queues_Option,
           Operation_Not_Implemented, E1, "A052");
      when E2 : others =>
         Unexpected_Exception (E2, "A053");
   end;

   ---------------------------------------------------------------------

   Test ("Signal notification with async. I/O");

   declare
      New_Mask : Signal_Set;
      Sig_Info : Signal_Info;
      Sig_D    : Signal_Data := To_Signal_Data (40);
      Sig_E    : Signal_Event;
      AD_1     : AIO_Descriptor;
      AD_2     : AIO_Descriptor;
      FD       : File_Descriptor;
      Buf_1    : IO_Array_Pointer := new Stream_Element_Array (1 .. 10);
      Buf_2    : IO_Array_Pointer := new Stream_Element_Array (1 .. 10);
      List     : AIO_Descriptor_List (1 .. 1);
      Count    : Natural := 0;

      Received_B_Signal : Boolean := False;
      pragma Volatile (Received_B_Signal);
      Received_C_Signal : Boolean := False;
      pragma Volatile (Received_C_Signal);

      task Watchdog;

      task body Watchdog is
      begin
         delay 2.0;
         if not Received_B_Signal then
            Send_Signal (Get_Process_ID, B_Signal);
         end if;
         delay 2.0;
         if not Received_C_Signal then
            Send_Signal (Get_Process_ID, C_Signal);
         end if;
      end Watchdog;

   begin
      Delete_All_Signals  (New_Mask);
      Add_Signal (New_Mask, B_Signal);
      Add_Signal (New_Mask, C_Signal);
      Comment ("Blocking signals");
      Block_Signals (New_Mask, Old_Mask);

      begin
         Comment ("Enabling queueing");
         Enable_Queueing (C_Signal);
      exception
         when E1 : POSIX_Error =>
            Optional (Realtime_Signals_Option, Operation_Not_Supported,
                E1, "A054");
         when E2 : others =>
            Unexpected_Exception (E2, "A055");
      end;

      AD_1 := Create_AIO_Control_Block;
      Comment ("Setting buffer");
      Set_Buffer (AD_1, Buf_1);
      Buf_1.all := To_Stream_Element_Array ("hello....1");
      Set_Length (AD_1, 6);

      AD_2 := Create_AIO_Control_Block;
      Comment ("Setting buffer");
      Set_Buffer (AD_2, Buf_2);
      Buf_2.all := To_Stream_Element_Array ("hello....2");
      Set_Length (AD_2, 6);

      Comment ("Setting signal");
      Set_Signal (Sig_E, B_Signal);
      Set_Notification (Sig_E, Signal_Notification);
      Set_Data (Sig_E, Sig_D);

      Comment ("Setting event");
      Set_Event (AD_1, Sig_E);

      Comment ("Setting signal");
      Set_Signal (Sig_E, C_Signal);
      Set_Notification (Sig_E, Signal_Notification);
      Set_Data (Sig_E, Sig_D);

      Comment ("Setting event");
      Set_Event (AD_2, Sig_E);

      Comment ("Opening file");
      FD := Open_Or_Create
        (Valid_AIO_File_Name, Read_Write, Owner_Permission_Set);

      Set_File (AD_1, FD);
      Set_Operation (AD_1, Write);
      Set_File (AD_2, FD);
      Set_Operation (AD_2, Write);

      List (1) := AD_1;

      Comment ("List_IO_No_Wait call");
      List_IO_No_Wait (List, Sig_E);

      Comment ("Write call");
      Write (AD_2);

      Comment ("Delaying");
      delay 1.0;

      Delete_Signal (New_Mask, C_Signal);
      Assert (not Is_Member (New_Mask, C_Signal), "A056");
      Assert (Is_Member (New_Mask, B_Signal), "A057");

      Comment ("Awaiting signal B");
      Sig_Info := Await_Signal (New_Mask);
      Received_B_Signal := True;

      Assert (Get_Signal (Sig_Info) = B_Signal, "A058: signal = "
        & Signal'Image (Get_Signal (Sig_Info)));

      Assert (Get_Data (Sig_Info) = Sig_D, "A059: signal data = "
        & Long_Integer'Image (To_Integer (Get_Data (Sig_Info))));
      Assert (not Is_Member (Pending_Signals, B_Signal), "A060");
      Assert (Is_Member (Pending_Signals, C_Signal), "A061");

      Delete_Signal (New_Mask, B_Signal);
      Add_Signal (New_Mask, C_Signal);
      Assert (not Is_Member (New_Mask, B_Signal), "A062");
      Assert (Is_Member (New_Mask, C_Signal), "A063");

      Comment ("Awaiting signal C");
      Sig_Info := Await_Signal (New_Mask);
      Received_C_Signal := True;

      Assert (Get_Signal (Sig_Info) = C_Signal, "A064: signal = "
        & Signal'Image (Get_Signal (Sig_Info)));

      Assert (Get_Data (Sig_Info) = Sig_D, "A065: signal data = "
        & Long_Integer'Image (To_Integer (Get_Data (Sig_Info))));
      Assert (not Is_Member (Pending_Signals, B_Signal), "A066");

      Count := 1;
      while Is_Member (Pending_Signals, C_Signal)
        and Count < 100 loop
         Count := Count + 1;
      end loop;
      Assert (Count = 1, "A067: Count =" & Integer'Image (Count));

      Comment ("Ignoring signals");
      Ignore_Signal (B_Signal);
      Ignore_Signal (C_Signal);

      Comment ("Unignoring signals");
      Unignore_Signal (B_Signal);
      Unignore_Signal (C_Signal);

      Assert (not Is_Member (Pending_Signals, C_Signal), "A068");
      Assert (not Is_Member (Pending_Signals, B_Signal), "A069");

      Comment ("Unblocking signals");
      Add_Signal (New_Mask, B_Signal);
      Unblock_Signals (New_Mask, New_Mask);

      Comment ("Closing aio test file");
      Close (FD);

      Comment ("Unlinking aio test file");
      Unlink (Valid_AIO_File_Name);

   exception
      when E1 : POSIX_Error =>
         Optional (Realtime_Signals_Option, Asynchronous_IO_Option,
           Operation_Not_Implemented, E1, "A070");
      when E2 : others =>
         Unexpected_Exception (E2, "A071");
   end;

   ---------------------------------------------------------------------

   Test ("Signal notification with timers");
   declare
      New_Mask  : Signal_Set;
      Sig_Info  : Signal_Info;
      Sig_D     : Signal_Data := To_Signal_Data (30);
      Sig_E     : Signal_Event;
      Tid       : Timer_ID;
      New_State : Timer_State;
      Initial   : Timespec;
      Interval  : Timespec;
      Count     : Natural := 0;
   begin

      Comment ("Creating timer");
      Set_Signal (Sig_E, C_Signal);
      Set_Notification (Sig_E, Signal_Notification);
      Set_Data (Sig_E, Sig_D);

      Tid := Create_Timer (Clock_Realtime, Sig_E);

      Comment ("Blocking signal");
      Delete_All_Signals  (New_Mask);
      Add_Signal (New_Mask, C_Signal);
      Block_Signals (New_Mask, Old_Mask);

      begin
         Comment ("Enabling queueing");
         Enable_Queueing (C_Signal);
      exception
         when E1 : POSIX_Error =>
            Optional (Realtime_Signals_Option, Operation_Not_Supported, E1,
              "A072");
         when E2 : others =>
            Unexpected_Exception (E2, "A073");
      end;

      Comment ("Setting up timer");
      POSIX.Set_Seconds (Initial, 1);
      POSIX.Set_Nanoseconds (Initial, 1);
      POSIX.Set_Seconds (Interval, 0);
      POSIX.Set_Nanoseconds (Interval, 0);
      Set_Initial (New_State, Initial);
      Set_Interval (New_State, Interval);
      --  Since Absolute_Timer is specified, timer is set to expire to
      --  Epoch+1 seconds,  so it will generate a signal immediately.
      --  Interval = 0, thus it only generates the signal once.

      Arm_Timer (Tid, Absolute_Timer, New_State);

      Comment ("Delaying");
      delay 0.1;

      Comment ("Awaiting signal");

      Sig_Info := Await_Signal (New_Mask);
      Assert (not Is_Member (Pending_Signals, C_Signal), "A074");

      Assert (Get_Signal (Sig_Info) = C_Signal, "A075: signal = "
        & Signal'Image (Get_Signal (Sig_Info)));

      Assert (Get_Data (Sig_Info) = Sig_D, "A076: signal data = "
        & Long_Integer'Image (To_Integer (Get_Data (Sig_Info))));

      Count := 1;
      while Is_Member (Pending_Signals, C_Signal)
        and Count < 100 loop
         Count := Count + 1;
      end loop;
      Assert (Count = 1, "A077: Count =" & Integer'Image (Count));

      Comment ("Ignoring signals");
      Ignore_Signal (B_Signal);
      Ignore_Signal (C_Signal);

      Comment ("Unignoring signals");
      Unignore_Signal (B_Signal);
      Unignore_Signal (C_Signal);

      Assert (not Is_Member (Pending_Signals, C_Signal), "A078");

      Unblock_Signals (New_Mask, New_Mask);

      Delete_Timer (Tid);

   exception
      when E1 : POSIX_Error =>
         if Get_Error_Code = Operation_Not_Supported then
            Set_Error_Code (Operation_Not_Implemented);
         end if;

         --  POSIX.5b erroneously specifies OPERATION_NOT_SUPPORTED for
         --  Create/Delete_Timer.  That is inconsistent with POSIX.1b.
         --  Therefore, we allow Operation_Not_Implemented as well as
         --  Operation_Not_Supported.

         Optional (Realtime_Signals_Option, Timers_Option,
           Operation_Not_Implemented, E1, "A079");
      when E2 : others =>
         Unexpected_Exception (E2, "A080");
   end;

   Done;
exception
   when E : others => Fatal_Exception (E, "A081");
end p030301;

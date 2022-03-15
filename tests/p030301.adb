------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                             P 0 3 0 3 0 1                                --
--                                                                          --
--                                B o d y                                   --
--                                                                          --
--                                                                          --
--  Copyright (c) 1995-1999 Florida  State  University  (FSU).  All Rights  --
--  Reserved.                                                               --
--                     Copyright (C) 2000-2022, AdaCore                     --
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

--  Setup: The program must be run with the executable file for
--  program p030301b accessible via the pathname "./p030301b".

with Ada.Streams,
     Ada_Task_Identification,
     POSIX,
     POSIX_Asynchronous_IO,
     POSIX.C,
     POSIX_Files,
     POSIX_IO,
     POSIX_Message_Queues,
     POSIX_Permissions,
     POSIX_Process_Identification,
     POSIX_Process_Primitives,
     POSIX_Report,
     POSIX_Signals,
     POSIX_Timers,
     p030300a,
     System,
     Test_Parameters,
     Unchecked_Conversion;

procedure p030301 is
   use Ada.Streams,
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
       p030300a,
       Test_Parameters;

   type Signal_Scalar is mod 2 ** (C.sigval_byte_size * System.Storage_Unit);

   function To_Signal_Data is
     new Unchecked_Conversion (Signal_Scalar, Signal_Data);
   function To_Integer is
     new Unchecked_Conversion (Signal_Data, Signal_Scalar);

   Old_Mask : Signal_Set;

   Signals : constant array (1 .. 3) of Signal :=
     (SIGHUP, SIGUSR1, SIGUSR2);

   Valid_MQ_Name : constant POSIX.POSIX_String := "/test_mq";
   Valid_AIO_File_Name : constant POSIX.POSIX_String := "aio_test_file";
   Child_Pathname : POSIX_String := "./p030301b";
   Child_Name : POSIX_String := "p030301b";

   Queueing_Is_Enabled : Boolean := False;
   --  It would be nice if this were a standard function.

   procedure Try_Enable_Queueing (Sig : Signal; Msg : String);
   procedure Try_Disable_Queueing (Sig : Signal);

   procedure Try_Enable_Queueing (Sig : Signal; Msg : String) is
   begin
      Enable_Queueing (Sig);
      Queueing_Is_Enabled := True;
   exception
   when E1 : POSIX_Error =>
      Optional (Realtime_Signals_Option, Operation_Not_Supported,
        E1, Msg & 'a');
   when E2 : others =>
      Unexpected_Exception (E2, Msg & 'b');
   end Try_Enable_Queueing;

   procedure Try_Disable_Queueing (Sig : Signal) is
   begin
      if Queueing_Is_Enabled then
         Disable_Queueing (Sig);
         Queueing_Is_Enabled := False;
      end if;
   end Try_Disable_Queueing;

begin

   Header ("p030301");

   Test ("Validity of Signals used for this test ");

   declare
      Mask : Signal_Set;
   begin
      for I in Signals'Range loop
         Delete_All_Signals (Mask);
         begin
            Add_Signal (Mask, Signals (I));
         exception
         when E1 : POSIX_Error =>
            Comment (Image (Signals (I)) & " is not a valid signal");
            Fatal_Exception (E1, "A001:" & Image (Signals (I)));
         when E2 : others =>
            Unexpected_Exception (E2, "A002: " & Image (Signals (I)));
         end;
      end loop;
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
   --  [3.3.4]

   --  Stopped_Child_Signal_Enabled shall return True if and only if
   --  the signal specified by Signal_Child will be generated for the
   --  calling process whenever any of its child processes stop.

   Test ("Set_Stopped_Child_Signal [3.3.10]");
   declare
      New_Mask : Signal_Set;
      Child_ID : Process_ID;
      Template : Process_Template;
      Args     : POSIX_String_List;
      Status   : Termination_Status;

      procedure Do_Test (Expect_Timeout : Yes_No_Maybe; Msg : String);

      procedure Do_Test (Expect_Timeout : Yes_No_Maybe; Msg : String) is
      begin
         Comment ("Starting child process");
         Start_Process (Child_ID, Child_Pathname, Template, Args);
         Wait_For_Child_Process (Status, Child_ID, Block => False);
         Assert (not Status_Available (Status), "A003: " & Msg);

         Comment ("Parent: stopping child");
         Send_Signal (Child_ID, SIGSTOP);

         Comment ("Parent: awaiting SIGCHLD");
         begin
            Try_Await_Signal
              (SIGCHLD, New_Mask, 3.0, Expect_Timeout, "A004: " & Msg);
         exception
         when Local_Failure => null;
         when E : others => Unexpected_Exception (E, "A005: " & Msg);
         end;

         Comment ("Parent: continuing child");
         Send_Signal (Child_ID, SIGCONT);

         Wait_For_Child_Process (Status, Child_ID);
         Check_Child_Status (Status, Child_ID, 0, "A006: " & Msg);

      end Do_Test;

   begin

      Open_Template (Template);
      Make_Empty (Args);
      POSIX.Append (Args, Child_Name);
      POSIX.Append (Args, "-child");
      Pass_Through_Verbosity (Args);

      ------------------------------------------------------------------
      --  The initial state of the process has the generation of
      --  SIGCHLD enabled for stopped child processes.

      begin
         Assert (Stopped_Child_Signal_Enabled, "A007");
      exception
      when E : others => Unexpected_Exception (E, "A008");
      end;

      ------------------------------------------------------------------
      --  If the parameter Enable has the value True, the
      --  Signal_Child signal shall be generated for the calling
      --  process whenever any of its child processes stop.

      Comment ("Setting Stopped_Child_Signal to True");
      begin
         Set_Stopped_Child_Signal (True);
         Assert (Stopped_Child_Signal_Enabled, "A009");
      exception
      when E : others => Unexpected_Exception (E, "A010");
      end;

      Delete_All_Signals  (New_Mask);
      Add_Signal (New_Mask, SIGCHLD);
      Comment ("Blocking SIGCHLD");
      Block_Signals (New_Mask, Old_Mask);

      Do_Test (No, "initial task");

      ------------------------------------------------------------------
      --  Redo the test case above, where the task awaiting the
      --  child is not the initial task of the process.

      declare
         task T;
         task body T is
         begin
            Do_Test (No, "subsidiary task");
         end T;
      begin
         null;
      end;

      ------------------------------------------------------------------
      --  If Enable is False, the implementation shall not generate
      --  the Signal_Child signal in this way.

      Comment ("Setting Stopped_Child_Signal to False");
      Set_Stopped_Child_Signal (False);
      Assert (not Stopped_Child_Signal_Enabled, "A011");

      Do_Test (Yes, "initial task, no child signal");

   exception
   when E1 : POSIX_Error =>
      Optional (Job_Control_Option, Invalid_Argument, E1, "A012");
   when E2 : others =>
      Unexpected_Exception (E2, "A013");
   end;

   ---------------------------------------------------------------------
   --  If a signal is sent to a process while the signal is masked
   --  for all tasks in the process, the signal remains pending, and
   --  can be detected via a call to Pending_Signals.
   --  As soon as the pending signal is cleared, it is no longer
   --  detected by Pending_Signals.

   Test ("Pending_Signals [3.3.11]");
   declare
      New_Mask  : Signal_Set;
   begin
      for I in Signals'Range loop
         Delete_All_Signals (New_Mask);
         Add_Signal (New_Mask, Signals (I));
         Comment ("Blocking signal");
         Block_Signals (New_Mask, Old_Mask);
         Comment ("Sending signal to self");
         Send_Signal (POSIX_Process_Identification.Get_Process_ID,
           Signals (I));
         Assert (Is_Member (Pending_Signals, Signals (I)), "A014");
         Comment ("Discarding signal");
         Ignore_Signal (Signals (I));
         Unblock_Signals (New_Mask, Old_Mask);
         Assert (not Is_Member (Pending_Signals, Signals (I)), "A015");
         Unignore_Signal (Signals (I));
      end loop;
   exception
   when E1 : others => Unexpected_Exception (E1, "A016");
   end;

   ---------------------------------------------------------------------

   Test ("Await_Signal without info [3.3.15]");
   declare
      Old_Sig   : Signal;
      Old_Mask  : Signal_Set;
      New_Mask  : Signal_Set;

      procedure Test_Signal (Sig : Signal);
      procedure Test_Signal (Sig : Signal) is
      begin
         Comment ("Testing Await_Signal on signal " & Image (Sig));

         Delete_All_Signals  (New_Mask);
         Add_Signal (New_Mask, Sig);
         Block_Signals (New_Mask, Old_Mask);

         Comment ("Sending self " & Image (Sig));
         Send_Signal (POSIX_Process_Identification.Get_Process_ID, Sig);

         Comment ("Awaiting signal " & Image (Sig));
         Old_Sig := Await_Signal (New_Mask);
         --  This should return immediately since there is a signal pending
         Assert (Old_Sig = Sig, "A017");

         Send_Signal (POSIX_Process_Identification.Get_Process_ID, Sig);

         Comment ("Awaiting " & Image (Sig));
         begin
            Old_Sig := Await_Signal_Or_Timeout (New_Mask, To_Timespec (1, 0));
            --  This should return immediately since there is a signal pending.
            Assert (Old_Sig = Sig, "A018");
         exception
         when E1 : others => Unexpected_Exception (E1, "A019");
         end;

         Comment ("Sending self " & Image (Sig));
         Send_Signal (POSIX_Process_Identification.Get_Process_ID, Sig);

         Comment ("Awaiting " & Image (Sig));
         begin
            Old_Sig := Await_Signal_Or_Timeout (New_Mask, To_Timespec (1, 0));
            --  This should return immediately since there is a signal pending.
            Assert (Old_Sig = Sig, "A020");
         exception
         when E1 : others => Unexpected_Exception (E1, "A021");
         end;

         Comment ("Awaiting " & Image (Sig));
         begin
            Old_Sig := Await_Signal_Or_Timeout (New_Mask, To_Timespec (1, 0));
            --  This should raise POSIX_Error when the time expires,
            --  since there should no longer be an instance of the signal
            --  pending.
            Expect_Exception ("A022");
         exception
         when POSIX_Error =>
            Check_Error_Code (Resource_Temporarily_Unavailable, "A023");
         when E1 : others => Unexpected_Exception (E1, "A024");
         end;

         Unblock_Signals (New_Mask, Old_Mask);
      exception
      when E1 : others => Unexpected_Exception (E1, "A025");
      end Test_Signal;

   begin
      for I in Signals'Range loop
         Test_Signal (Signals (I));
      end loop;
   exception
   when E : others => Unexpected_Exception (E, "A026");
   end;

   ---------------------------------------------------------------------

   Test ("Await_Signal with info [3.3.16]");
   declare
      New_Mask  : Signal_Set;
      Sig_Info  : Signal_Info;

      procedure Test_Signal (Sig : Signal);

      procedure Test_Signal (Sig : Signal) is
      begin
         Comment ("Testing Await_Signal with info on signal " & Image (Sig));
         Delete_All_Signals  (New_Mask);
         Add_Signal (New_Mask, Sig);
         Block_Signals (New_Mask, Old_Mask);
         Send_Signal (POSIX_Process_Identification.Get_Process_ID, Sig);
         Sig_Info := Await_Signal (New_Mask);
         Assert (Get_Signal (Sig_Info) = Sig, "A027");
         --  This should return immediately since there is a signal pending
         Send_Signal (POSIX_Process_Identification.Get_Process_ID, Sig);
         begin
            Sig_Info := Await_Signal_Or_Timeout (New_Mask, To_Timespec (1, 0));
            --  This should return immediately since there is a signal pending
            Assert (Get_Signal (Sig_Info) = Sig, "A028");
         exception
         when E1 : others => Unexpected_Exception (E1, "A029");
         end;
         begin
            Sig_Info := Await_Signal_Or_Timeout (New_Mask, To_Timespec (1, 0));
            --  This should not return with signal catching. Instead it
            --  should return a POSIX_Error when the time expires.
            Expect_Exception ("A030");
         exception
         when POSIX_Error =>
            Check_Error_Code (Resource_Temporarily_Unavailable, "A031");
         when E1 : others => Unexpected_Exception (E1, "A032");
         end;
         Unblock_Signals (New_Mask, Old_Mask);
      exception
      when E1 : POSIX_Error =>
         Optional (Realtime_Signals_Option,
           Operation_Not_Implemented, E1, "A033");
      when E2 : others => Unexpected_Exception (E2, "A034");
      end Test_Signal;

   begin
      for I in Signals'Range loop
         Test_Signal (Signals (I));
      end loop;
   exception
   when E : others => Unexpected_Exception (E, "A035");
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
      begin

         accept Get_ID (ID : out Task_Id) do
            ID := Current_Task;
         end Get_ID;

         POSIX_IO.Read
           (POSIX_IO.Standard_Input, Buf_1, Last);
      exception
      when POSIX_Error =>
         Check_Error_Code (Interrupted_Operation, "A036");
      when E1 : others =>
         Unexpected_Exception (E1, "A037");
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

      for I in Signals'Range loop
         Delete_All_Signals  (New_Mask);
         Add_Signal (New_Mask, Signals (I));
         Block_Signals (New_Mask, Old_Mask);

         Try_Enable_Queueing (Signals (I), "A038");
         delay 0.1;
         Queue_Signal
           (POSIX_Process_Identification.Get_Process_ID, Signals (I), Sig_D);

         Sig_Info := Await_Signal (New_Mask);

         Assert (Get_Data (Sig_Info) = Sig_D, "A039: signal data = "
           & Signal_Scalar'Image (To_Integer (Get_Data (Sig_Info))));

         Try_Disable_Queueing (Signals (I));
         Unblock_Signals (New_Mask, New_Mask);
      end loop;

   exception
   when E1 : POSIX_Error =>
      Optional (Realtime_Signals_Option, Operation_Not_Implemented, E1,
             "A040");
   when E2 : others =>
      Unexpected_Exception (E2, "A041");
   end;

   ---------------------------------------------------------------------

   Test ("Signal notification with message queue [3.3.13.2]");

   declare
      New_Mask : Signal_Set;
      Sig_Info : Signal_Info;
      Sig_D    : Signal_Data := To_Signal_Data (20);
      Sig_E    : Signal_Event;
      MQ : Message_Queue_Descriptor;

      procedure Test_Signal (Sig : Signal);

      procedure Test_Signal (Sig : Signal) is

         Received_Signal : Boolean := False;
         pragma Volatile (Received_Signal);

         task Watchdog;

         task body Watchdog is
         begin
            delay 2.0;
            if not Received_Signal then
               Comment ("Watchdog time out");
               Send_Signal (Get_Process_ID, Sig);
            end if;
         end Watchdog;

      begin
         Comment ("Testing " & Image (Sig));
         Comment ("Checking that no residual message queue exists.");
         begin
            Unlink_Message_Queue (Valid_MQ_Name);
         exception
         when E1 : POSIX_Error =>
            if Get_Error_Code /= No_Such_File_Or_Directory then
               Optional (Realtime_Signals_Option,
                 Operation_Not_Implemented, E1, "A042");
            end if;
         when E2 : others => Unexpected_Exception (E2, "A043");
         end;

         Comment ("Creating message queue");
         MQ := Open_Or_Create
           (Valid_MQ_Name, Read_Write, Owner_Permission_Set);

         Set_Signal (Sig_E, Sig);

         Set_Notification (Sig_E, Signal_Notification);
         Set_Data (Sig_E, Sig_D);

         Comment ("Requesting notification");
         Request_Notify (MQ, Sig_E);

         Delete_All_Signals  (New_Mask);
         Add_Signal (New_Mask, Sig);
         Comment ("Blocking signals");
         Block_Signals (New_Mask, Old_Mask);

         Comment ("Checking blocked signals");
         Assert (Is_Member (Blocked_Signals, Sig), "A044");

         Try_Enable_Queueing (Sig, "A045");

         Comment ("Sending message");
         Send (MQ, To_Stream_Element_Array ("Hello....."), 1);
         Comment ("Checking pending signals");
         Assert (Is_Member (Pending_Signals, Sig), "A046");

         Comment ("Delaying");
         delay 0.1;

         Comment ("Awaiting signal");
         Sig_Info := Await_Signal (New_Mask);
         Received_Signal := True;

         Assert (Get_Signal (Sig_Info) = Sig, "A047: signal = "
           & Signal'Image (Get_Signal (Sig_Info)));

         Assert (Get_Data (Sig_Info) = Sig_D, "A048: signal data = "
           & Signal_Scalar'Image (To_Integer (Get_Data (Sig_Info))));

         Try_Disable_Queueing (Sig);

         Comment ("Unblocking signals");
         Unblock_Signals (New_Mask, New_Mask);

         Comment ("Closing MQ");
         Close (MQ);

         Comment ("Unlinking MQ");
         Unlink_Message_Queue (Valid_MQ_Name);

      exception
      when E1 : POSIX_Error =>
         Optional (Realtime_Signals_Option, Message_Queues_Option,
           Operation_Not_Implemented, E1, "A049");
      when E2 : others =>
         Unexpected_Exception (E2, "A050");
      end Test_Signal;

   begin
      for I in Signals'Range loop
         if not Is_Reserved_Signal (Signals (I)) then
            Test_Signal (Signals (I));
         end if;
      end loop;
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

      Received_Signals : array (Signals'Range) of Boolean :=
        (others => False);
      pragma Volatile_Components (Received_Signals);

      task Watchdog;

      task body Watchdog is
      begin
         delay 2.0;
         if not Received_Signals (2) then
            Send_Signal (Get_Process_ID, Signals (2));
         end if;
         delay 2.0;
         if not Received_Signals (3) then
            Send_Signal (Get_Process_ID, Signals (3));
         end if;
      end Watchdog;

   begin
      Delete_All_Signals  (New_Mask);
      Add_Signal (New_Mask, Signals (2));
      Add_Signal (New_Mask, Signals (3));
      Comment ("Blocking signals");
      Block_Signals (New_Mask, Old_Mask);

      Try_Enable_Queueing (Signals (3), "A051");

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
      Set_Signal (Sig_E, Signals (2));
      Set_Data (Sig_E, Sig_D);
      Set_Notification (Sig_E, Signal_Notification);

      Comment ("Setting event");
      Set_Event (AD_1, Sig_E);

      Comment ("Setting signal");
      Set_Signal (Sig_E, Signals (3));
      Set_Data (Sig_E, Sig_D);
      Set_Notification (Sig_E, Signal_Notification);

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

      Delete_Signal (New_Mask, Signals (3));
      Assert (not Is_Member (New_Mask, Signals (3)), "A052");
      Assert (Is_Member (New_Mask, Signals (2)), "A053");

      Comment ("Awaiting Signal (2)");
      Sig_Info := Await_Signal (New_Mask);
      Received_Signals (2) := True;

      Assert (Get_Signal (Sig_Info) = Signals (2), "A054: signal = "
        & Signal'Image (Get_Signal (Sig_Info)));

      Assert (Get_Data (Sig_Info) = Sig_D, "A055: signal data = "
        & Signal_Scalar'Image (To_Integer (Get_Data (Sig_Info))));
      Assert (not Is_Member (Pending_Signals, Signals (2)), "A056");
      Assert (Is_Member (Pending_Signals, Signals (3)), "A057");

      Delete_Signal (New_Mask, Signals (2));
      Add_Signal (New_Mask, Signals (3));
      Assert (not Is_Member (New_Mask, Signals (2)), "A058");
      Assert (Is_Member (New_Mask, Signals (3)), "A059");

      Comment ("Awaiting Signal (3)");
      Sig_Info := Await_Signal (New_Mask);
      Received_Signals (3) := True;

      Assert (Get_Signal (Sig_Info) = Signals (3), "A060: signal = "
        & Signal'Image (Get_Signal (Sig_Info)));

      Assert (Get_Data (Sig_Info) = Sig_D, "A061: signal data = "
        & Signal_Scalar'Image (To_Integer (Get_Data (Sig_Info))));
      Assert (not Is_Member (Pending_Signals, Signals (2)), "A062");

      Count := 1;
      while Is_Member (Pending_Signals, Signals (3))
        and Count < 100 loop
         Count := Count + 1;
      end loop;
      Assert (Count = 1, "A063: Count =" & Integer'Image (Count));

      Comment ("Ignoring signals");
      Ignore_Signal (Signals (2));
      Ignore_Signal (Signals (3));

      Comment ("Unignoring signals");
      Unignore_Signal (Signals (2));
      Unignore_Signal (Signals (3));

      Assert (not Is_Member (Pending_Signals, Signals (3)), "A064");
      Assert (not Is_Member (Pending_Signals, Signals (2)), "A065");

      Try_Disable_Queueing (Signals (3));

      Comment ("Unblocking signals");
      Add_Signal (New_Mask, Signals (2));
      Unblock_Signals (New_Mask, New_Mask);

      Comment ("Closing aio test file");
      Close (FD);

      Comment ("Unlinking aio test file");
      Unlink (Valid_AIO_File_Name);

   exception
   when E1 : POSIX_Error =>
      Optional (Realtime_Signals_Option, Asynchronous_IO_Option,
        Operation_Not_Implemented, E1, "A066");
   when E2 : others =>
      Unexpected_Exception (E2, "A067");
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
      Set_Signal (Sig_E, Signals (3));
      Set_Data (Sig_E, Sig_D);
      Set_Notification (Sig_E, Signal_Notification);

      Tid := Create_Timer (Clock_Realtime, Sig_E);

      Comment ("Blocking signal");
      Delete_All_Signals  (New_Mask);
      Add_Signal (New_Mask, Signals (3));
      Block_Signals (New_Mask, Old_Mask);

      Try_Enable_Queueing (Signals (3), "A068");

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
      Assert (not Is_Member (Pending_Signals, Signals (3)), "A069");

      Assert (Get_Signal (Sig_Info) = Signals (3), "A070: signal = "
        & Signal'Image (Get_Signal (Sig_Info)));

      Assert (Get_Data (Sig_Info) = Sig_D, "A071: signal data = "
        & Signal_Scalar'Image (To_Integer (Get_Data (Sig_Info))));

      Count := 1;
      while Is_Member (Pending_Signals, Signals (3))
        and Count < 100 loop
         Count := Count + 1;
      end loop;
      Assert (Count = 1, "A072: Count =" & Integer'Image (Count));

      Try_Disable_Queueing (Signals (3));

      Comment ("Ignoring signals");
      Ignore_Signal (Signals (2));
      Ignore_Signal (Signals (3));

      Comment ("Unignoring signals");
      Unignore_Signal (Signals (2));
      Unignore_Signal (Signals (3));

      Assert (not Is_Member (Pending_Signals, Signals (3)), "A073");

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
        Operation_Not_Implemented, E1, "A074");
   when E2 : others =>
      Unexpected_Exception (E2, "A075");
   end;

   Done;
exception
when E : others => Fatal_Exception (E, "A076");
end p030301;

------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                             P 0 3 0 3 0 4                                --
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

--  This a test of the POSIX_Signals package, and other features of
--  section 3.3 of POSIX.5b.  It does not test functionality that relies
--  on support for multiple processes.

--  This test contains checks originally contained in p030300, which involve
--  a task awaiting a signal that is sent by another task.  The test has
--  been broken out, to shorten the running time of test p030300, and
--  make isolating failures easier.

with Ada_Task_Identification,
     p030300a,
     POSIX,
     POSIX_IO,
     POSIX_Process_Identification,
     POSIX_Report,
     POSIX_Signals,
     System,
     System.Interrupts,
     System.Storage_Elements,
     Test_Parameters;

procedure p030304 is
   use  Ada_Task_Identification,
        p030300a,
        POSIX,
        POSIX_Process_Identification,
        POSIX_Report,
        POSIX_Signals,
        Test_Parameters,
        System;

   Old_Mask : Signal_Set;

begin

   Header ("p030304");

   ---------------------------------------------------------------------
   --  It is implemenentation-defined whether the signal mask is
   --  per-task or per-process. [3.3.1]
   --  The tests for operations related to signal blocking
   --  are intended to have the same outcome, regardless of
   --  whether the mask is per-task or per-process.
   --  This is achieved by never having a signal unmasked by
   --  more than one task at the same time.

   Block_Signals (All_Signal_Mask, Old_Mask);

   -----------------------------------------------------------------------
   --  For all other tasks, the initial signal mask shall include all the
   --  signals that are not reserved signals and are not bound to entries
   --  of the task.
   --  If the signal mask is per process, this requirement is in conflict
   --  with the requirement that the initial signal mask of the environment
   --  task is that specified for the process, so this test is conditional
   --  on not Signal_Mask_Is_Process_Wide.

   Test ("Initial signal mask of a task [3.3.1]");
   declare
      task T;
      task body T is
         Set : Signal_Set := Blocked_Signals;
      begin
         for Sig in Signal loop
            if not Is_Reserved (Sig)
              and then not Is_Member (Set, Sig)
              and then not Signal_Mask_Is_Process_Wide then
               Add_Signal (Not_Initially_Masked, Sig);
               Fail ("A001: " & Image (Sig) & " not initially blocked");
            end if;
         end loop;
      end T;
   begin
      null;
   exception
   when E1 : others => Unexpected_Exception (E1, "A002");
   end;

   Test ("Block and Unblock Signals [3.3.8]");
   declare
      N : constant Integer := 3;
      New_Mask : Signal_Set;

      procedure Test_Signal (Sig : Signal);
      procedure Test_Signal (Sig : Signal) is
      begin
         --  New_Mask is initially empty.
         Set_Blocked_Signals (New_Mask, Old_Mask);
         declare
            task T;
            task body T is
            begin

               ---------------------------------------------------------------
               --  POSIX_Error may be raised with Operation_Not_Permitted when
               --  an attempt is made to unblock a signal that was already
               --  unblocked by another task in the same process. [3.3.8]

               Set_Blocked_Signals (New_Mask, New_Mask);
            exception
            when POSIX_Error =>
               Check_Error_Code (Operation_Not_Permitted, "A003");
            when E1 : others => Unexpected_Exception (E1, "A004");
            end T;
         begin
            null;
         exception
         when E : others => Unexpected_Exception (E, "A005");
         end;
         Set_Blocked_Signals (Old_Mask, New_Mask);
      end Test_Signal;

   begin

      for Sig in Signal loop
         Test_Signal (Sig);
      end loop;

   exception
      when E1 : others => Unexpected_Exception (E1, "A006");
   end;

   ------------------------------------------------------------------------

   Test ("Ignore Signals [3.3.9]");
   for Sig in Signal loop
      if Default_Action (Sig) /= Termination
        or else Action_Cannot_Be_Set (Sig)
        or else Is_Member (Not_Initially_Masked, Sig)
        or else Is_Reserved (Sig) then
         Add_Signal (Do_Not_Test, Sig);
      end if;
   end loop;

   declare

      N : constant Integer := 3;
      New_Mask : Signal_Set;

      procedure Test_Signal (Sig : Signal);
      procedure Test_Signal (Sig : Signal) is

         Count : Integer := 0;
         pragma Volatile (Count);

         task T is
            entry Sync;
            entry Expect_Signal;
         end T;

         task body T is
            The_Sig : Signal;
            New_Mask,
            Old_Mask : Signal_Set;
            Timeout : constant Timespec := To_Timespec (5*DU);
         begin
            if not Is_Member (Do_Not_Test, Sig) then
               Add_Signal (New_Mask, Sig);
               Block_Signals (New_Mask, Old_Mask);
               loop
                  select
                     accept Sync;
                  or accept Expect_Signal;
                     begin
                        The_Sig :=
                          Await_Signal_Or_Timeout (New_Mask, Timeout);
                        Comment ("received " & Image (Sig));
                        Assert (The_Sig = Sig, "A007");
                        Count := Count + 1;
                     exception
                     when E : POSIX_Error =>
                        Comment ("TIMED OUT waiting for " & Image (Sig));
                        Check_Error_Code (EAGAIN, "A008: " & Image (Sig));
                     when E : others =>
                        Unexpected_Exception (E, "A009");
                     end;
                  or terminate;
                  end select;
               end loop;
            end if;
         exception
         when E : others =>
            Unexpected_Exception (E, "A010: " & Image (Sig));
         end T;

      begin
         Comment ("testing " & Image (Sig));

         Block_Signals (All_Signal_Mask, Old_Mask);

         ---------------------------------------------------------------
         --  When signal is not ignored, signals sent to the process
         --  can be caught using Await_Signal.

         Assert (not Is_Ignored (Sig), "A011");
         if Action_Cannot_Be_Set (Sig) then
            Expect_Exception ("A012: " & Image (Sig));
            Set_Error_Code (Invalid_Argument);
            raise POSIX_Error;
         end if;

         if not Is_Member (Do_Not_Test, Sig) then
            Count := 0;
            for I in 1 .. N loop
               T.Expect_Signal;
               --  Give T a chance to execute Await_Signal.
               delay DU;
               Comment ("sending " & Image (Sig));
               Send_Signal (Get_Process_ID, Sig);
            end loop;
            --  Give T a chance to increment Count.
            T.Sync;
            Assert (Count = N, "A013: only" & Integer'Image (Count)
              & " signals received");
         else
            Comment ("not sending " & Image (Sig));
         end if;

         --------------------------------------------------------------
         --  N signals were sent and N were received, so there should
         --  be no more pending occurrences of Sig at this point.
         --  If this check fails, it may mean that Send_Signal
         --  delivers the signal to ALL the threads in a process,
         --  rather than just one.

         declare
            Set : Signal_Set;
         begin
            Set := Pending_Signals;
            for Sig in Signal loop
               Assert (Sig = SIGNULL or not Is_Member (Set, Sig),
                 "A014: " & Image (Sig) & " is pending");
            end loop;
         end;

         ---------------------------------------------------------------
         --  If the action associated with a blocked signal is to ignore
         --  the signal, and if that signal is generated for the process
         --  or task it is unspecified whether the signal is discarded
         --  immediately upon generation or remains pending. [3.3.1]
         --  The effect of changing the signal action for a signal that
         --  is currently awaited by a task is unspecified. [3.3.1]
         --  The effect of a call to Await_Signal on the signal
         --  actions for the signals in Set is unspecified. [3.3.15]
         --  Therefore, task T waits until the signal has been sent,
         --  has been unmasked, and has been masked again, before trying
         --  to await it.

         Comment ("should now be ignoring signals");

         Ignore_Signal (Sig);

         Assert (Is_Ignored (Sig), "A015");
         if not Is_Member (Do_Not_Test, Sig) then
            Count := 0;
            Comment ("sending " & Image (Sig));
            Send_Signal (Get_Process_ID, Sig);
            Add_Signal (New_Mask, Sig);
            Unblock_Signals (New_Mask, Old_Mask);
            Block_Signals (New_Mask, Old_Mask);
            T.Expect_Signal;
            --  T should time out, without receiving the signal.
            T.Sync;
            Assert (Count = 0, "A016: " & Integer'Image (Count)
              & " signals received");
         else
            Comment ("not sending " & Image (Sig));
         end if;

         ---------------------------------------------------------------
         --  When signal is unignored, signals sent to the process
         --  cause the default action (again).  It should again be
         --  possible to use Await_Signal to catch the signal, if the
         --  default action allows the signal to be caught.

         Comment ("should stop ignoring signals");

         Unignore_Signal (Sig);
         Assert (not Is_Ignored (Sig), "A017");
         if not Is_Member (Do_Not_Test, Sig) then
            Count := 0;
            for I in 1 .. N loop
               T.Expect_Signal;
               --  give T a chance to execute Await_Signal
               delay DU;
               Comment ("sending " & Image (Sig));
               Send_Signal (Get_Process_ID, Sig);
            end loop;
            T.Sync;
            Assert (Count = N, "A018: only" & Integer'Image (Count)
              & " signals received");
         else
            Comment ("not sending " & Image (Sig));
         end if;

         --------------------------------------------------------------------
         --  Now make sure any pending occurrences of the signal will be
         --  cleared out safely.

         declare
            Set, Old_Set : Signal_Set;
         begin
            Ignore_Signal (Sig);
            Add_Signal (Set, Sig);
            Unblock_Signals (Set, Old_Set);
            Block_Signals (Old_Set, Set);
            Unignore_Signal (Sig);
         exception
         when E : others => Unexpected_Exception (E, "A019");
         end;

      exception
      when E1 : POSIX_Error =>
         Assert (Get_Error_Code = Invalid_Argument and
           Action_Cannot_Be_Set (Sig), "A020: " & Image (Sig)
             & " " & Image (Get_Error_Code));
      when E2 : others => Unexpected_Exception (E2, "A021");
      end Test_Signal;

   begin

      for Sig in Signal loop
         if Default_Action (Sig) /= Termination
           or else Action_Cannot_Be_Set (Sig)
           or else Is_Member (Not_Initially_Masked, Sig)
           or else Is_Reserved (Sig) then
            Add_Signal (Do_Not_Test, Sig);
         end if;
      end loop;
      for Sig in Signal loop
         begin
            Test_Signal (Sig);
         exception
         when E : others => Unexpected_Exception (E, "A029");
         end;
      end loop;
   end;

   ---------------------------------------------------------------------

   Test ("Wait for Signal [3.3.15]");

   --  This interface is mostly covered by other tests.
   --  Here, we repeat some of the checks above, but with the
   --  signal being sent to the environment task by another task.

   declare

      procedure Test_Signal (Sig : Signal);
      procedure Test_Signal (Sig : Signal) is
         The_Sig : Signal;
         Mask : Signal_Set;
         Timeout : constant Timespec := To_Timespec (5*DU);

         task T;
         task body T is
         begin
            Comment ("sending signal " & Image (Sig));
            Send_Signal (Get_Process_ID, Sig);
         exception
         when E : others =>
            Unexpected_Exception (E, "A032: " & Image (Sig));
         end T;

      begin
         Comment ("testing " & Image (Sig));
         Add_Signal (Mask, Sig);
         Comment ("awaiting " & Image (Sig));
         The_Sig := Await_Signal_Or_Timeout (Mask, Timeout);
         Comment ("received " & Image (Sig));
         Assert (The_Sig = Sig, "A033: " & Image (The_Sig));
      exception
      when E : others => Unexpected_Exception (E, "A034: " & Image (Sig));
      end Test_Signal;

   begin

      for Sig in Signal loop
         if not Is_Reserved (Sig) then
            Test_Signal (Sig);
         end if;
      end loop;

   exception
      when E1 : others => Unexpected_Exception (E1, "A035");
   end;

   ---------------------------------------------------------------------

   Test ("Wait for Signal with Information [3.3.16]");

   --  This interface is mostly covered by other tests.
   --  Here, we repeat some of the checks above, but with the
   --  signal being sent to the environment task by another task.

   declare

      procedure Test_Signal (Sig : Signal);
      procedure Test_Signal (Sig : Signal) is

         Mask : Signal_Set;
         Timeout : constant Timespec := To_Timespec (5*DU);
         Info : Signal_Info;
         I : Integer := 999;

         task T;
         task body T is
         begin
            Comment ("sending signal " & Image (Sig));
            Queue_Signal (Get_Process_ID, Sig, +I);
         exception
         when E1 : POSIX_Error =>
            Optional (Realtime_Signals_Option,
              Operation_Not_Implemented, E1, "A036");
         when E2 : others => Unexpected_Exception (E2, "A037");
         end T;

      begin
         Comment ("testing " & Image (Sig));
         Enable_Queueing (Sig);
         Add_Signal (Mask, Sig);
         Info := Await_Signal_Or_Timeout (Mask, Timeout);
         Comment ("received " & Image (Sig));
         Assert (Get_Signal (Info) = Sig, "A038");
         Assert (Get_Source (Info) = From_Queue_Signal, "A039");
         Assert (Get_Data (Info) = +I, "A040");
         Disable_Queueing (Sig);
      exception
      when E1 : POSIX_Error =>
         Optional (Realtime_Signals_Option,
           Operation_Not_Implemented, E1, "A041");
      when E2 : others => Unexpected_Exception (E2, "A042");
      end Test_Signal;

   begin

      for Sig in Signal loop
         if not Is_Reserved (Sig) then
            Test_Signal (Sig);
         end if;
      end loop;

   exception
   when E : others => Unexpected_Exception (E, "A043");
   end;

   --  Queue_Signal is partly covered above.
   --  More extensive testing of Queue_Signal requires the use of
   --  multiple processes, and so is in a separate program.

   ---------------------------------------------------------------------
   --  If the task is executing an interruptible operation
   --  the operation is interrupted by Interrupt_Task.
   --  In this case, if the task is not interrupted the read operation
   --  will hang.

   Test ("Interrupt a Task [3.3.20]");
   declare
      task T;
      task body T is
         Timeout : constant Timespec := To_Timespec (3*DU);
         Set : Signal_Set;
         Buffer : POSIX_String (1 .. 3);
         Last : IO_Count;
      begin
         --  This assumes that the standard input file does not
         --  have any input ready.
         Comment ("making blocking system call (will hang if fails)");
         POSIX_IO.Read (POSIX_IO.Standard_Input, Buffer, Last);
         Comment ("system call aborted OK");
      exception
      when E1 : POSIX_Error =>
         Check_Error_Code (Interrupted_Operation, "A050");
      when E2 : others => Unexpected_Exception (E2, "A051");
      end T;
   begin
      delay 3 * DU;
      Comment ("interrupting task");
      Interrupt_Task (T'Identity);
   exception
   when E : others => Unexpected_Exception (E, "A052");
   end;

   ---------------------------------------------------------------------

   Done;
exception
   when E : others => Fatal_Exception (E, "A053");
end p030304;

------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                             P 0 3 0 3 0 5                                --
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
--  a task accepting a signal that is sent by another task.  The test has
--  been broken out, to shorten the running time of test p030300, and
--  make isolating failures easier.

--  Consider splitting this test further, to separate checks that
--  require signal entries from those that do not.

with p030300a,
     POSIX,
     POSIX_IO,
     POSIX_Process_Identification,
     POSIX_Report,
     POSIX_Signals,
     System,
     System.Storage_Elements,
     Test_Parameters;

procedure p030305 is
   use  p030300a,
        POSIX,
        POSIX_Process_Identification,
        POSIX_Report,
        POSIX_Signals,
        Test_Parameters,
        System;

   Old_Mask : Signal_Set;

begin

   Header ("p030305");

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

      procedure Test_Signal (Sig : Signal);
      procedure Test_Signal (Sig : Signal) is

         Sig_Ref : constant System.Address := Signal_Reference (Sig);
         Ret : Integer;

         task T is
            entry Reset_Count;
            entry Current_Count (X : out Integer);
            entry Signal;
            for Signal use at Sig_Ref;
         end T;

         task body T is
            Count : Integer := 0;
         begin
            if not Is_Member (Do_Not_Test, Sig) then
               loop
                  select
                     accept Signal do
                        Count := Count + 1;
                        Comment ("received " & Image (Sig));
                     end Signal;
                  or
                     accept Reset_Count do
                        Count := 0;
                     end Reset_Count;
                  or
                     accept Current_Count (X : out integer) do
                        X := Count;
                     end Current_Count;
                  or terminate;
                  end select;
               end loop;
            end if;
         exception
         when E : others => Unexpected_Exception (E, "A007");
         end T;

      begin
         Comment ("testing " & Image (Sig) & " with entry");

         ----------------------------------------------------------------
         --  Ensure all blockable signals are blocked
         --  in the environment task.

         Block_Signals (All_Signal_Mask, Old_Mask);

         ---------------------------------------------------------------
         --  When signal is not ignored, signals sent to the process
         --  cause the handler to execute.

         Assert (not Is_Ignored (Sig), "A008");
         if not Is_Member (Do_Not_Test, Sig) then
            T.Reset_Count;
            for I in 1 .. N loop
               --  give T a chance to accept the signal
               delay DU;
               Comment ("sending " & Image (Sig));
               Send_Signal (Get_Process_ID, Sig);
            end loop;
            delay DU;
            T.Current_Count (Ret);
            Assert (Ret = N, "A009: " & Image (Sig) & " received"
              & Integer'Image (Ret) & " times");
         else
            Comment ("not sending " & Image (Sig));
         end if;

         ---------------------------------------------------------------
         --  For the Ignore_Signal operation if the signal is bound to
         --  a task entry, the effect shall be to discard any pending or
         --  subsequent deliveries of the that signal.  The binding to
         --  the entry MAY remain in force. [3.3.17.2]
         --  Thus, signals sent to the process do not cause the handler
         --  to execute.

         Ignore_Signal (Sig);
         Assert (Is_Ignored (Sig), "A010");
         if not Is_Member (Do_Not_Test, Sig) then
            T.Reset_Count;
            for I in 1 .. N loop
               --  give T a chance to accept the signal
               delay DU;
               Comment ("sending " & Image (Sig));
               Send_Signal (Get_Process_ID, Sig);
            end loop;
            delay DU;
            T.Current_Count (Ret);
            Assert (Ret = 0, "A011: Ret =" & Integer'Image (Ret));
         else
            Comment ("not sending " & Image (Sig));
         end if;

         ---------------------------------------------------------------
         --  When signal is unignored, the default action is restored.
         --  The effect of this on entries that are attached is not
         --  specified, since POSIX.5b says only that "the binding to the
         --  entry MAY remain in force". [3.3.17.2]
         --  If it is not in force, we expect the default action, which
         --  may be to terminate the process.  Therefore, this check
         --  is deferred to a separate test.

         Unignore_Signal (Sig);
         Assert (not Is_Ignored (Sig), "A012");

         --------------------------------------------------------------------
         --  Now make sure any pending occurrences of the signal will be
         --  cleared out safely when we next unblock signals.

         Ignore_Signal (Sig);

      exception
      when E1 : others => Unexpected_Exception (E1, "A013");
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
         when E1 : POSIX_Error =>
            if Is_Supported (Signal_Entries_Option)
              and then Get_Error_Code /= Invalid_Argument
            then
               Unexpected_Exception (E1, "A014");
            end if;
         when E2 : others => Unexpected_Exception (E2, "A015");
         end;
      end loop;
   end;

   ---------------------------------------------------------------------

   Test ("Signal Entries [3.3.17]");

   --  This is also a test of several other operations, including
   --  signal sending.

   declare
      procedure Test_Signal (Sig : Signal);
      procedure Test_Signal (Sig : Signal) is
      begin
         declare
            Sig_Ref : constant System.Address := Signal_Reference (Sig);
            task T is
               entry E1;
               entry E2;
               for E2 use at Sig_Ref;
            end T;
            task body T is
            begin
               Block_Signals (All_Signal_Mask, Old_Mask);
               --  At this point, Sig is masked, but that does not
               --  prevent delivery of the signal to a task entry.
               --  Try to arrange for signal to arrive after the accept.
               select
                  accept E2;
               or delay DU;
                  Fail ("A016: " & Image (Sig));
               end select;
               --  Then arrange for signal to arrive before the accept.
               accept E1;
               delay 2*DU;
               select
                  accept E2;
               or delay DU;
                  Fail ("A017: " & Image (Sig));
               end select;
            exception when E : others => Unexpected_Exception (E, "A018");
            end T;
         begin
            Block_Signals (All_Signal_Mask, Old_Mask);
            Comment ("sending " & Image (Sig));
            Send_Signal (Get_Process_ID, Sig);
            T.E1;
            Comment ("sending " & Image (Sig));
            Send_Signal (Get_Process_ID, Sig);
         end;
      exception
      when E1 : POSIX_Error =>
         Optional (Option => Signal_Entries_Option,
           Expected_If_Not_Supported => Invalid_Argument,
           Expected_If_Supported => Invalid_Argument,
           E => E1,
           Message => "A019");
      when E2 : others => Unexpected_Exception (E2, "A020");
      end Test_Signal;
   begin
      for Sig in Signal loop
         Test_Signal (Sig);
      end loop;
   exception
   when E : others => Unexpected_Exception (E, "A021");
   end;

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
      when POSIX_Error =>
         Check_Error_Code (Interrupted_Operation, "A022");
      when E2 : others => Unexpected_Exception (E2, "A023");
      end T;
   begin
      delay 3*DU;
      Comment ("interrupting task");
      Interrupt_Task (T'Identity);
   exception
   when E : others => Unexpected_Exception (E, "A024");
   end;

   ---------------------------------------------------------------------

   Done;
exception
   when E : others => Fatal_Exception (E, "A025");
end p030305;

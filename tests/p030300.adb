------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                             P 0 3 0 3 0 0                                --
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
--  on support for multiple processes, or multiple tasks.

--  Run this test in a fashion that initially all signals are unmasked
--  in the process.

with Ada_Task_Identification,
     p030300a,
     POSIX,
     POSIX_Asynchronous_IO,
     POSIX_Files,
     POSIX_IO,
     POSIX_Limits,
     POSIX_Message_Queues,
     POSIX_Permissions,
     POSIX_Process_Identification,
     POSIX_Report,
     POSIX_Signals,
     POSIX_Timers,
     System,
     System.Interrupts,
     System.Storage_Elements,
     Test_Parameters;

procedure p030300 is
   use  Ada_Task_Identification,
        p030300a,
        POSIX,
        POSIX_Asynchronous_IO,
        POSIX_Files,
        POSIX_IO,
        POSIX_Limits,
        POSIX_Message_Queues,
        POSIX_Permissions,
        POSIX_Process_Identification,
        POSIX_Report,
        POSIX_Signals,
        POSIX_Timers,
        Test_Parameters,
        System;

   Old_Mask : Signal_Set;

begin

   Header ("p030300");

   ----------------------------------------------------------------------
   --  The implementation is only allowed to reserve additional signals
   --  that are not defined (named) in the standard.

   declare
      Reserved_Signals : Signal_Set;
   begin
      Add_Signal (Reserved_Signals, SIGABRT);
      Add_Signal (Reserved_Signals, SIGALRM);
      Add_Signal (Reserved_Signals, SIGFPE);
      Add_Signal (Reserved_Signals, SIGILL);
      Add_Signal (Reserved_Signals, SIGSEGV);
      Add_Signal (Reserved_Signals, SIGBUS);
      --  Notwithstanding the fact that the standard does not list
      --  SIGABRT and SIGSTOP as reserved signals, they cannot be
      --  used with sigwait(), because they cannot be masked.
      Add_Signal (Reserved_Signals, SIGKILL);
      Add_Signal (Reserved_Signals, SIGSTOP);
      for I in Defined_Signals'Range loop
         Assert (not Is_Reserved_Signal (Defined_Signals (I))
           or else Is_Member (Reserved_Signals, Defined_Signals (I)),
           "A001: p030300a " & Image (Defined_Signals (I)) & " is reserved");
      end loop;
   end;

   -------------------------------------------------------------------------
   --  "For the environment task, the initial signal mask is that specified
   --  for the process in 3.1.2 and 3.2.1."
   --  However, that only covers processes created via the Start_Process and
   --  Fork operations.  The implementation is allowed to choose the
   --  masking state of the reserved signals.   The following check assumes
   --  that the environment task starts out with all non-reserved signals
   --  unmasked.  Since that assumption goes beyond the standard, this
   --  check only produces Comments as output.

   Test ("Environment task blocked signals [3.3.1]");
   declare
      Set : Signal_Set;
   begin
      Set := Blocked_Signals;
      for Sig in Signal loop
         if not Is_Reserved (Sig)
           and then not Is_Member (Set, Sig) then
            Comment ("WARNING: " & Image (Sig) & " not blocked in env. task");
         end if;
      end loop;
   exception
   when E1 : others => Unexpected_Exception (E1, "A002");
   end;

   ---------------------------------------------------------------------
   --  The values of type Signal shall represent valid signals in the
   --  implementation.
   --  It should be possible to add any valid signal to a Signal_Set.

   Test ("Signal Type [3.3.2]");

   declare
      Mask : Signal_Set;
   begin
      for Sig in Signal loop
         Delete_All_Signals (Mask);
         begin
            Add_Signal (Mask, Sig);
         exception
         when E1 : POSIX_Error =>
            if Get_Error_Code = Invalid_Argument then
               Fail ("A003: " & Image (Sig) & " is invalid");
            else
               Unexpected_Exception (E1, "A004");
            end if;
            when E2 : others => Unexpected_Exception (E2, "A005");
         end;
      end loop;
   exception
   when E1 : others => Unexpected_Exception (E1, "A006");
   end;

   ---------------------------------------------------------------------
   --  Image and Value functions are defined for all valid signals,
   --  and have inverse effects.  Image and Value are consistent with
   --  the standard signal names, for the signals named in the standard.

   Test ("Standard Signals [3.3.3]");
   begin

      --------------------------------------------------------------
      --  If Sig is the value of one of the signals defined by this
      --  standard, the value returned by Image shall be the identifier
      --  ... in uppercase.

      Assert
        (Image (Signal_Null) = "SIGNAL_NULL", "A007");
      Assert
        (Image (SIGNULL) = Image (Signal_Null), "A008");

      Assert
        (Image (Signal_Abort) = "SIGNAL_ABORT", "A009");
      Assert
        (Image (SIGABRT) = Image (Signal_Abort), "A010");

      Assert
        (Image (Signal_Alarm) = "SIGNAL_ALARM", "A011");
      Assert
        (Image (SIGALRM) = Image (Signal_Alarm), "A012");
      Assert
        (Image (Signal_Floating_Point_Error) =
         "SIGNAL_FLOATING_POINT_ERROR", "A013");
      Assert
        (Image (SIGFPE) = Image (Signal_Floating_Point_Error), "A014");
      Assert
        (Image (Signal_Hangup) = "SIGNAL_HANGUP", "A015");
      Assert
        (Image (SIGHUP) = Image (Signal_Hangup), "A016");
      Assert
        (Image (Signal_Illegal_Instruction) =
         "SIGNAL_ILLEGAL_INSTRUCTION", "A017");
      Assert
        (Image (SIGILL) = Image (Signal_Illegal_Instruction), "A018");
      Assert
        (Image (Signal_Interrupt) = "SIGNAL_INTERRUPT", "A019");
      Assert
        (Image (SIGINT) = Image (Signal_Interrupt), "A020");
      Assert
        (Image (Signal_Kill) = "SIGNAL_KILL", "A021");
      Assert
        (Image (SIGKILL) = Image (Signal_Kill), "A022");
      Assert
        (Image (Signal_Pipe_Write) = "SIGNAL_PIPE_WRITE", "A023");
      Assert
        (Image (SIGPIPE) = Image (Signal_Pipe_Write), "A024");
      Assert
        (Image (Signal_Quit) = "SIGNAL_QUIT", "A025");
      Assert
        (Image (SIGQUIT) = Image (Signal_Quit), "A026");
      Assert
        (Image (Signal_Segmentation_Violation) =
         "SIGNAL_SEGMENTATION_VIOLATION", "A027");
      Assert
        (Image (SIGSEGV) = Image (Signal_Segmentation_Violation), "A028");
      Assert
        (Image (Signal_Terminate) = "SIGNAL_TERMINATE", "A029");
      Assert
        (Image (SIGTERM) = Image (Signal_Terminate), "A030");
      Assert
        (Image (Signal_User_1) = "SIGNAL_USER_1", "A031");
      Assert
        (Image (SIGUSR1) = Image (Signal_User_1), "A032");
      Assert
        (Image (Signal_User_2) = "SIGNAL_USER_2", "A033");
      Assert
        (Image (SIGUSR2) = Image (Signal_User_2), "A034");

      if Is_Supported (Memory_Protection_Option) then
         Assert
           (Image (Signal_Bus_Error) = "SIGNAL_BUS_ERROR", "A035");
         Assert
           (Image (SIGBUS) = Image (Signal_Bus_Error), "A036");
      end if;

      if Is_Supported (Job_Control_Option) then
         Assert
           (Image (Signal_Child) = "SIGNAL_CHILD", "A037");
         Assert
           (Image (SIGCHLD) = Image (Signal_Child), "A038");
         Assert
           (Image (Signal_Continue) = "SIGNAL_CONTINUE", "A039");
         Assert
           (Image (SIGCONT) = Image (Signal_Continue), "A040");
         Assert
           (Image (Signal_Stop) = "SIGNAL_STOP", "A041");
         Assert
           (Image (SIGSTOP) = Image (Signal_Stop), "A042");
         Assert
           (Image (Signal_Terminal_Stop) = "SIGNAL_TERMINAL_STOP", "A043");
         Assert
           (Image (SIGTSTP) = Image (Signal_Terminal_Stop), "A044");
         Assert
           (Image (Signal_Terminal_Input) = "SIGNAL_TERMINAL_INPUT", "A045");
         Assert
           (Image (SIGTTIN) = Image (Signal_Terminal_Input), "A046");
         Assert
           (Image (Signal_Terminal_Output) = "SIGNAL_TERMINAL_OUTPUT", "A047");
         Assert
           (Image (SIGTTOU) = Image (Signal_Terminal_Output), "A048");
      end if;

   exception
      --  No exceptions shall be raised by Image.
      when E1 : others => Unexpected_Exception (E1, "A049");
   end;

   begin

      ------------------------------------------------------------
      --  If Str matches the short name or the long name of an
      --  identifier for a signal supported by the implementation ...
      --  Value shall return the corresponding signal value.

      Assert
        (Value ("Signal_Null") = Signal_Null, "A050");
      Assert
        (Value ("SIGNULL") = Signal_Null, "A051");
      Assert
        (Value ("Signal_Abort") = Signal_Abort, "A052");
      Assert
        (Value ("siGnal_aBort") = Signal_Abort, "A053");
      Assert
        (Value ("SIGABRT") = Signal_Abort, "A054");
      Assert
        (Value ("siGabrt") = Signal_Abort, "A055");
      Assert
        (Value (" Signal_Abort ") = Signal_Abort, "A056");
      Assert
        (Value ("Signal_Alarm") = Signal_Alarm, "A057");
      Assert
        (Value ("SIGALRM") = Signal_Alarm, "A058");
      Assert
        (Value ("Signal_Floating_Point_Error") =
         Signal_Floating_Point_Error, "A059");
      Assert
        (Value ("SIGFPE") = Signal_Floating_Point_Error, "A060");
      Assert
        (Value ("Signal_Hangup") = Signal_Hangup, "A061");
      Assert
        (Value ("SIGHUP") = Signal_Hangup, "A062");
      Assert
        (Value ("Signal_Illegal_Instruction") =
         Signal_Illegal_Instruction, "A063");
      Assert
        (Value ("SIGILL") = Signal_Illegal_Instruction, "A064");
      Assert
        (Value ("Signal_Interrupt") = Signal_Interrupt, "A065");
      Assert
        (Value ("SIGINT") = Signal_Interrupt, "A066");
      Assert
        (Value ("Signal_Kill") = Signal_Kill, "A067");
      Assert
        (Value ("SIGKILL") = Signal_Kill, "A068");
      Assert
        (Value ("Signal_Pipe_Write") = Signal_Pipe_Write, "A069");
      Assert
        (Value ("SIGPIPE") = Signal_Pipe_Write, "A070");
      Assert
        (Value ("Signal_Quit") = Signal_Quit, "A071");
      Assert
        (Value ("SIGQUIT") = Signal_Quit, "A072");
      Assert
        (Value ("Signal_SegmeNTATION_Violation") =
         Signal_Segmentation_Violation, "A073");
      Assert
        (Value ("SIGSEGV") = Signal_Segmentation_Violation, "A074");
      Assert
        (Value ("Signal_Terminate") = Signal_Terminate, "A075");
      Assert
        (Value ("SIGTERM") = Signal_Terminate, "A076");
      Assert
        (Value ("Signal_User_1") = Signal_User_1, "A077");
      Assert
        (Value ("SIGUSR1") = Signal_User_1, "A078");
      Assert
        (Value ("Signal_User_2") = Signal_User_2, "A079");
      Assert
        (Value ("SIGUSR2") = Signal_User_2, "A080");

      if Is_Supported (Memory_Protection_Option) then
         Assert
           (Value ("Signal_Bus_ERROR") = Signal_Bus_Error, "A081");
         Assert
           (Value ("SIGBUS") = Signal_Bus_Error, "A082");
      end if;

      if Is_Supported (Job_Control_Option) then
         Assert
           (Value ("Signal_Child") = Signal_Child, "A083");
         Assert
           (Value ("SIGCHLD") = Signal_Child, "A084");
         Assert
           (Value ("Signal_Continue") = Signal_Continue, "A085");
         Assert
           (Value ("SIGCONT") = Signal_Continue, "A086");
         Assert
           (Value ("Signal_Stop") = Signal_Stop, "A087");
         Assert
           (Value ("SIGSTOP") = Signal_Stop, "A088");
         Assert
           (Value ("Signal_Terminal_Stop") = Signal_Terminal_Stop, "A089");
         Assert
           (Value ("SIGTSTP") = Signal_Terminal_Stop, "A090");
         Assert
           (Value ("Signal_Terminal_Input") = Signal_Terminal_Input, "A091");
         Assert
           (Value ("SIGTTIN") = Signal_Terminal_Input, "A092");
         Assert
           (Value ("Signal_Terminal_Output") = Signal_Terminal_Output, "A093");
         Assert
           (Value ("SIGTTOU") = Signal_Terminal_Output, "A094");
      end if;

      -----------------------------------------------------------------------
      --  Value (Value ("S")) = S

      for Sig in Signal loop
         Assert (Value (Image (Sig)) = Sig,
           "A095: image/value of "  & Image (Sig)
             & " " & Image (Sig) & " "
             & POSIX_Signals.Image (Value (Image (Sig))));
      end loop;

      --------------------------------------------------------------------
      --  Constraint_Error is raised by Value if Str does not match the
      --  image of any of the signals supported by the implementation.

      declare
         Sig : Signal;
      begin
         Sig := Value ("Garbage");
         Assert (False, "A096");
      exception
         when E1 : Constraint_Error => null;
         when E2 : others => Unexpected_Exception (E2, "A097");
      end;

   exception
      when E1 : others => Unexpected_Exception (E1, "A098");
   end;

   ---------------------------------------------------------------------
   --  For all the reserved signals, Await_Signal raises POSIX_Error
   --  with Invalid_Argument. [3.3.15]

   for Sig in Signal loop
      if Is_Reserved (Sig)
        and then Sig /= SIGNULL then
         declare
            Set : Signal_Set;
            Result : Signal;
         begin
            Delete_All_Signals (Set);
            Add_Signal (Set, Sig);
            Result := Await_Signal (Set);
            Expect_Exception ("A099: reserved " & Image (Sig));
         exception
         when POSIX_Error =>
            Check_Error_Code (Invalid_Argument, "A100: reserved signal");
         when E : others =>
            Unexpected_Exception (E, "A101");
         end;
      end if;
   end loop;

   ---------------------------------------------------------------------
   --  All the required signals shall be supported by every implementation.
   --  if the Job Control option is supported, the job control signals
   --  shall be supported.
   --  if the Memory Protection option is supported, the memory protection
   --  signal shall be supported.
   --  if the Realtime Signals option is supported, the realtime signals
   --  shall be supported.
   --  An implementation shall not impose restrictions on the ability of
   --  an application to send, accept, block, or ignore signals defined
   --  by this standard, except as specified in this standard.
   --  These and other general semantic requirements for support of
   --  certain signals are not tested separately, since the signals are used
   --  in the tests of several operations.

   ---------------------------------------------------------------------
   --  Signal_Null has the value zero.

   Assert (Signal_Null = 0, "A102");

   ---------------------------------------------------------------------
   --  if the Realtime Signals option is supported, the range
   --  Realtime_Signal shall include at least
   --  Portable_Realtime_Signals_Maximum values, and shall not overlap
   --  with the named signals.

   if Is_Supported (Realtime_Signals_Option) then
      Assert (Integer (Realtime_Signal'Last - Realtime_Signal'First + 1)
        >= POSIX_Limits.Portable_Realtime_Signals_Maximum,
        "A103: Too few realtime signals:" &
        Image (Realtime_Signal'Last - Realtime_Signal'First + 1));
      for Sig in Realtime_Signal'Range loop
         for I in Defined_Signals'Range loop
            Assert (Sig /= Defined_Signals (I),
              "A104: " & Image (Sig) & " in Realtime_Signal");
         end loop;
      end loop;
   end if;

   ---------------------------------------------------------------------

   Test ("Signal Sets [3.3.7]");
   declare
      Set, Set_2, Set_3 : Signal_Set;
   begin

      ------------------------------------------------------------------
      --  Objects of type Signal_Set shall be implicitly initialized to
      --  include no signals.

      for Sig in 1 .. Signal'Last loop
         begin
            Assert (not Is_Member (Set, Sig), "A105");
         exception
         when POSIX_Error => Check_Error_Code (Invalid_Argument, "A106");
         when E1 : others => Unexpected_Exception (E1, "A107");
         end;
      end loop;

      -------------------------------------------------------------------
      --  Is_Member shall return the value True if and only if the set
      --  specified by the parameter Set includes the signal specified by
      --  the parameter Sig or the value of the parameter Sig is
      --  Signal_Null.

      Assert (Is_Member (Set, Signal_Null), "A108");

      -------------------------------------------------------------
      --  Add_All_Signals updates Set so that it
      --  includes all values of the type Signal.

      Add_All_Signals (Set);

      for Sig in 1 .. Signal'Last loop
         begin
            Assert (Is_Member (Set, Sig), "A109");
         exception
         when POSIX_Error => Check_Error_Code (Invalid_Argument, "A110");
         when E1 : others => Unexpected_Exception (E1, "A111");
         end;
      end loop;

      ---------------------------------------------------------------------
      --  Signal_Set is a private type, so assignment and equality test
      --  must work with the standard Ada semantics.

      Add_All_Signals (Set_3);
      Set_2 := Set;
      Assert (Set = Set_2, "A112");
      Assert (Set = Set_3, "A113");
      Assert (Set_2 = Set_3, "A114");

      for Sig in 1 .. Signal'Last loop
         begin
            --  Check Signal membership
            Assert (Is_Member (Set, Sig), "A115");
         exception
         when POSIX_Error => Check_Error_Code (Invalid_Argument, "A116");
         when E1 : others => Unexpected_Exception (E1, "A117");
         end;
      end loop;

      ---------------------------------------------------------------
      --  Delete_All_Signals updates the set specified by Set
      --  so that it includes no signals.

      Delete_All_Signals (Set);

      for Sig in 1 .. Signal'Last loop
         begin
            Assert (not Is_Member (Set, Sig), "A118");
         exception
         when POSIX_Error => Check_Error_Code (Invalid_Argument, "A119");
         when E1 : others => Unexpected_Exception (E1, "A120");
         end;
      end loop;

      ---------------------------------------------------------------
      --  Add_Signal adds the signal specified by Sig
      --  to the set of signals specified by Set.  Any other
      --  members of the set remain.

      for Sig in 1 .. Signal'Last loop
         begin
            Add_Signal (Set, Sig);
            Assert (Is_Member (Set, Sig), "A121");
            for Sig2 in Sig + 1 .. Signal'Last loop
               Assert (not Is_Member (Set, Sig2), "A122");
            end loop;
         exception
         when POSIX_Error => Check_Error_Code (Invalid_Argument, "A123");
         when E1 : others => Unexpected_Exception (E1, "A124");
         end;
      end loop;

      --------------------------------------------------------------
      --  Delete_Signal updates Set so that it does
      --  not include the signal specified by Sig.  No other
      --  signals are deleted from the set.

      for Sig in 1 .. Signal'Last loop
         begin
            Delete_Signal (Set, Sig);
            Assert (not Is_Member (Set, Sig), "A125");
            for Sig2 in Signal loop
               Assert (Sig2 = Sig or else
                 Is_Member (Set, Sig2), "A126");
            end loop;
            Add_Signal (Set, Sig);
         exception
         when POSIX_Error => Check_Error_Code (Invalid_Argument, "A127");
         when E1 : others => Unexpected_Exception (E1, "A128");
         end;
      end loop;

      ---------------------------------------------------------------
      --  Delete_All_Signals updates the set specified by Set
      --  so that it includes no signals.

      Delete_All_Signals (Set);

      for Sig in 1 .. Signal'Last loop
         begin
            Assert (not Is_Member (Set, Sig), "A129");
         exception
         when POSIX_Error => Check_Error_Code (Invalid_Argument, "A130");
         when E1 : others => Unexpected_Exception (E1, "A131");
         end;
      end loop;

   exception
      when E1 : others => Unexpected_Exception (E1, "A132");
   end;

   ---------------------------------------------------------------------
   --  It is implemenentation-defined whether the signal mask is
   --  per-task or per-process. [3.3.1]
   --  The tests for operations related to signal blocking
   --  are intended to have the same outcome, regardless of
   --  whether the mask is per-task or per-process.
   --  This is achieved by never having a signal unmasked by
   --  more than one task at the same time.

   Test ("Block and Unblock Signals [3.3.8]");

   declare
      Mask1, Mask2, Mask3, Mask4 : Signal_Set;
      New_Mask : Signal_Set;

      procedure Test_Signal (Sig : Signal);
      procedure Test_Signal (Sig : Signal) is
      begin
         Comment ("testing " & Image (Sig));
         Mask1 := Blocked_Signals;
         Add_Signal (Mask1, Sig);
         Block_Signals (Mask1, Mask2);
         --  At this point, if Sig is not reserved, it is masked.
         --  Masking of other signals is not changed.
         Mask3 := Blocked_Signals;
         Assert (Is_Reserved (Sig) or else Is_Member (Mask3, Sig),
           "A133: " & Image (Sig));
         Unblock_Signals (Mask1, Mask2);
         for I in Signal loop
            Assert ((I = Sig)
              or else (Is_Member (Mask1, I) = Is_Member (Mask3, I)),
              "A134:" & Image (I));
         end loop;
         --  At this point, all user-unmaskable signals are unmasked.
         Mask3 := Blocked_Signals;
         for I in Signal loop
            if not Is_Reserved (I) and then I /= SIGNULL then
               Assert (not Is_Member (Mask3, I), "A135: " & Image (I));
            end if;
         end loop;
      end Test_Signal;

   begin

      -----------------------------------------------------------------
      --  Set_Blocked_Signals returns in Old_Mask the value given
      --  by New_Mask in the previous call to Set_Blocked_Signals,
      --  except for reserved signals.

      Delete_All_Signals (Mask1);
      Delete_All_Signals (Mask2);
      Set_Blocked_Signals (Mask1, Mask2);
      Set_Blocked_Signals (Mask2, Mask3);
      for Sig in Signal loop
         if not Is_Reserved (Sig) then
            Assert (Is_Member (Mask1, Sig) = Is_Member (Mask3, Sig),
              "A136: " & Image (Sig));
         end if;
      end loop;

      -----------------------------------------------------------------
      --  Attempting to block reserved signals, SIGKILL, or SIGSTOP
      --  does not raise an exception, but the masking of the signals
      --  is not changed.
      --  Blocked_Signals returns the set of signals that is blocked.

      Add_All_Signals (Mask1);
      Set_Blocked_Signals (Mask1, Mask3);
      Mask4 := Blocked_Signals;
      for Sig in Signal loop
         if not Is_Reserved (Sig) then
            Assert (Is_Member (Mask4, Sig), "A137: " & Image (Sig));
         end if;
      end loop;

      Mask1 := Blocked_Signals;

      --  At this point, all maskable signals are masked and Mask1
      --  is equal to the current mask.

      ---------------------------------------------------------
      --  Set_Blocked_Signals returns the old mask in Old_Mask

      Set_Blocked_Signals (Mask2, Mask3);
      Assert (Mask1 = Mask3, "A138");

      Delete_All_Signals (Mask1);
      Set_Blocked_Signals (Mask1, Mask3);

      --  at this point,
      --  all user-unmaskable signals should be unmasked

      for Sig in Signal loop
         Test_Signal (Sig);
      end loop;

   exception
      when E1 : others => Unexpected_Exception (E1, "A139");
   end;

   ------------------------------------------------------------------------

   Test ("Ignore Signals [3.3.9]");

   declare

      New_Mask : Signal_Set;

      procedure Test_Signal (Sig : Signal);
      procedure Test_Signal (Sig : Signal) is

      begin
         Comment ("testing " & Image (Sig));

         Block_Signals (All_Signal_Mask, Old_Mask);

         ---------------------------------------------------------------
         --  When signal is not ignored, signals sent to the process
         --  can be caught using Await_Signal.

         Assert (not Is_Ignored (Sig), "A140");
         if Action_Cannot_Be_Set (Sig) then
            Expect_Exception ("A141: " & Image (Sig));
            Set_Error_Code (Invalid_Argument);
            raise POSIX_Error;
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
                 "A142: " & Image (Sig) & " is pending");
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

         Assert (Is_Ignored (Sig), "A143");

         ---------------------------------------------------------------
         --  When signal is unignored, signals sent to the process
         --  cause the default action (again).  It should again be
         --  possible to use Await_Signal to catch the signal, if the
         --  default action allows the signal to be caught.

         declare
            The_Sig : Signal;
            Timeout : Timespec := To_Timespec (DU);
         begin
            Add_Signal (New_Mask, Sig);
            Block_Signals (New_Mask, Old_Mask);
            Send_Signal (Get_Process_ID, Sig);
            --  Unblocking the signal should ensure it is discarded.
            Unblock_Signals (New_Mask, Old_Mask);
            Block_Signals (New_Mask, Old_Mask);
            The_Sig := Await_Signal_Or_Timeout (New_Mask, Timeout);
            Comment ("received " & Image (The_Sig) & " (not ignored)");
            Expect_Exception ("A144");
         exception
         when POSIX_Error =>
            Comment ("TIMED OUT waiting for " & Image (Sig) & " (OK)");
            Check_Error_Code (EAGAIN, "A145: " & Image (Sig));
         when E : others => Unexpected_Exception (E, "A146");
         end;

         ---------------------------------------------------------------
         --  When signal is unignored, signals sent to the process
         --  cause the default action (again).  It should again be
         --  possible to use Await_Signal to catch the signal, if the
         --  default action allows the signal to be caught.

         Comment ("should stop ignoring signals");

         Unignore_Signal (Sig);
         Assert (not Is_Ignored (Sig), "A147");

         declare
            The_Sig : Signal;
            Timeout : Timespec := To_Timespec (DU);
         begin
            Add_Signal (New_Mask, Sig);
            Block_Signals (New_Mask, Old_Mask);
            Send_Signal (Get_Process_ID, Sig);
            The_Sig := Await_Signal_Or_Timeout (New_Mask, Timeout);
            Assert (The_Sig = Sig, "A148");
         exception
         when E : others => Unexpected_Exception (E, "A149");
         end;

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
         when E : others => Unexpected_Exception (E, "A150");
         end;

      exception
      when E1 : POSIX_Error =>
         Assert (Get_Error_Code = Invalid_Argument and
           Action_Cannot_Be_Set (Sig), "A151: " & Image (Sig)
             & " " & Image (Get_Error_Code));
      when E2 : others => Unexpected_Exception (E2, "A152");
      end Test_Signal;

      procedure Test_Signal_2 (Sig : Signal);
      procedure Test_Signal_2 (Sig : Signal) is
      begin
         Comment ("testing " & Image (Sig) & " with entry");

         ----------------------------------------------------------------
         --  Ensure all blockable signals are blocked
         --  in the environment task.

         Block_Signals (All_Signal_Mask, Old_Mask);

         ---------------------------------------------------------------
         --  When signal is not ignored, signals sent to the process
         --  cause the handler to execute.

         Assert (not Is_Ignored (Sig), "A153");

         ---------------------------------------------------------------
         --  For the Ignore_Signal operation if the signal is bound to
         --  a task entry, the effect shall be to discard any pending or
         --  subsequent deliveries of the that signal.  The binding to
         --  the entry MAY remain in force. [3.3.17.2]
         --  Thus, signals sent to the process do not cause the handler
         --  to execute.

         Ignore_Signal (Sig);
         Assert (Is_Ignored (Sig), "A154");

         ---------------------------------------------------------------
         --  When signal is unignored, the default action is restored.
         --  The effect of this on entries that are attached is not
         --  specified, since POSIX.5b says only that "the binding to the
         --  entry MAY remain in force". [3.3.17.2]
         --  If it is not in force, we expect the default action, which
         --  may be to terminate the process.  Therefore, this check
         --  is deferred to a separate test.

         Unignore_Signal (Sig);
         Assert (not Is_Ignored (Sig), "A155");

         --------------------------------------------------------------------
         --  Now make sure any pending occurrences of the signal will be
         --  cleared out safely when we next unblock signals.

         Ignore_Signal (Sig);

      exception
      when E1 : others => Unexpected_Exception (E1, "A156");
      end Test_Signal_2;

   begin
      for Sig in Signal loop
         begin
            Test_Signal (Sig);
         exception
         when E : others => Unexpected_Exception (E, "A157");
         end;
      end loop;
      for Sig in Signal loop
         begin
            Test_Signal_2 (Sig);
         exception
         when E1 : POSIX_Error =>
            if Is_Supported (Signal_Entries_Option)
              and then Get_Error_Code /= Invalid_Argument
            then
               Unexpected_Exception (E1, "A158");
            end if;
         when E2 : others => Unexpected_Exception (E2, "A159");
         end;
      end loop;
   end;

   Unblock_Signals (All_Signal_Mask, Old_Mask);

   ---------------------------------------------------------------------

   Test ("Controlling Generation of Signal for Child Process [3.3.10]");

   begin
      Assert (Stopped_Child_Signal_Enabled, "A160");
      Set_Stopped_Child_Signal; -- by default, Enable := True
      Assert (Stopped_Child_Signal_Enabled, "A161");
      Set_Stopped_Child_Signal (Enable => False);
      Assert (not Stopped_Child_Signal_Enabled, "A162");
   exception
   when E : others => Unexpected_Exception (E, "A163");
   end;

   --  To check that this operation actually works, a child process
   --  is required.  In this test we avoid creating child processes,
   --  so that the test can still be run on systems that do not allow more
   --  than one process.

   ---------------------------------------------------------------------

   Test ("Pending Signals [3.3.11]");

   declare
      Set : Signal_Set;
   begin
      Block_Signals (All_Signal_Mask, Old_Mask);

      ----------------------------------------------------------
      --  Initially, no signals are pending, and all blockable
      --  signals are blocked.

      Set := Pending_Signals;
      for Sig in Signal loop
         Assert (Sig = SIGNULL or not Is_Member (Set, Sig),
           "A164: " & Image (Sig) & " is pending");
      end loop;

      Comment ("sending " & Image (SIGUSR1));
      Send_Signal (Get_Process_ID, SIGUSR1);
      Comment ("sending " & Image (SIGHUP));
      Send_Signal (Get_Process_ID, SIGHUP);

      -----------------------------------------------------------
      --  Signals sent to the current process are pending,
      --  but no others.

      Set := Pending_Signals;
      for Sig in Signal loop
         if Sig = SIGUSR1 or Sig = SIGHUP or Sig = SIGNULL then
            Assert (Is_Member (Set, Sig), "A165");
         else
            Assert (not Is_Member (Set, Sig), "A166: " & Image (Sig)
              & " is pending");
         end if;
      end loop;

      --------------------------------------------------------------------
      --  Now make sure any pending occurrences of the signal will be
      --  cleared out safely.

      declare
         Set, Old_Set : Signal_Set;
         procedure Clear_Signal (Sig : Signal);
         procedure Clear_Signal (Sig : Signal) is
         begin
            Add_Signal (Set, Sig);
            Ignore_Signal (Sig);
            Unblock_Signals (Set, Old_Set);
            Block_Signals (Old_Set, Set);
            Unignore_Signal (Sig);
         exception
         when E : others =>
            Unexpected_Exception (E, "A167: " & Image (Sig));
         end Clear_Signal;
      begin
         Clear_Signal (SIGUSR1);
         Clear_Signal (SIGHUP);
      exception
      when E : others => Unexpected_Exception (E, "A168");
      end;

   exception
   when E : others => Unexpected_Exception (E, "A169");
   end;

   Unblock_Signals (All_Signal_Mask, Old_Mask);

   ---------------------------------------------------------------------

   Test ("Signal Event Notification [3.3.12]");

   declare
      procedure Test_Signal (Sig : Signal);
      procedure Test_Signal (Sig : Signal) is
         Int_Data : constant Integer := 10;
         Sig_E    : Signal_Event;
         Sig_D    : Signal_Data := +Int_Data;
         Sig_N    : Notification := Signal_Notification;
      begin
         Set_Signal (Sig_E, Sig);
         Set_Notification (Sig_E, No_Notification);
         Set_Notification (Sig_E, Sig_N);
         Set_Data (Sig_E, Sig_D);
         Assert (Sig = Get_Signal (Sig_E), "A170");
         Assert (Signal_Notification = Get_Notification (Sig_E), "A171");
         Assert (Int_Data = +(Get_Data (Sig_E)), "A172");
      exception
         when E1 : others => Unexpected_Exception (E1, "A173");
      end Test_Signal;
   begin
      Assert (Signal_Notification /= No_Notification, "A174");
      for Sig in Signal loop
         Test_Signal (Sig);
      end loop;
   end;

   ------------------------------------------------------------------
   --  Unchecked_Conversion between Signal_Data and System.Address,
   --  Standard.Integer, and POSIX_Timers.Timer_ID. [3.3.12]

   declare
      I : Integer := 999;
      A : System.Address := I'Address;
      S : Signal_Data;
   begin
      S := +I;
      Assert (+S = I and +I = S, "A175");
      S := +A;
      Assert (+S = A and +A = S, "A176");
      declare
         T : POSIX_Timers.Timer_ID;
         Event : Signal_Event;
      begin
         Set_Notification (Event, No_Notification);
         T := Create_Timer (Clock_Realtime, Event);
         S := +T;
         Assert (+S = T and +T = S, "A177");
      exception
      when E1 : POSIX_Error =>
         if Get_Error_Code /= Invalid_Argument then
            if Get_Error_Code = Operation_Not_Supported then
               Set_Error_Code (Operation_Not_Implemented);
            end if;

            --  POSIX.5b erroneously specifies OPERATION_NOT_SUPPORTED
            --  for Create/Delete_Timer.  That is inconsistent with
            --  POSIX.1b.  Therefore, we allow Operation_Not_Implemented
            --  as well as Operation_Not_Supported.

            Optional (Timers_Option, Operation_Not_Implemented, E1, "A178");
         end if;
      when E2 : others => Unexpected_Exception (E2, "A179");
      end;
   exception when E : others => Unexpected_Exception (E, "A180");
   end;

   --  .... Functional testing of signal notification is done separately,
   --  in the tests for each of the interfaces (e.g., message queues)
   --  that use it.

   ---------------------------------------------------------------------

   Test ("Signal Information [3.3.13]");

   declare

      Signal_Sources : constant array (1 .. 5) of Signal_Source :=
        (From_Send_Signal,
         From_Queue_Signal,
         From_Timer,
         From_Async_IO,
         From_Message_Queue);

      procedure Test_Signal (Sig : Signal);
      procedure Test_Signal (Sig : Signal) is
         Sig_I    : Signal_Info;
      begin
         Set_Signal (Sig_I, Sig);
         Set_Source (Sig_I, From_Timer);
         Set_Data (Sig_I, Signal_Data'(+10));
         Assert (Sig = Get_Signal (Sig_I), "A181");
         Assert (From_Timer = Get_Source (Sig_I), "A182");
         Assert (+10 = Get_Data (Sig_I), "A183");
      exception
      when E1 : POSIX_Error =>
         --  delivery until the signal is unmasked.
         Optional (Realtime_Signals_Option,
           Operation_Not_Supported, E1, "A184");
      when E2 : others => Unexpected_Exception (E2, "A185");
      end Test_Signal;

   begin

      for I in Signal_Sources'Range loop
         for J in Signal_Sources'Range loop
            Assert ((I = J) = (Signal_Sources (I) = Signal_Sources (J)),
              "A186" & Integer'Image (I) & " " & Integer'Image (J));
         end loop;
         if Signal_Sources (I) = From_Send_Signal then
            Assert (not Has_Data (Signal_Sources (I)),
              "A187: source" & Integer'Image (I)
              & " has no data");
         else
            Assert (Has_Data (Signal_Sources (I)),
              "A188" & Integer'Image (I));
         end if;
      end loop;

      for Sig in Signal loop
         Test_Signal (Sig);
      end loop;
   end;

   ---------------------------------------------------------------------

   Test ("Control Signal Queueing [3.3.14]");

   declare
      N : constant Integer :=
          POSIX_Limits.Portable_Queued_Signals_Maximum;
      subtype Test_Range is Integer range 700 .. 700 + N - 1;
      procedure Test_Signal (Sig : Signal);
      procedure Test_Signal (Sig : Signal) is
         Timeout : Timespec := To_Timespec (2*DU);
         Info : Signal_Info;
         Set : Signal_Set;
      begin
         Add_Signal (Set, Sig);
         Block_Signals (All_Signal_Mask, Old_Mask);
         Set_Signal (Info, Sig);
         Set_Source (Info, From_Timer);
         Set_Data (Info, Signal_Data'(+10));
         Enable_Queueing (SIGUSR1);
         for I in Test_Range loop
            Queue_Signal (Get_Process_ID, Sig, +I);
         end loop;
         for I in Test_Range loop
            Info := Await_Signal_Or_Timeout (Set, Timeout);
            Assert (Get_Signal (Info) = Sig, "A189");
            Assert (Get_Source (Info) = From_Queue_Signal, "A190");
            Assert (Get_Data (Info) = +777, "A191");
         end loop;
         Disable_Queueing (SIGUSR1);
         --  Data may still be queued, even if queuing is disabled.
         --  Either way, we should get at least one of the signals.
         for I in Test_Range loop
            Queue_Signal (Get_Process_ID, Sig, +I);
         end loop;
         for I in Test_Range loop
            Set_Source (Info, From_Timer);
            Set_Data (Info, +0);
            begin
               Info := Await_Signal_Or_Timeout (Set, Timeout);
               Comment ("received signal: " & Integer'Image (I));
            exception
            when E : POSIX_Error =>
               if I = Test_Range'First then
                  Unexpected_Exception (E, "A192");
               else
                  Check_Error_Code (Resource_Temporarily_Unavailable, "A193");
               end if;
            end;
            Assert (Get_Signal (Info) = Sig, "A194");
            Assert (Get_Source (Info) = From_Queue_Signal
              or Get_Source (Info) = From_Timer, "A195");
            Assert (Get_Data (Info) = +777
              or Get_Data (Info) = +0, "A196");
         end loop;
         Enable_Queueing (Sig);
         for I in Test_Range loop
            Queue_Signal (Get_Process_ID, Sig, +I);
         end loop;
         for I in Test_Range loop
            Info := Await_Signal_Or_Timeout (Set, Timeout);
            Assert (Get_Signal (Info) = Sig, "A197");
            Assert (Get_Source (Info) = From_Queue_Signal, "A198");
            Assert (Get_Data (Info) = +777, "A199");
         end loop;
      exception
      when E1 : POSIX_Error =>
         Optional (Realtime_Signals_Option,
           Operation_Not_Implemented, E1, "A200");
      when E2 : others => Unexpected_Exception (E2, "A201");
      end Test_Signal;
   begin
      for Sig in Signal loop
         case Sig is
         when SIGABRT | SIGALRM | SIGFPE | SIGILL | SIGSEGV |
              SIGBUS | SIGKILL | SIGSTOP =>
            null;
            --  It is too risky to send reserved signals to the process.
         when others =>
            Test_Signal (Sig);
         end case;
      end loop;
   exception
   when E1 : POSIX_Error =>
      Optional (Realtime_Signals_Option,
        Operation_Not_Supported, E1, "A202");
   when E2 : others => Unexpected_Exception (E2, "A203");
   end;

   Unblock_Signals (All_Signal_Mask, Old_Mask);

   ---------------------------------------------------------------------
   --  The POSIX.5 standard does not specify whether a signal entry
   --  may have parameters.  The interpretation here is that at least
   --  parameterless entries must be supported, if the Signal Entries
   --  option is supported at all.

   Test ("Signal Entries [3.3.17]");
   declare
      procedure Test_Signal (Sig : Signal);
      procedure Test_Signal (Sig : Signal) is
      begin
         Comment ("testing " & Image (Sig));
         begin
            case Sig is
            when Signal_Abort =>
               Assert (Signal_Abort_Ref = Signal_Reference (SIGABRT),
                    "A204");
            when Signal_Hangup =>
               Assert (Signal_Hangup_Ref = Signal_Reference (SIGHUP),
                    "A205");
            when Signal_Interrupt =>
               Assert (Signal_Interrupt_Ref = Signal_Reference (SIGINT),
                    "A206");
            when Signal_Pipe_Write =>
               Assert (Signal_Pipe_Write_Ref = Signal_Reference (SIGPIPE),
                    "A207");
            when Signal_Quit =>
               Assert (Signal_Quit_Ref = Signal_Reference (SIGQUIT),
                    "A208");
            when Signal_Terminate =>
               Assert (Signal_Terminate_Ref = Signal_Reference (SIGTERM),
                    "A209");
            when Signal_User_1 =>
               Assert (Signal_User_1_Ref = Signal_Reference (SIGUSR1),
                    "A210");
            when Signal_User_2 =>
               Assert (Signal_User_2_Ref = Signal_Reference (SIGUSR2),
                    "A211");
            when Signal_Child =>
               Assert (Signal_Child_Ref = Signal_Reference (SIGCHLD),
                    "A212");
            when Signal_Continue =>
               Assert (Signal_Continue_Ref = Signal_Reference (SIGCONT),
                    "A213");
            when Signal_Terminal_Stop =>
               Assert (Signal_Terminal_Stop_Ref = Signal_Reference (SIGTSTP),
                    "A214");
            when Signal_Terminal_Input =>
               Assert (Signal_Terminal_Input_Ref = Signal_Reference (SIGTTIN),
                    "A215");
            when Signal_Terminal_Output =>
               Assert (Signal_Terminal_Output_Ref = Signal_Reference (SIGTTOU),
                    "A216");
            when others =>
               null;
            end case;
         exception
         when E1 : POSIX_Error =>
            if Is_Reserved (Sig) then
               Check_Error_Code (Invalid_Argument, "A217");
            else
               Unexpected_Exception (E1, "A218");
            end if;
         when E2 : others => Unexpected_Exception (E2, "A219");
         end;
      exception
      when E1 : POSIX_Error =>
         if Is_Supported (Signal_Entries_Option)
           and then Get_Error_Code /= Invalid_Argument
         then
            Unexpected_Exception (E1, "A220");
         end if;
      when E2 : others => Unexpected_Exception (E2, "A221");
      end Test_Signal;
   begin
      for Sig in Signal loop
         Test_Signal (Sig);
      end loop;
   exception
   when E : others => Unexpected_Exception (E, "A222");
   end;

   --  Active tests of signal entries are in other programs.

   ---------------------------------------------------------------------

   Test ("Send a Signal [3.3.18]");

   --  This interface is tested already above, for a process sending
   --  a signal to itself.
   declare
      Uninitialized_Process_ID : Process_ID;
      Uninitialized_Group_ID : Process_ID;
      pragma Warnings (Off, Uninitialized_Process_ID);
      pragma Warnings (Off, Uninitialized_Group_ID);
      --  Let these variables uninitialized.
   begin
      -----------------------------------------------------------------
      --  Sending Signal_Null can be used to check the validity of
      --  a process ID or process group ID.

      --  This setup is not 100% reliable.  With finite probability,
      --  the garbage might be the ID of the current process,
      --  killing the current process.
      --  If that happens, run the test again.

      begin
         Comment ("sending ");
         Send_Signal (Uninitialized_Process_ID, Signal_Null);
         Comment ("UNLIKELY: garbage ID is killable process?");
      exception
      when E1 : POSIX_Error =>
         if Get_Error_Code = Operation_Not_Permitted then
            Comment ("UNLIKELY: garbage ID is real process?");
         else Check_Error_Code (No_Such_Process, "A223");
         end if;
      when E : others => Unexpected_Exception (E, "A224");
      end;

      --  This setup is not 100% reliable.  With finite probability,
      --  the garbage might be the ID of the current process group,
      --  killing the current process.
      --  If that happens, run the test again.

      begin
         Comment ("sending ");
         Send_Signal (Uninitialized_Group_ID, Signal_Null);
         Comment ("UNLIKELY: garbage ID is killable group?");
      exception
      when E1 : POSIX_Error =>
         if Get_Error_Code = Operation_Not_Permitted then
            Comment ("UNLIKELY: garbage ID is real group?");
         else Check_Error_Code (No_Such_Process, "A225");
         end if;
      when E : others => Unexpected_Exception (E, "A226");
      end;

   exception
   when E : others => Unexpected_Exception (E, "A227");
   end;

   --  Active tests of Send_Signal are in other programs.

   ---------------------------------------------------------------------

   Test ("Interrupt a Task [3.3.20]");

   --  The following is risky, since the error is not required to be
   --  detected, and if it is not detected the effect is undefined.

   declare
      Uninitialized_Task_ID : Task_Id;
   begin
      if not Is_Callable (Uninitialized_Task_ID) then
         Interrupt_Task (Uninitialized_Task_ID);
      end if;
   exception
   when POSIX_Error =>
      Check_Error_Code (Invalid_Argument, "A228");
   when Program_Error =>
      Comment ("raised Program_Error (correctly)");
   when E : others => Unexpected_Exception (E, "A229");
   end;

   --  Active tests of Interrupt_Task are in other programs.

   ---------------------------------------------------------------------

   Done;
exception
   when E : others => Fatal_Exception (E, "A230");
end p030300;

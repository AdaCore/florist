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

--  Setup:  Run this test in a fashion that initially all signals are unmasked
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
   use Ada_Task_Identification,
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
   --  The implementation must reserve the signals
   --  that are not defined as reserved in the standard.
   --  An implementation shall not impose restrictions on the ability
   --  of an application to send, accept, block, or ignore the signals
   --  defined by this standard, except as specified in this standard.
   --  [3.3.2]
   --  The implementation of the tailorable functions Is_Reserved_Signal and
   --  Action_Cannot_Be_Set, in package Test_Parameters, must be consistent
   --  with the above requirements.

   for I in Defined_Signals'Range loop
      Assert (not Is_Reserved_Signal (Defined_Signals (I))
        or else Is_Reserved_Defined_Signal (Defined_Signals (I)),
        "A001: p030300a " & Image (Defined_Signals (I)) & " is reserved");
      Assert (Is_Reserved_Signal (Defined_Signals (I))
        or else not Is_Reserved_Defined_Signal (Defined_Signals (I)),
        "A002: p030300a " & Image (Defined_Signals (I)) & " is not reserved");
   end loop;

   for Sig in 1 .. Signal'Last loop
      Assert (not Action_Cannot_Be_Set (Sig) or else
        Is_Reserved_Signal (Sig) or else
        Sig = SIGKILL or else
        Sig = SIGSTOP, "A003: " & Image (Sig));
      Assert (not Is_Reserved_Signal (Sig) or else
        Sig /= SIGKILL or else
        Sig /= SIGSTOP or else
        Action_Cannot_Be_Set (Sig), "A004: " & Image (Sig));
   end loop;

   ----------------------------------------------------------------------
   --  The default actions specified in Test_Parameters must be
   --  consistent with the requirements of the standard.

   declare
      Act, Req_Act : Signal_Action;
   begin
      for Sig in Signal loop
         Req_Act := Required_Default_Action (Sig);
         Act := Default_Action (Sig);
         Assert (Act /= Unspecified, "A005: " & Image (Sig));
         if Req_Act /= Unspecified then
            Assert (Act = Req_Act, "A006: " & Image (Sig));
         end if;
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
         if Sig /= SIGNULL
           and then Is_Member (Set, Sig) then
            Comment ("WARNING: " & Image (Sig) & " blocked in env. task");
         end if;
      end loop;
   exception
   when E1 : others => Unexpected_Exception (E1, "A007");
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
               Fail ("A008: " & Image (Sig) & " is invalid");
            else
               Unexpected_Exception (E1, "A009");
            end if;
            when E2 : others => Unexpected_Exception (E2, "A010");
         end;
      end loop;
   exception
   when E1 : others => Unexpected_Exception (E1, "A011");
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
        (Image (Signal_Null) = "SIGNAL_NULL", "A012");
      Assert
        (Image (SIGNULL) = Image (Signal_Null), "A013");
      Assert
        (Image (Signal_Abort) = "SIGNAL_ABORT", "A014");
      Assert
        (Image (SIGABRT) = Image (Signal_Abort), "A015");
      Assert
        (Image (Signal_Alarm) = "SIGNAL_ALARM", "A016");
      Assert
        (Image (SIGALRM) = Image (Signal_Alarm), "A017");
      Assert
        (Image (Signal_Floating_Point_Error) =
         "SIGNAL_FLOATING_POINT_ERROR", "A018");
      Assert
        (Image (SIGFPE) = Image (Signal_Floating_Point_Error), "A019");
      Assert
        (Image (Signal_Hangup) = "SIGNAL_HANGUP", "A020");
      Assert
        (Image (SIGHUP) = Image (Signal_Hangup), "A021");
      Assert
        (Image (Signal_Illegal_Instruction) =
         "SIGNAL_ILLEGAL_INSTRUCTION", "A022");
      Assert
        (Image (SIGILL) = Image (Signal_Illegal_Instruction), "A023");
      Assert
        (Image (Signal_Interrupt) = "SIGNAL_INTERRUPT", "A024");
      Assert
        (Image (SIGINT) = Image (Signal_Interrupt), "A025");
      Assert
        (Image (Signal_Kill) = "SIGNAL_KILL", "A026");
      Assert
        (Image (SIGKILL) = Image (Signal_Kill), "A027");
      Assert
        (Image (Signal_Pipe_Write) = "SIGNAL_PIPE_WRITE", "A028");
      Assert
        (Image (SIGPIPE) = Image (Signal_Pipe_Write), "A029");
      Assert
        (Image (Signal_Quit) = "SIGNAL_QUIT", "A030");
      Assert
        (Image (SIGQUIT) = Image (Signal_Quit), "A031");
      Assert
        (Image (Signal_Segmentation_Violation) =
         "SIGNAL_SEGMENTATION_VIOLATION", "A032");
      Assert
        (Image (SIGSEGV) = Image (Signal_Segmentation_Violation), "A033");
      Assert
        (Image (Signal_Terminate) = "SIGNAL_TERMINATE", "A034");
      Assert
        (Image (SIGTERM) = Image (Signal_Terminate), "A035");
      Assert
        (Image (Signal_User_1) = "SIGNAL_USER_1", "A036");
      Assert
        (Image (SIGUSR1) = Image (Signal_User_1), "A037");
      Assert
        (Image (Signal_User_2) = "SIGNAL_USER_2", "A038");
      Assert
        (Image (SIGUSR2) = Image (Signal_User_2), "A039");

      if Is_Supported (Memory_Protection_Option) then
         Assert
           (Image (Signal_Bus_Error) = "SIGNAL_BUS_ERROR", "A040");
         Assert
           (Image (SIGBUS) = Image (Signal_Bus_Error), "A041");
      end if;

      if Is_Supported (Job_Control_Option) then
         Assert
           (Image (Signal_Child) = "SIGNAL_CHILD", "A042");
         Assert
           (Image (SIGCHLD) = Image (Signal_Child), "A043");
         Assert
           (Image (Signal_Continue) = "SIGNAL_CONTINUE", "A044");
         Assert
           (Image (SIGCONT) = Image (Signal_Continue), "A045");
         Assert
           (Image (Signal_Stop) = "SIGNAL_STOP", "A046");
         Assert
           (Image (SIGSTOP) = Image (Signal_Stop), "A047");
         Assert
           (Image (Signal_Terminal_Stop) = "SIGNAL_TERMINAL_STOP", "A048");
         Assert
           (Image (SIGTSTP) = Image (Signal_Terminal_Stop), "A049");
         Assert
           (Image (Signal_Terminal_Input) = "SIGNAL_TERMINAL_INPUT", "A050");
         Assert
           (Image (SIGTTIN) = Image (Signal_Terminal_Input), "A051");
         Assert
           (Image (Signal_Terminal_Output) = "SIGNAL_TERMINAL_OUTPUT", "A052");
         Assert
           (Image (SIGTTOU) = Image (Signal_Terminal_Output), "A053");
      end if;

   exception
      --  No exceptions shall be raised by Image.
      when E1 : others => Unexpected_Exception (E1, "A054");
   end;

   begin

      ------------------------------------------------------------
      --  If Str matches the short name or the long name of an
      --  identifier for a signal supported by the implementation ...
      --  Value shall return the corresponding signal value.

      Assert
        (Value ("Signal_Null") = Signal_Null, "A055");
      Assert
        (Value ("SIGNULL") = Signal_Null, "A056");
      Assert
        (Value ("Signal_Abort") = Signal_Abort, "A057");
      Assert
        (Value ("siGnal_aBort") = Signal_Abort, "A058");
      Assert
        (Value ("SIGABRT") = Signal_Abort, "A059");
      Assert
        (Value ("siGabrt") = Signal_Abort, "A060");
      Assert
        (Value (" Signal_Abort ") = Signal_Abort, "A061");
      Assert
        (Value ("Signal_Alarm") = Signal_Alarm, "A062");
      Assert
        (Value ("SIGALRM") = Signal_Alarm, "A063");
      Assert
        (Value ("Signal_Floating_Point_Error") =
         Signal_Floating_Point_Error, "A064");
      Assert
        (Value ("SIGFPE") = Signal_Floating_Point_Error, "A065");
      Assert
        (Value ("Signal_Hangup") = Signal_Hangup, "A066");
      Assert
        (Value ("SIGHUP") = Signal_Hangup, "A067");
      Assert
        (Value ("Signal_Illegal_Instruction") =
         Signal_Illegal_Instruction, "A068");
      Assert
        (Value ("SIGILL") = Signal_Illegal_Instruction, "A069");
      Assert
        (Value ("Signal_Interrupt") = Signal_Interrupt, "A070");
      Assert
        (Value ("SIGINT") = Signal_Interrupt, "A071");
      Assert
        (Value ("Signal_Kill") = Signal_Kill, "A072");
      Assert
        (Value ("SIGKILL") = Signal_Kill, "A073");
      Assert
        (Value ("Signal_Pipe_Write") = Signal_Pipe_Write, "A074");
      Assert
        (Value ("SIGPIPE") = Signal_Pipe_Write, "A075");
      Assert
        (Value ("Signal_Quit") = Signal_Quit, "A076");
      Assert
        (Value ("SIGQUIT") = Signal_Quit, "A077");
      Assert
        (Value ("Signal_SegmeNTATION_Violation") =
         Signal_Segmentation_Violation, "A078");
      Assert
        (Value ("SIGSEGV") = Signal_Segmentation_Violation, "A079");
      Assert
        (Value ("Signal_Terminate") = Signal_Terminate, "A080");
      Assert
        (Value ("SIGTERM") = Signal_Terminate, "A081");
      Assert
        (Value ("Signal_User_1") = Signal_User_1, "A082");
      Assert
        (Value ("SIGUSR1") = Signal_User_1, "A083");
      Assert
        (Value ("Signal_User_2") = Signal_User_2, "A084");
      Assert
        (Value ("SIGUSR2") = Signal_User_2, "A085");

      if Is_Supported (Memory_Protection_Option) then
         Assert
           (Value ("Signal_Bus_ERROR") = Signal_Bus_Error, "A086");
         Assert
           (Value ("SIGBUS") = Signal_Bus_Error, "A087");
      end if;

      if Is_Supported (Job_Control_Option) then
         Assert
           (Value ("Signal_Child") = Signal_Child, "A088");
         Assert
           (Value ("SIGCHLD") = Signal_Child, "A089");
         Assert
           (Value ("Signal_Continue") = Signal_Continue, "A090");
         Assert
           (Value ("SIGCONT") = Signal_Continue, "A091");
         Assert
           (Value ("Signal_Stop") = Signal_Stop, "A092");
         Assert
           (Value ("SIGSTOP") = Signal_Stop, "A093");
         Assert
           (Value ("Signal_Terminal_Stop") = Signal_Terminal_Stop, "A094");
         Assert
           (Value ("SIGTSTP") = Signal_Terminal_Stop, "A095");
         Assert
           (Value ("Signal_Terminal_Input") = Signal_Terminal_Input, "A096");
         Assert
           (Value ("SIGTTIN") = Signal_Terminal_Input, "A097");
         Assert
           (Value ("Signal_Terminal_Output") = Signal_Terminal_Output, "A098");
         Assert
           (Value ("SIGTTOU") = Signal_Terminal_Output, "A099");
      end if;

      -----------------------------------------------------------------------
      --  Value (Value ("S")) = S

      for Sig in Signal loop
         Assert (Value (Image (Sig)) = Sig,
           "A100: image/value of "  & Image (Sig)
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
         Assert (False, "A101");
      exception
         when E1 : Constraint_Error => null;
         when E2 : others => Unexpected_Exception (E2, "A102");
      end;

   exception
      when E1 : others => Unexpected_Exception (E1, "A103");
   end;

   ---------------------------------------------------------------------
   --  For all the reserved signals,
   --  Await_Signal raises POSIX_Error with Invalid_Argument. [3.3.15]

   Test ("Await_Signal on reserved signals [3.3.15]");

   for Sig in Signal loop
      if Is_Reserved_Signal (Sig) then
         declare
            Set : Signal_Set;
            Result : Signal;
         begin
            Delete_All_Signals (Set);
            Add_Signal (Set, Sig);
            Result := Await_Signal (Set);
            Expect_Exception ("A104: reserved " & Image (Sig));
         exception
         when POSIX_Error =>
            Check_Error_Code (Invalid_Argument, "A105: reserved signal");
         when E : others =>
            Unexpected_Exception (E, "A106");
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

   Assert (Signal_Null = 0, "A107");

   ---------------------------------------------------------------------
   --  if the Realtime Signals option is supported, the range
   --  Realtime_Signal shall include at least
   --  Portable_Realtime_Signals_Maximum values, and shall not overlap
   --  with the named signals.

   if Is_Supported (Realtime_Signals_Option) then
      Assert (Integer (Realtime_Signal'Last - Realtime_Signal'First + 1)
        >= POSIX_Limits.Portable_Realtime_Signals_Maximum,
        "A108: Too few realtime signals:" &
        Image (Realtime_Signal'Last - Realtime_Signal'First + 1));
      for Sig in Realtime_Signal'Range loop
         for I in Defined_Signals'Range loop
            Assert (Sig /= Defined_Signals (I),
              "A109: " & Image (Sig) & " in Realtime_Signal");
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
            Assert (not Is_Member (Set, Sig), "A110");
         exception
         when POSIX_Error => Check_Error_Code (Invalid_Argument, "A111");
         when E1 : others => Unexpected_Exception (E1, "A112");
         end;
      end loop;

      -------------------------------------------------------------------
      --  Is_Member shall return the value True if and only if the set
      --  specified by the parameter Set includes the signal specified by
      --  the parameter Sig or the value of the parameter Sig is
      --  Signal_Null.

      Assert (Is_Member (Set, Signal_Null), "A113");

      -------------------------------------------------------------
      --  Add_All_Signals updates Set so that it
      --  includes all values of the type Signal.

      Add_All_Signals (Set);

      for Sig in 1 .. Signal'Last loop
         begin
            Assert (Is_Member (Set, Sig), "A114");
         exception
         when POSIX_Error => Check_Error_Code (Invalid_Argument, "A115");
         when E1 : others => Unexpected_Exception (E1, "A116");
         end;
      end loop;

      ---------------------------------------------------------------------
      --  Signal_Set is a private type, so assignment and equality test
      --  must work with the standard Ada semantics.

      Add_All_Signals (Set_3);
      Set_2 := Set;
      Assert (Set = Set_2, "A117");
      Assert (Set = Set_3, "A118");
      Assert (Set_2 = Set_3, "A119");

      for Sig in 1 .. Signal'Last loop
         begin
            --  Check Signal membership
            Assert (Is_Member (Set, Sig), "A120");
         exception
         when POSIX_Error => Check_Error_Code (Invalid_Argument, "A121");
         when E1 : others => Unexpected_Exception (E1, "A122");
         end;
      end loop;

      ---------------------------------------------------------------
      --  Delete_All_Signals updates the set specified by Set
      --  so that it includes no signals.

      Delete_All_Signals (Set);

      for Sig in 1 .. Signal'Last loop
         begin
            Assert (not Is_Member (Set, Sig), "A123");
         exception
         when POSIX_Error => Check_Error_Code (Invalid_Argument, "A124");
         when E1 : others => Unexpected_Exception (E1, "A125");
         end;
      end loop;

      ---------------------------------------------------------------
      --  Add_Signal adds the signal specified by Sig
      --  to the set of signals specified by Set.  Any other
      --  members of the set remain.

      for Sig in 1 .. Signal'Last loop
         begin
            Add_Signal (Set, Sig);
            Assert (Is_Member (Set, Sig), "A126");
            for Sig2 in Sig + 1 .. Signal'Last loop
               Assert (not Is_Member (Set, Sig2), "A127");
            end loop;
         exception
         when POSIX_Error => Check_Error_Code (Invalid_Argument, "A128");
         when E1 : others => Unexpected_Exception (E1, "A129");
         end;
      end loop;

      --------------------------------------------------------------
      --  Delete_Signal updates Set so that it does
      --  not include the signal specified by Sig.  No other
      --  signals are deleted from the set.

      for Sig in 1 .. Signal'Last loop
         begin
            Delete_Signal (Set, Sig);
            Assert (not Is_Member (Set, Sig), "A130");
            for Sig2 in Signal loop
               Assert (Sig2 = Sig or else
                 Is_Member (Set, Sig2), "A131");
            end loop;
            Add_Signal (Set, Sig);
         exception
         when POSIX_Error => Check_Error_Code (Invalid_Argument, "A132");
         when E1 : others => Unexpected_Exception (E1, "A133");
         end;
      end loop;

      ---------------------------------------------------------------
      --  Delete_All_Signals updates the set specified by Set
      --  so that it includes no signals.

      Delete_All_Signals (Set);

      for Sig in 1 .. Signal'Last loop
         begin
            Assert (not Is_Member (Set, Sig), "A134");
         exception
         when POSIX_Error => Check_Error_Code (Invalid_Argument, "A135");
         when E1 : others => Unexpected_Exception (E1, "A136");
         end;
      end loop;

   exception
      when E1 : others => Unexpected_Exception (E1, "A137");
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
         Comment ("Testing " & Image (Sig));
         Mask1 := Blocked_Signals;
         Add_Signal (Mask1, Sig);
         Block_Signals (Mask1, Mask2);
         --  At this point, if Sig is not reserved, it is masked.
         --  Masking of other signals is not changed.
         Mask3 := Blocked_Signals;
         Assert (Cannot_Be_Blocked (Sig) or else Is_Member (Mask3, Sig),
           "A138: " & Image (Sig));
         Unblock_Signals (Mask1, Mask2);
         for I in Signal loop
            Assert ((I = Sig)
              or else (Is_Member (Mask1, I) = Is_Member (Mask3, I)),
              "A139:" & Image (I));
         end loop;
         --  At this point, all user-unmaskable signals are unmasked.
         Mask3 := Blocked_Signals;
         for I in Signal loop
            if not Is_Reserved_Signal (I) and then I /= SIGNULL then
               Assert (not Is_Member (Mask3, I), "A140: " & Image (I));
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
         Comment (Boolean'Image (Is_Reserved_Signal (Sig)) & ' ' & 
                  Boolean'Image (Is_Member (Mask1, Sig)) & ' ' &
                  Boolean'Image (Is_Member (Mask2, Sig)) & ' ' &
                  Boolean'Image (Is_Member (Mask3, Sig)) & ' ' &
                  Image (Sig));
         if not Is_Reserved_Signal (Sig) then
            Assert (Is_Member (Mask1, Sig) = Is_Member (Mask3, Sig),
              "A141: " & Image (Sig));
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
         if not Is_Member (Mask4, Sig) then
            Fails_Blocking_Test (Sig) := True;
            Assert (Cannot_Be_Blocked (Sig), "A142: " & Image (Sig));
         elsif Is_Reserved_Signal (Sig) then
            Fails_Blocking_Test (Sig) := True;
            --  Do not try blocking and unblocking reserved signals.
         end if;
      end loop;

      Mask1 := Blocked_Signals;

      --  At this point, all maskable signals are masked and Mask1
      --  is equal to the current mask.

      ---------------------------------------------------------
      --  Set_Blocked_Signals returns the old mask in Old_Mask

      Set_Blocked_Signals (Mask2, Mask3);
      Assert (Mask1 = Mask3, "A143");

      Delete_All_Signals (Mask1);
      Set_Blocked_Signals (Mask1, Mask3);

      --  at this point,
      --  all user-unmaskable signals should be unmasked

      for Sig in Signal loop
         if not Fails_Blocking_Test (Sig) then
            Test_Signal (Sig);
         end if;
      end loop;

   exception
      when E1 : others => Unexpected_Exception (E1, "A144");
   end;

   ------------------------------------------------------------------------

   Test ("Ignore Signals [3.3.9]");

   declare

      New_Mask : Signal_Set;
      Not_Applicable : exception;
      procedure Test_Signal (Sig : Signal);

      procedure Test_Signal (Sig : Signal) is
      begin
         Comment ("Testing " & Image (Sig));

         ---------------------------------------------------------------
         --  Ignore_Signal, Unignore_Signal, or Is_Ignored raises
         --  POSIX_Error with Invalid_Argument if Sig is a signal for
         --  which the signal action is not permitted to be set by the
         --  application.

         if Action_Cannot_Be_Set (Sig) then
            begin
               if Is_Ignored (Sig) then null;
               end if;
               Expect_Exception ("A145: " & Image (Sig));
            exception
            when E1 : POSIX_Error =>
               Assert (Get_Error_Code = Invalid_Argument,
                 "A146: " & Image (Sig) & " " & Image (Get_Error_Code));
            end;
            begin
               Ignore_Signal (Sig);
               Expect_Exception ("A147: " & Image (Sig));
               Unignore_Signal (Sig);
               Expect_Exception ("A148: " & Image (Sig));
            exception
            when E1 : POSIX_Error =>
               Assert (Get_Error_Code = Invalid_Argument,
                 "A149: " & Image (Sig) & " " & Image (Get_Error_Code));
            end;
            begin
               Unignore_Signal (Sig);
               Expect_Exception ("A150: " & Image (Sig));
            exception
            when E1 : POSIX_Error =>
               Assert (Get_Error_Code = Invalid_Argument,
                 "A151: " & Image (Sig) & " " & Image (Get_Error_Code));
            end;
            raise Not_Applicable;
         end if;

         ---------------------------------------------------------------
         --  If we get here the signal should be one that we expect to
         --  be able to ignore and unignore.

         Block_Signals (All_Signal_Mask, Old_Mask);

         ---------------------------------------------------------------
         --  When signal is not ignored, signals sent to the process
         --  can be caught using Await_Signal.

         begin
            Assert (not Is_Ignored (Sig), "A152");
            Add_Signal (New_Mask, Sig);
            Block_Signals (New_Mask, Old_Mask);
            Send_Signal (Get_Process_ID, Sig);
            Try_Await_Signal (Sig, New_Mask, DU, No, "A153");
         exception
         when Local_Failure => null;
         when E : others => Unexpected_Exception (E, "A154: " & Image (Sig));
         end;

         --------------------------------------------------------------
         --  There should be no pending occurrences of Sig at this point.
         --  If this check fails, it may mean that Send_Signal
         --  delivers the signal to ALL the threads in a process,
         --  rather than just one.

         declare
            Set : Signal_Set;
         begin
            Set := Pending_Signals;
            for Sig in Signal loop
               Assert (Sig = SIGNULL or not Is_Member (Set, Sig),
                 "A155: " & Image (Sig) & " is pending");
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
         --  to await it with the expection of the signal being ignored.

         Comment ("Should now be ignoring signals");

         Ignore_Signal (Sig);

         Assert (Is_Ignored (Sig), "A156");

         begin
            Add_Signal (New_Mask, Sig);
            Block_Signals (New_Mask, Old_Mask);
            Send_Signal (Get_Process_ID, Sig);
            --  Unblocking the signal should ensure it is discarded.
            Unblock_Signals (New_Mask, Old_Mask);
            Block_Signals (New_Mask, Old_Mask);
            Try_Await_Signal (Sig, New_Mask, DU, Yes, "A157");
         exception
         when Local_Failure => null;
         when E : others => Unexpected_Exception (E, "A158");
         end;

         ---------------------------------------------------------------
         --  When signal is unignored, signals sent to the process
         --  cause the default action (again).  It should again be
         --  possible to use Await_Signal to catch the signal, if the
         --  default action allows the signal to be caught.

         Comment ("Should stop ignoring signals");

         Unignore_Signal (Sig);
         Assert (not Is_Ignored (Sig), "A159");

         begin
            Add_Signal (New_Mask, Sig);
            Block_Signals (New_Mask, Old_Mask);
            Send_Signal (Get_Process_ID, Sig);
            Comment ("Awaiting signal " & Image (Sig));
            Try_Await_Signal (Sig, New_Mask, DU, No, "A160");
         exception
         when Local_Failure => null;
         when E : others => Unexpected_Exception (E, "A161: " & Image (Sig));
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
         when E : others => Unexpected_Exception (E, "A162");
         end;

      exception
      when Not_Applicable => null;
      when E : others => Unexpected_Exception (E, "A163: " & Image (Sig));
      end Test_Signal;

      procedure Test_Signal_2 (Sig : Signal);

      procedure Test_Signal_2 (Sig : Signal) is
      begin
         Comment ("Testing " & Image (Sig) & " with entry");

         ----------------------------------------------------------------
         --  Ensure all blockable signals are blocked
         --  in the environment task.

         Comment ("Blocking all signals");
         Block_Signals (All_Signal_Mask, Old_Mask);

         ---------------------------------------------------------------
         --  When signal is not ignored, signals sent to the process
         --  cause the handler to execute.

         Comment ("Checking Is_Ignored");
         Assert (not Is_Ignored (Sig), "A164");

         ---------------------------------------------------------------
         --  For the Ignore_Signal operation if the signal is bound to
         --  a task entry, the effect shall be to discard any pending or
         --  subsequent deliveries of the that signal.  The binding to
         --  the entry MAY remain in force. [3.3.17.2]
         --  Thus, signals sent to the process do not cause the handler
         --  to execute.

         Comment ("Ignoring signal");
         Ignore_Signal (Sig);
         Assert (Is_Ignored (Sig), "A165");

         ---------------------------------------------------------------
         --  When signal is unignored, the default action is restored.
         --  The effect of this on entries that are attached is not
         --  specified, since POSIX.5b says only that "the binding to the
         --  entry MAY remain in force". [3.3.17.2]
         --  If it is not in force, we expect the default action, which
         --  may be to terminate the process.  Therefore, this check
         --  is deferred to a separate test.

         Comment ("Unignoring signal");
         Unignore_Signal (Sig);
         Assert (not Is_Ignored (Sig), "A166");

         --------------------------------------------------------------------
         --  Now make sure any pending occurrences of the signal will be
         --  cleared out safely when we next unblock signals.

         Comment ("Ignoring signal");
         Ignore_Signal (Sig);
         Unignore_Signal (Sig);

      exception
      when E1 : others => Unexpected_Exception (E1, "A167: " & Image (Sig));
      end Test_Signal_2;

   begin
      for Sig in Signal loop
         begin
            if not Fails_Blocking_Test (Sig) then
               Test_Signal (Sig);
            end if;
         exception
         when E : others => Unexpected_Exception (E, "A168");
         end;
      end loop;
      for Sig in Signal loop
         begin
            if not Action_Cannot_Be_Set (Sig) and then
              not Fails_Blocking_Test (Sig) then
               Test_Signal_2 (Sig);
            end if;
         exception
         when E1 : POSIX_Error =>
            if Is_Supported (Signal_Entries_Option)
              and then Get_Error_Code /= Invalid_Argument
            then
               Unexpected_Exception (E1, "A169");
            end if;
         when E2 : others => Unexpected_Exception (E2, "A170");
         end;
      end loop;
   end;

   Unblock_Signals (All_Signal_Mask, Old_Mask);

   ---------------------------------------------------------------------

   Test ("Controlling Generation of Signal for Child Process [3.3.10]");

   begin
      Assert (Stopped_Child_Signal_Enabled, "A171");
      Set_Stopped_Child_Signal; -- by default, Enable := True
      Assert (Stopped_Child_Signal_Enabled, "A172");
      Set_Stopped_Child_Signal (Enable => False);
      Assert (not Stopped_Child_Signal_Enabled, "A173");
   exception
   when E : others => Unexpected_Exception (E, "A174");
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
           "A175: " & Image (Sig) & " is pending");
      end loop;

      Comment ("Sending " & Image (SIGUSR1));
      Send_Signal (Get_Process_ID, SIGUSR1);
      Comment ("Sending " & Image (SIGHUP));
      Send_Signal (Get_Process_ID, SIGHUP);

      -----------------------------------------------------------
      --  Signals sent to the current process are pending,
      --  but no others.

      Set := Pending_Signals;
      for Sig in Signal loop
         if Sig = SIGUSR1 or Sig = SIGHUP or Sig = SIGNULL then
            Assert (Is_Member (Set, Sig), "A176");
         else
            Assert (not Is_Member (Set, Sig), "A177: " & Image (Sig)
              & " is pending");
         end if;
      end loop;

      --------------------------------------------------------------------
      --  Now make sure any pending occurrences of the signal will be
      --  cleared out safely.

      Clear_Signal (SIGUSR1, "A178");
      Clear_Signal (SIGHUP, "A179");

   exception
   when E : others => Unexpected_Exception (E, "A180");
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
         Assert (Sig = Get_Signal (Sig_E), "A181");
         Assert (Signal_Notification = Get_Notification (Sig_E), "A182");
         Assert (Int_Data = +(Get_Data (Sig_E)), "A183");
      exception
         when E1 : others => Unexpected_Exception (E1, "A184");
      end Test_Signal;
   begin
      Assert (Signal_Notification /= No_Notification, "A185");
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
      Assert (+S = I and +I = S, "A186");
      S := +A;
      Assert (+S = A and +A = S, "A187");
      declare
         T : POSIX_Timers.Timer_ID;
         Event : Signal_Event;
      begin
         Set_Notification (Event, No_Notification);
         T := Create_Timer (Clock_Realtime, Event);
         S := +T;
         Assert (+S = T and +T = S, "A188");
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

            Optional (Timers_Option, Operation_Not_Implemented, E1, "A189");
         end if;
      when E2 : others => Unexpected_Exception (E2, "A190");
      end;
   exception when E : others => Unexpected_Exception (E, "A191");
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
         Assert (Sig = Get_Signal (Sig_I), "A192");
         Assert (From_Timer = Get_Source (Sig_I), "A193");
         Assert (+10 = Get_Data (Sig_I), "A194");
      exception
      when E1 : POSIX_Error =>
         --  delivery until the signal is unmasked.
         Optional (Realtime_Signals_Option,
           Operation_Not_Supported, E1, "A195");
      when E2 : others => Unexpected_Exception (E2, "A196");
      end Test_Signal;

   begin

      for I in Signal_Sources'Range loop
         for J in Signal_Sources'Range loop
            Assert ((I = J) = (Signal_Sources (I) = Signal_Sources (J)),
              "A197" & Integer'Image (I) & " " & Integer'Image (J));
         end loop;
         if Signal_Sources (I) = From_Send_Signal then
            Assert (not Has_Data (Signal_Sources (I)),
              "A198: source" & Integer'Image (I)
              & " has no data");
         else
            Assert (Has_Data (Signal_Sources (I)),
              "A199" & Integer'Image (I));
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
         Timeout : Duration := 2*DU;
         Info : Signal_Info;
         Set : Signal_Set;
         Installed_Empty_Handler : Boolean := False;
      begin
         Comment ("Testing " & Image (Sig));
         Add_Signal (Set, Sig);
         Block_Signals (All_Signal_Mask, Old_Mask);
         Set_Signal (Info, Sig);
         Set_Source (Info, From_Timer);
         Set_Data (Info, Signal_Data'(+10));
         Assert (Is_Member (Pending_Signals, Sig) = (Sig = SIGNULL), "A200");
         if Sig /= SIGNULL then
            Comment ("Enable queueing");
            if Try_Install_Empty_Handler (Sig) then
               Installed_Empty_Handler := True;
            end if;
            Enable_Queueing (Sig);
         end if;
         for I in Test_Range loop
               Comment ("Queue_Signal " & Image (Sig) & " :"
                 & Integer'Image (I));
               Queue_Signal (Get_Process_ID, Sig, +I);
         end loop;

         ---------------------------------------------------------------
         --  If the parameter Sig is equal to Signal_Null, no signal
         --  shall be queued, but error checking shall be performed.
         --  [3.3.19]

         if Sig = Signal_Null then
            Comment ("Expect to timeout on SIGNULL");
            begin
               Info :=
                 Try_Await_Signal (Sig, Set, Timeout, Yes, "A201");
            exception
            when Local_Failure => null;
            when E1 : POSIX_Error =>
               Comment ("TIMED OUT waiting for " & Image (Sig) & " (OK)");
               Check_Error_Code (EAGAIN, "A202: " & Image (Sig));
            when E2 : others => Unexpected_Exception (E2, "A203");
            end;
            return;
         end if;

         Comment ("Await signals");
         begin
            for I in Test_Range loop
               if Installed_Empty_Handler then
                  Info := Try_Await_Signal (Sig, Set, Timeout, No, "A204");
               else
                  Info := Try_Await_Signal (Sig, Set, Timeout, Maybe, "A205");
               end if;
               Assert (Get_Signal (Info) = Sig, "A206");
               Assert (Get_Source (Info) = From_Queue_Signal,
                 "A207: " & Image (Sig) & ' '
                 & Signal_Source'Image (Get_Source (Info)));
               Assert (Get_Data (Info) = +I, "A208: " & Image (Sig) & ' '
                 & Integer'Image (+Get_Data (Info)));
               Comment ("Get_Data (Info) = "
                 & Integer'Image (+Get_Data (Info)));
            end loop;
         exception
         when Local_Failure => null;
         when E : others =>
            Unexpected_Exception (E, "A209");
            Clear_Signal (Sig, "A210");
         end;
         Comment ("Disable queueing");
         Disable_Queueing (Sig);
         --  Data may still be queued, even if queuing is disabled.
         --  Either way, we should get at least one of the signals.
         for I in Test_Range loop
            Comment ("Queue_Signal " & Image (Sig) & " :"
              & Integer'Image (I));
            Queue_Signal (Get_Process_ID, Sig, +I);
         end loop;
         for I in Test_Range loop
            Set_Source (Info, From_Timer);
            Set_Data (Info, +0);
            begin
               if I = Test_Range'First and Installed_Empty_Handler then
                  Info := Try_Await_Signal (Sig, Set, Timeout, No, "A211");
               else
                  Info := Try_Await_Signal (Sig, Set, Timeout, Maybe, "A212");
               end if;
               Comment ("received signal: " & Integer'Image (I));
               Assert (Get_Signal (Info) = Sig, "A213");
               Assert (Get_Source (Info) = From_Queue_Signal
                 or Get_Source (Info) = From_Timer, "A214");

               ----------------------------------------------------------
               --  If queueing is not enabled the  Data attribute
               --  is undefined. [3.3.16.2]

            exception
            when Local_Failure => null;
            when E : others => Unexpected_Exception (E, "A215");
            end;
         end loop;

         if Installed_Empty_Handler then
            Unignore_Signal (Sig);
         end if;

      exception
      when E1 : POSIX_Error =>
         Optional (Realtime_Signals_Option,
           Operation_Not_Implemented, E1, "A216: " & Image (Sig));
      when E2 : others => Unexpected_Exception (E2, "A217");
      end Test_Signal;
   begin
      for Sig in 1 .. Signal'Last loop
         if not Fails_Blocking_Test (Sig)
           and then not (Default_Action (Sig) in Ignore .. Stop) then
            Test_Signal (Sig);
            --  Clear out any signals possibly left if test failed.
            Clear_Signal (Sig, "A218");
            --  Repeat the test, to make sure resources can be reused.
            Test_Signal (Sig);
            --  Clear out any signals possibly left if test failed.
            Clear_Signal (Sig, "A219");
         end if;
      end loop;
   exception
   when E1 : POSIX_Error =>
      Optional (Realtime_Signals_Option,
        Operation_Not_Supported, E1, "A220");
   when E2 : others => Unexpected_Exception (E2, "A221");
   end;

   Unblock_Signals (All_Signal_Mask, Old_Mask);

   ---------------------------------------------------------------------
   --  .... It would be good to also check the behavior if queuing
   --  is not enabled.

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
                    "A222");
            when Signal_Hangup =>
               Assert (Signal_Hangup_Ref = Signal_Reference (SIGHUP),
                    "A223");
            when Signal_Interrupt =>
               Assert (Signal_Interrupt_Ref = Signal_Reference (SIGINT),
                    "A224");
            when Signal_Pipe_Write =>
               Assert (Signal_Pipe_Write_Ref = Signal_Reference (SIGPIPE),
                    "A225");
            when Signal_Quit =>
               Assert (Signal_Quit_Ref = Signal_Reference (SIGQUIT),
                    "A226");
            when Signal_Terminate =>
               Assert (Signal_Terminate_Ref = Signal_Reference (SIGTERM),
                    "A227");
            when Signal_User_1 =>
               Assert (Signal_User_1_Ref = Signal_Reference (SIGUSR1),
                    "A228");
            when Signal_User_2 =>
               Assert (Signal_User_2_Ref = Signal_Reference (SIGUSR2),
                    "A229");
            when Signal_Child =>
               Assert (Signal_Child_Ref = Signal_Reference (SIGCHLD),
                    "A230");
            when Signal_Continue =>
               Assert (Signal_Continue_Ref = Signal_Reference (SIGCONT),
                    "A231");
            when Signal_Terminal_Stop =>
               Assert (Signal_Terminal_Stop_Ref = Signal_Reference (SIGTSTP),
                    "A232");
            when Signal_Terminal_Input =>
               Assert (Signal_Terminal_Input_Ref = Signal_Reference (SIGTTIN),
                    "A233");
            when Signal_Terminal_Output =>
               Assert (Signal_Terminal_Output_Ref = Signal_Reference (SIGTTOU),
                    "A234");
            when others =>
               null;
            end case;
         exception
         when E1 : POSIX_Error =>
            if Is_Reserved_Signal (Sig) then
               Check_Error_Code (Invalid_Argument, "A235");
            else
               Unexpected_Exception (E1, "A236: " & Image (Sig));
            end if;
         when E2 : others => Unexpected_Exception (E2, "A237");
         end;
      exception
      when E1 : POSIX_Error =>
         if Is_Supported (Signal_Entries_Option)
           and then Get_Error_Code /= Invalid_Argument
         then
            Unexpected_Exception (E1, "A238");
         end if;
      when E2 : others => Unexpected_Exception (E2, "A239");
      end Test_Signal;
   begin
      for Sig in Signal loop
         if not Fails_Blocking_Test (Sig) then
            Test_Signal (Sig);
         end if;
      end loop;
   exception
   when E : others => Unexpected_Exception (E, "A240");
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
         else Check_Error_Code (No_Such_Process, "A241");
         end if;
      when E : others => Unexpected_Exception (E, "A242");
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
         else Check_Error_Code (No_Such_Process, "A243");
         end if;
      when E : others => Unexpected_Exception (E, "A244");
      end;

   exception
   when E : others => Unexpected_Exception (E, "A245");
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
      Check_Error_Code (Invalid_Argument, "A246");
   when Program_Error =>
      Comment ("raised Program_Error (correctly)");
   when E : others => Unexpected_Exception (E, "A247");
   end;

   --  Active tests of Interrupt_Task are in other programs.

   ---------------------------------------------------------------------

   Done;
exception
   when E : others => Fatal_Exception (E, "A248");
end p030300;

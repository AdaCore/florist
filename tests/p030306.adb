------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                             P 0 3 0 3 0 6                                --
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

with p030300a,
     POSIX,
     POSIX_Configurable_System_Limits,
     POSIX_Process_Identification,
     POSIX_Process_Primitives,
     POSIX_Report,
     POSIX_Signals,
     System,
     System.Storage_Elements;

procedure p030306 is
   use  p030300a,
        POSIX,
        POSIX_Process_Identification,
        POSIX_Process_Primitives,
        POSIX_Report,
        POSIX_Signals,
        System;

   Child_Pathname : constant POSIX_String := "./p030306a";

   procedure Test_Signal (Sig : Signal);
   procedure Await_Normal_Child_Termination
     (Child_ID : Process_ID;
      Sig   : Signal;
      Default : Boolean := False);

   procedure Await_Normal_Child_Termination
     (Child_ID : Process_ID;
      Sig   : Signal;
      Default : Boolean := False) is
      Status : Termination_Status;
   begin
      --  Delay long enough for child to receive signal or
      --  terminate.  5 * DU should be long enough, even if
      --  signal is not received, since child internal
      --  timeout is 2 * DU.
      delay 5 * DU;
      if Default and then Default_Action (Sig) = Stop then
         --  Verify that child is stopped.
         Wait_For_Child_Process
           (Status => Status,
            Block => False,
            Trace_Stopped => True);
         Assert (Status_Available (Status), "A001");
         Assert (Process_ID_Of (Status) = Child_ID, "A002");
         Assert (Termination_Cause_Of (Status) = Stopped_By_Signal, "A003");
         Assert (Stopping_Signal_Of (Status) = Sig, "A004");
         --  Allow child to continue.
         Send_Signal (Child_ID, Signal_Continue);
         --  Allow time for child to terminate.
         delay DU;
      end if;
      --  Kill child if it has not terminated normally.
      Send_Signal (Child_ID, Signal_Kill);
      --  Wait for child to terminate.
      delay DU;
      --  Should not block here, since time delays should have
      --  arranged for child to terminate by now.
      Wait_For_Child_Process (Status, Child_ID);
      if Default then
         case Default_Action (Sig) is
         when Unspecified =>
            null;
         when Ignore
            | Continue
            | Stop =>
            Check_Child_Status (Status, Child_ID, 0, "A008: " & Image (Sig));
         when Termination =>
            if not Status_Available (Status) then
               --  Fail when status not available
               Fail ("A009: no status available");
               return;
            end if;
            Assert (Process_ID_Of (Status) = Child_ID,
              "A010: wrong child");
            if Termination_Cause_Of (Status) /= Terminated_By_Signal then
               --  Fail when did not exit
               Assert (False, "A011: not terminated by signal");
               return;
            end if;
            declare
               The_Sig : Signal;
            begin
               The_Sig := Termination_Signal_Of (Status);
               Assert (Sig = The_Sig, "A012");
            exception
            when E : others => Unexpected_Exception (E, "A013");
            end;
         end case;
      else
         Check_Child_Status (Status, Child_ID, 0, "A014: " & Image (Sig));
      end if;
   exception when E : others => Unexpected_Exception (E, "A015");
   end Await_Normal_Child_Termination;

   procedure Test_Signal (Sig : Signal) is
      Template : Process_Template;
      Mask     : Signal_Set;  --  initially empty
      Args     : POSIX_String_List; --  initially empty
      Child_ID : Process_ID;

   begin
      Open_Template (Template);
      Add_Signal (Mask, Sig);
      Set_Signal_Mask (Template, Mask);
      for I in Block_And_Await .. Unblock_And_Unignore loop
         Make_Empty (Args);
         Append (Args, "");
         Append (Args, "-child"
           & To_POSIX_String (Integer'Image (Child_Action'Pos (I))));
         Append (Args, "-sig"
           & To_POSIX_String (Signal'Image (Sig)));
         Pass_Through_Verbosity (Args);
         Start_Process
           (Child => Child_ID,
            Pathname => Child_Pathname,
            Template => Template,
            Arg_List => Args);
         case I is
         when Block_And_Await =>
            --  Child will wait for Sig, with just Sig blocked;
            --  expecting to receive signal and then exit normally.
            --  Wait for child to get ready to receive signal.
            delay DU;
            Send_Signal (Child_ID, Sig);
            Await_Normal_Child_Termination (Child_ID, Sig);
         when Block_And_Await_With_Info =>
            --  Child will wait for Sig, with just Sig blocked;
            --  expecting to receive signal with info and then exit normally.
            --  Wait for child to get ready to receive signal.
            delay DU;
            begin
               Queue_Signal (Child_ID, Sig, +999);
            exception
            when E1 : POSIX_Error =>
               Optional (Realtime_Signals_Option,
                 Operation_Not_Implemented, E1, "A016");
            when E2 : others => Unexpected_Exception (E2, "A017");
            end;
            Await_Normal_Child_Termination (Child_ID, Sig);
         when Block_And_Await_With_No_Info =>
            --  Child will wait for Sig, with just Sig blocked;
            --  expecting to receive signal with no info
            --  and then exit normally.
            --  Wait for child to get ready to receive signal.
            delay DU;
            Send_Signal (Child_ID, Sig);
            Await_Normal_Child_Termination (Child_ID, Sig);
         when Unblock_And_Ignore =>
            --  Child will delay, with all signals unblocked;
            --  expecting to time out without receiving signal
            --  and then exit normally.
            --  Wait for child to get ready to receive signal.
            delay DU;
            Send_Signal (Child_ID, Sig);
            Await_Normal_Child_Termination (Child_ID, Sig);
         when Block_Unignore_And_Await =>
            --  Child will wait for Sig, with just Sig blocked;
            --  expecting to receive signal and then exit normally.
            --  Wait for child to get ready to receive signal.
            delay DU;
            Send_Signal (Child_ID, Sig);
            Await_Normal_Child_Termination (Child_ID, Sig);
         when Unblock_And_Unignore =>
            --  Child will delay, with all signals unblocked;
            --  expecting to receive signal and perform default action.
            --  Wait for child to get ready to receive signal.
            delay DU;
            Send_Signal (Child_ID, Sig);
            Await_Normal_Child_Termination
              (Child_ID, Sig, Default => True);
         when others => Fatal ("A000: invalid child action");
         end case;
      end loop;
      Close_Template (Template);
   exception
   when E : others => Unexpected_Exception (E, "A018");
   end Test_Signal;

begin

   Header ("p030306");

   ----------------------------------------------------------------------

   if not POSIX_Configurable_System_Limits.Job_Control_Is_Supported then
      for Sig in Signal loop
         --  Do_Not_Test should be initially empty.
         Assert (not Is_Member (Do_Not_Test, Sig), "A019");
         if Is_Reserved (Sig)
           or else Default_Action (Sig) = Unspecified then
            Add_Signal (Do_Not_Test, Sig);
         end if;
      end loop;
      for I in Job_Control_Signals'Range loop
         Add_Signal (Do_Not_Test, Job_Control_Signals (I));
      end loop;
   end if;

   for Sig in Signal loop
      if not Is_Member (Do_Not_Test, Sig) then
         Test_Signal (Sig);
      end if;
   end loop;

   ---------------------------------------------------------------------

   Done;
exception
   when E : others => Fatal_Exception (E, "A020");
end p030306;

------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                            P 0 3 0 3 0 6 a                               --
--                                                                          --
--                                B o d y                                   --
--                                                                          --
--                                                                          --
--  Copyright (c) 1998      Florida  State  University  (FSU).  All Rights  --
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

--  Child process for test p030306.

with POSIX,
     p030300a,
     POSIX_Report,
     POSIX_Signals;

procedure p030306a is
   use  POSIX,
        p030300a,
        POSIX_Report,
        POSIX_Signals;

   Mask,
   Old_Mask : Signal_Set;
   Sig,
   The_Sig  : Signal;
   Info     : Signal_Info;

begin

   Comment ("child: starting "
     & Child_Action'Image (Child_Action'Val (Child)));

   --  Argument Child is a signal number passed to this
   --  process by its parent.

   Sig := Arg_Sig;

   ---------------------------------------------------------------
   --  The initial signal mask of the child process shall
   --  be the set specified by the Signal Mask attribute of the
   --  process template.  For this test, the parent process should
   --  have specified that only Sig is masked.

   Mask := Blocked_Signals;
   for I in Signal loop
      if I = Sig then
         Assert (Is_Member (Mask, I),
           "A001: p030306a " & POSIX_Signals.Image (I));
      elsif not Is_Reserved (I) then
         Assert (not Is_Member (Mask, I), "A002: p030306a " & Image (I));
      end if;
   end loop;

   Delete_All_Signals (Mask);

   case Child_Action'Val (Child) is
   when Block_And_Await =>
      --  Wait for Sig, with just Sig blocked;
      --  expect to receive signal and then exit normally.
      Add_Signal (Mask, Sig);
      Set_Blocked_Signals (Mask, Old_Mask);
      Comment ("child: waiting for " & Image (Sig));
      The_Sig := Await_Signal (Mask);
      Comment ("child: received " & Image (Sig) & " (OK)");
      Assert (The_Sig = Sig, "A003: p030306a " & Image (The_Sig));
   when Block_And_Await_With_Info =>
      --  Wait for Sig, with just Sig blocked;
      --  expect to receive signal with info and then exit normally.
      Add_Signal (Mask, Sig);
      Set_Blocked_Signals (Mask, Old_Mask);
      Comment ("child: waiting for " & Image (Sig));
      begin
         Enable_Queueing (Sig);
         Info := Await_Signal (Mask);
         Comment ("child: received " & Image (Sig) & " (OK)");
         Assert (Get_Signal (Info) = Sig, "A006: p030306a");
         Assert (Get_Source (Info) = From_Queue_Signal, "A007: p030306a");
         Assert (Get_Data (Info) = +999, "A008: p030306a");
      exception
      when E1 : POSIX_Error =>
         Optional (Realtime_Signals_Option,
           Operation_Not_Supported, E1, "A004: p030306a");
         --  If Enable_Queueing succeeds, then Await_Signal is not
         --  allowed to fail with Operation_Not_Implemented.
      when E2 : others => Unexpected_Exception (E2, "A005: p030306a");
      end;
   when Block_And_Await_With_No_Info =>
      --  Wait for Sig, with just Sig blocked;
      --  expect to receive signal with no info and then exit normally.
      Add_Signal (Mask, Sig);
      Set_Blocked_Signals (Mask, Old_Mask);
      Comment ("child: waiting for " & Image (Sig));
      begin
         Enable_Queueing (Sig);
         Info := Await_Signal (Mask);
         Comment ("child: received " & Image (Sig) & " (OK)");
         Assert (Get_Source (Info) = From_Send_Signal, "A011: p030306a");
      exception
      when E1 : POSIX_Error =>
         Optional (Realtime_Signals_Option,
           Operation_Not_Supported, E1, "A009: p030306a");
      when E2 : others => Unexpected_Exception (E2, "A010: p030306a");
      end;
      --  If Enable_Queueing succeeds, then Await_Signal is not
      --  allowed to fail with Operation_Not_Implemented.
   when Unblock_And_Ignore =>
      --  Delay, with all signals unblocked;
      --  expect to time out without receiving signal
      --  and then exit normally.
      Ignore_Signal (Sig);
      Set_Blocked_Signals (Mask, Old_Mask);
      --  Delay long enough for parent to send signal.
      delay 2 * DU;
   when Block_Unignore_And_Await =>
      --  Wait for Sig, with just Sig blocked;
      --  expect to receive signal and then exit normally.
      Unignore_Signal (Sig);
      Add_Signal (Mask, Sig);
      Set_Blocked_Signals (Mask, Old_Mask);
      The_Sig := Await_Signal (Mask);
      Comment ("child: received " & Image (Sig) & " (OK)");
      Assert (The_Sig = Sig, "A012: p030306a " & Image (The_Sig));
   when Unblock_And_Unignore =>
      --  Delay, with all signals unblocked;
      --  expect to receive signal and perform default action.
      Unignore_Signal (Sig);
      Set_Blocked_Signals (Mask, Old_Mask);
      --  Delay long enough for parent to send signal.
      delay 2 * DU;
   when others => Fatal ("A000: invalid child action");
   end case;

   -----------------------------------------------

   Comment ("child: exiting");

   Done;

exception
when E : others => Fatal_Exception (E, "A013: p030306a");
end p030306a;




------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                            P 0 3 0 3 0 0 a                               --
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

--  Common declarations used by tests of package POSIX_Signals.

with Ada.Command_Line;
with POSIX, POSIX_Report;

package body p030300a is

   use POSIX_Report;
   use Ada.Command_Line;

   --  Reserved_Signals contains the signals that are reserved.

   Reserved_Signals : Signal_Set;

   function Arg_Sig return Signal is
   begin
      for I in 1 .. Argument_Count loop
         if Argument (I)'Length >= 4 and then
           Argument (I)(Argument (I)'First .. Argument (I)'First + 3) = "-sig"
         then
            declare
               Arg : constant String := Argument (I);
               J : Integer := Arg'First + 4;
               Tmp : Integer := 0;
            begin
               while J <= Arg'Last and then Arg (J) = ' ' loop
                  J := J + 1;
               end loop;
               while J <= Arg'Last and then Arg (J) in '0' .. '9' loop
                  Tmp := Tmp * 10 +
                    Character'Pos (Arg (J)) - Character'Pos ('0');
                  J := J + 1;
               end loop;
               while J <= Arg'Last and then Arg (J) = ' ' loop
                  J := J + 1;
               end loop;
               if J /= Arg'Last + 1 then return 1;
               else return Signal (Tmp);
               end if;
            exception when others =>
               Fail ("bad command-line argument");
            end;
         end if;
      end loop;
      return Signal_Null;
   end Arg_Sig;

   --  Clear_Signal clears out any pending occurrences of Sig.

   procedure Clear_Signal (Sig : Signal; Msg : String) is
      Set, Old_Set : Signal_Set;
   begin
      if Sig = SIGNULL then return;
      end if;
      Add_Signal (Set, Sig);
      Ignore_Signal (Sig);
      Unblock_Signals (Set, Old_Set);
      Block_Signals (Old_Set, Set);
      Unignore_Signal (Sig);
   exception
   when E : others =>
      Unexpected_Exception (E, Msg & ": " & Image (Sig));
   end Clear_Signal;

   procedure Check_Time
     (Expect_Timeout : Yes_No_Maybe;
      Start_Time : POSIX.Timespec;
      Timeout : POSIX.Timespec;
      Msg : String);

   procedure Check_Time
     (Expect_Timeout : Yes_No_Maybe;
      Start_Time : POSIX.Timespec;
      Timeout : POSIX.Timespec;
      Msg : String) is
      Elapsed_Time : POSIX.Timespec;
      use POSIX;
   begin
      if Start_Time /= POSIX.To_Timespec (0.0) then
         Elapsed_Time := Get_Time (Clock_Realtime) - Start_Time;
         if Elapsed_Time > Timeout then
            Comment ("time delay", Elapsed_Time);
            if Expect_Timeout = Yes then
               Assert (Elapsed_Time > Timeout, Msg & "(a)");
            end if;
         end if;
      end if;
   end Check_Time;

   procedure Try_Await_Signal
     (Sig : Signal;
      --  the signal we are expecting
      New_Mask : Signal_Set;
      --  the set of signals to await
      Timeout : Duration;
      --  how long to wait before timing out
      Expect_Timeout : Yes_No_Maybe;
      --  whether we expect to time out
      Msg : String) is
      use POSIX;
      Expect_Timeout_Local : Yes_No_Maybe := Expect_Timeout;
      Installed_Empty_Handler : Boolean;
      Result : Signal;
      Start_Time : POSIX.Timespec := POSIX.To_Timespec (0.0);
   begin
      Assert ((Expect_Timeout /= No) or not Is_Ignored (Sig), Msg & "(b)");
      begin
         Start_Time := Get_Time (Clock_Realtime);
      exception
      when E1 : POSIX_Error =>
         Optional (Timers_Option, Operation_Not_Implemented,
           E1, Msg & "(c)");
      when E2 : others =>
         Unexpected_Exception (E2, Msg & "(d)");
      end;
      if Default_Action (Sig) in Ignore .. Stop and then
        not Is_Ignored (Sig) then
         if Try_Install_Empty_Handler (Sig) then
            Comment ("Installed empty handler for " & Image (Sig));
            Installed_Empty_Handler := True;
         else
            --  We expect the signal, if any, to be ignored.
            Expect_Timeout_Local := No;
         end if;
      end if;
      Result := Await_Signal_Or_Timeout (New_Mask, To_Timespec (Timeout));
      if Installed_Empty_Handler then
         Unignore_Signal (Sig);
      end if;
      Check_Time (No, Start_Time, To_Timespec (Timeout), Msg);
      if Expect_Timeout_Local = Yes then
         Expect_Exception (Msg & "(e): " & Image (Sig));
         raise Local_Failure;
      else
         Assert (Result = Sig, Msg & "(f): " & Image (Result));
      end if;
   exception
   when Local_Failure => raise;
   when E1 : POSIX_Error =>
      if Expect_Timeout_Local = No then
         Unexpected_Exception (E1, Msg & "(g)");
         raise Local_Failure;
      end if;
      if Get_Error_Code =  EAGAIN then
         Check_Time
           (Expect_Timeout_Local, Start_Time, To_Timespec (Timeout), Msg);
      else
         Check_Error_Code (EAGAIN, Msg & "(h): " & Image (Sig));
         raise Local_Failure;
      end if;
   when E2 : others => Unexpected_Exception (E2, Msg & "(i)");
      raise Local_Failure;
   end Try_Await_Signal;

   function Try_Await_Signal
     (Sig : Signal;
      New_Mask : Signal_Set;
      Timeout : Duration;
      Expect_Timeout : Yes_No_Maybe;
      Msg : String) return Signal_Info is
      use POSIX;
      Expect_Timeout_Local : Yes_No_Maybe := Expect_Timeout;
      Installed_Empty_Handler : Boolean;
      Start_Time : POSIX.Timespec := POSIX.To_Timespec (0.0);
      Info : Signal_Info;
   begin
      Assert ((Expect_Timeout /= No) or not Is_Ignored (Sig), Msg & "(b)");
      begin
         Start_Time := Get_Time (Clock_Realtime);
      exception
      when E1 : POSIX_Error =>
         Optional (Timers_Option, Operation_Not_Implemented,
           E1, Msg & "(c)");
      when E2 : others =>
         Unexpected_Exception (E2, Msg & "(d)");
      end;
      if Default_Action (Sig) in Ignore .. Stop and then
        not Is_Ignored (Sig) then
         if Try_Install_Empty_Handler (Sig) then
            Comment ("Installed empty handler for " & Image (Sig));
            Installed_Empty_Handler := True;
         else
            --  We expect the signal, if any, to be ignored.
            Expect_Timeout_Local := No;
         end if;
      end if;
      Info := Await_Signal_Or_Timeout (New_Mask, To_Timespec (Timeout));
      if Installed_Empty_Handler then
         Unignore_Signal (Sig);
      end if;
      Check_Time (No, Start_Time, To_Timespec (Timeout), Msg);
      if Expect_Timeout_Local = Yes then
         Expect_Exception (Msg & "(e): " & Image (Sig));
         raise Local_Failure;
      else
         Assert (POSIX_Signals.Get_Signal (Info) = Sig, Msg & "(f): "
           & Image (POSIX_Signals.Get_Signal (Info)));
      end if;
      return Info;
   exception
   when Local_Failure => raise;
   when E1 : POSIX_Error =>
      if Expect_Timeout_Local = No then
         Unexpected_Exception (E1, Msg & "(g)");
         raise Local_Failure;
      end if;
      if Get_Error_Code =  EAGAIN then
         Check_Time
          (Expect_Timeout_Local, Start_Time, To_Timespec (Timeout), Msg);
         return Info;
      else
         Check_Error_Code (EAGAIN, Msg & "(h): " & Image (Sig));
         raise Local_Failure;
      end if;
   when E2 : others => Unexpected_Exception (E2, Msg & "(i)");
      raise Local_Failure;
   end Try_Await_Signal;


begin

   Add_All_Signals (All_Signal_Mask);

   for Sig in Realtime_Signal loop
      Required_Default_Action (Sig) := Termination;
   end loop;

   for Sig in Signal loop
      Cannot_Be_Blocked (Sig) :=
        Is_Reserved_Signal (Sig) or Sig = SIGKILL or Sig = SIGSTOP;
   end loop;

end p030300a;

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
package body p030300a is

   use Ada.Command_Line;

   --  Reserved_Signals contains the signals that are reserved.

   Reserved_Signals : Signal_Set;

   function Is_Reserved (Sig : Signal) return Boolean is
   begin
      return Is_Member (Reserved_Signals, Sig);
   end Is_Reserved;

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
               if J /= Arg'Last + 1 or Tmp = 0 then return 1;
               else return Signal (Tmp);
               end if;
            exception when others =>
               Fail ("bad command-line argument");
            end;
         end if;
      end loop;
      return Signal_Null;
   end Arg_Sig;

begin

   Add_All_Signals (All_Signal_Mask);

   -----------------------------------------------------------------------
   --  The signals SIGABRT, SIGARLM, SIGFPR, SIGILL, SIGSEGV, and SIGBUS
   --  are reserved.  The implementation may reserve other signals whose
   --  names are not defined by the standard. [2.2.2.117].
   --  Await_Signal raises POSIX_Error is raised with Invalid_Argument
   --  for reserved signals. [3.3.15]

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

   for Sig in Signal loop
      if Is_Reserved_Signal (Sig) then
         Add_Signal (Reserved_Signals, Sig);
      end if;
   end loop;

   for Sig in Realtime_Signal loop
      Default_Action (Sig) := Termination;
   end loop;

end p030300a;






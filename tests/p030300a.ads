------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                            P 0 3 0 3 0 0 a                               --
--                                                                          --
--                                S p e c                                   --
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

with POSIX,
     POSIX_Report,
     POSIX_Signals,
     POSIX_Timers,
     System,
     System.Storage_Elements,
     Test_Parameters,
     Unchecked_Conversion;

package p030300a is

   use POSIX,
       POSIX_Report,
       POSIX_Signals,
       POSIX_Timers,
       System,
       System.Storage_Elements,
       Test_Parameters;

   pragma Warnings (Off);
   --  for now, just ignore warnings on 64 bits machines.
   function "+" is
      new Unchecked_Conversion (Integer, Signal_Data);
   function "+" is
      new Unchecked_Conversion (Signal_Data, Integer);
   pragma Warnings (On);
   function "+" is
      new Unchecked_Conversion (System.Address, Signal_Data);
   function "+" is
      new Unchecked_Conversion (Signal_Data, System.Address);
   function "+" is
      new Unchecked_Conversion (Timer_ID, Signal_Data);
   function "+" is
      new Unchecked_Conversion (Signal_Data, Timer_ID);

   type Signal_List is array (Positive range <>) of Signal;

   --  These special classes of signals are all defined in 3.3.3.1.

   Defined_Signals : Signal_List :=
     (SIGABRT, SIGALRM, SIGBUS,
      SIGFPE,  SIGHUP,  SIGILL,
      SIGINT,  SIGKILL, SIGPIPE,
      SIGQUIT, SIGSEGV, SIGTERM,
      SIGUSR1, SIGUSR2, SIGCHLD,
      SIGCONT, SIGSTOP, SIGTSTP,
      SIGTTIN, SIGTTOU);

   Job_Control_Signals : constant Signal_List :=
     (SIGCHLD, SIGCONT, SIGSTOP, SIGTSTP,
      SIGTTIN, SIGTTOU);

   type Signal_Action is
     (Unspecified, Ignore, Continue, Stop, Termination);

   Default_Action : array (Signal) of Signal_Action :=
     (SIGCHLD => Ignore,
      SIGCONT => Continue,
      SIGSTOP | SIGTSTP | SIGTTIN | SIGTTOU => Stop,
      SIGHUP  | SIGINT  | SIGKILL | SIGPIPE |
      SIGQUIT | SIGTERM | SIGUSR1 | SIGUSR2 => Termination,
      others => Unspecified);

   --  Default action for Realtime_Signal cannot be specified
   --  in aggregate, in case the range happens to be null.
   --  Hence this table is updated in the subprogram body.

   All_Signal_Mask : Signal_Set;
   --  All_Signals contains all the values of type Signal.

   DU : constant Duration := Test_Parameters.Delay_Unit;
   --  DU is a common unit of delay duration.

   function Is_Reserved (Sig : Signal) return Boolean;
   --  Is_Reserved returns True iff Sig is reserced

   function Arg_Sig return Signal;
   --  Arg_Sig returns the value NNN if there is a command-line
   --  argument of the form "-sig NNN".

   type Child_Action is
     (Not_Specified,
      Block_And_Await,
      Block_And_Await_With_Info,
      Block_And_Await_With_No_Info,
      Unblock_And_Ignore,
      Block_Unignore_And_Await,
      Unblock_And_Unignore);

   --  Not_Initially_Masked is a set of signals that this test
   --  discovers are not initially masked in all tasks, and so
   --  certain further tests cannot safely use these signals.
   --  It is up to the individual test to populat this set, if
   --  the test uses it.

   Not_Initially_Masked : Signal_Set;

   --  It is up to the individual test to populate Do_Not_Test,
   --  if it uses it.

   Do_Not_Test : Signal_Set;

end p030300a;






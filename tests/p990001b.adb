------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                            P 9 9 0 0 0 1 b                               --
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
--  AVAILABLE OR DISCLOSED,  OR THE OWNERSHIP,  MECHANTABILITY, OR FITNESS  --
--  FOR A PARTICULAR PURPOSE OF SAID MATERIAL.                              --
--                                                                          --
------------------------------------------------------------------------------
--  [$Revision$]

with P990000,
     POSIX,
     POSIX_Condition_Variables,
     POSIX_Mutexes,
     POSIX_Report;

package body P990001b is

   use P990000,
       POSIX,
       POSIX_Condition_Variables,
       POSIX_Mutexes,
       POSIX_Report;

   MA : POSIX_Mutexes.Attributes;
   IO_Mutex : Mutex;  --  protects IO
   IO_MutexD : Mutex_Descriptor;
   Sync_Mutex : Mutex;   --  protects startup and termination
   Sync_MutexD : Mutex_Descriptor;
   All_Go : Boolean := False;
   Done_Count : Integer := 0;
   C : Condition;
   CA : POSIX_Condition_Variables.Attributes;
   CD : Condition_Descriptor;

   procedure Do_Input (Load : Natural) is
   begin
      Lock (IO_MutexD);
      Do_Input (Load);
      Unlock (IO_MutexD);
   exception when E : others => Fatal_Exception (E, "A001: P990001b");
   end Do_Input;

   procedure Do_Output (Load : Natural) is
   begin
      Lock (IO_MutexD);
      Do_Output (Load);
      Unlock (IO_MutexD);
   exception when E : others => Fatal_Exception (E, "A002: P990001b");
   end Do_Output;

   procedure Start_All_Jobs is
   begin
      Lock (Sync_MutexD);
      All_Go := True;
      POSIX_Condition_Variables.Signal (CD);
      Unlock (Sync_MutexD);
   exception when E : others => Fatal_Exception (E, "A003: P990001b");
   end Start_All_Jobs;

   procedure Await_Start is
   begin
      Lock (Sync_MutexD);
      while not All_Go loop
         Wait (CD, Sync_MutexD);
      end loop;
      Unlock (Sync_MutexD);
   exception when E : others => Fatal_Exception (E, "A004: P990001b");
   end Await_Start;

   procedure Done_Job is
   begin
      Lock (Sync_MutexD);
      Done_Count := Done_Count + 1;
      POSIX_Condition_Variables.Signal (CD);
      Unlock (Sync_MutexD);
   exception when E : others => Fatal_Exception (E, "A005: P990001b");
   end Done_Job;

   procedure Await_All_Jobs_Done is
   begin
      Lock (Sync_MutexD);
      while Done_Count < Jobs'Last loop
         Wait (CD, Sync_MutexD);
      end loop;
      Done_Count := 0;
      All_Go := False;
      Unlock (Sync_MutexD);
   exception when E : others => Fatal_Exception (E, "A006: P990001b");
   end Await_All_Jobs_Done;

   procedure Finalize is
   begin
      null;
   end Finalize;

begin
   Optional (Mutex_Option, "A007: P990001b");
   begin
      Initialize (MA);
      Set_Locking_Policy (MA, Highest_Ceiling_Priority);
      Initialize (IO_Mutex, MA);
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code /= Operation_Not_Supported then
         Optional (Mutex_Option, Mutex_Priority_Ceiling_Option,
           Operation_Not_Implemented, E1, "A008: P990001b");
      end if;
      Initialize (MA);
      Initialize (IO_Mutex, MA);
   end;
   IO_MutexD := Descriptor_Of (IO_Mutex);
   begin
      Initialize (MA);
      Set_Locking_Policy (MA, Highest_Ceiling_Priority);
      Initialize (Sync_Mutex, MA);
      Comment ("initialized Sync_Mutex w/priority ceiling");
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code /= Operation_Not_Supported then
         Optional (Mutex_Option, Mutex_Priority_Ceiling_Option,
           Operation_Not_Implemented, E1, "A009: P990001b ");
      end if;
      Initialize (MA);
      Initialize (Sync_Mutex, MA);
      Comment ("initialized Sync_Mutex w/o priority ceiling");
   end;
   Sync_MutexD := Descriptor_Of (Sync_Mutex);
   Comment ("initialized Sync_MutexD");
   Initialize (CA);
   Initialize (C, CA);
   CD := Descriptor_Of (C);
exception
when E : others => Fatal_Exception (E, "A010: P990001b");
end P990001b;


----------------------
-- REVISION HISTORY --
----------------------

--  ----------------------------
--  revision 1.5
--  date: 1998/06/18 01:51:14;  author: jiankuyu;  state: Exp;  lines: +11 -9
--  added exception when E : ....
--  ----------------------------
--  revision 1.6
--  date: 1998/06/28 21:36:22;  author: baker;  state: Exp;  lines: +112 -121
--  Restructured p9900** series of tests completely, to use
--  generic package.
--  ----------------------------
--  revision 1.7
--  date: 1998/06/30 13:27:24;  author: baker;  state: Exp;  lines: +16 -11
--  Added Finalize.
--  ----------------------------
--  New changes after this line.  Each line starts with: "--  "

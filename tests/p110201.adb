------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                             P 1 1 0 2 0 1                                --
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
--------------------------------------------------------------------------
--  [$Revision$]

--  Test for POSIX_Mutexes package

--  This test starts 4 tasks, inside each task is a section blocked of by
--  mutex-lock and unlock.  This section sets the value of a protected
--  variable then calls a delay, to allow the other tasks to run, then
--  checks to see that the variable is still set properly.

with POSIX,
     POSIX_Mutexes,
     POSIX_Report;

procedure p110201 is
   use POSIX,
       POSIX_Mutexes,
       POSIX_Report;

   task type Shared_Mutex_Task (number : integer)is
     entry StartRunning;
   end Shared_Mutex_Task;

   Mutex_Error : exception;
   Task1 : Shared_Mutex_Task (number => 1);
   Task2 : Shared_Mutex_Task (number => 2);
   Task3 : Shared_Mutex_Task (number => 3);
   Task4 : Shared_Mutex_Task (number => 4);
   M : Mutex;
   MD : Mutex_Descriptor;
   Attr : Attributes;
   protected_var : integer;

   task body Shared_Mutex_Task is
      count : integer;
   begin
      accept StartRunning;

      if (number = 1) then
         while count <= 100 loop
            Lock (MD);
            count := count + 1;
            protected_var := 1;
            delay 0.1;
            if (protected_var /= 1) then
               raise Mutex_Error;
            end if;
            Unlock (MD);
         end loop;

      elsif (number = 2) then
         while count <= 100 loop
            Lock (MD);
            count := count + 1;
            protected_var := 2;
            delay 0.1;
            if (protected_var /= 2) then
               raise Mutex_Error;
            end if;
            Unlock (MD);
         end loop;

      elsif (number = 3) then
         while count <= 100 loop
            Lock (MD);
            count := count + 1;
            protected_var := 3;
            delay 0.1;
            if (protected_var /= 3) then
               raise Mutex_Error;
            end if;
            Unlock (MD);
         end loop;

      elsif (number = 4) then
         while count <= 100 loop
            Lock (MD);
            count := count + 1;
            protected_var := 4;
            delay 0.1;
            if (protected_var /= 4) then
               raise Mutex_Error;
            end if;
            Unlock (MD);
         end loop;
      end if;

   exception
   when E1 : Mutex_Error =>
      Unexpected_Exception (E1, "A001: failure of mutual exclusion");
   when E2 : POSIX_Error =>
      Optional (Mutex_Option, Operation_Not_Implemented, E2, "A002");
   when E3 : others =>
      Unexpected_Exception (E3, "A003");
   end Shared_Mutex_Task;

begin

   Header ("p110201.adb", true);

   protected_var := 0;
   Initialize (Attr);
   Set_Process_Shared (Attr, true);
   Initialize (M, Attr);
   Finalize (Attr);
   MD := Descriptor_Of (M);

   Comment ("This test may take some time to run.");

   begin

      Task1.StartRunning;
      Task2.StartRunning;
      Task3.StartRunning;
      Task4.StartRunning;

      while not (Task1'Terminated and Task2'Terminated and
                 Task3'Terminated and Task4'Terminated) loop
         delay 0.2;
      end loop;

   end;

   Finalize (M);
   Done;

exception
when E1 : POSIX_Error =>
   Optional (Mutex_Option, Operation_Not_Implemented, E1, "A004");
when E2 : others => Unexpected_Exception (E2, "A005");

end p110201;

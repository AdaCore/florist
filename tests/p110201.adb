------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                             P 1 1 0 2 0 1                                --
--                                                                          --
--                                B o d y                                   --
--                                                                          --
--                                                                          --
--  Copyright (c) 1995-1999 Florida  State  University  (FSU).  All Rights  --
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

--  There are four tasks.  Inside each task is a critical section,
--  enclosed by mutex lock and unlock calls, which sets the value of
--  a protected variable then calls a delay, to allow the other tasks
--  to run, then checks to see that the variable is still set properly.

with POSIX,
     POSIX_Mutexes,
     POSIX_Report,
     Test_Parameters;

procedure p110201 is
   use POSIX,
       POSIX_Mutexes,
       POSIX_Report,
       Test_Parameters;

   task type Shared_Mutex_Task (Task_Number : Integer) is
     entry Start_Running;
   end Shared_Mutex_Task;

   Task1 : Shared_Mutex_Task (Task_Number => 1);
   Task2 : Shared_Mutex_Task (Task_Number => 2);
   Task3 : Shared_Mutex_Task (Task_Number => 3);
   Task4 : Shared_Mutex_Task (Task_Number => 4);
   M     : Mutex;
   MD    : Mutex_Descriptor;
   Attr  : Attributes;
   Shared_Var : Integer;

   task body Shared_Mutex_Task is
      Count : Integer := 0;
   begin
      accept Start_Running;
      while Count <= 20 loop
         Lock (MD);
         Count := Count + 1;
         Shared_Var := Task_Number;
         delay Delay_Unit;
         Comment ("task" & Integer'Image (Task_Number) &
           " in critical section:" & Integer'Image (Count));
         if (Shared_Var /= Task_Number) then
            Unlock (MD);
            Fail ("A001: failure of mutual exclusion");
            exit;
         end if;
         Unlock (MD);
         delay Delay_Unit * Task_Number;
      end loop;
      Comment ("task" & Integer'Image (Task_Number) & " exiting");
   exception
   when E1 : POSIX_Error =>
      Optional (Mutex_Option, Operation_Not_Implemented, E1, "A002");
   when E2 : others =>
      Unexpected_Exception (E2, "A003");
   end Shared_Mutex_Task;

begin

   Header ("p110201", True);

   Shared_Var := 0;

   Initialize (Attr);

   --  ... It would be better to write a separate test, that uses
   --  multiple processes and shared memory to really test the
   --  process-shared option for mutexes.
   begin
      Set_Process_Shared (Attr, Is_Shared => True);
   exception
   when E1 : POSIX_Error =>
      Optional (Process_Shared_Option, Operation_Not_Implemented, E1, "A004");
   when E2 : others =>
      Unexpected_Exception (E2, "A005");
   end;

   Initialize (M, Attr);
   Finalize (Attr);
   MD := Descriptor_Of (M);

   Comment ("This test may take some time to run.");

   begin

      Task1.Start_Running;
      Task2.Start_Running;
      Task3.Start_Running;
      Task4.Start_Running;

      while not (Task1'Terminated and Task2'Terminated and
                 Task3'Terminated and Task4'Terminated) loop
         delay 0.2;
      end loop;
      Comment ("all tasks terminated");

   end;

   Finalize (M);
   Done;

exception
when E1 : POSIX_Error =>
   Optional (Mutex_Option, Operation_Not_Implemented, E1, "A004");
   abort Task1, Task2, Task3, Task4;
when E2 : others => Unexpected_Exception (E2, "A005");
   abort Task1, Task2, Task3, Task4;
end p110201;

------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                             P 0 6 0 2 0 0                                --
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

--  Test file locking operations.

with Ada.Text_IO,
     POSIX,
     POSIX_Files,
     POSIX_File_Locking,
     POSIX_IO,
     POSIX_Process_Identification,
     POSIX_Process_Primitives,
     POSIX_Report,
     POSIX_Signals,
     POSIX_Unsafe_Process_Primitives;

use Ada.Text_IO,
     POSIX,
     POSIX_Files,
     POSIX_File_Locking,
     POSIX_IO,
     POSIX_Process_Identification,
     POSIX_Process_Primitives,
     POSIX_Report,
     POSIX_Signals,
     POSIX_Unsafe_Process_Primitives;

procedure p060200 is

   Test_fd : File_Descriptor;  -- for test_file
   Child : Process_ID;

   --  The Pause procedure Can Be Rewritten As An Infinite loop if Necessary.
   procedure Pause;
   procedure C_Pause;
   pragma Import (C, C_Pause, "pause");

   procedure Pause is
   begin
      loop
         C_Pause;
      end loop;
   end Pause;

begin

   Header ("p060200");
   Test ("package POSIX_File_Locking [6.2]");

   ---------------------------------------------------------

   --  Test requires us to have a file called "test_file" with a
   --  content of 6 characters. For example, hello(NEWLINE).
   Comment ("create test_file");
   declare
      Test_File : File_Type;
   begin
      Create (Test_File, Out_File, "test_file");
      Put (Test_File, "hello");
      Close (Test_File);
   end;

   ---------------------------------------------------------

   Test ("create child process to hold lock");
   declare
      Read_Fd,                    -- one end of pipe
      Write_Fd : File_Descriptor; -- other end of pipe
      Last : IO_Count;
      Buf : POSIX_String (1 .. 5);
   begin
      Test_fd := Open ("test_file", Read_Write);
      Create_Pipe (Read_Fd, Write_Fd);
      Child := Fork;
      if Child = Null_Process_ID then
         Set_Lock  (Test_fd,  (Whole_File => False, Lock => Read_Lock,
         Starting_Point => From_Beginning,
           Start => 3, Length => 2));
         Comment ("child has read lock on 3..4");
         Close (Read_Fd);
         Write (Write_Fd, "Ready", Last, No_Signals);
         Close (Write_Fd);
         Pause;
         --  Fail because of a runaway child
         Fail ("A001");
         Exit_Process (0);
      end if;

      Close (Write_Fd);
      --  should block until child process has locked 3..4
      Read (Read_Fd, Buf, Last, No_Signals);
      Close (Read_Fd);
      Assert (Buf (1 .. Integer (Last)) = "Ready", "A002");
      --  Now The File Has A Read Lock.
   exception when E : others => Fatal_Exception (E, "A003");
   end;

   ---------------------------------------------------------

   Test ("no write lock conflict on 0..2 from beginning");
   begin
      Set_Lock (Test_fd, (False, Write_Lock, From_Beginning, 0, 3));
   exception when E : others => Unexpected_Exception (E, "A004");
   end;

   ---------------------------------------------------------

   Test ("no write lock conflict on 0..2 from end");
   begin
      Set_Lock (Test_fd, (False, Write_Lock, From_End_Of_File, -6, 3));
   exception when E : others => Unexpected_Exception (E, "A005");
   end;

   ---------------------------------------------------------

   Test ("no write lock conflict on 5..5 from beginning");
   begin
      Set_Lock (Test_fd, (False, Write_Lock, From_Beginning, 5, 1));
   exception when E : others => Unexpected_Exception (E, "A006");
   end;

   ---------------------------------------------------------

   Test ("no write lock conflict on 5..5 from end");
   begin
      Set_Lock (Test_fd, (False, Write_Lock, From_End_Of_File, -1, 1));
   exception when E : others => Unexpected_Exception (E, "A007");
   end;

   ---------------------------------------------------------

   Test ("write lock conflict on 3..4 from end");
   declare
      EC : Error_Code;
   begin
      Set_Lock (Test_fd, (False, Write_Lock, From_End_Of_File, -3, 2));
      Assert (False, "A008");
   exception
   when E1 : POSIX.POSIX_Error =>
      --  P1003.1c says either EACCES (Permission_Denied) or
      --  EAGAIN (Resource_Temporarily_Unavailable) may be returned.
      --  .... Change POSIX.5?
      EC := POSIX.Get_Error_Code;
      if EC /= Resource_Temporarily_Unavailable
         and EC /= Permission_Denied then
         --  Fail because expected EACCES or EAGAIN
         Unexpected_Exception (E1, "A009");
      end if;
   when E2 : others => Unexpected_Exception (E2, "A010");
   end;

   ---------------------------------------------------------

   Test ("write lock conflict on 3..4 from beginning");
   declare
      EC : Error_Code;
   begin
      Set_Lock (Test_fd,  (False, Write_Lock, From_Beginning, 3, 2));
      Assert (False, "A011");
   exception
   when E1 : POSIX_Error =>
      EC := POSIX.Get_Error_Code;
      if EC /= Resource_Temporarily_Unavailable
         and EC /= Permission_Denied then
         --  Fail because expected EACCES or EAGAIN
         Unexpected_Exception (E1, "A012");
      end if;
   when E2 : others => Unexpected_Exception (E2, "A013");
   end;

   ---------------------------------------------------------

   Test ("info about write lock conflict on 3..3");
   declare
      Locker : Process_ID;
      Lock1  : File_Lock;
   begin
      Get_Lock (Test_fd,  (False, Write_Lock, From_End_Of_File, -3, 1),
        Lock1, Locker);
      if Locker = Null_Process_ID then Fail ("No Lock Found");
      else
         --  Checking Locking Process
         Assert (Locker = Child, "A014");
         --  Checking Read Lock
         Assert (Lock1.Lock = Read_Lock, "A015");
         if Lock1.Whole_File then Fail ("Whole File Locked");
         elsif Lock1.Starting_Point /= From_Beginning
              or Lock1.Start /= 3
              or Lock1.Length /= 2 then
            Fail ("A016");
         end if;
      end if;
   exception when E : others => Unexpected_Exception (E, "A017");
   end;

   ---------------------------------------------------------

   Test ("no read lock conflict on 3..3");
   declare
      Locker : Process_ID;
      Lock1  : File_Lock;
   begin
      Get_Lock (Test_fd,  (False, Read_Lock, From_End_Of_File, -3, 1),
        Lock1, Locker);
      Assert (Locker = Null_Process_ID, "A018");
   exception when E : others => Unexpected_Exception (E, "A019");
   end;

   ---------------------------------------------------------

   Test ("no read lock conflict on 3..4 from beginning");
   begin
      Set_Lock (Test_fd, (False, Read_Lock, From_Beginning, 3, 2));
   exception when E : others => Unexpected_Exception (E, "A020");
   end;

   ---------------------------------------------------------

   Test ("no read lock conflict on whole file");
   begin
      Set_Lock (Test_fd, (True, Read_Lock));
   exception when E : others => Unexpected_Exception (E, "A021");
   end;

   ---------------------------------------------------------

   Test ("write lock conflict for whole file");
   declare
      EC : Error_Code;
   begin
      Set_Lock (Test_fd, (True, Write_Lock));
      Assert (False, "A022");
   exception
   when E1 : POSIX_Error =>
      EC := POSIX.Get_Error_Code;
      if EC /= Resource_Temporarily_Unavailable
         and EC /= Permission_Denied then
         --  Fail because expected EACCES or EAGAIN
         Unexpected_Exception (E1, "A023");
      end if;
   when E2 : others => Unexpected_Exception (E2, "A024");
   end;

   ---------------------------------------------------------

   Test ("release read lock on whole file");
   begin
      Set_Lock (Test_fd, (True, Unlock));
   exception when E : others => Unexpected_Exception (E, "A025");
   end;

   ---------------------------------------------------------

   Test ("kill child holding read lock");
   declare
      Status : Termination_Status;
   begin
      Send_Signal (Child, Signal_Kill);
      Wait_For_Child_Process (Status, Child);
      Assert (Termination_Cause_Of (Status) = Terminated_By_Signal, "A026");
      Assert (Termination_Signal_Of (Status) = Signal_Kill, "A027");
   exception when E : others => Unexpected_Exception (E, "A028");
   end;

   ---------------------------------------------------------

   Test ("no conflict for write lock on whole file");
   begin
      Set_Lock (Test_fd, (True, Write_Lock));
   exception when E : others => Unexpected_Exception (E, "A029");
   end;

   ---------------------------------------------------------

   Test ("new child must wait to lock file");
   declare
      Status : Termination_Status;
   begin
      Child := Fork;
      if Child = Null_Process_ID then
         Comment ("child waiting for write lock on 3..4");
         Wait_To_Set_Lock (Test_fd, (True, Write_Lock));
         Exit_Process (0);
      end if;
      --  give child time to try to lock the file
      Comment ("we just hope the child gets time to run");
      --      delay 0.1;
      --  make sure the child is still there
      Comment ("checking child status");
      Wait_For_Child_Process (Status, Child, Block => False);
      Comment ("got child status");
      Assert (not Status_Available (Status), "A030");
   exception when E : others => Unexpected_Exception (E, "A031");
   end;

   ---------------------------------------------------------

   Test ("release whole-file lock");
   begin
      Set_Lock (Test_fd,  (True, Unlock));
   exception when E : others => Unexpected_Exception (E, "A032");
   end;

   ---------------------------------------------------------

   Test ("child can now get lock and exit");
   declare
      Status : Termination_Status;
   begin
      Wait_For_Child_Process (Status, Child);
      Assert (Termination_Cause_Of (Status) = Exited, "A033");
      Assert (Exit_Status_Of (Status) = 0, "A034");
   exception when E : others => Unexpected_Exception (E, "A035");
   end;

   ---------------------------------------------------------

   Close (Test_fd);

   ---------------------------------------------------------

   Test ("cannot lock with stale file descriptor");
   declare
      Locker : Process_ID;
      Lock1  : File_Lock;
   begin
      Get_Lock (Test_fd, (True, Write_Lock), Lock1, Locker);
      Assert (False, "A036");
   exception
   when E1 : POSIX_Error => Check_Error_Code (Bad_File_Descriptor, E1, "A037");
   when E2 : others => Unexpected_Exception (E2, "A038");
   end;

   --  remove the file created for this test.
   Unlink ("test_file");

   ---------------------------------------------------------

   Done;

exception
when E : others =>
   Unlink ("test_file");
   Fatal_Exception (E, "A039");
end p060200;

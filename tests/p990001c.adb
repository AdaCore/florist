------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                            P 9 9 0 0 0 1 c                               --
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
     POSIX_IO,
     POSIX_Configurable_System_Limits,
     POSIX_Report,
     POSIX_Permissions,
     POSIX_Process_Environment,
     POSIX_Semaphores,
     Test_Parameters;
package body P990001c is

   use P990000,
       POSIX,
       POSIX_IO,
       POSIX_Configurable_System_Limits,
       POSIX_Report,
       POSIX_Permissions,
       POSIX_Process_Environment,
       POSIX_Semaphores,
       Test_Parameters;

   IO_Lock,
   Sync_Lock,
   Waiting_To_Start,
   Waiting_For_Completion : Semaphore_Descriptor;

   procedure Do_Input (Load : Natural) is
   begin
      Wait (Sem => IO_Lock, Masked_Signals => RTS_Signals);
      P990000.Do_Input (Load);
      Post (Sem => IO_Lock);
   exception when E : others => Fatal_Exception (E, "A001: P990001c");
   end Do_Input;

   procedure Do_Output (Load : Natural) is
   begin
      Wait (Sem => IO_Lock, Masked_Signals => RTS_Signals);
      P990000.Do_Output (Load);
      Post (Sem => IO_Lock);
   exception when E : others => Fatal_Exception (E, "A002: P990001c");
   end Do_Output;

   procedure Start_All_Jobs is
   begin
      for J in Jobs loop
         Post (Waiting_To_Start);
      end loop;
   exception when E : others => Fatal_Exception (E, "A003: P990001c");
   end Start_All_Jobs;

   procedure Await_Start is
   begin
      Wait (Sem => Waiting_To_Start, Masked_Signals => RTS_Signals);
   exception when E : others => Fatal_Exception (E, "A004: P990001c");
   end Await_Start;

   procedure Done_Job is
   begin
      Wait (Sem => Sync_Lock, Masked_Signals => RTS_Signals);
      Post (Sem => Waiting_For_Completion);
      Post (Sem => Sync_Lock);
   exception when E : others => Fatal_Exception (E, "A005: P990001c");
   end Done_Job;

   procedure Await_All_Jobs_Done is
   begin
      for I in Jobs'Range loop
         Wait (Sem => Waiting_For_Completion, Masked_Signals => RTS_Signals);
      end loop;
   exception when E : others => Fatal_Exception (E, "A006: P990001c");
   end Await_All_Jobs_Done;

   procedure Finalize is
   begin
      --  clear out leftover semaphores
      for I in 1 .. 4 loop
         begin
            Unlink_Semaphore (Valid_Semaphore_Name (I));
         exception
         when POSIX_Error =>
            Check_Error_Code (No_Such_File_Or_Directory, "A007");
         when E : others => Unexpected_Exception (E, "A008");
         end;
      end loop;
   exception when E : others => Fatal_Exception (E, "A009: P990001c");
   end Finalize;

   procedure Initialize is
      procedure Set (Semd : Semaphore_Descriptor; Val : Natural);
      procedure Set (Semd : Semaphore_Descriptor; Val : Natural) is
      begin
         if Get_Value (Semd) = Val then return;
         end if;
         while Get_Value (Semd) > 0 loop
            Wait (Semd);
         end loop;
         while Get_Value (Semd) < Val loop
            Post (Semd);
         end loop;
         Comment ("forced value of leftover semaphore to " &
           Integer'Image (Get_Value (Semd)));
      exception when E : others => Fatal_Exception (E, "A010: P990001c");
      end Set;
   begin
      Set (IO_Lock, 1);
      Set (Waiting_To_Start, 0);
      Set (Sync_Lock, 1);
      Set (Waiting_For_Completion, 0);
   exception when E : others => Fatal_Exception (E, "A011: P990001c");
   end Initialize;

begin
   Optional (Semaphores_Option, "A012: P990001c");
   IO_Lock := Open_Or_Create
    (Name => Valid_Semaphore_Name (1),
     Permissions => Owner_Permission_Set,
     Value => 1,  --  open
     Options => POSIX_IO.Empty_Set);
   Waiting_To_Start := Open_Or_Create
    (Name => Valid_Semaphore_Name (2),
     Permissions => Owner_Permission_Set,
     Value => 0,  --  closed
     Options => POSIX_IO.Empty_Set);
   Sync_Lock := Open_Or_Create
    (Name => Valid_Semaphore_Name (3),
     Permissions => Owner_Permission_Set,
     Value => 1,  --  open
     Options => POSIX_IO.Empty_Set);
   Waiting_For_Completion := Open_Or_Create
    (Name => Valid_Semaphore_Name (4),
     Permissions => Owner_Permission_Set,
     Value => 0,  --  closed
     Options => POSIX_IO.Empty_Set);
exception
when E : others => Fatal_Exception (E, "A013: P990001c");
end P990001c;

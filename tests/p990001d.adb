------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                            P 9 9 0 0 0 1 d                               --
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


with p990000b,
     POSIX,
     POSIX_IO,
     POSIX_Configurable_System_Limits,
     POSIX_Report,
     POSIX_Permissions,
     POSIX_Process_Environment,
     POSIX_Semaphores,
     Test_Parameters;
package body p990001d is

   use p990000b,
       POSIX,
       POSIX_IO,
       POSIX_Configurable_System_Limits,
       POSIX_Report,
       POSIX_Permissions,
       POSIX_Process_Environment,
       POSIX_Semaphores,
       Test_Parameters;

   package body IO is

      Mutex : Semaphore_Descriptor;

      procedure Initialize is
      begin
         Optional (Semaphores_Option, "A001: p99001c");
         begin
            Comment ("IO.Initialize: Creating semaphore (1) "
              & To_String (Valid_Semaphore_Name (1)));
            Mutex := Open_Or_Create
             (Name => Valid_Semaphore_Name (1),
              Permissions => Owner_Permission_Set,
              Value => 1,  --  open
              Options => POSIX_IO.Empty_Set);
         exception
         when E1 : POSIX_Error =>
            Optional (Semaphores_Option,
              Operation_Not_Implemented, E1, "A002: p99001c");
         when E2 : others =>
            Unexpected_Exception (E2, "A003: p99001c");
         end;
         exception when E : others => Fatal_Exception (E, "A004: p9900001c");
      end Initialize;

      procedure Input (Self : Jobs) is
      begin
         Comment ("IO.Input");
         Wait (Sem => Mutex, Masked_Signals => RTS_Signals);
         Do_Input (Self);
         Post (Sem => Mutex);
         exception when E : others => Fatal_Exception (E, "A005: p9900001c");
      end Input;

      procedure Output (Self : Jobs) is
      begin
         Comment ("IO.Output");
         Wait (Sem => Mutex, Masked_Signals => RTS_Signals);
         Do_Output (Self);
         Post (Sem => Mutex);
         exception when E : others => Fatal_Exception (E, "A006: p9900001c");
      end Output;

   end IO;

   package body Sync is

      Waiting_To_Start       : Semaphore_Descriptor;
      Waiting_For_Completion : Semaphore_Descriptor;
      Mutex                  : Semaphore_Descriptor;
      Done_Count             : Integer := 0;

      procedure Initialize is
      begin
         Optional (Semaphores_Option, "A007: p99001c");
         begin
            Comment ("Sync.Initialize: Creating semaphore (2) "
              & To_String (Valid_Semaphore_Name (2)));
            Waiting_To_Start := Open_Or_Create
             (Name => Valid_Semaphore_Name (2),
              Permissions => Owner_Permission_Set,
              Value => 0,  --  closed
              Options => POSIX_IO.Empty_Set);
            Comment ("Sync.Initialize: Creating semaphore (3) "
              & To_String (Valid_Semaphore_Name (3)));
            Mutex := Open_Or_Create
             (Name => Valid_Semaphore_Name (3),
              Permissions => Owner_Permission_Set,
              Value => 0,
              Options => POSIX_IO.Empty_Set);
            Comment ("Sync.Initialize: Creating semaphore (4) "
              & To_String (Valid_Semaphore_Name (4)));
            Waiting_For_Completion := Open_Or_Create
             (Name => Valid_Semaphore_Name (4),
              Permissions => Owner_Permission_Set,
              Value => 0,
              Options => POSIX_IO.Empty_Set);
         exception
         when E1 : POSIX_Error =>
            Optional (Semaphores_Option,
              Operation_Not_Implemented, E1, "A008: p99001c");
         when E2 : others =>
            Unexpected_Exception (E2, "A009: p99001c");
         end;
         exception when E : others => Fatal_Exception (E, "A010: p9900001c");
      end Initialize;

      procedure Start_All_Jobs is
      --  called by the main program to start all jobs
      begin
         Comment ("Starting jobs");
         for J in Jobs loop
            Post (Waiting_To_Start);
         end loop;
         exception when E : others => Fatal_Exception (E, "A011: p9900001c");
      end Start_All_Jobs;

      procedure Await_Start is
      --  called once by each process to wait to start
      begin
         Comment ("Awaiting start");
         Wait (Sem => Waiting_To_Start, Masked_Signals => RTS_Signals);
         exception when E : others =>
            Fatal_Exception (E, "A012: p99001c");
      end Await_Start;

      procedure Done is
      --  called once by each process to complete
      begin
         Comment ("Done one job");
         Wait (Sem => Mutex, Masked_Signals => RTS_Signals);
         Done_Count := Done_Count + 1;
         if Done_Count = Jobs'Last - Jobs'First + 1 then
            --  wake up the main program
            Post (Waiting_For_Completion);
         end if;
         Post (Sem => Mutex);
         exception when E : others =>
            Fatal_Exception (E, "A013: p99001c");
      end Done;

      procedure Await_All_Jobs_Done is
      --  called by the main program to wait for all jobs to finish
      begin
         Comment ("Awaiting completion of all jobs");
         Wait (Sem => Waiting_For_Completion, Masked_Signals => RTS_Signals);
         exception when E : others => Fatal_Exception (E, "A014: p9900001c");
      end Await_All_Jobs_Done;

   end Sync;

begin
   IO.Initialize;
   Sync.Initialize;
end p990001d;

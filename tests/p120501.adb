------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                             P 1 2 0 5 0 1                                --
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


--  Test for POSIX_Generic_Shared_Memory_Test package

--  Four tasks communicate through a shared memory object
--  containing four counters.  Each task increments its own
--  counter, and then waits until the other task's counter
--  has caught up.  They quit after a fixed number of
--  iterations.

--  .....
--  This test detected an error in the Florist implementation,
--  that the operations for opening and creating a shared memory object
--  were not atomic.  The problem was intermittent, depending on the
--  timing.  Since those operations are only called once here
--  by each task, the chance of interleaving are small, especially if
--  this is run on a single processor.  It would be good to either modify
--  this test to cycle through the operations in each task, or (better?)
--  to write a new test that concentrates on just that.

with POSIX,
     POSIX_IO,
     POSIX_Generic_Shared_Memory,
     POSIX_Memory_Mapping,
     POSIX_Permissions,
     POSIX_Shared_Memory_Objects,
     POSIX_Report,
     Test_Parameters;

procedure p120501 is
   use POSIX,
       POSIX_Permissions,
       POSIX_IO,
       POSIX_Memory_Mapping,
       POSIX_Shared_Memory_Objects,
       POSIX_Report;

   package TP renames Test_Parameters;

   type Window is
      record
        A : integer;
        B : integer;
        C : integer;
        D : integer;
        Done : integer;
        pragma Volatile (A);
        pragma Volatile (B);
        pragma Volatile (C);
        pragma Volatile (D);
        pragma Volatile (Done);
      end record;

   package P is new POSIX_Generic_Shared_Memory (Window);
   Object_Name : POSIX_String := TP.Valid_Shared_Memory_Object_Name (1);


   Access_Failed : exception;

   task type Shared_Mem_Task (number : integer)is
     entry StartRunning;
   end Shared_Mem_Task;

   Side_A : Shared_Mem_Task (number => 1);
   Side_B : Shared_Mem_Task (number => 2);
   Side_C : Shared_Mem_Task (number => 3);
   Side_D : Shared_Mem_Task (number => 4);

   task body Shared_Mem_Task is
      Shmd : File_Descriptor;
      Shared_Var : Window;
      Test_Perm : Permission_Set := Owner_Permission_Set;
      Obj : P.Shared_Access;
      Count : integer := 0;

   begin
      Comment ("Waiting to start ("
        & Integer'Image (number) & ")");

      accept StartRunning;

      Comment ("Opening or creating object ("
        & Integer'Image (number) & ")");

      begin
         --  Try to create a new shared memory object.
         Shmd := P.Open_Or_Create_And_Map_Shared_Memory
           (Object_Name, Allow_Write, Test_Perm, Exclusive);
      exception
         when E1 : POSIX_Error =>
            --  If it already exists, just open the existing object.
            if POSIX.Get_Error_Code = File_Exists then
               Shmd := P.Open_And_Map_Shared_Memory (Object_Name, Allow_Write);
            else Optional (Shared_Memory_Objects_Option,
                   Operation_Not_Implemented, E1, "A001");
            end if;
         when E2 : others =>
            Unexpected_Exception (E2, "A002");
      end;

      Comment ("Accessing object ("
        & Integer'Image (number) & ")");

      Obj := P.Access_Shared_Memory (Shmd);
      Obj.Done := 0;

      Comment ("Entering loop ("
        & Integer'Image (number) & ")");

      if number = 1 then
         for I in 0 .. 10 loop
            Obj.A := I;
            Count := 0;
            while  ((Obj.B < I) or (Obj.C < I) or (Obj.D < I)) loop
               delay 0.1;
               Count := Count + 1;
               if Count >= 520 then
                  Obj.A := 100;
                  raise Access_Failed;
               end if;
            end loop;
         end loop;
      elsif number = 2 then
         for I in 0 .. 10 loop
            Obj.B := I;
            Count := 0;
            while  ((Obj.A < I) or (Obj.C < I) or (Obj.D < I)) loop
               delay 0.1;
               Count := Count + 1;
               if Count >= 520 then
                  Obj.B := 100;
                  raise Access_Failed;
               end if;
            end loop;
         end loop;
      elsif number = 3 then
         for I in 0 .. 10 loop
            Obj.C := I;
            Count := 0;
            while ((Obj.A < I) or (Obj.B < I) or (Obj.D < I)) loop
               delay 0.1;
               Count := Count + 1;
               if Count >= 520 then
                  Obj.C := 100;
                  raise Access_Failed;
               end if;
            end loop;
         end loop;
      elsif number = 4 then
         for I in 0 .. 10 loop
            Obj.D := I;
            Count := 0;
            while ((Obj.A < I) or (Obj.B < I) or (Obj.C < I))loop
               delay 0.1;
               Count := Count + 1;
               if Count >= 520 then
                  Obj.D := 100;
                  raise Access_Failed;
               end if;
            end loop;
         end loop;
      end if;

      Comment ("Done  loop ("
        & Integer'Image (number) & ")");

      if Obj.Done = 0 then
         Obj.Done := 1;
         --  The four counters should stop at the same value.
         Assert (Obj.A = Obj.B, "A003: values do not match");
      end if;

      begin
         P.Unmap_And_Close_Shared_Memory (Shmd);
         Unlink_Shared_Memory (Object_Name);
      exception
      when E1 : POSIX_Error =>
         Check_Error_Code (No_Such_File_Or_Directory, "A004");
      end;

      Comment ("Exiting task ("
        & Integer'Image (number) & ")");

   exception
   when Access_Failed =>
      --  A task may not be responding, or shared memory may
      --  not be working correctly.
      Assert (False, "A005: other counter is not changing");
   when E1 : POSIX_Error =>
      Optional (Shared_Memory_Objects_Option,
        Operation_Not_Implemented, E1, "A006");
   when E2 : others =>
      Unexpected_Exception (E2, "A007");
   end Shared_Mem_Task;

begin

   Header ("p120501.adb", true);

   --  Clean up old memory objects, in case another test
   --  shut down improperly, leaving object behind.

   begin
      Unlink_Shared_Memory (Object_Name);
   exception
   when E1 : POSIX_Error =>
      Check_Error_Code (No_Such_File_Or_Directory, "A008");
   end;

   Side_A.StartRunning;
   Side_B.StartRunning;
   Side_C.StartRunning;
   Side_D.StartRunning;

   while not (Side_A'Terminated) loop
      delay 0.5;
   end loop;
   Comment ("Side_A Terminated");

   while not (Side_B'Terminated) loop
      delay 0.5;
   end loop;
   Comment ("Side_B Terminated");

   while not (Side_C'Terminated) loop
      delay 0.5;
   end loop;
   Comment ("Side_C Terminated");

   while not (Side_D'Terminated) loop
      delay 0.5;
   end loop;
   Comment ("Side_D Terminated");

   Done;

exception
when E : others => Fatal_Exception (E, "A009");
end p120501;

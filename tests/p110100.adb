------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                             P 1 1 0 1 0 0                                --
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
--  AVAILABLE OR DISCLOSED, OR THE OWNERSHIP,  MERCHANTABILITY, OR FITNESS  --
--  FOR A PARTICULAR PURPOSE OF SAID MATERIAL.                              --
--                                                                          --
------------------------------------------------------------------------------
--  [$Revision$]

--  Test package POSIX_Semaphores,
--  in IEEE Std 1003.5b Section 11.1.

with POSIX,
     POSIX_Report,
     POSIX_Configurable_System_Limits,
     POSIX_Permissions,
     POSIX_IO,
     POSIX_Semaphores,
     Test_Parameters;

procedure p110100 is

   use POSIX,
       POSIX_Report,
       POSIX_Configurable_System_Limits,
       POSIX_Permissions,
       POSIX_IO,
       POSIX_Semaphores,
       Test_Parameters;

begin

   Header ("p110100");

   ----------------------------------------------------------------------
   --  1.1.1
   --  There should be a test that type Semaphore is limited, i.e., there
   --  is no assignment or "=" operator on it, but that would need to be
   --  a compile-time check, so it must be done in a separate test.

   ----------------------------------------------------------------------

   ----------------------------------------------------------------------
   --  Tests of anonymous semaphores
   ----------------------------------------------------------------------

   ----------------------------------------------------------------------
   --  Semaphore can be created and used with normal arguments.

   declare
      Sem : Semaphore;
      Semd : Semaphore_Descriptor;
   begin
      Test ("Initialize for Valid_Arguments [11.2.2]");
      Initialize (Sem, 0);
      Semd := Descriptor_Of (Sem);
      Assert (Get_Value (Semd) = 0, "A001");
      Assert (not Try_Wait (Semd), "A002");
      Post (Semd);
      Assert (Get_Value (Semd) = 1, "A003");
      Post (Semd);
      Assert (Get_Value (Semd) = 2, "A004");
      Assert (Try_Wait (Semd), "A005");
      Assert (Get_Value (Semd) = 1, "A006");
      Wait (Semd);
      Assert (Get_Value (Semd) = 0, "A007");
      Finalize (Sem);
   exception
   when E1 : POSIX_Error =>
      Optional (Semaphores_Option,
        Expected => Operation_Not_Implemented,
        E => E1, Message => "A008");
   when E2 : others =>
      Unexpected_Exception (E2, "A009");
   end;

   -----------------------------------------------------------------------

   declare
      Sem : Semaphore;
      Semd : Semaphore_Descriptor;
      Sem_Value_Max : Integer := 0;
   begin
      Test ("Initialize with an over-large value [11.2.2]");

      Sem_Value_Max :=
        POSIX_Configurable_System_Limits.Semaphores_Value_Maximum;
      if Sem_Value_Max < Natural'Last then
         Initialize (Sem, Sem_Value_Max + 1);
         Expect_Exception ("A010");
         Finalize (Sem);
      else
         begin
            Initialize (Sem, Natural'Last);
            Semd := Descriptor_Of (Sem);
            Assert (Get_Value (Semd) = Natural'Last, "A011");
            Finalize (Sem);
            Comment ("Natural'Last is a valid semaphore value");
         exception
         when E1 : POSIX_Error =>
            Optional (Semaphores_Option,
              Expected => Operation_Not_Implemented,
              E => E1, Message => "A012");
         when E2 : others =>
            Unexpected_Exception (E2, "A013");
         end;
      end if;
   exception
   when E1 : POSIX_Error =>
      Optional (Semaphores_Option,
        Expected_If_Not_Supported => Operation_Not_Implemented,
        Expected_If_Supported => Invalid_Argument,
        E => E1, Message => "A014");
   when E2 : others =>
      Unexpected_Exception (E2, "A015");
   end;

   -----------------------------------------------------------------------

   declare
      Sem : Semaphore;
   begin
      Test ("Initialize with Is_Shared => True [11.2.2]");
      Initialize (Sem, 1, True);
      Finalize (Sem);
   exception
   when E1 : POSIX_Error =>
      Optional (Semaphores_Option,
        Expected => Operation_Not_Implemented,
        E => E1, Message => "A016");
   when E2 : others =>
      Unexpected_Exception (E2, "A017");
   end;

   ----------------------------------------------------------------------

   declare
      Sem : Semaphore;
   begin
      Test ("Initialize with Is_Shared =>False [11.2.2]");
      Initialize (Sem, 1, False);
      Finalize (Sem);
   exception
   when E1 : POSIX_Error =>
      Optional (Semaphores_Option,
        Expected => Operation_Not_Implemented,
        E => E1, Message => "A018");
   when E2 : others =>
      Unexpected_Exception (E2, "A019");
   end;

   ---------------------------------------------------------------------

   declare
      Sem_A : Semaphore;
      Sem_B : Semaphore;
      Semd_A : Semaphore_Descriptor;
      Semd_B : Semaphore_Descriptor;
   begin
      Test ("Descriptor_Of [11.2.2]");
      Initialize (Sem_A, 1);
      Initialize (Sem_B, 2);
      Semd_A := Descriptor_Of (Sem_A);
      Semd_B := Descriptor_Of (Sem_B);
      Assert (Semd_A /= Semd_B, "A020");
      Finalize (Sem_A);
      Finalize (Sem_B);
   exception
   when E1 : POSIX_Error =>
      Optional (Semaphores_Option,
        Expected => Operation_Not_Implemented,
        E => E1, Message => "A021");
   when E2 : others =>
      Unexpected_Exception (E2, "A022");
   end;

   -----------------------------------------------------------------------
   --  The following error cases are not tested, because we
   --  could not think of a portable way of testing them:

   --    No_Space_Left_On_Device

   ----------------------------------------------------------------------

   declare
      Uninitialized_Descriptor : Semaphore_Descriptor;
   begin
      Test ("Wait for an invalid argument [11.1.7]");
      Wait (Uninitialized_Descriptor, No_Signals);
      Assert (False, "A023");
   exception
   when E1 : POSIX_Error =>
      Optional (Semaphores_Option,
        Expected_If_Not_Supported => Operation_Not_Implemented,
        Expected_If_Supported => Invalid_Argument,
        E => E1, Message => "A024");
   when E2 : others =>
      Unexpected_Exception (E2, "A025");
   end;

   ----------------------------------------------------------------------

   declare
      Uninitialized_Descriptor : Semaphore_Descriptor;
      Result : Boolean;
   begin
      Test ("Try_Wait for an invalid argument [11.1.7]");
      Result := Try_Wait (Uninitialized_Descriptor);
      Expect_Exception ("A026");
   exception
   when E1 : POSIX_Error =>
      Optional (Semaphores_Option,
        Expected_If_Not_Supported => Operation_Not_Implemented,
        Expected_If_Supported => Invalid_Argument,
        E => E1, Message => "A027");
   when E2 : others =>
      Unexpected_Exception (E2, "A028");
   end;

   ----------------------------------------------------------------------
   --  Post raises POSIX_Error with EINVAL if argument does not refer
   --  to a valid semaphore.

   declare
      Uninitialized_Descriptor : Semaphore_Descriptor;
   begin
      Test ("Post for invalid argument [11.1.8]");
      Post (Uninitialized_Descriptor);
      Expect_Exception ("A029");
   exception
   when E1 : POSIX_Error =>
      Optional (Semaphores_Option,
        Expected_If_Not_Supported => Operation_Not_Implemented,
        Expected_If_Supported => Invalid_Argument,
        E => E1, Message => "A030");
   when E2 : others =>
      Unexpected_Exception (E2, "A031");
   end;

   ----------------------------------------------------------------------
   --  Get_Value raises POSIX_Error with EINVAL if argument does not refer
   --  to a valid semaphore.

   declare
      Uninitialized_Descriptor : Semaphore_Descriptor;
      Temp_Int : Integer;
   begin
      Test ("Get_Value for Invalid_Argument [11.1.9]");
      Temp_Int := Get_Value (Uninitialized_Descriptor);
      Expect_Exception ("A032");
   exception
   when E1 : POSIX_Error =>
      Optional (Semaphores_Option,
        Expected_If_Not_Supported => Operation_Not_Implemented,
        Expected_If_Supported => Invalid_Argument,
        E => E1, Message => "A033");
   when E2 : others =>
      Unexpected_Exception (E2, "A034");
   end;

   ----------------------------------------------------------------------
   --  Tests of named semaphores
   ----------------------------------------------------------------------

   ----------------------------------------------------------------------
   --  Clear out any junk semaphores left from previous tests.

   for I in 1 .. 5 loop
      begin
         Comment ("Unlinking Semaphore");
         Unlink_Semaphore (Valid_Semaphore_Name (I));
         Comment ("Cleared out semaphore " &
           To_String (Valid_Semaphore_Name (I)));
      exception
      when E1 : POSIX_Error =>
         Optional (Semaphores_Option,
           Expected_If_Not_Supported => Operation_Not_Implemented,
           Expected_If_Supported => No_Such_File_Or_Directory,
           E => E1, Message => "A033");
      when E2 : others => Unexpected_Exception (E2, "A036");
      end;
   end loop;

   ----------------------------------------------------------------------
   --  Create a named semaphore that does previously exist, use it,
   --  close it, and then unlink it.

   declare
      Name : POSIX_String := Valid_Semaphore_Name (1);
      Semd : Semaphore_Descriptor;
   begin
      Test ("Create a nonexistent semaphore [11.1.4]");
      Semd := Open_Or_Create
        (Name, Owner_Permission_Set, 1, Exclusive, No_Signals);
      Assert (Get_Value (Semd) = 1, "A037");
      Post (Semd);  --  should set value to 2
      Assert (Get_Value (Semd) = 2, "A038");
      Wait (Semd);  --  should not block
      Assert (Get_Value (Semd) = 1, "A039");
      Assert (Try_Wait (Semd), "A040");
      Assert (Get_Value (Semd) = 0, "A041");
      Assert (not Try_Wait (Semd), "A042");
      Close (Semd);
      Unlink_Semaphore (Name);
   exception
   when E1 : POSIX_Error =>
      Optional (Semaphores_Option,
        Expected => Operation_Not_Implemented,
        E => E1, Message => "A043");
   when E2 : others =>
      Unexpected_Exception (E2, "A044");
   end;

   ----------------------------------------------------------------------
   --  Try to open a nonexistent semaphore.

   declare
      Name : POSIX_String := Valid_Semaphore_Name (2);
      Semd : Semaphore_Descriptor;
   begin
      Test ("Open of nonexistent semaphore [11.1.4]");
      Semd := Open (Name, No_Signals);
      Expect_Exception ("A045");
   exception
   when E1 : POSIX_Error =>
      Optional (Semaphores_Option,
        Expected_If_Not_Supported => Operation_Not_Implemented,
        Expected_If_Supported => No_Such_File_Or_Directory,
        E => E1, Message => "A046");
   when E2 : others =>
      Unexpected_Exception (E2, "A047");
   end;

   ----------------------------------------------------------------------
   --  Try to open a named semaphore that is already open.
   --  If a process makes multiple calls (even from different
   --  tasks within the same process) to Open or Open_Or_Create
   --  with the same value for Name, the same Semaphore_Descriptor
   --  value shall be returned for each such call ... [11.1.4]

   declare
      Name : POSIX_String := Valid_Semaphore_Name (3);
      Semd_1, Semd_2 : Semaphore_Descriptor;
   begin
      Test ("Open of already-open semaphore [11.1.4]");
      Semd_1 := Open_Or_Create
        (Name, Owner_Permission_Set, 1, Exclusive, No_Signals);
      Semd_2 := Open (Name, No_Signals);
      Assert (Semd_1 = Semd_2, "A048");
      Close (Semd_1);
      Unlink_Semaphore (Name);
   exception
   when E1 : POSIX_Error =>
      Optional (Semaphores_Option,
        Expected => Operation_Not_Implemented,
        E => E1, Message => "A049");
   when E2 : others =>
      Unexpected_Exception (E2, "A050");
   end;

   ----------------------------------------------------------------------
   --  Try to create a named semaphore that already exists.
   --  First, create a semaphore; then, create another with the same name.
   --  The second create should fail, with error code File_Exists.

   declare
      Name : POSIX_String := Valid_Semaphore_Name (4);
      Semd_1, Semd_2 : Semaphore_Descriptor;
   begin
      Test ("Create of existing semaphore [11.1.4]");
      Semd_1 := Open_Or_Create
        (Name, Owner_Permission_Set, 1, Exclusive, No_Signals);
      Semd_2 := Open_Or_Create
        (Name, Owner_Permission_Set, 1, Exclusive, No_Signals);
      Expect_Exception ("A051");
   exception
   when E1 : POSIX_Error =>
      Optional (Semaphores_Option,
        Expected_If_Not_Supported => Operation_Not_Implemented,
        Expected_If_Supported => File_Exists,
        E => E1, Message => "A052");
      --  Clean up first semaphore
      begin
         Close (Semd_1);
         Unlink_Semaphore (Name);
      exception when others => null;
      end;
   when E2 : others =>
      Unexpected_Exception (E2, "A053");
   end;

   ----------------------------------------------------------------------
   --  Try to create a named semaphore with an invalid name.

   declare
      Name : POSIX_String := Invalid_Semaphore_Name (5);
      Semd : Semaphore_Descriptor;
   begin
      Test ("Create of semaphore with invalid name [11.1.4]");
      Semd := Open_Or_Create
        (Name, Owner_Permission_Set, 1, Exclusive, No_Signals);
      Expect_Exception ("A054");
      Comment ("Invalid name not detected");
      Close (Semd);
      Unlink_Semaphore (Name);
   exception
   when E1 : POSIX_Error =>
      Optional (Semaphores_Option,
        Expected_If_Not_Supported => Operation_Not_Implemented,
        Expected_If_Supported => Invalid_Argument,
        E => E1, Message => "A055");
   when E2 : others =>
      Unexpected_Exception (E2, "A056");
   end;

   Done;

exception
when E : others =>
   Fatal_Exception (E, "A057");
end p110100;

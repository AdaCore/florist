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

--  This test is a type A test for Semaphores

with POSIX,
     POSIX_Report,
     POSIX_Configurable_System_Limits,
     POSIX_Permissions,
     POSIX_IO,
     POSIX_Semaphores;

procedure p110100 is

   use POSIX,
       POSIX_Report,
       POSIX_Configurable_System_Limits,
       POSIX_Permissions,
       POSIX_IO,
       POSIX_Semaphores;

   Masking : constant Signal_Masking := No_Signals;

begin

   Header ("p110100");
   --  This test is designed as follows:
   --  First the procedure/function is tested to see if it works
   --  Second the procedure/function is programmed to fail and subsequently
   --  the proper Error Handling exceptions should be invoked


   ----------------------------------------------------------------------

   --  1.1.1
   --  Need to test that type Semaphore is limited, i.e. there
   --  is no assignment or "=" operator on it; that is a compile-time
   --  check, so it must be done in a separate test.

   ----------------------------------------------------------------------

   declare
      Sem_A : Semaphore;

   begin
      Test ("Initialize for Valid_Arguments [11.2.2]");
      Initialize (Sem_A, 0);

   exception
   when E1 : POSIX_Error =>
      Unexpected_Exception (E1, "A001");
   when E2 : others =>
      Unexpected_Exception (E2, "A002");
   end;

   -----------------------------------------------------------------------

   declare
      Sem_A : Semaphore;
      Sem_Value_Max : Integer := 0;
      Max_Possible : Integer := 0;

   begin
      Test ("Initialize with an over-large value [11.2.2]");

      Sem_Value_Max :=
        POSIX_Configurable_System_Limits.Semaphores_Value_Maximum;
      Max_Possible := Natural'Last;

      if Sem_Value_Max < Max_Possible then
         Initialize (Sem_A, Sem_Value_Max + 1);
         --  Fail because Initialize DID NOT FAIL, Invalid_Argument expected
         Assert (False, "A003");
      else
         Comment ("unable to test Initialize for Invalid_Argument");
      end if;

   exception
   when E1 : POSIX_Error =>
      Optional (Semaphores_Option,
        Expected_If_Not_Supported => Operation_Not_Implemented,
        Expected_If_Supported => Invalid_Argument,
        E => E1, Message => "A004");
   when E2 : others =>
      Unexpected_Exception (E2, "A005");
   end;

   -----------------------------------------------------------------------

   declare
      Sem_A : Semaphore;

   begin
      Test ("Initialize with Is_Shared := True [11.2.2]");
      Initialize (Sem_A, 1, True);

   exception
   when E1 : POSIX_Error =>
      Optional (Semaphores_Option,
        Expected_If_Not_Supported => Operation_Not_Implemented,
        Expected_If_Supported => Operation_Not_Implemented,
        E => E1, Message => "A006");
   when E2 : others =>
      Unexpected_Exception (E2, "A007");
   end;

   ----------------------------------------------------------------------

   declare
      Sem_A : Semaphore;

   begin
      Test ("Initialize with Is_Shared := False [11.2.2]");
      Initialize (Sem_A, 1, False);

   exception
   when E1 : POSIX_Error =>
      Optional (Semaphores_Option,
        Expected_If_Not_Supported => Operation_Not_Implemented,
        Expected_If_Supported => Operation_Not_Implemented,
        E => E1, Message => "A008");
   when E2 : others =>
      Unexpected_Exception (E2, "A009");
   end;

   ---------------------------------------------------------------------

   declare
      Sem_A : Semaphore;
      Sem_B : Semaphore;
      Sem_C : Semaphore;
      Sem_Def : Semaphore_Descriptor;

   begin
      Test ("Descriptor_Of [11.2.2]");
      Initialize (Sem_A, 1);
      Initialize (Sem_B, 2, True);
      Initialize (Sem_C, 3, False);
      Sem_Def := Descriptor_Of (Sem_A);
      Sem_Def := Descriptor_Of (Sem_B);
      Sem_Def := Descriptor_Of (Sem_C);

   exception
   when E1 : POSIX_Error =>
      Optional (Semaphores_Option,
        Expected_If_Not_Supported => Operation_Not_Implemented,
        Expected_If_Supported => Operation_Not_Implemented,
        E => E1, Message => "A010");
   when E2 : others =>
      Unexpected_Exception (E2, "A011");
   end;

   -----------------------------------------------------------------------
   --  The following error cases are not tested, because we
   --  could not think of a portable way of testing them:

   --    No_Space_Left_On_Device

   ----------------------------------------------------------------------

   declare
      Sem_A : Semaphore;

   begin
      Test ("Finalize an initialized semaphore [11.2.2]");
      Initialize (Sem_A, 1);
      Finalize (Sem_A);
   exception
   when E1 : POSIX_Error =>
      Optional (Semaphores_Option,
        Expected_If_Not_Supported => Operation_Not_Implemented,
        Expected_If_Supported => Operation_Not_Implemented,
        E => E1, Message => "A012");
   when E2 : others =>
      Unexpected_Exception (E2, "A013");
   end;

   ----------------------------------------------------------------------

   declare
      Sem_A : Semaphore;
   begin
      Test ("Finalize an uninitialized semaphore [11.1.3]");
      Finalize (Sem_A);
      --  Fail because Finalize DID NOT FAIL, Invalid_Argument expected
      Assert (False, "A014");
   exception
   when E1 : POSIX_Error =>
      Optional (Semaphores_Option,
        Expected_If_Not_Supported => Operation_Not_Implemented,
        Expected_If_Supported => Invalid_Argument,
        E => E1, Message => "A015");
   when E2 : others =>
      Unexpected_Exception (E2, "A016");
   end;

   -----------------------------------------------------------------------

   --  ????
   --  Move any tests for "undefined" situations to one or more
   --  separate test programs, so that if the case causes a crash
   --  (which in this case would not be a failure!)
   --  get meaningful results for the rest of the test.
   --  Alternatively, leave out such cases, since they are of marginal
   --  value.

   Test ("Finalize of busy semaphore [11.1.3]");
   declare
      Sem_A : Semaphore;

      task Simple_Task is
        entry Start_Running;
      end Simple_Task;

      task body Simple_Task is
      begin
         accept Start_Running;
         Wait (Descriptor_Of (Sem_A), Masking);
      exception
      when E : others =>
         Fatal_Exception (E, "A017");
      end Simple_Task;

   begin
      Initialize (Sem_A, 0);
      Simple_Task.Start_Running;
      delay 0.2;
      Finalize (Sem_A);
      --  Fail because Finalize DID NOT FAIL, Resource_Busy expected
      Assert (false, "A018");
      abort Simple_Task;
   exception
   when E1 : POSIX_Error =>
      if POSIX.Get_Error_Code /= Resource_Busy then
         Optional (Semaphores_Option,
           Expected_If_Not_Supported => Operation_Not_Implemented,
           Expected_If_Supported => Invalid_Argument,
           E => E1, Message => "A019");
      end if;
      abort Simple_Task;
   when E2 : others =>
      Unexpected_Exception (E2, "A020");
      abort Simple_Task;
   end;

   ----------------------------------------------------------------------

   --  Try to create a semaphore that does not exist.

   declare
      Name : POSIX_String := "nonexistent";
      Sem_Def : Semaphore_Descriptor;

   begin
      Test ("Create a nonexistent semaphore [11.1.4]");
      Sem_Def := Open_Or_Create (Name, Owner_Permission_Set, 1,
                                 Exclusive, Masking);
   exception
   when E1 : POSIX_Error =>
      --  Fail because Open_Or_Create failed
      Unexpected_Exception (E1, "A021");
   when E2 : others =>
      Unexpected_Exception (E2, "A022");
   end;

   ----------------------------------------------------------------------

   --  Try to open a semaphore that does not exist.
   --  First, create a semaphore; then, create another with the same name.
   --  The second create should fail, with error code File_Exists

   declare
      Name : POSIX_String := "nonexistent";
      Sem_Def : Semaphore_Descriptor;

   begin
      Test ("Open of nonexistent semaphore [11.1.4]");
      Sem_Def := Open (Name, Masking);

   exception
   when E1 : POSIX_Error =>
      Optional (Semaphores_Option,
        Expected_If_Not_Supported => Operation_Not_Implemented,
        Expected_If_Supported => No_Such_File_Or_Directory,
        E => E1, Message => "A023");
   when E2 : others =>
      Unexpected_Exception (E2, "A024");
   end;

   ----------------------------------------------------------------------


   --  Try to open a semaphore that does exist.

   declare
      Name : POSIX_String := "nonexistent";
      Sem_Def : Semaphore_Descriptor;

   begin
      Test ("Open of existent semaphore [11.1.4]");
      Sem_Def := Open_Or_Create (Name, Owner_Permission_Set, 1,
                                 Exclusive, Masking);
      Sem_Def := Open (Name, Masking);
   exception
   when E1 : POSIX_Error =>
      Unexpected_Exception (E1, "A025");
   when E2 : others =>
      Unexpected_Exception (E2, "A026");
   end;

   ----------------------------------------------------------------------

   --  Close [11.1.5] and Unlink_Semaphore [11.1.6] are not tested
   --  as named semaphores are unable to be created

   ----------------------------------------------------------------------
   declare
      Sem_A : Semaphore;
      Sem_Def : Semaphore_Descriptor;

   begin
      Test ("Wait for a Valid_Argument [11.1.7]");
      Initialize (Sem_A, 1);
      Sem_Def := Descriptor_Of (Sem_A);
      Wait (Sem_Def, Masking);
   exception
   when E1 : POSIX_Error =>
      Unexpected_Exception (E1, "A027");
   when E2 : others =>
      Unexpected_Exception (E2, "A028");
   end;

   ----------------------------------------------------------------------

   declare
      Sem_Def : Semaphore_Descriptor;
   begin
      Test ("Wait for an Invalid_Argument [11.1.7]");
      Wait (Sem_Def, Masking);
      --  Fail because Wait DID NOT FAIL, Invalid_Argument expected
      Assert (false, "A029");
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

   declare
      Sem_A : Semaphore;
      Sem_Def : Semaphore_Descriptor;
      result : boolean;

   begin
      Test ("TryWait for a Valid_Argument [11.1.7]");
      Initialize (Sem_A, 1);
      Sem_Def := Descriptor_Of (Sem_A);
      result := Try_Wait (Sem_Def);
   exception
   when E1 : POSIX_Error =>
      Unexpected_Exception (E1, "A032");
   when E2 : others =>
      Unexpected_Exception (E2, "A033");
   end;

   ----------------------------------------------------------------------

   begin
      Test ("Try_Wait for an Invalid_Argument [11.1.7]");
      --  Fail because Try_Wait for Invalid_Argument causes Segmentation Faults
      Assert (false, "A034");
   end;

   ----------------------------------------------------------------------
   declare
      Sem_A : Semaphore;
      Sem_Def : Semaphore_Descriptor;
   begin
      Test ("Post for Valid_Argument [11.1.8]");
      Initialize (Sem_A, 1);
      Sem_Def := Descriptor_Of (Sem_A);
      Post (Sem_Def);
   exception
   when E1 : POSIX_Error =>
      Unexpected_Exception (E1, "A035");
   when E2 : others =>
      Unexpected_Exception (E2, "A036");
   end;

   ----------------------------------------------------------------------

   --  Post for Invalid_Argument not tested as any Invalid_Argument
   --  causes a Segmentation Fault

   begin
      Test ("Post for Invalid_Argument [11.1.8]");
      --  Fail because Post for Invalid_Argument causes Segmentation Faults
      Assert (False, "A037");
   end;

   ----------------------------------------------------------------------
   declare
      Sem_A : Semaphore;
      Sem_Def : Semaphore_Descriptor;
      Temp_Int : Integer;
   begin
      Test ("Get_Value for Valid_Argument [11.1.9]");
      Initialize (Sem_A, 2);
      Sem_Def := Descriptor_Of (Sem_A);
      Temp_Int := Get_Value (Sem_Def);
      if Temp_Int /= 2 then
         --  Fail because Get_Value DID NOT Retrieve correct value
         Assert (False, "A038");
      end if;
   exception
   when E1 : POSIX_Error =>
      Unexpected_Exception (E1, "A039");
   when E2 : others =>
      Unexpected_Exception (E2, "A040");
   end;

   ----------------------------------------------------------------------

   begin
      Test ("Get_Value for Invalid_Argument [11.1.9]");
      --  Temp_Int := Get_Value (Sem_Def);
      --  Fail because Get_Value for Invalid_Argument causes
      --    Segmentation Faults
      Assert (false, "A041");
   exception
   when E1 : POSIX_Error =>
      Unexpected_Exception (E1, "A042");
   when E2 : others =>
      Unexpected_Exception (E2, "A043");
   end;

   ----------------------------------------------------------------------

   begin
      Test ("Next Test");
   exception
   when E1 : POSIX_Error =>
      Unexpected_Exception (E1, "A044");
   when E2 : others =>
      Unexpected_Exception (E2, "A045");
   end;

   Done;

exception
when E : others =>
   Fatal_Exception (E, "A046");
end p110100;

------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                             P 1 2 0 4 0 0                                --
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
--  AVAILABLE OR DISCLOSED,  OR THE OWNERSHIP,  MERCHANTABILITY, OR FITNESS  --
--  FOR A PARTICULAR PURPOSE OF SAID MATERIAL.                              --
--                                                                          --
------------------------------------------------------------------------------
--  [$Revision$]

--  Test package POSIX_Shared_Memory,
--  in IEEE STd 1003.5b Section 12.4.

with POSIX,
     POSIX_File_Status,
     POSIX_IO,
     POSIX_Permissions,
     POSIX_Process_Identification,
     POSIX_Report,
     POSIX_Shared_Memory_Objects,
     Test_Parameters;

procedure p120400 is

   use POSIX,
       POSIX_File_Status,
       POSIX_IO,
       POSIX_Permissions,
       POSIX_Process_Identification,
       POSIX_Report,
       POSIX_Shared_Memory_Objects;

   Valid_Name_1 : constant POSIX_String :=
     Test_Parameters.Valid_Shared_Memory_Object_Name (1);

begin

   Header ("p120400");

   ------------------------------------------------------------------
   --  Trying to open non-existent shared memory object
   --  should result in POSIX_Error with No_Such_File_Or_Directory.

   declare
      test_mode : POSIX_IO.File_Mode := Read_Only;
      shmd : POSIX_IO.File_Descriptor;
   begin
      Test ("cannot open non-existent shared memory object");
      shmd :=
        Open_Shared_Memory (Valid_Name_1, test_mode);
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code /= No_Such_File_Or_Directory then
         Optional (Shared_Memory_Objects_Option,
           Operation_Not_Implemented, E1, "A001");
      end if;
      Check_Error_Code (No_Such_File_Or_Directory, "A002");
   when E2 : others => Unexpected_Exception (E2, "A003");
   end;

   ------------------------------------------------------------------
   --  Trying to unlink non-existent shared memory object
   --  should result in POSIX_Error with No_Such_File_Or_Directory.

   begin
      Test ("cannot unlink non-existent shared memory object");
      Unlink_Shared_Memory (Valid_Name_1);
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code /= No_Such_File_Or_Directory then
         Optional (Shared_Memory_Objects_Option,
           Operation_Not_Implemented, E1, "A004");
      end if;
      Check_Error_Code (No_Such_File_Or_Directory, "A005");
   when E2 : others => Unexpected_Exception (E2, "A006");
   end;

   ------------------------------------------------------------------
   --  It should be possible to open a single shared memory object
   --  with more than one file descriptor concurrently, and to
   --  open distinct shared memory objects serially with a single
   --  file descriptor.
   --  An open object should persist after being unlinked,
   --  and if open with more than one file descriptor it should
   --  persist until the last close.
   --  Meanwhile, it should be possible to create a new object using
   --  the name of the unlinked object.

   --  .... This test would be better if some use were made of the
   --  shared memory objects, to verify that indeed they persist and
   --  the new one is distinct from the old one of the same name.

   declare
      test_mode : POSIX_IO.File_Mode := Read_Write;
      test_perm : Permission_Set := Owner_Permission_Set;
      shmd  : POSIX_IO.File_Descriptor;
      shmd1 : POSIX_IO.File_Descriptor;
      opt1  : POSIX_IO.Open_Option_Set :=
             POSIX_IO.Exclusive + POSIX_IO.Truncate;
   begin
      Test ("Open_Or_Create_Shared_Memory [12.4.1]");
      Comment ("opening with first descriptor");
      shmd := Open_Or_Create_Shared_Memory
        (Valid_Name_1, test_mode, test_perm, POSIX_IO.Exclusive);
      Comment ("opening with second descriptor");
      shmd1 := Open_Shared_Memory (Valid_Name_1, test_mode);
      Comment ("unlinking original object");
      Unlink_Shared_Memory (Valid_Name_1);
      Comment ("closing first descripplactor");
      Close (shmd);
      Comment ("creating second object with original descriptor and name");
      shmd := Open_Or_Create_Shared_Memory
        (Valid_Name_1, test_mode, test_perm, POSIX_IO.Truncate);
      Comment ("unlinking second object");
      Unlink_Shared_Memory (Valid_Name_1);
      Comment ("creating third shared object");
      shmd := Open_Or_Create_Shared_Memory
        (Valid_Name_1, test_mode, test_perm, opt1);
      Comment ("opening third shared object with another descriptor");
      shmd1 := Open_Shared_Memory (Valid_Name_1, test_mode);
      Comment ("unlinking third shared object");
      Unlink_Shared_Memory (Valid_Name_1);
      Close (shmd);
      Close (shmd1);
   exception
   when E1 : POSIX_Error =>
      Optional (Shared_Memory_Objects_Option,
        Operation_Not_Implemented, E1, "A007");
   when E2 : others => Unexpected_Exception (E2, "A008");
   end;

   -----------------------------------------------------------------------
   --  Once an object is unlinked it should be possible to create
   --  another object with the same name.

   declare
      test_mode : POSIX_IO.File_Mode := Read_Write;
      test_perm : Permission_Set := Owner_Permission_Set;
      shmd1, shmd2 : POSIX_IO.File_Descriptor;
   begin
      Test ("Unlink_Shared_Memory [12.4.2]");
      shmd1 := Open_Or_Create_Shared_Memory
        (Valid_Name_1, test_mode, test_perm);
      Comment ("creating first shared object");
      Unlink_Shared_Memory (Valid_Name_1);
      shmd2 := Open_Or_Create_Shared_Memory
        (Valid_Name_1, test_mode, test_perm);
      Comment ("creating second shared object");
      Unlink_Shared_Memory (Valid_Name_1);
      Close (shmd1);
      Close (shmd2);
   exception
   when E1 : POSIX_Error =>
      Optional (Shared_Memory_Objects_Option,
        Operation_Not_Implemented, E1, "A009");
   when E2 : others => Unexpected_Exception (E2, "A010");
   end;

   -----------------------------------------------------------------------
   --  The user and group ID of the shared Object should be inherited
   --  from the process that created it.

   declare
      test_mode : POSIX_IO.File_Mode := Read_Write;
      test_perm : Permission_Set := Owner_Permission_Set;
      shmd : POSIX_IO.File_Descriptor;
      file_status : Status;
      userID : User_ID;
      groupID : Group_ID;
      object_userID : User_ID;
      object_groupID : Group_ID;
      object_permissions : Permission_Set;

   begin
      Test ("Shared_Memory_object Owner");
      shmd := Open_Or_Create_Shared_Memory
        (Valid_Name_1, test_mode, test_perm);

      userID := Get_Real_User_ID;
      groupID := Get_Real_Group_ID;
      file_status := Get_File_Status (shmd);
      object_userID := Owner_Of (file_status);
      object_groupID := Group_Of (file_status);
      object_permissions :=  Permission_Set_Of (file_status);

      Assert (userID = object_userID, "A011");
      Assert (groupID = object_groupID, "A012");
      Assert (test_perm = object_permissions, "A013");

      Unlink_Shared_Memory (Valid_Name_1);
   exception
   when E1 : POSIX_Error =>
      Optional (Shared_Memory_Objects_Option,
        Operation_Not_Implemented, E1, "A014");
   when E2 : others => Unexpected_Exception (E2, "A015");
   end;

   -----------------------------------------------------------------------
   --  It should be possible to use the Exclusive and Truncate options,
   --  in all combinations.

   declare
      test_mode : POSIX_IO.File_Mode := Read_Write;
      test_perm : Permission_Set := Owner_Permission_Set;
      shmd : POSIX_IO.File_Descriptor;
      opt1 : POSIX_IO.Open_Option_Set :=
             POSIX_IO.Exclusive + POSIX_IO.Truncate;
   begin
      Test ("Open_Or_Create_Shared_Memory [12.4.1]");
      Comment ("creating with exclusive");
      shmd := Open_Or_Create_Shared_Memory
        (Valid_Name_1, test_mode,
         test_perm, POSIX_IO.Exclusive);
      Close (shmd);
      Comment ("unlinking");
      Unlink_Shared_Memory (Valid_Name_1);
      Comment ("creating with truncate");
      shmd := Open_Or_Create_Shared_Memory
        (Valid_Name_1, test_mode,
         test_perm, POSIX_IO.Truncate);
      Close (shmd);
      Comment ("unlinking");
      Unlink_Shared_Memory (Valid_Name_1);
      Comment ("creating with exclusive and truncate");
      shmd := Open_Or_Create_Shared_Memory
        (Valid_Name_1, test_mode, test_perm, opt1);
      Close (shmd);
      Comment ("opening with default options");
      shmd := Open_Shared_Memory
        (Valid_Name_1, test_mode);
      Close (shmd);
      Comment ("unlinking");
      Unlink_Shared_Memory (Valid_Name_1);
   exception
   when E1 : POSIX_Error =>
      Optional (Shared_Memory_Objects_Option,
        Operation_Not_Implemented, E1, "A016");
   when E2 : others => Unexpected_Exception (E2, "A017");
   end;

   -----------------------------------------------------------------------
   --  After an object is unlinked and last closed,
   --  it is no longer be possible to open it.

   declare
      test_mode : POSIX_IO.File_Mode := Read_Write;
      test_perm : Permission_Set := Owner_Permission_Set;
      shmd : POSIX_IO.File_Descriptor;
   begin
      Test ("Unlink_Shared_Memory [12.4.2]");
      Comment ("creating with default options");
      shmd := Open_Or_Create_Shared_Memory
        (Valid_Name_1, test_mode, test_perm);
      Close (shmd);
      Comment ("unlinking");
      Unlink_Shared_Memory (Valid_Name_1);
      begin
         Comment ("trying to open after unlink");
         shmd :=
           Open_Shared_Memory (Valid_Name_1, test_mode);
         Assert (False, "should have raised POSIX_Error");
      exception
      when E1 : POSIX_Error =>
         if Get_Error_Code /= No_Such_File_Or_Directory then
            Optional (Shared_Memory_Objects_Option,
              Operation_Not_Implemented, E1, "A018");
         end if;
      when E2 : others => Unexpected_Exception (E2, "A019");
      end;
   exception
   when E1 : POSIX_Error =>
      Optional (Shared_Memory_Objects_Option,
        Operation_Not_Implemented, E1, "A020");
   when E2 : others => Unexpected_Exception (E2, "A021");
   end;

   -----------------------------------------------------------------------

   Done;

exception
when E : others => Fatal_Exception (E, "A022");
end p120400;

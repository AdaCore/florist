------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                             P 1 2 0 5 0 0                                --
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
--  Test for POSIX_Generic_Shared_Memory_Test package

with POSIX,
     POSIX_IO,
     POSIX_Generic_Shared_Memory,
     POSIX_Memory_Mapping,
     POSIX_Permissions,
     POSIX_Shared_Memory_Objects,
     POSIX_Report,
     Test_Parameters;
procedure p120500 is

   use POSIX,
       POSIX_Permissions,
       POSIX_IO,
       POSIX_Memory_Mapping,
       POSIX_Shared_Memory_Objects,
       POSIX_Report;

   package TP renames Test_Parameters;

   subtype String10 is String (1 .. 10);
   package GT is new POSIX_Generic_Shared_Memory (String10);
   use GT;

   Object_Name : constant POSIX_String :=
     TP.Valid_Shared_Memory_Object_Name (1);
   Local_Failure : exception;

begin

   Header ("p120500", True);

   ------------------------------------------------------------------------

   declare
      Shmd : File_Descriptor;
      Test_perm : Permission_Set := Owner_Permission_Set;
      Mode : File_Mode;
      Option : Open_Option_Set;
   begin
      Test ("Open_Or_Create_And_Map_Shared_Memory [12.5.1]");
      begin
         Shmd := Open_Or_Create_And_Map_Shared_Memory
           (Object_Name, Allow_Write, Test_perm);
      exception
         when E1 : POSIX_Error =>
            if POSIX.Get_Error_Code = No_Space_Left_On_Device then
               Comment ("insufficient space left to create a new object");
            elsif POSIX.Get_Error_Code = Not_Enough_Space then
               Comment ("insufficient room to effect mapping");
            elsif POSIX.Get_Error_Code = Operation_Not_Implemented then
               Optional (Shared_Memory_Objects_Option,
                         Operation_Not_Implemented, E1, "A001");
            else Unexpected_Exception (E1, "A002");
            end if;
            raise Local_Failure;
         when E2 : others => Unexpected_Exception (E2, "A003");
            raise Local_Failure;
      end;

      Get_File_Control (Shmd, Mode, Option);

      Assert (Mode = Read_Write, "A004");

      begin
         Shmd := Open_Or_Create_And_Map_Shared_Memory
           (Object_Name, Allow_Write, Test_perm, Exclusive);
         Fail ("A005: Exclusive option improperly implemented");
      exception
         when E1 : POSIX_Error =>

            Check_Error_Code (File_Exists, E1, "A006");
         when E2 : others => Unexpected_Exception (E2, "A007");
      end;

      Unmap_And_Close_Shared_Memory (Shmd);
      Unlink_Shared_Memory (Object_Name);

   exception
   when Local_Failure => null;
   when E1 : POSIX_Error =>
      Optional (Shared_Memory_Objects_Option,
               Operation_Not_Implemented, E1, "A008");
   when E2 : others => Unexpected_Exception (E2, "A009");
   end;

   -----------------------------------------------------------------------

   declare
      Shmd : File_Descriptor;
      Option : Open_Option_Set;
      Test_perm : Permission_Set := Owner_Permission_Set;
      Mode : File_Mode;

   begin
      Test ("Open_And_Map_Shared_Memory [12.5.1]");
      Shmd := Open_Or_Create_And_Map_Shared_Memory
              (Object_Name, Allow_Write, Test_perm);

      Unmap_And_Close_Shared_Memory (Shmd);

      begin
         Shmd := Open_And_Map_Shared_Memory
                 (Object_Name, Allow_Write);
      exception

      when E1 : POSIX_Error =>
            if POSIX.Get_Error_Code = No_Space_Left_On_Device then
               Unexpected_Exception (E1, "A010");
            elsif POSIX.Get_Error_Code = Not_Enough_Space then
               Unexpected_Exception (E1, "A011");
            else Optional (Shared_Memory_Objects_Option,
                           Operation_Not_Implemented, E1, "A012");
            end if;
         when E2 : others =>
            Unexpected_Exception (E2, "A013");
      end;

      Get_File_Control (Shmd, Mode, Option);


      Assert (Mode = Read_Write, "A014");

      Unmap_And_Close_Shared_Memory (Shmd);
      Unlink_Shared_Memory (Object_Name);

   exception
   when Local_Failure => null;
   when E1 : POSIX_Error =>
      Optional (Shared_Memory_Objects_Option,
        Operation_Not_Implemented, E1, "A015");
   when E2 : others => Unexpected_Exception (E2, "A016");
   end;

   -----------------------------------------------------------------------

   declare
      Shmd : File_Descriptor;
      Test_perm : Permission_Set := Owner_Permission_Set;
   begin
      Test ("creation with invalid name");
      Shmd := Open_Or_Create_And_Map_Shared_Memory
        (TP.Invalid_Shared_Memory_Object_Name (1), Allow_Write, Test_perm);
      Unmap_And_Close_Shared_Memory (Shmd);
      Unlink_Shared_Memory (Object_Name);

   exception
      when E1 : POSIX_Error =>
         Optional (Shared_Memory_Objects_Option,
           Operation_Not_Implemented, Invalid_Argument, E1,
           "A017");
      when E2 : others => Unexpected_Exception (E2, "A018");
   end;

   -----------------------------------------------------------------------

   declare
      Shmd : File_Descriptor;
      acc : Shared_Access;
   begin
      Test ("Access Shared Memory [12.5.2]");
      Shmd := Open_Or_Create_And_Map_Shared_Memory
              (Object_Name, Allow_Write, Owner_Permission_Set);
      begin
         acc := Access_Shared_Memory (Shmd);
      exception
      when E1 : POSIX_Error =>
         if POSIX.Get_Error_Code = Bad_File_Descriptor then
            Unexpected_Exception (E1, "A019");
         end if;
      when E2 : others =>
         Unexpected_Exception (E2, "A020");
      end;
      Test ("Close Shared Memory [12.5.3]");
      Unmap_And_Close_Shared_Memory (Shmd);
      Test ("Remove Shared Memory [12.5.4]");
      Unlink_Shared_Memory (Object_Name);
   exception
   when Local_Failure => null;
   when E1 : POSIX_Error =>
      Optional (Shared_Memory_Objects_Option,
                Operation_Not_Implemented, E1, "A021");
   when E2 : others => Unexpected_Exception (E2, "A022");
   end;

   -----------------------------------------------------------------------

   declare
      Shmd : File_Descriptor;
   begin
      Test ("Lock/Unlock_Shared_Memory [12.5.5]");
      Shmd := Open_Or_Create_And_Map_Shared_Memory
           (Object_Name, Allow_Write, Owner_Permission_Set);
      begin
         Lock_Shared_Memory (Shmd);

      exception
      when E1 : POSIX_Error =>
         if POSIX.Get_Error_Code = Operation_Not_Permitted then
            Unexpected_Exception (E1, "A023");
         elsif POSIX.Get_Error_Code = Not_Enough_Space then
            Unexpected_Exception (E1, "A024");
         elsif POSIX.Get_Error_Code =  Operation_Not_Implemented then
            Optional (Shared_Memory_Objects_Option,
                    Operation_Not_Implemented, E1, "A025");
         else Unexpected_Exception (E1, "A026");
         end if;
      when E2 : others =>
         Unexpected_Exception (E2, "A027");
      end;

      begin
         Unlock_Shared_Memory (Shmd);
      exception
      when E1 : POSIX_Error =>
         if POSIX.Get_Error_Code = Operation_Not_Permitted then
            Unexpected_Exception (E1, "A028");
         else Optional (Shared_Memory_Objects_Option,
                      Operation_Not_Implemented, E1, "A029");
         end if;
      when E2 : others => Unexpected_Exception (E2, "A030");
      end;

      Unmap_And_Close_Shared_Memory (Shmd);
      Unlink_Shared_Memory (Object_Name);

   exception
   when Local_Failure => null;
   when E1 : POSIX_Error =>
      Optional (Shared_Memory_Objects_Option,
                Operation_Not_Implemented, E1, "A031");
   when E2 : others => Unexpected_Exception (E2, "A032");
   end;

   -----------------------------------------------------------------------

   Done;
exception
when E : others => Fatal_Exception (E, "A033");
end p120500;

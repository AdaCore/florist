------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                             P 0 5 0 3 0 0                                --
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

--  Setup:  This test should be run with umask "022", i.e.,
--  POSIX_Permissions.Get_Allowed_Process_Permissions" should
--  return (Owner_Read | Owner_Write | Owner_Execute |
--          Group_Read | Others_Read => True, others => False);

with POSIX,
     POSIX_Calendar,
     POSIX_Files,
     POSIX_File_Status,
     POSIX_Permissions,
     POSIX_Process_Identification,
     POSIX_Report,
     Test_Parameters,
     Text_IO;

procedure p050300 is

   use POSIX,
       POSIX_Calendar,
       POSIX_Files,
       POSIX_File_Status,
       POSIX_Permissions,
       POSIX_Process_Identification,
       POSIX_Report,
       Test_Parameters,
       Text_IO;

   type File_Types is
     (Unknown,
      Directory,
      Regular,
      FIFO,
      Character_Special,
      Block_Special);

   The_Status : Status;

   procedure Check_Status
     (S : Status;
      Expected_Type : File_Types);

   procedure Check_Status
     (S : Status;
      Expected_Type : File_Types) is
      Found_Type : File_Types;
      Now : POSIX_Time;

   begin
      if Is_Directory (S) then Found_Type := Directory;
      elsif Is_Regular_File (S) then Found_Type := Regular;
      elsif Is_FIFO (S) then Found_Type := FIFO;
      elsif Is_Character_Special_File (S) then Found_Type := Character_Special;
      elsif Is_Block_Special_File (S) then Found_Type := Block_Special;
      else Found_Type := Unknown;
      end if;
      Assert (Found_Type = Expected_Type,
        "File type not as declared: " & File_Types'Image (Found_Type));

      Now := Clock;
      Assert (Last_Status_Change_Time_Of (S) <= Now
        and then Last_Access_Time_Of (S) <= Now
        and then Last_Modification_Time_Of (S) <= Now,
        "A001: time stamp on this file is newer than current time");
   exception when E : others => Unexpected_Exception (E, "A002");
   end Check_Status;

begin

   Header ("p050300");
   Test ("package POSIX_File_Status [5.3]");
   ---------------------------------------------------------------------

   if POSIX_Permissions.Get_Allowed_Process_Permissions /=
      POSIX_Permissions.Permission_Set'
        (Owner_Read | Owner_Write | Owner_Execute |
         Group_Read | Others_Read => True, others => False) then
      Fatal ("A003: Incorrect test setup");
   end if;

   declare
      --  Tests require us to have a file called "The_Test_File" with
      --  a file permission of 4700.
      --  We generate the file here.
      Test_File : Text_IO.File_Type;
      Test_File_Perm : constant Permission_Set :=
          (Owner_Read | Owner_Write | Owner_Execute
         | Set_User_ID => True,
         others => False);
   begin

      Create (Test_File, Out_File, "The_Test_File");
      Put (Test_File, "hello");
      Close (Test_File);

      Change_Permissions ("The_Test_File", Test_File_Perm);
   end;

   ---------------------------------------------------------------------

   declare
      My_Uid : constant User_ID := Get_Real_User_ID;
      My_Gid : constant Group_ID := Get_Real_Group_ID;
      Dev : Device_ID;
      Ino : File_ID;
      Size : IO_Count;
      Test_File_Perm : constant Permission_Set :=
        (Owner_Read | Owner_Write | Owner_Execute | Set_User_ID => True,
         others => False);
   begin
      Comment ("status of regular file");
      The_Status := Get_File_Status ("The_Test_File");
      Comment ("Device/File_ID_Of regular file");
      Check_Status (The_Status, Regular);
      Dev := Device_ID_Of (The_Status);
      Ino := File_ID_Of (The_Status);
      --  Check if permissions are 4700
      Assert (Permission_Set_Of (The_Status) = Test_File_Perm, "A004");

      --  Check if Link count is 1
      Assert (Link_Count_Of (The_Status) = 1, "A005");
      --  Check Uid
      Assert (Owner_Of (The_Status) = My_Uid, "A006");
      --  Check Gid
      Assert (Group_Of (The_Status) = My_Gid, "A007");
      --  Check Size is 6
      Assert (Size_Of (The_Status) = 6, "A008");

   ---------------------------------------------------------------------

      Test ("Last_Access/Modification/Status_Change_Time_Of");
      --  Test versions that return Ada.Calendar.Time instead of POSIX_Time.
      --  Check access time
      Assert (Last_Access_Time_Of (The_Status) =
         To_POSIX_Time (To_Time
           (Last_Access_Time_Of (The_Status))), "A009");
      --  Check mod time
      Assert (Last_Modification_Time_Of (The_Status) =
         To_POSIX_Time (To_Time
           (Last_Modification_Time_Of (The_Status))), "A010");
      --  Check change time
      Assert (Last_Status_Change_Time_Of (The_Status) =
         To_POSIX_Time (To_Time
           (Last_Status_Change_Time_Of (The_Status))), "A011");

   ---------------------------------------------------------------------

      Comment ("Get_File_Status");
      The_Status := Get_File_Status (".");

   ---------------------------------------------------------------------

      Comment ("Size_Of directory");
      Check_Status (The_Status, Directory);
      begin
         Size := Size_Of (The_Status);
         Expect_Exception ("A012");
      exception
      when POSIX_Error =>
         Check_Error_Code (Invalid_Argument, "A013");
      end;
      Assert (Dev /= Device_ID_Of (The_Status)
        or Ino /= File_ID_Of (The_Status), "A014");

   ---------------------------------------------------------------------

      Comment ("status of character special file");
      The_Status := Get_File_Status (Valid_Character_Special_File_Name);
      Check_Status (The_Status, Character_Special);

   ---------------------------------------------------------------------

      Comment ("status of block special device");
      begin
         The_Status := Get_File_Status (Valid_Block_Device_Name);
         Check_Status (The_Status, Block_Special);
         exception
      when E : POSIX_Error =>
         Unexpected_Exception (E, "A015");
      end;

   ---------------------------------------------------------------------

      Comment ("status of nonexistent file");
      begin
         The_Status := Get_File_Status (Valid_Nonexistent_File_Name);
         Expect_Exception ("A016");
      exception
      when POSIX_Error =>
         Check_Error_Code (No_Such_File_Or_Directory, "A017");
      end;
   exception when E : others => Unexpected_Exception (E, "A018");
   end;

   --  remove the file created for this test.
   Unlink ("The_Test_File");

   ---------------------------------------------------------------------

   Done;
exception when E : others =>
   Unlink ("The_Test_File");
   Fatal_Exception (E, "A019");
end p050300;

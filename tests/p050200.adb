-----------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                             P 0 5 0 2 0 0                                --
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

--  This is a test of package POSIX_Files.
--  It is far from comprehensive, since the package is outside the scope
--  of the POSIX realtime extensions.

with POSIX,
     POSIX_Calendar,
     POSIX_Files,
     POSIX_File_Status,
     POSIX_Permissions,
     POSIX_Process_Environment,
     POSIX_Process_Identification,
     POSIX_Report,
     POSIX_Configurable_File_Limits,
     Test_Parameters,
     Text_IO;

procedure p050200 is

   use POSIX,
       POSIX_Calendar,
       POSIX_Files,
       POSIX_File_Status,
       POSIX_Permissions,
       POSIX_Process_Environment,
       POSIX_Process_Identification,
       POSIX_Report,
       POSIX_Configurable_File_Limits,
       Test_Parameters,
       Text_IO;

   Default_Permission_Set : constant Permission_Set :=
     (Owner_Read    | Owner_Write | Owner_Execute | Group_Read |
      Group_Execute | Others_Read | Others_Execute => True,
      others => false);

   type File_Types is
     (Unknown,
      Directory,
      Regular,
      FIFO,
      Character_Special,
      Block_Special);

   type Time_Stamps is
     (None,
      Last_Status_Change,
      Last_Access,
      Last_Modification);

   Status_A,
   Status_B : Status;

   SC_Time_1,
   Mod_Time_1,
   SC_Time_2,
   Mod_Time_2 : POSIX_Time := POSIX_Calendar.Clock;

   procedure Check_Status
     (S : Status;
      Expected_Type : File_Types;
      Compare_Time : POSIX_Time;
      Time_Check_Type : Time_Stamps);
   procedure Check_Equal
     (T1, T2 : POSIX_Time;
      Msg : String);
   procedure Check_Precedes
     (T1, T2 : POSIX_Time;
      Msg : String);
   function Image (T : POSIX_Time) return String;

   procedure Check_Status
     (S : Status;
      Expected_Type : File_Types;
      Compare_Time  : POSIX_Time;
      Time_Check_Type : Time_Stamps) is
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

      case Time_Check_Type is
      when Last_Status_Change =>
         Assert (Last_Status_Change_Time_Of (S) /= Compare_Time,
           "A002: Last_Status_Change_Time did not change");
      when Last_Access =>
         Assert (Last_Access_Time_Of (S) /= Compare_Time,
           "A003: Last_Access_Time did not change");
      when Last_Modification =>
         Assert (Last_Modification_Time_Of (S) /= Compare_Time,
           "A004: Last_Modification_Time did not change");
      when others => null;
      end case;

   exception when E : others => Unexpected_Exception (E, "A005");
   end Check_Status;

   type Months is
    (NAM, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec);

   function Image (T : POSIX_Time) return String is
      Year    : Year_Number;
      Month   : Month_Number;
      Day     : Day_Number;
      Seconds : Duration;
   begin
      Split (T, Year, Month, Day, Seconds);
      if Duration (Integer (Seconds)) /= Seconds then
         return Year_Number'Image (Year) & " " &
           Months'Image (Months'Val (Month)) &
           Day_Number'Image (Day) &
           Integer'Image (Integer (Seconds)) &
           "+ ...";
      else
         return Year_Number'Image (Year) & " " &
           Months'Image (Months'Val (Month)) &
           Day_Number'Image (Day) &
           Integer'Image (Integer (Seconds));
      end if;
   end Image;

   procedure Check_Equal (T1, T2 : POSIX_Time; Msg : String) is
   begin
      if T1 = T2 then return; end if;
      if abs (T1 - T2) < 1.0 then
         --  Fail times off, but by less than one second
         Fail (Msg & ": times unequal by "
           & Integer'Image (Integer ((T1 - T2) * 1_000_000)) & "us");
      else
         --  Fail times off by more than one second
         Fail (Msg & ": times unequal by "
           & Integer'Image (Integer ((T1 - T2))) & "s");
      end if;
   exception when E : others => Unexpected_Exception (E, Msg);
   end Check_Equal;

   procedure Check_Precedes (T1, T2 : POSIX_Time; Msg : String) is
   begin
      if T1 <= T2 then return; end if;
      if T1 - T2 < 1.0 then
         --  Fail times out of order, but by less than one second
         Fail (Msg & ": times out of order by "
           & Integer'Image (Integer ((T1 - T2) * 1_000_000)) & "us");
      else
         --  Fail times out of order by more than one second
         Fail (Msg & ": times out of order by "
           & Integer'Image (Integer ((T1 - T2))) & "s");
      end if;
   exception when E : others => Unexpected_Exception (E, Msg);
   end Check_Precedes;

   task Watchdog;

   task body Watchdog is
   begin
      delay Short_Watchdog_Timeout;
      Fatal ("A006: watchdog timeout");
   end Watchdog;

begin

   Header ("p050201");
   Test ("package POSIX_Files [5.2]");

   -------------------------------------------------------------------------

   Test ("Create and Remove Files [5.2.1]");

   Comment ("Create_Directory (A_New_Directory)");
   Create_Directory ("A_New_Directory", Access_Permission_Set);
   Comment ("Get_File_Status (A_New_Directory)");
   Status_B := Get_File_Status ("A_New_Directory");
   Comment ("Verify A_New_Directory is a directory");
   Check_Status (Status_B, Directory, SC_Time_1, None);
   Comment ("Verify the permission set of A_New_Directory is as default");
   Assert (Permission_Set_Of (Status_B) (Owner_Read .. Others_Execute)
            = Default_Permission_Set (Owner_Read .. Others_Execute), "A007");

   Comment ("Create_FIFO A_New_FIFO");
   Create_FIFO ("A_New_FIFO", Access_Permission_Set);
   Status_B := Get_File_Status ("A_New_FIFO");
   Comment ("Verify A_New_FIFO is a FIFO");
   Check_Status (Status_B, FIFO, SC_Time_1, None);
   Comment ("Verify the permission set of the new FIFO is as default");
   Assert (Permission_Set_Of (Status_B) (Owner_Read .. Others_Execute)
            = Default_Permission_Set (Owner_Read .. Others_Execute), "A008");

   Comment ("Unlink A_New_FIFO");
   Unlink ("A_New_FIFO");
   Comment ("Verify A_New_FIFO is removed by unlinking again");
   begin
      Unlink ("A_New_FIFO");
      Expect_Exception ("A009");
   exception
   when POSIX_Error => Check_Error_Code (No_Such_File_Or_Directory, "A010");
   end;

   Comment ("Remove A_New_Directory");
   Remove_Directory ("A_New_Directory");
   Comment ("Verify A_New_Directory is removed by removing again");
   begin
      Remove_Directory ("A_New_Directory");
      Expect_Exception ("A011");
   exception
   when POSIX_Error => Check_Error_Code (No_Such_File_Or_Directory, "A012");
   end;

   ---------------------------------------------------------------------------

   Test ("Create and Remove Files [5.2.1]");

   Comment ("Testing Error Handling for Permission_Denied");
   Comment ("Create A_New_Directory with only Owner_Write access");
   Create_Directory
     ("A_New_Directory", (Owner_Write => true, others => false));
   Comment ("Try Create_Directory under A_New_Directory");
   begin
      Create_Directory ("A_New_Directory/Sub_Directory",
        Access_Permission_Set);
   exception
   when POSIX_Error => Check_Error_Code (Permission_Denied, "A013");
   end;
   Comment ("Try Create_FIFO under A_New_Directory");
   begin
      Create_Directory ("A_New_Directory/A_FIFO",
        Access_Permission_Set);
   exception
   when POSIX_Error => Check_Error_Code (Permission_Denied, "A014");
   end;

   Remove_Directory ("A_New_Directory");

   --  .... Revisit the tests below to see how they are affected by
   --  filename truncation, as reported by
   --  POSIX_Configurable_File_Limits.Filename_Is_Truncated.

   Comment ("Testing Handling of Filename_Too_Long");
   declare
      Long_Path_Limit : Integer :=
        Integer (Pathname_Maximum (Get_Working_Directory));
      type String_Access is access String;
      Over_Long_Name : String_Access;
   begin
      Over_Long_Name := new String'("L");
      Comment ("Pathname_Maximum("
        & To_String (Get_Working_Directory)
        & ")=" & Integer'Image (Long_Path_Limit));
      for I in 1 .. Long_Path_Limit loop
         Over_Long_Name := new String'(Over_Long_Name.all & "L");
      end loop;

      begin
         Create_Directory
          (To_POSIX_String (Over_Long_Name.all), Access_Permission_Set);
         Expect_Exception ("A015");
      exception
      when POSIX_Error => Check_Error_Code (Filename_Too_Long, "A016");
      end;

      begin
         Create_FIFO (To_POSIX_String (Over_Long_Name.all),
                    Access_Permission_Set);
         Expect_Exception ("A017");
      exception
      when POSIX_Error => Check_Error_Code (Filename_Too_Long, "A018");
      end;

      begin
         Unlink (To_POSIX_String (Over_Long_Name.all));
         Expect_Exception ("A019");
      exception
      when POSIX_Error => Check_Error_Code (Filename_Too_Long, "A020");
      end;

      begin
         Remove_Directory (To_POSIX_String (Over_Long_Name.all));
         Expect_Exception ("A021");
      exception
      when POSIX_Error => Check_Error_Code (Filename_Too_Long, "A022");
      end;
   end;

   Comment ("Testing Error Handling for File_Exists");
   Comment ("Create a directory A_New_Directory");
   Create_Directory ("A_New_Directory", Access_Permission_Set);
   Comment ("Try create A_New_Directory again, should fail");

   begin
      Create_Directory ("A_New_Directory", Access_Permission_Set);
      Expect_Exception ("A023");
   exception when POSIX_Error => Check_Error_Code (File_Exists, "A024");
   end;

   Comment ("Remove A_New_Directory");
   Remove_Directory ("A_New_Directory");

   Comment ("Create a directory A_New_Directory with owner write only");
   Create_Directory
     ("A_New_Directory", (Owner_Write => true, others => false));

   Comment ("Try create A_New_Directory again, should fail");
   begin
      Create_Directory ("A_New_Directory", Access_Permission_Set);
      Expect_Exception ("A025");
   exception when POSIX_Error => Check_Error_Code (File_Exists, "A026");
   end;

   Comment ("Remove A_New_Directory");
   Remove_Directory ("A_New_Directory");

   Comment ("Create a FIFO A_New_FIFO");
   Create_FIFO ("A_New_FIFO", Access_Permission_Set);
   Comment ("Try create A_New_FIFO again, should fail");
   begin
      Create_FIFO ("A_New_FIFO", Access_Permission_Set);
      Expect_Exception ("A027");
   exception when POSIX_Error => Check_Error_Code (File_Exists, "A028");
   end;
   Unlink ("A_New_FIFO");

   --  .... Consider testing error handling for Too_Many_Links.
   --   Comment ("Testing error handling for Too_Many_Links");
   --   Comment (string (Links_Maximum (Current_Working_Directory)));

   Comment ("Testing error handling for No_Such_File_Or_Directory");

   Comment ("Try create a directory with a nonexistent pathname component");
   begin
      Create_Directory (Valid_Nonexistent_File_Name & "/Test",
                          Access_Permission_Set);
      Expect_Exception ("A029");
   exception
   when POSIX_Error => Check_Error_Code (No_Such_File_Or_Directory, "A030");
   end;

   Comment ("Try creating a directory with name as null string");
   begin
      Create_Directory ("", Access_Permission_Set);
      Expect_Exception ("A031");
   exception
   when POSIX_Error => Check_Error_Code (No_Such_File_Or_Directory, "A032");
   end;

   Comment ("Try create a FIFO with a nonexistent pathname component");
   begin
      Create_FIFO (Valid_Nonexistent_File_Name
        & "/Test", Access_Permission_Set);
      Expect_Exception ("A033");
   exception
   when POSIX_Error => Check_Error_Code (No_Such_File_Or_Directory, "A034");
   end;

   Comment ("Try create a FIFO with name as null string");
   begin
      Create_FIFO ("", Access_Permission_Set);
      Expect_Exception ("A035");
   exception
   when POSIX_Error => Check_Error_Code (No_Such_File_Or_Directory, "A036");
   end;

   Comment ("Try remove a directory with a nonexistent pathname");
   begin
      Remove_Directory (Valid_Nonexistent_File_Name);
      Expect_Exception ("A037");
   exception
   when POSIX_Error => Check_Error_Code (No_Such_File_Or_Directory, "A038");
   end;

   Comment ("Try remove a directory with name as a null string ");
   begin
      Remove_Directory ("");
      Expect_Exception ("A039");
   exception
   when POSIX_Error => Check_Error_Code (No_Such_File_Or_Directory, "A040");
   end;

   Comment ("Try unlink a file with a nonexistent pathname");
   begin
      Unlink (Valid_Nonexistent_File_Name);
      Expect_Exception ("A041");
   exception
   when POSIX_Error => Check_Error_Code (No_Such_File_Or_Directory, "A042");
   end;

   Comment ("Try unlink a file with name as a null string ");
   begin
      Remove_Directory ("");
      Expect_Exception ("A043");
   exception
   when POSIX_Error => Check_Error_Code (No_Such_File_Or_Directory, "A044");
   end;

   --  .... No_Space_Left_On_Device is not practical to test.
   --  .... Read_Only_File_System might be testable, but will Check
   --  an addition to Test_Parameters, to specify the name of a file that is
   --  in a read-only filesystem, if one exists.

   Comment ("Testing handling of Not_A_Directory");
   Comment ("Create A_FIFO, then try to treat A_FIFO as a directory");
   Create_FIFO ("A_FIFO", Access_Permission_Set);
   begin
      Create_Directory ("A_FIFO/Dir", Access_Permission_Set);
      Expect_Exception ("A045");
   exception when POSIX_Error => Check_Error_Code (Not_A_Directory, "A046");
   end;
   begin
      Create_FIFO ("A_FIFO/FIFO", Access_Permission_Set);
      Expect_Exception ("A047");
   exception when POSIX_Error => Check_Error_Code (Not_A_Directory, "A048");
   end;
   begin
      Unlink ("A_FIFO/File");
      Expect_Exception ("A049");
   exception when POSIX_Error => Check_Error_Code (Not_A_Directory, "A050");
   end;

   Comment ("Unlink A_FIFO generated for test");
   Unlink ("A_FIFO");

   --  .... Resource_Busy is implementation related,
   --  and so is not tested here

   Comment ("Testing handling of Directory_Not_Empty");
   begin
      Comment ("Create a Not_Empty_Directory with a FIFO in it");
      Create_Directory ("Not_Empty_Directory", Access_Permission_Set);
      Create_FIFO ("Not_Empty_Directory/A_FIFO", Access_Permission_Set);
      Comment ("Try to remove Not_Empty_Directory");
      Remove_Directory ("Not_Empty_Directory");
      Expect_Exception ("A051");
   exception when POSIX_Error =>
      if POSIX.Get_Error_Code = Directory_Not_Empty then
         Comment ("Correctly returned error code Directory_Not_Empty");
      else Check_Error_Code (File_Exists, "A052");
      end if;
   end;
   Comment ("Now delete the FIFO first");
   Unlink ("Not_Empty_Directory/A_FIFO");
   Comment ("Then remove the directory");
   Remove_Directory ("Not_Empty_Directory");

   -----------------------------------------------------------------------

   Test ("Inquiries on File Types [5.2.2]");

   Comment ("Create a small file A_Test_File");
   declare
      A_File : Text_IO.File_Type;
   begin
      Create (A_File, Out_File, "A_Test_File");
      Put (A_File, "small");
      Close (A_File);
   exception when E : others => Fatal_Exception (E, "A053");
   end;
   Comment ("Create a small FIFO");
   Create_FIFO ("A_Test_FIFO", Access_Permission_Set);

   begin
      Comment ("Testing Is_File on A_Test_File");
      Assert (Is_File ("A_Test_File"), "A054");
      Comment ("Testing Is_File on A_Test_FIFO");
      Assert (not Is_File ("A_Test_FIFO"), "A055");
      Comment ("Testing Is_File on a nonexistent file");
      Assert (not Is_File (Valid_Nonexistent_File_Name), "A056");
      Comment ("Testing Is_File on a null string");
      Assert (not Is_File (""), "A057");
      Assert (not Is_File ("///"), "A058");
      Comment ("Testing Is_File on current directory");
      Assert (not Is_File ("."), "A059");

      Comment ("Testing Is_Directory on Get_Working_Directory");
      Assert (Is_Directory (Get_Working_Directory), "A060");
      Comment ("Testing Is_Directory on A_Test_File");
      Assert (not Is_Directory ("A_Test_File"), "A061");
      Comment ("Testing Is_Directory on a nonexistent pathname");
      Assert (not Is_Directory (Valid_Nonexistent_File_Name), "A062");
      Comment ("Testing Is_Directory on a null string");
      Assert (not Is_Directory (""), "A063");
      Comment ("Testing Is_Directory on a not qualified pathname");
      Assert (Is_Directory ("."), "A064");
      Comment ("Testing Is_Directory on current directory");

      Comment ("Tesing Is_FIFO on A_Test_FIFO");
      Assert (Is_FIFO ("A_Test_FIFO"), "A065");
      Comment ("Testing Is_FIFO on A_Test_File");
      Assert (not Is_FIFO ("A_Test_File"), "A066");
      Comment ("Testing Is_FIFO on a nonexistent file");
      Assert (not Is_FIFO (Valid_Nonexistent_File_Name), "A067");
      Comment ("Testing Is_FIFO on a null string");
      Assert (not Is_FIFO (""), "A068");
      Comment ("Testing Is_FIFO on a not qualified filename");
      Assert (not Is_FIFO ("///"), "A069");

      Comment ("Testing Is_Character_Special_File on valid name");
      Assert (Is_Character_Special_File
        (Valid_Character_Special_File_Name), "A070");
      Comment ("Testing Is_Character_Special_File on A_Test_File");
      Assert (not Is_Character_Special_File ("A_Test_File"), "A071");
      Comment ("Testing Is_Character_Special_File on a nonexistent file");
      Assert (not Is_Character_Special_File
        (Valid_Nonexistent_File_Name), "A072");
      Comment ("Testing Is_Character_Special_File on a null string");
      Assert (not Is_Character_Special_File (""), "A073");
      Comment ("Testing Is_Character_Special_File on a not valid filename");
      Assert (not Is_Character_Special_File ("///"), "A074");

      Comment ("Testing Is_Block_Special_File on a block device");
      Assert (Is_Block_Special_File (Valid_Block_Device_Name), "A075");
      Comment ("Testing Is_Block_Special_File on A_Test_File");
      Assert (not Is_Block_Special_File ("A_Test_File"), "A076");
      Comment ("Testing Is_Block_Special_File on a nonexistent file");
      Assert (not Is_Block_Special_File
        (Valid_Nonexistent_File_Name), "A077");
      Comment ("Testing Is_Block_Special_File on a null string");
      Assert (not Is_Block_Special_File (""), "A078");
      Comment ("Testing Is_Block_Special_File on a not qualified filename");
      Assert (not Is_Block_Special_File ("///"), "A079");
      Comment ("No exceptions returned by above inquiry funtions");

      Assert (Is_File_Present ("A_Test_File"), "A080");
      Assert (Is_File_Present ("."), "A081");
      Assert (not Is_File_Present (Valid_Nonexistent_File_Name), "A082");
      Assert (Existence ("A_Test_File") = No_Error, "A083");
      Assert (Is_File_Present ("."), "A084");
      Assert (Existence (Valid_Nonexistent_File_Name)
        = No_Such_File_Or_Directory, "A085");

   exception
      when E : others => Unexpected_Exception (E, "A086");
   end;

   -----------------------------------------------------------------------

   Test ("Modify File Pathnames [5.2.3]");

   Comment ("Create A_New_FIFO as a test file");
   Status_A := Get_File_Status (Get_Working_Directory);
   Create_FIFO ("A_New_FIFO", Access_Permission_Set);
   Status_B := Get_File_Status ("A_New_FIFO");
   SC_Time_1 := Last_Status_Change_Time_Of (Status_B);
   Mod_Time_1 := Last_Modification_Time_Of (Status_A);
   SC_Time_2 := Last_Status_Change_Time_Of (Status_A);
   Comment ("Pause a second for testing purpose");
   Comment ("current time", To_Timespec (Clock));
   delay 1.0;
   Comment ("current time", To_Timespec (Clock));
   Comment ("Link A_New_FIFO_Link to A_New_FIFO");
   Link ("A_New_FIFO", "A_New_FIFO_Link");
   Status_B := Get_File_Status ("A_New_FIFO");
   Comment ("Verify the Link_Count_Of (A_New_FIFO) = 2");
   Assert (Link_Count_Of (Status_B) = 2, "A087");
   Comment ("Verify time stamps of the file and parent directory is changed");
   Check_Status (Status_B, FIFO, SC_Time_1, Last_Status_Change);
   Status_A := Get_File_Status (Get_Working_Directory);
   Check_Status (Status_A, Directory, Mod_Time_1, Last_Modification);
   Check_Status (Status_A, Directory, SC_Time_2, Last_Status_Change);
   begin
      Comment ("Try linking current working directory to A_New_FIFO");
      Link ("A_New_FIFO", Get_Working_Directory);
      Expect_Exception ("A088");
   exception
   when POSIX_Error => Check_Error_Code (File_Exists, "A000");
   end;
   Status_B := Get_File_Status ("A_New_FIFO");
   Comment ("Verify the Link_Count remains unchanged if Link fails");
   Assert (Link_Count_Of (Status_B) = 2, "A089");
   Comment ("Testing linking directories");
   begin
      Comment ("First create A_New_Directory_1");
      Create_Directory ("A_New_Directory_1", Access_Permission_Set);
      Comment ("Then link A_New_Directory_Link to A_New_Directory_1");
      Link ("A_New_Directory_1", "A_New_Directory_Link");
      Comment ("System support links between directories");
      Comment ("Remove A_New_Directory_Link");
      Remove_Directory ("A_New_Directory_Link");
   exception when POSIX_Error =>
      Comment ("System does not support links between directories");
      Comment ("or process does not have appropriate privileges");
   end;
   Comment ("Testing procedure Rename");
   Comment ("Rename A_New_FIFO to A_Renamed_FIFO under A_FIFO_Dir");
   Create_Directory ("A_FIFO_Dir", Access_Permission_Set);
   Status_B := Get_File_Status ("A_FIFO_Dir");
   Status_A := Get_File_Status (Get_Working_Directory);
   SC_Time_1 := Last_Status_Change_Time_Of (Status_B);
   Mod_Time_1 := Last_Modification_Time_Of (Status_B);
   SC_Time_2 := Last_Status_Change_Time_Of (Status_A);
   Mod_Time_2 := Last_Modification_Time_Of (Status_A);
   delay 1.0;
   Rename ("A_New_FIFO", "A_FIFO_Dir/A_Renamed_FIFO");
   Assert (not Is_FIFO ("A_New_FIFO"), "A090");
   Assert (Is_FIFO ("A_FIFO_Dir/A_Renamed_FIFO"), "A091");
   begin
      Comment ("Try renaming a nonexistent file");
      Rename (Valid_Nonexistent_File_Name, "Valid_File_Name");
      Expect_Exception ("A092");
   exception
   when POSIX_Error => Check_Error_Code (No_Such_File_Or_Directory, "A000");
   end;

   ------------------------------------------------------------------
   --  .... A more thorough test would check the effects on open
   --  files, for the so-called "last close" semantics.

   -----------------------------------------------------------
   --  The link count of a directory includes one downward link
   --  plus one upward link from itself (.)
   --  plus one upward link for each subdirectory (..).
   --  In this case, A_FIFO_Dir has no subdirectories,
   --  so the link count is 2.

   Status_B := Get_File_Status ("A_FIFO_Dir");
   Assert (Link_Count_Of (Status_B) = 2,
     "A093: " & Integer'Image (Link_Count_Of (Status_B)));
   Check_Status (Status_B, Directory, SC_Time_1, Last_Status_Change);
   Check_Status (Status_B, Directory, Mod_Time_1, Last_Modification);
   Status_A := Get_File_Status (Get_Working_Directory);
   Check_Status (Status_A, Directory, SC_Time_2, Last_Status_Change);
   Check_Status (Status_A, Directory, Mod_Time_2, Last_Modification);


   ----------------------------------------------------------------------
   --  If the two pathnames both refer to links to the same existing
   --  file, Rename returns successfully and performs no other action.

   Comment ("A_New_FIFO_Link is the link to previous A_New_FIFO");
   Assert (Is_FIFO ("A_New_FIFO_Link"), "A094");
   Comment ("Rename A_FIFO_Dir/A_Renamed_FIFO to A_New_FIFO_Link");
   Rename ("A_FIFO_Dir/A_Renamed_FIFO", "A_New_FIFO_Link");
   Status_B := Get_File_Status ("A_New_FIFO_Link");
   Assert (Link_Count_Of (Status_B) = 2, "A095");
   Unlink ("A_New_FIFO_Link");
   begin
      Unlink ("A_FIFO_Dir/A_Renamed_FIFO");
   exception when POSIX_Error => null;
   end;
   Remove_Directory ("A_FIFO_Dir");

   Comment ("Create A_New_Directory_2");
   Comment ("Rename A_New_Directory_1 to A_New_Directory_2");
   begin
      Rename ("A_New_Directory_1", "A_New_Directory_2");
      Comment ("Remove A_New_Directory_2");
      Remove_Directory ("A_New_Directory_2");
   exception when E : POSIX_Error =>
      Unexpected_Exception (E, "A096: Renaming failed");
      Comment ("Remove both A_New_Directory_1 and A_New_Directory_2");
      Remove_Directory ("A_New_Directory_1");
      Remove_Directory ("A_New_Directory_2");
   end;

   begin

      Test ("Directory Iteration [5.2.4]");
      declare
         Saw_test_File : Boolean := False;

         procedure Check_One_File
           (D : Directory_Entry;
            Quit : in out Boolean);
         procedure Check_One_File
           (D : Directory_Entry;
            Quit : in out Boolean)
         is
            Name : constant POSIX_String  := Filename_Of (D);
         begin
            Assert (Name'First = 1, "A097");
            if Name = "A_Test_File" then
               --  Check if test_file occurs more than once
               Assert (Saw_test_File = False, "A098");
               Saw_test_File := True;
            end if;
         end Check_One_File;

         procedure Iterate is new
           For_Every_Directory_Entry (Check_One_File);
      begin
         Test ("Directory iterator");
         Iterate (".");
      end;

      declare
         First_Call : Boolean := True;

         procedure Check_One_File
           (D : Directory_Entry;
            Quit : in out Boolean);
         procedure Check_One_File
           (D : Directory_Entry;
            Quit : in out Boolean) is
         begin
            Assert (First_Call = True, "A099");
            First_Call := False;
            Quit := True;
         end Check_One_File;

         procedure Iterate is new
           For_Every_Directory_Entry (Check_One_File);

      begin
         Iterate (".");
         First_Call := True;
         Iterate ("..");
      end;

      declare
         My_exception : exception;
         First_Call : Boolean := True;

         procedure Check_One_File
           (D : Directory_Entry;
            Quit : in out Boolean);
         procedure Check_One_File
           (D : Directory_Entry;
            Quit : in out Boolean) is
         begin
            Assert (First_Call = True, "A100");
            First_Call := False;
            raise My_exception;
         end Check_One_File;

         procedure Iterate is new
           For_Every_Directory_Entry (Check_One_File);
      begin
         Iterate (".");
         Expect_Exception ("A101");
      exception
      when My_exception => null;
      end;

      Test ("Change_Owner_And_Group [5.2.5]");

      Create_Directory ("A_New_Directory", Access_Permission_Set);
      Change_Owner_And_Group
        ("A_Test_File", Get_Real_User_ID, Get_Real_Group_ID);
      --  File Should Be Unchanged.
      begin
         Change_Owner_And_Group
           (Valid_Nonexistent_File_Name, Get_Real_User_ID, Get_Real_Group_ID);
         Expect_Exception ("A102");
      exception
      when POSIX_Error => Check_Error_Code (No_Such_File_Or_Directory, "A103");
      end;

      Test ("Change_Permissions [5.2.5]");
      Change_Permissions ("A_New_Directory", Access_Permission_Set);
      Status_B := Get_File_Status ("A_New_Directory");
      Check_Status (Status_B, Directory, SC_Time_1, None);
      Assert (Permission_Set_Of (Status_B)
        = Access_Permission_Set, "A104");
      begin
         Change_Permissions
           (Valid_Nonexistent_File_Name, Access_Permission_Set);
         Expect_Exception ("A105");
      exception
      when POSIX_Error => Check_Error_Code (No_Such_File_Or_Directory, "A106");
      end;

      Remove_Directory ("A_New_Directory");

   exception when E : others => Unexpected_Exception (E, "A107");
   end;

   ----------------------------------------------------------------
   --  The following tests will fail if the representation
   --  of type POSIX_Calendar.Time and the value returned by
   --  POSIX_Calendar.Clock are more precise than the
   --  representation of file times.
   --  On pragmatic grounds, it is debatable whether this is truly
   --  an error, since a user may actually want this capability.

   begin

      Create_Directory ("A_New_Directory", Access_Permission_Set);

      Test ("Set_File_Times [5.2.5]");
      declare
         Access_Time : POSIX_Time;
         Mod_Time : POSIX_Time;
         Now : POSIX_Time := Clock;
      begin
         Comment ("Now =" & Image (Now));
         Access_Time := Now - 3600.0;
         Comment ("Access_Time =" & Image (Access_Time));
         Mod_Time := Access_Time + 1800.0;
         Comment ("Mod_Time =" & Image (Mod_Time));
         Set_File_Times ("A_New_Directory", Access_Time, Mod_Time);
         Status_B := Get_File_Status ("A_New_Directory");
         Check_Equal
           (Last_Access_Time_Of (Status_B), Access_Time, "A108");
         Check_Equal
           (Last_Modification_Time_Of (Status_B), Mod_Time, "A109");
      end;

      Test ("Set_File_Times, defaults [5.2.5]");
      declare
         Before : POSIX_Time := Clock;
         After  : POSIX_Time;
      begin
         Comment ("Before =" & Image (Before));
         Set_File_Times ("A_New_Directory");
         After := Clock;
         Comment ("After =" & Image (After));
         Status_B := Get_File_Status ("A_New_Directory");
         Comment ("Last_Access_Time =" &
           Image (Last_Access_Time_Of (Status_B)));
         Comment ("Last_Modification =" &
           Image (Last_Modification_Time_Of (Status_B)));
         Check_Precedes
           (Before, Last_Access_Time_Of (Status_B), "A110");
         Check_Precedes
           (Last_Access_Time_Of (Status_B), After, "A111");
         Check_Precedes
           (Before, Last_Modification_Time_Of (Status_B), "A112");
         Check_Precedes
           (Last_Modification_Time_Of (Status_B), After, "A113");
      end;

      Test ("Set_File_Times, nonexistent file [5.2.5]");
      declare
         Access_Time : POSIX_Time;
         Mod_Time : POSIX_Time;
      begin
         Access_Time := Clock;
         Mod_Time := Clock;
         Comment ("Mod_Time =" & Image (Mod_Time));
         Set_File_Times
           (Valid_Nonexistent_File_Name, Access_Time, Mod_Time);
         Expect_Exception ("A114");
      exception
      when POSIX_Error => Check_Error_Code (No_Such_File_Or_Directory, "A115");
      end;

      Test ("Set_File_Times, defaults, nonexistent file");
      begin
         Set_File_Times (Valid_Nonexistent_File_Name);
         Expect_Exception ("A116");
      exception
      when POSIX_Error => Check_Error_Code (No_Such_File_Or_Directory, "A117");
      end;

      Remove_Directory ("A_New_Directory");

   exception when E : others => Unexpected_Exception (E, "A118");
   end;

   -----------------------------------------------------------------------

   Assert (Is_File_Present ("A_Test_File"), "A119");
   Unlink ("A_Test_File");

   Assert (Is_File_Present ("A_Test_FIFO"), "A120");
   Unlink ("A_Test_FIFO");

   abort Watchdog;

   -----------------------------------------------------------------------

   Done;

exception when E : others =>  Fatal_Exception (E, "A121");
end p050200;

------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                             P 0 2 0 4 0 0                                --
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

--  Test package POSIX,
--  in IEEE Std 1003.5b Section 2.4.

with POSIX,
     POSIX_Report;

procedure p020400 is

   use POSIX,
       POSIX_Report;

begin

   Header ("p020400");
   Test ("package POSIX");

   ---------------------------------------------------------------------

   Test ("Version Identification [2.4.1]");
   begin
      Comment ("POSIX_Version = " & Integer'Image (POSIX_Version));
      Comment ("POSIX_Ada_Version = " & Integer'Image (POSIX_Ada_Version));
   exception when E : others => Unexpected_Exception (E, "A001");
   end;

   ---------------------------------------------------------------------

   Test ("Optional Facility Subtypes [2.4.1]");
   declare
      procedure Check_Option
         (First, Last : Boolean;
          Option : String);
      procedure Check_Option
         (First, Last : Boolean;
          Option : String) is
      begin
         if First = True then
            Comment (Option & " option is supported");
         elsif Job_Control_Support'Last = False then
            Comment (Option & " option is not supported");
         else Comment (Option & " option is indeterminate");
         end if;
      end Check_Option;
   begin
      Check_Option
        (Job_Control_Support'First, Job_Control_Support'Last,
         "Job Control Support");
      Check_Option
        (Saved_IDs_Support'First, Saved_IDs_Support'Last,
         "Saved IDs Support");
      Check_Option
        (Change_Owner_Restriction'First, Change_Owner_Restriction'Last,
         "Change Owner Restriction");
      Check_Option
        (Filename_Truncation'First, Filename_Truncation'Last,
         "Filename Truncation");
   exception when E : others => Unexpected_Exception (E, "A002");
   end;

   ---------------------------------------------------------------------

   Test ("Bytes and I/O Counts [2.4.1]");
   begin
      if Byte_Size /= 8 then
         Comment ("Byte_Size = " & Integer'Image (Byte_Size));
      end if;
      Comment ("IO_Count'Last = " & IO_Count'Image (IO_Count'Last));
      Assert (IO_Count_Maxima'First = 32767, "A003");
      Assert (Portable_Groups_Maximum = 0, "A004");
      Assert (Portable_Groups_Maximum <= Groups_Maxima'First, "A005");
      Assert (Portable_Argument_List_Maximum = 4096, "A006");
      Assert (Portable_Argument_List_Maximum <=
        Argument_List_Maxima'First, "A007");
      Assert (Portable_Child_Processes_Maximum = 6, "A008");
      Assert (Child_Processes_Maxima'First >=
        Portable_Child_Processes_Maximum, "A009");
      Assert (Portable_Open_Files_Maximum = 16, "A010");
      Assert (Portable_Open_Files_Maximum <=
         Open_Files_Maxima'First, "A011");
      Assert (Portable_Stream_Maximum = 8, "A012");
      Assert (Portable_Stream_Maximum <= Stream_Maxima'First,
        "A013");
      Assert (Portable_Time_Zone_String_Maximum = 3, "A014");
      Assert (Portable_Time_Zone_String_Maximum <=
        Time_Zone_String_Maxima'First, "A015");
      Assert (Portable_Link_Limit_Maximum = 8, "A016");
      Assert (Portable_Link_Limit_Maximum <=
        Link_Limit_Maxima'First, "A017");
      Assert (Portable_Input_Line_Limit_Maximum = 255, "A018");
      Assert (Portable_Input_Line_Limit_Maximum <=
        Input_Line_Limit_Maxima'First, "A019");
      Assert (Portable_Input_Queue_Limit_Maximum = 255, "A020");
      Assert (Portable_Input_Queue_Limit_Maximum <=
        Input_Queue_Limit_Maxima'First, "A021");
      Assert (Portable_Filename_Limit_Maximum = 14, "A022");
      Assert (Portable_Filename_Limit_Maximum <=
        Filename_Limit_Maxima'First, "A023");
      Assert (Portable_Pathname_Limit_Maximum = 255, "A024");
      Assert (Portable_Pathname_Limit_Maximum <=
         Pathname_Limit_Maxima'First, "A025");
      Assert (Portable_Pipe_Limit_Maximum = 512, "A026");
      Assert (Portable_Pipe_Limit_Maximum <= Pipe_Limit_Maxima'First,
        "A027");
   exception when E : others => Unexpected_Exception (E, "A028");
   end;

   ---------------------------------------------------------------------

   Test ("Blocking_Behavior type [2.4.1]");
   begin
      Assert (Tasks = Blocking_Behavior'First and
        Blocking_Behavior'Pos (Program) = 1 and
        Special = Blocking_Behavior'Last and
        Text_IO_Blocking_Behavior'First in Blocking_Behavior and
        IO_Blocking_Behavior in Blocking_Behavior and
        File_Lock_Blocking_Behavior in Blocking_Behavior and
        Wait_For_Child_Blocking_Behavior in Blocking_Behavior and
        Realtime_Blocking_Behavior'First in Blocking_Behavior,
        "A029");
   exception when E : others => Unexpected_Exception (E, "A030");
   end;

   ---------------------------------------------------------------------

   Test ("Signal_Masking type [2.4.1] ");
   begin
      Assert (No_Signals = Signal_Masking'First and
        Signal_Masking'Pos (RTS_Signals) = 1 and
        All_Signals = Signal_Masking'Last,
        "A031");
   exception when E : others => Unexpected_Exception (E, "A032");
   end;

   ---------------------------------------------------------------------

   Test ("POSIX_Character type [2.4.2]");
   declare
      S : constant POSIX_String :=
        " 0123456789" &
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ" &
        "abcdefghijklmnopqrstuvwxyz" &
        "._-/" & '"' & "#&'*)*+,:;<=>|";
   begin
      null;
   exception when E : others => Unexpected_Exception (E, "A033");
   end;

   ---------------------------------------------------------------------

   Test ("POSIX_String type [2.4.3]");
   begin

      Assert (POSIX_String' (To_POSIX_String (String'
        ("ABCD/abcd"))) = "ABCD/abcd", "A034");
      Assert (To_String
        ("ABCD/abcd") = "ABCD/abcd", "A035");
      Assert (To_POSIX_String (To_Stream_Element_Array
        ("ABCD/abcd")) = "ABCD/abcd", "A036");

      Assert (Is_Filename ("Legal_Filename"), "A037");
      Assert (not Is_Filename ("Contains/Slash"), "A038");

      Assert (Is_Portable_Filename ("ABCD"), "A039");
      Assert (not Is_Portable_Filename ("ABCD!abcd"), "A040");

      --  check with legal pathname
      Assert (Is_Pathname ("PATH/NAME"), "A041");
      --  check with pathname containing NUL
      Assert (not Is_Pathname
        (To_POSIX_String ("NU" & Character'Val (0) & "LL")),
        "A042");

      Assert (Is_Portable_Pathname ("ABCD/abcd"), "A043");
      Assert (not Is_Portable_Pathname ("~/NAME"), "A044");

   exception when E : others => Unexpected_Exception (E, "A045");
   end;

   ---------------------------------------------------------------------

   Test ("POSIX_String_List type [2.4.4]");
   declare
      X, Y : POSIX_String_List;
      N : Integer;
      procedure Never (Item : in POSIX_String; Quit : in out Boolean);
      procedure Never (Item : in POSIX_String; Quit : in out Boolean) is
      --  check Nonempty string list
      begin Fail ("A046"); Quit := True;
      end Never;
      procedure Check_Empty is new For_Every_Item (Never);
      function  Next_Number return POSIX_String;

      function  Next_Number return POSIX_String is
      begin
         N := N + 1;
         return To_POSIX_String (Integer'Image (N - 1));
      end Next_Number;
      procedure Check (Item : in POSIX_String; Quit : in out Boolean);
      procedure Check (Item : in POSIX_String; Quit : in out Boolean) is
      begin Assert (Item = To_POSIX_String (Integer'Image (N)), "A047");
         N := N + 1;
         if N = 5 then Quit := True; end if;
      end Check;
      procedure Check_Sequence is new For_Every_Item (Never);
   begin
      Assert (Length (Empty_String_List) = 0, "A048");
      Check_Empty (X);
      Assert (Length (X) = 0, "A049");
      N := 0;
      for I in 1 .. 100 loop
         Append (X, Next_Number);
      end loop;
      Assert (Value (X, 3) = To_POSIX_String (Integer'Image (2)), "A050");
      Assert (Length (X) = 100, "A051");
      Make_Empty (X);
      Check_Empty (X);
      Assert (Length (X) = 0, "A052");
      begin
         if Value (X, 2) = "" then null; end if;
         Assert (False, "A053");
      exception
      when Constraint_Error => null;
      when E : others => Unexpected_Exception (E, "A054");
      end;
   exception when E : others => Unexpected_Exception (E, "A055");
   end;

   ---------------------------------------------------------------------

   Test ("Option_Sets type [2.4.5]");
   declare
      X : Option_Set;
      A : constant array (1 .. 31) of Option_Set :=
        (Option_1,
         Option_2,
         Option_3,
         Option_4,
         Option_5,
         Option_6,
         Option_7,
         Option_8,
         Option_9,
         Option_10,
         Option_11,
         Option_12,
         Option_13,
         Option_14,
         Option_15,
         Option_16,
         Option_17,
         Option_18,
         Option_19,
         Option_20,
         Option_21,
         Option_22,
         Option_23,
         Option_24,
         Option_25,
         Option_26,
         Option_27,
         Option_28,
         Option_29,
         Option_30,
         Option_31);
   begin
      Assert (X = Empty_Set, "A056");
      Assert (Option_1 + Option_2 - Option_1 = Option_2, "A057");
      Assert (Option_1 + Option_2 > Option_1, "A058");
      Assert (not (Option_1 > Option_1), "A059");
      Assert (Option_1 < Option_1 + Option_2, "A060");
      Assert (not (Option_1 < Option_1), "A061");
      Assert (Option_1 <= Option_1 + Option_2, "A062");
      Assert (Option_1 <= Option_1, "A063");
      Assert (Option_1 + Option_2 >= Option_1, "A064");
      Assert (Option_1 >= Option_1, "A065");
      for I in A'Range loop
         for J in A'Range loop
            --  check that option set constants are distinct
            Assert (A (I) /= A (J) or I = J, "A066");
         end loop;
      end loop;
   exception when E : others => Unexpected_Exception (E, "A067");
   end;

   ---------------------------------------------------------------------

   Test ("Error_Code type [2.4.6]");
   declare
      Uninitialized : Error_Code;
   begin
      declare
         Dummy1 : constant String := Image (Uninitialized);
         Dummy2 : constant String := Image (No_Error);
      begin null;
      end;
      Set_Error_Code (ENAMETOOLONG);
      Assert (Get_Error_Code = ENAMETOOLONG, "A068");
      Assert (Is_POSIX_Error (ENAMETOOLONG), "A069");
      Assert (not Is_POSIX_Error (99999), "A070");
      Assert (Image (ENAMETOOLONG) =  "FILENAME_TOO_LONG", "A071");

      Assert (Is_POSIX_Error (E2BIG), "A072");
      Assert (Is_POSIX_Error (Argument_List_Too_Long), "A073");
      Assert (Is_POSIX_Error (EFAULT), "A074");
      Assert (Is_POSIX_Error (Bad_Address), "A075");
      Assert (Is_POSIX_Error (EBADF), "A076");
      Assert (Is_POSIX_Error (Bad_File_Descriptor), "A077");
      Assert (Is_POSIX_Error (EBADMSG), "A078");
      Assert (Is_POSIX_Error (Bad_Message), "A079");
      Assert (Is_POSIX_Error (EPIPE), "A080");
      Assert (Is_POSIX_Error (Broken_Pipe), "A081");
      Assert (Is_POSIX_Error (ENOTEMPTY), "A082");
      Assert (Is_POSIX_Error (Directory_Not_Empty), "A083");
      Assert (Is_POSIX_Error (ENOEXEC), "A084");
      Assert (Is_POSIX_Error (Exec_Format_Error), "A085");
      Assert (Is_POSIX_Error (EEXIST), "A086");
      Assert (Is_POSIX_Error (File_Exists), "A087");
      Assert (Is_POSIX_Error (EFBIG), "A088");
      Assert (Is_POSIX_Error (File_Too_Large), "A089");
      Assert (Is_POSIX_Error (ENAMETOOLONG), "A090");
      Assert (Is_POSIX_Error (Filename_Too_Long), "A091");
      Assert (Is_POSIX_Error (EXDEV), "A092");
      Assert (Is_POSIX_Error (Improper_Link), "A093");
      Assert (Is_POSIX_Error (ENOTTY), "A094");
      Assert (Is_POSIX_Error (Inappropriate_IO_Control_Operation), "A095");
      Assert (Is_POSIX_Error (EIO), "A096");
      Assert (Is_POSIX_Error (Input_Output_Error), "A097");
      Assert (Is_POSIX_Error (EINTR), "A098");
      Assert (Is_POSIX_Error (Interrupted_Operation), "A099");
      Assert (Is_POSIX_Error (EINVAL), "A100");
      Assert (Is_POSIX_Error (Invalid_Argument), "A101");
      Assert (Is_POSIX_Error (ESPIPE), "A102");
      Assert (Is_POSIX_Error (Invalid_Seek), "A103");
      Assert (Is_POSIX_Error (EISDIR), "A104");
      Assert (Is_POSIX_Error (Is_A_Directory), "A105");
      Assert (Is_POSIX_Error (EMSGSIZE), "A106");
      Assert (Is_POSIX_Error (Message_Too_Long), "A107");
      Assert (Is_POSIX_Error (ECHILD), "A108");
      Assert (Is_POSIX_Error (No_Child_Process), "A109");
      Assert (Is_POSIX_Error (ENOLCK), "A110");
      Assert (Is_POSIX_Error (No_Locks_Available), "A111");
      Assert (Is_POSIX_Error (ENOSPC), "A112");
      Assert (Is_POSIX_Error (No_Space_Left_On_Device), "A113");
      Assert (Is_POSIX_Error (ENODEV), "A114");
      Assert (Is_POSIX_Error (No_Such_Operation_On_Device), "A115");
      Assert (Is_POSIX_Error (ENXIO), "A116");
      Assert (Is_POSIX_Error (No_Such_Device_Or_Address), "A117");
      Assert (Is_POSIX_Error (ENOENT), "A118");
      Assert (Is_POSIX_Error (No_Such_File_Or_Directory), "A119");
      Assert (Is_POSIX_Error (ESRCH), "A120");
      Assert (Is_POSIX_Error (No_Such_Process), "A121");
      Assert (Is_POSIX_Error (ENOTDIR), "A122");
      Assert (Is_POSIX_Error (Not_A_Directory), "A123");
      Assert (Is_POSIX_Error (ENOMEM), "A124");
      Assert (Is_POSIX_Error (Not_Enough_Space), "A125");
      Assert (Is_POSIX_Error (ECANCELED), "A126");
      Assert (Is_POSIX_Error (Operation_Canceled), "A127");
      Assert (Is_POSIX_Error (EINPROGRESS), "A128");
      Assert (Is_POSIX_Error (Operation_In_Progress), "A129");
      Assert (Is_POSIX_Error (ENOSYS), "A130");
      Assert (Is_POSIX_Error (Operation_Not_Implemented), "A131");
      Assert (Is_POSIX_Error (EPERM), "A132");
      Assert (Is_POSIX_Error (Operation_Not_Permitted), "A133");
      Assert (Is_POSIX_Error (ENOTSUP), "A134");
      Assert (Is_POSIX_Error (Operation_Not_Supported), "A135");
      Assert (Is_POSIX_Error (EACCES), "A136");
      Assert (Is_POSIX_Error (Permission_Denied), "A137");
      Assert (Is_POSIX_Error (EROFS), "A138");
      Assert (Is_POSIX_Error (Read_Only_File_System), "A139");
      Assert (Is_POSIX_Error (EBUSY), "A140");
      Assert (Is_POSIX_Error (Resource_Busy), "A141");
      Assert (Is_POSIX_Error (EDEADLK), "A142");
      Assert (Is_POSIX_Error (Resource_Deadlock_Avoided), "A143");
      Assert (Is_POSIX_Error (EAGAIN), "A144");
      Assert (Is_POSIX_Error (Resource_Temporarily_Unavailable),
        "A145");
      Assert (Is_POSIX_Error (ETIMEDOUT), "A146");
      Assert (Is_POSIX_Error (Timed_Out), "A147");
      Assert (Is_POSIX_Error (EMLINK), "A148");
      Assert (Is_POSIX_Error (Too_Many_Links), "A149");
      Assert (Is_POSIX_Error (EMFILE), "A150");
      Assert (Is_POSIX_Error (Too_Many_Open_Files), "A151");
      Assert (Is_POSIX_Error (ENFILE), "A152");
      Assert (Is_POSIX_Error (Too_Many_Open_Files_In_System), "A153");
   exception when E : others => Unexpected_Exception (E, "A154");
   end;

   ---------------------------------------------------------------------

   Test ("Uname-derived functions [2.4.7]");
   begin
      Comment ("System_Name = " & To_String (System_Name));
      Comment ("Node_Name = " & To_String (Node_Name));
      Comment ("Release = " & To_String (Release));
      Comment ("Version = " & To_String (Version));
      Comment ("Machine = " & To_String (Machine));
   exception when E : others => Unexpected_Exception (E, "A155");
   end;

   ---------------------------------------------------------------------

   Test ("Timespec type [2.4.8]");
   declare
      NS : Nanoseconds;
      S  : Seconds;
      T  : Timespec;
   begin
      Set_Seconds (T, -1);
      Set_Nanoseconds (T, 1);
      Assert (Get_Nanoseconds (T) = 1, "A156");
      Set_Seconds (T, Seconds'Last);
      Set_Nanoseconds (T, Nanoseconds'Last);
      Assert (Get_Seconds (T) = Seconds'Last, "A157");
      Comment ("Seconds'Last =" & Seconds'Image (Seconds'Last));
      Comment ("Get_Seconds (T) =" & Seconds'Image (Get_Seconds (T)));
      Assert (Get_Nanoseconds (T) = Nanoseconds'Last, "A158");
      Comment ("Nanoseconds'Last =" & Nanoseconds'Image (Nanoseconds'Last));
      Comment ("Get_Nanoseconds (T) =" &
        Nanoseconds'Image (Get_Nanoseconds (T)));
      Split (T, S, NS);
      Assert (S = Get_Seconds (T), "A159");
      Assert (S = Seconds'Last, "A160");
      Assert (NS = Get_Nanoseconds (T), "A161");
      Assert (NS = Nanoseconds'Last, "A162");
      T := To_Timespec (88, 99);
      Assert (Get_Seconds (T) = 88, "A163");
      Assert (Get_Nanoseconds (T) = 99, "A164");
      T := T + To_Timespec (1, 1);
      Assert (Get_Seconds (T) = 89, "A165");
      Assert (Get_Nanoseconds (T) = 100, "A166");
      T := T + 1;
      Assert (Get_Seconds (T) = 89, "A167");
      Assert (Get_Nanoseconds (T) = 101, "A168");
      --  ....
      --  still need tests for wrap-arounds
      T := -T;
      Assert (Get_Seconds (T) = -90, "A169");
      Assert (Get_Nanoseconds (T) = Nanoseconds'Last + 1 - 101, "A170");
      T := To_Timespec (1, 2) + To_Timespec (3, 7);
      Assert (T = To_Timespec (4, 9), "A171");
      T := To_Timespec (1, 2) + To_Timespec (3, 7);
      Assert (T /= To_Timespec (4, 8), "A172");
      Assert (To_Timespec (1, 2) - To_Timespec (1, 2) =
        To_Timespec (0, 0), "A173");
      --  ....
      --  still need tests for other arithmetic
      Assert (To_Duration (To_Timespec (1, 1)) = 1.000_000_001, "A174");
      Assert (To_Duration (To_Timespec (1.0)) = 1.0, "A175");
      Assert (To_Duration (To_Timespec (2.0)) /= 1.0, "A176");
      Assert (To_Duration (To_Timespec (0, 1)) /= 0.0, "A177");
   exception when E : others => Unexpected_Exception (E, "A178");
   end;

   ---------------------------------------------------------------------

   Done;

exception when E : others =>  Fatal_Exception (E, "A179");
end p020400;

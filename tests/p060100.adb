------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                             P 0 6 0 1 0 0                                --
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

--  Test POSIX_IO Package.

--  Setup: When running this test make sure Standard_Error corresponds
--  to a terminal device.

with IO_Exceptions,
     POSIX,
     POSIX_IO,
     POSIX_Files,
     POSIX_File_Status,
     POSIX_Permissions,
     POSIX_Report;

procedure p060100 is

   use POSIX,
       POSIX_IO,
       POSIX_Files,
       POSIX_File_Status,
       POSIX_Permissions,
       POSIX_Report;

   Test_fd,
   Out_Fd,
   In_Fd,
   Fd      : File_Descriptor;
   NL      : constant POSIX_Character := POSIX_Character'Val (10);
   Read_Write_Perms : constant Permission_Set :=
     (Owner_Read | Owner_Write => True,
      Group_Read | Group_Write => True,
      Others_Read | Others_Write => True,
      others => False);

begin

   Header ("p060100");
   Test ("package POSIX_IO [6.1]");

   -------------------------------------------------------------------

   Test ("Standard File Descriptors [6.1.1]");
   Assert (POSIX_IO.Standard_Input = 0, "A001");
   Assert (POSIX_IO.Standard_Output = 1, "A002");
   Assert (POSIX_IO.Standard_Error = 2, "A003");

   -------------------------------------------------------------------

   Test ("Operations on Open_Option_Set [6.1.1]");
   declare
      O1, O2 : Open_Option_Set;
      Empty : constant Open_Option_Set := Open_Option_Set (POSIX.Empty_Set);
   begin
      Assert (O1 = Empty, "A004");
      Assert (O2 = Empty, "A005");
      Assert (O1 + O2 = Empty, "A006");
      Assert (O1 - O2 = Empty, "A007");
      O1 := POSIX_IO.Append + Non_Blocking;
      O2 := POSIX_IO.Append + Truncate;
      Assert (O1 + O2 = POSIX_IO.Append + Non_Blocking + Truncate, "A008");
      Assert (O1 - O2 = Non_Blocking, "A009");
   exception when E : others => Unexpected_Exception (E, "A010");
   end;

   -------------------------------------------------------------------

   Test ("file creation [6.1.1]");
   declare
      Last : IO_Count;
   begin
      if Is_File_Present ("test_file") then
         Unlink ("test_file");
      end if;
      Test_fd := Open_Or_Create
        ("test_file", Write_Only, Read_Write_Perms, Exclusive + Truncate);
      Assert (File_Size (Test_fd) = 0, "A011: file_size before write="
        & IO_Count'Image (File_Size (Test_fd)));
      Write (Test_fd, "hello" & NL, Last);
      Assert (File_Size (Test_fd) = 6, "A012: file_size after write="
        & IO_Count'Image (File_Size (Test_fd)));
      Assert (Last = 6, "A013: last=" & IO_Count'Image (Last));
      Close (Test_fd);
   exception
   when E : others => Fatal_Exception (E, "A014");
   end;

   -------------------------------------------------------------------

   Test ("file status [6.1.1]");
   declare
      St1, St2 : Status;
   begin
      Test_fd := Open ("test_file", Read_Write);
      St1 := Get_File_Status (Test_fd);
      St2 := Get_File_Status ("test_file");
      Assert (St1 = St2, "A015");
      Close (Test_fd);
   exception
   when E : others => Unexpected_Exception (E, "A016");
   end;

   -------------------------------------------------------------------

   Test ("status of closed file descriptor [6.1.1]");
   declare
      St1 : Status;
   begin
      St1 := Get_File_Status (Test_fd);
      Assert (False, "A017");
   exception
   when E1 : POSIX_Error => Check_Error_Code (Bad_File_Descriptor,
                            E1, "A018");
   when E2 : others => Unexpected_Exception (E2, "A019");
   end;

   -------------------------------------------------------------------

   Test ("re-close of closed file descriptor [6.1.1]");
   begin
      Assert (not Is_Open (Test_fd), "A020: is not open");
      Close (Test_fd);
      Assert (False, "A021");
   exception
   when E1 : POSIX_Error => Check_Error_Code (Bad_File_Descriptor,
                            E1, "A022");
   when E2 : others => Unexpected_Exception (E2, "A023");
   end;

   -------------------------------------------------------------------

   Test ("open nonexistent file [6.1.1]");
   begin
      if Is_File_Present ("Nonexistent_File") then
         Unlink ("test_file");
      end if;
      Fd := Open ("Nonexistent_File", Read_Only);
      Assert (False, "A024");
   exception
   when E1 : POSIX_Error => Check_Error_Code (No_Such_File_Or_Directory,
                            E1, "A025");
   when E2 : others => Unexpected_Exception (E2, "A026");
   end;

   -------------------------------------------------------------------

   Test ("open, truncate, size_of [6.1.1]");
   declare
      St1, St2 : Status;
   begin
      Out_Fd := Open_Or_Create ("Outfile", Write_Only, Read_Write_Perms);
      Truncate_File (Out_Fd, 0);
      St1 := Get_File_Status ("Outfile");
      Assert (Is_Regular_File (St1), "A027");
      Assert (Size_Of (St1) = 0, "A028");
      Assert (Permission_Set_Of (St1) = Permission_Set'
       (Owner_Read | Owner_Write | Group_Read | Others_Read => True,
        others => False), "A029");
      St2 := Get_File_Status (Out_Fd);
      Assert (St2 = St1, "A030: Status Structures Differ");
   exception
   when E : others =>  Unexpected_Exception (E, "A031");
   end;

   -------------------------------------------------------------------

   Test ("try to read write-only file [6.1.1]");
   declare
      Buf : POSIX_String (1 .. 4);
      Last : IO_Count;
   begin
      Read (Out_Fd, Buf, Last);
      Assert (False, "A032");
   exception
   when POSIX_Error => Check_Error_Code (Bad_File_Descriptor, "A033");
   when E : others => Unexpected_Exception (E, "A034");
   end;

   -------------------------------------------------------------------

   Test ("try to re-create existing file [6.1.1]");
   declare
      Fd : File_Descriptor;
   begin
      Fd := Open_Or_Create
        ("Outfile", Write_Only, Read_Write_Perms, Exclusive);
      Assert (False, "A035");
   exception
   when E1 : POSIX_Error => Check_Error_Code (File_Exists, E1, "A036");
   when E2 : others => Unexpected_Exception (E2, "A037");
   end;

   -------------------------------------------------------------------

   Test ("reading [6.1.3]");
   declare
      Buf_1 : POSIX_String (1 .. 4);
      Buf_2 : POSIX_String (101 .. 104);
      Last : IO_Count;
   begin
      Test_fd := Open ("test_file", Read_Only);
      Read (Test_fd, Buf_1, Last);
      Assert (Buf_1 (1 .. Integer (Last)) = "hell", "A038");
      Read (Test_fd, Buf_2, Last);
      Assert (Last = 102, "A039: wrong number of characters: "
        & IO_Count'Image (Last));
      Assert (Buf_2 (101 .. 102) = ('o' & NL), "A040: wrong data");
   exception
   when E : others => Unexpected_Exception (E, "A041");
   end;

   -------------------------------------------------------------------

   Test ("read past end of file [6.1.3]");
   declare
      Buf : POSIX_String (1 .. 4);
      Last : IO_Count;
   begin
      Read (Test_fd, Buf, Last);
      Assert (False, "A042");
   exception
   when IO_Exceptions.End_Error => null;
   when E : others => Unexpected_Exception (E, "A043");
   end;

   -------------------------------------------------------------------

   Test ("write ten characters [6.1.4]");
   declare
      Last : IO_Count;
      St1 : Status;
   begin
      Write (Out_Fd, "0123456789", Last);
      Assert (Last = 10, "A044");
      St1 := Get_File_Status (Out_Fd);
      Assert (Size_Of (St1) = 10, "A045");
   exception
   when E : others => Unexpected_Exception (E, "A046");
   end;

   -------------------------------------------------------------------

   Test ("write to read-only file [6.1.4]");
   declare
      Buf : POSIX_String := "Xxx";
      Last : IO_Count;
   begin
      Write (Test_fd, Buf, Last);
      Assert (False, "A047");
   exception
   when E1 : POSIX_Error => Check_Error_Code (Bad_File_Descriptor,
                            E1, "A048");
      Assert (File_Size (Test_fd) = 6, "A049: file_size="
        & IO_Count'Image (File_Size (Out_Fd)));
   when E2 : others => Unexpected_Exception (E2, "A050");
   end;

   -------------------------------------------------------------------

   Test ("File Sizes [6.1.5]");
   Assert (File_Size (Test_fd) = 6, "A051");
   Assert (File_Size (Out_Fd) = 10, "A052");

   -------------------------------------------------------------------

   Test ("File Position [6.1.5]");
   Assert (File_Position (Test_fd) = 6, "A053");

   -------------------------------------------------------------------

   Test ("seek and read [6.1.5]");
   declare
      Buf : POSIX_String (1 .. 1);
      Last : IO_Count;
      Offset : IO_Offset;
   begin
      Seek (Test_fd, 1, Offset, From_Beginning);
      Assert (Offset = 1, "A054");
      Read (Test_fd, Buf, Last);
      Assert (Last = 1, "A055");
      Assert (Buf (1) = 'e', "A056");
      Assert (File_Position (Test_fd) = 2, "A057");
   exception
   when E : others => Unexpected_Exception (E, "A058");
   end;

   -------------------------------------------------------------------

   Test ("seek from current position [6.1.5]");
   declare
      Offset : IO_Offset;
   begin
      Seek (Test_fd, 1, Offset, From_Current_Position);
      Assert (Offset = 3, "A059");
      Assert (File_Position (Test_fd) = 3, "A060");
   exception
   when E : others => Unexpected_Exception (E, "A061");
   end;

   -------------------------------------------------------------------

   Test ("seek from end [6.1.5]");
   declare
      Offset : IO_Offset;
   begin
      Seek (Test_fd, -1, Offset, From_End_Of_File);
      Assert (Offset = 5, "A062");
      Assert (File_Position (Test_fd) = 5, "A063");
   exception
   when E : others => Unexpected_Exception (E, "A064");
   end;

   -------------------------------------------------------------------

   Test ("close test_file [6.1.5]");
   begin
      Close (Test_fd);
   exception
   when E : others => Unexpected_Exception (E, "A065");
   end;

   -------------------------------------------------------------------

   Test ("seek on closed file [6.1.5]");
   declare
      Offset : IO_Offset;
   begin
      Seek (Test_fd, 1, Offset, From_Beginning);
      Assert (False, "A066");
   exception
   when E1 : POSIX_Error => Check_Error_Code (Bad_File_Descriptor,
                            E1, "A067");
   when E2 : others => Unexpected_Exception (E2, "A068");
   end;

   -------------------------------------------------------------------

   declare
      Old_Options,
      New_Options : Open_Option_Set;
      Io_Mode : POSIX_IO.File_Mode;
      Offset : IO_Offset;
      Buf : POSIX_String (3 .. 3) := "a";
      Last : IO_Count;
   begin
      Test ("Get File Control [6.1.7]");
      Get_File_Control (Out_Fd, Io_Mode, Old_Options);
      Assert (Io_Mode = Write_Only, "A069: Assert correct mode");

   -------------------------------------------------------------------

      Test ("set append [6.1.7]");
      Set_File_Control (Out_Fd, Old_Options + POSIX_IO.Append);

   -------------------------------------------------------------------

      Test ("get file control again 6.1.7]");
      Get_File_Control (Out_Fd, Io_Mode, New_Options);
      Assert (New_Options = Old_Options + POSIX_IO.Append, "A070: mode");

   -------------------------------------------------------------------

      Test ("seek to start [6.1.7]");
      Assert (File_Size (Out_Fd) = 10, "A071: file_size="
        & IO_Count'Image (File_Size (Out_Fd)));
      Seek (Out_Fd, 0, Offset, From_Beginning);
      Write (Out_Fd, Buf, Last);
      Assert (File_Size (Out_Fd) = 11, "A072: file_size="
        & IO_Count'Image (File_Size (Out_Fd)));
      Assert (Last = IO_Count (Buf'Last), "A073: last");
      Assert (File_Position (Out_Fd) = 11, "A074: position="
        & IO_Offset'Image (File_Position (Out_Fd)));
   exception
   when E : others => Unexpected_Exception (E, "A075");
   end;

   -------------------------------------------------------------------

   Test ("check file control [6.1.7]");
   declare
      Options : Open_Option_Set;
      Io_Mode : POSIX_IO.File_Mode;
   begin
      Get_File_Control (Out_Fd, Io_Mode, Options);
      Assert (Io_Mode = Write_Only, "A076: io_mode");
      Assert (Options >= POSIX_IO.Append, "A077: options");
   exception
   when E : others => Unexpected_Exception (E, "A078");
   end;

   -------------------------------------------------------------------

   Test ("close out file descriptor [6.1.7]");
   begin
      Assert (Is_Open (Out_Fd), "A079");
      Close (Out_Fd);
      --  Cause Subsequent Operations To fail
   exception
   when E : others => Unexpected_Exception (E, "A080");
   end;

   -------------------------------------------------------------------

   Test ("file control on closed file descriptor 6.1.7]");
   declare
      Options : Open_Option_Set;
      Io_Mode : POSIX_IO.File_Mode;
   begin
      Get_File_Control (Out_Fd, Io_Mode, Options);
      Assert (False, "A081");
   exception
   when E1 : POSIX_Error => Check_Error_Code (Bad_File_Descriptor,
                            E1, "A082");
   when E2 : others => Unexpected_Exception (E2, "A083");
   end;

   -------------------------------------------------------------------

   Test ("set file control on closed file descriptor [6.1.7]");
   begin
      Set_File_Control (Out_Fd, Open_Option_Set (POSIX.Empty_Set));
      Assert (False, "A084");
   exception
   when E1 : POSIX_Error => Check_Error_Code (Bad_File_Descriptor,
                            E1, "A085");
   when E2 : others => Unexpected_Exception (E2, "A086");
   end;

   -------------------------------------------------------------------

   Test ("get close on exec on closed file descriptor [6.1.7]");
   declare
      Flag : Boolean;
   begin
      Flag := Get_Close_On_Exec (Out_Fd);
      Assert (False, "A087");
   exception
   when E1 : POSIX_Error => Check_Error_Code (Bad_File_Descriptor,
                            E1, "A088");
   when E2 : others => Unexpected_Exception (E2, "A089");
   end;

   -------------------------------------------------------------------

   Test ("set close on exec on closed file descriptor [6.1.7]");
   begin
      Set_Close_On_Exec (Out_Fd, False);
      Assert (False, "A090");
   exception
   when E1 : POSIX_Error => Check_Error_Code (Bad_File_Descriptor,
                            E1, "A091");
   when E2 : others => Unexpected_Exception (E2, "A092");
   end;

   -------------------------------------------------------------------

   Test ("check files not open [6.1.1]");
   begin
      Assert (Is_Open (Out_Fd) = False, "A093: out_fd");
      Assert (Is_Open (Test_fd) = False, "A094: test_fd");
   exception
   when E : others => Unexpected_Exception (E, "A095");
   end;

   -------------------------------------------------------------------

   Test ("open and check file control [6.1.7]");
   declare
      Options : Open_Option_Set;
      Io_Mode : POSIX_IO.File_Mode;
   begin
      Out_Fd := Open ("Outfile", Read_Write);
      Get_File_Control (Out_Fd, Io_Mode, Options);
      Assert (Io_Mode = Read_Write, "A096");
   exception
   when E : others => Unexpected_Exception (E, "A097");
   end;

   -------------------------------------------------------------------

   Test ("duplicate [6.1.6]");
   declare
      St1, St2 : aliased Status;
   begin

      Test_fd := Open ("test_file", Read_Only);
      Comment ("get status via test_fd");
      St2 := Get_File_Status (Test_fd);
      Fd := Duplicate (Test_fd);
      Assert (Fd /= Test_fd, "A098");
      Comment ("get status via fd " & File_Descriptor'Image (Fd));
      St1 := Get_File_Status (Fd);
      Comment ("get status via test_fd");
      St2 := Get_File_Status (Test_fd);
      Assert (File_ID_Of (St1) = File_ID_Of (St2), "A099: file id");
      Assert (Device_ID_Of (St1) = Device_ID_Of (St2), "A100: device id");
   exception
   when E : others => Unexpected_Exception (E, "A101");
   end;

   -------------------------------------------------------------------

   Test ("duplicate and close [6.1.1]");
   declare
      St1, St2 : Status;
   begin
      if Duplicate_and_Close (Out_Fd, Fd) /= Fd then
         Fail ("A102: Wrong Duplicate");
      end if;
      Comment ("get status via fd");
      St1 := Get_File_Status (Fd);
      Comment ("get status via out_fd");
      St2 := Get_File_Status (Out_Fd);
      Assert (File_ID_Of (St1) = File_ID_Of (St2), "A103: file id");
      Assert (Device_ID_Of (St1) = Device_ID_Of (St2), "A104: device id");
      Comment ("close fd");
      Close (Fd);
      Comment ("close out_fd");
      Close (Out_Fd);
   exception
   when E : others => Unexpected_Exception (E, "A105");
   end;

   -------------------------------------------------------------------

   Test ("Create Pipe [6.1.1]");
   begin
      Create_Pipe (In_Fd, Out_Fd);
   exception
   when E : others => Unexpected_Exception (E, "A106");
   end;

   -------------------------------------------------------------------

   Test ("write then read pipe[6.1.3][6.1.4]");
   declare
      Outbuf : POSIX_String := "Ab";
      Inbuf : POSIX_String (1 .. 2);
      Last : IO_Count;
   begin
      Write (Out_Fd, Outbuf, Last);
      Assert (Last = 2, "A107");
      Read (In_Fd, Inbuf, Last);
      Assert (Last = 2, "A108");
      Assert (Inbuf = Outbuf, "A109");
   exception
   when E : others => Unexpected_Exception (E, "A110");
   end;

   -------------------------------------------------------------------

   Test ("nonblocking read [6.1.3]");
   declare
      Buf : POSIX_String (1 .. 2);
      Last : IO_Count;
   begin
      Set_File_Control (In_Fd, Non_Blocking);
      Read (In_Fd, Buf, Last);
      Assert (False, "A111");
   exception
   when E1 : POSIX_Error =>
      Check_Error_Code (Resource_Temporarily_Unavailable, E1, "A112");
   when E2 : others => Unexpected_Exception (E2, "A113");
   end;

   -------------------------------------------------------------------

   Test ("invalid seek [6.1.5]");
   declare
      Offset : IO_Offset;
   begin
      Seek (In_Fd, 1, Offset, From_Beginning);
      Assert (False, "A114");
   exception
   when E1 : POSIX_Error => Check_Error_Code (Invalid_Seek, E1, "A115");
   when E2 : others => Unexpected_Exception (E2, "A116");
   end;

   Close (In_Fd);
   Close (Out_Fd);

   -------------------------------------------------------------------

   Test ("test_file is not a terminal [6.1.6]");
   Assert (not Is_A_Terminal (Test_fd), "A117");

   --  Tests o82 require that the environment variable
   --  "TTY_NAME" be set to the "stderr" device (for example /dev/pts/5).

   -------------------------------------------------------------------

   Test ("is_a_terminal [6.1.6]");
   begin
      if Is_A_Terminal (POSIX_IO.Standard_Error) then
         declare
            Tty_Name : POSIX_String := Get_Terminal_Name
              (POSIX_IO.Standard_Error);
         begin
            Comment ("tty_name = " & To_String (Tty_Name));
         end;
      else
         Fail ("A118: Standard_Error should be a terminal device");
      end if;
      Close (Test_fd);
   end;

   --  remove the file created for this Test.
   Unlink ("test_file");

   -------------------------------------------------------------------

   Done;

exception when E : others => Fatal_Exception (E, "A119");
end p060100;

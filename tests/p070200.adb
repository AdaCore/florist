------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                             P 0 7 0 2 0 0                                --
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

--  .... A legacy test, not in real-time area; could be improved.

--  Setup:  This test requires that Standard_Error correspond to a
--  terminal device.

with POSIX,
     POSIX_Files,
     POSIX_IO,
     POSIX_Process_Identification,
     POSIX_Report,
     POSIX_Terminal_Functions,
     Text_IO;

procedure p070200 is

   use POSIX,
       POSIX_Files,
       POSIX_IO,
       POSIX_Process_Identification,
       POSIX_Report,
       POSIX_Terminal_Functions;

begin

   Header ("p070200");
   Test ("package POSIX_Terminal_Functions [7.2]");

   declare
      original_Tc : Terminal_Characteristics;
      Tc : Terminal_Characteristics;
      New_Tc : Terminal_Characteristics;
      Modes : Terminal_Modes_Set;
      New_Modes : Terminal_Modes_Set;
      test_Fd : File_Descriptor;
      y : POSIX.POSIX_Character;
      z : Baud_Rate;
      w : Bits_Per_Character;

   begin

      declare
         --  Tests require us to have a file called "test_file."
         --  We generate the file here.
         Test_File : Text_IO.File_Type;
      begin
         Text_IO.Create (Test_File, Text_IO.Out_File, "The_Test_File");
         Text_IO.Put (Test_File, "hello");
         Text_IO.Close (Test_File);
      end;

      Comment ("Get_Terminal_Characteristics");
      original_Tc := Get_Terminal_Characteristics (POSIX_IO.Standard_Error);
      Comment ("Terminal_Modes_Of");
      Tc := original_Tc;
      Modes := Terminal_Modes_Of (Tc);
      Assert (Modes = Terminal_Modes_Of (original_Tc), "A001");
      Comment ("Enable_Signals");
      Assert (Modes (Enable_Signals) = True, "A002");
      Comment ("more Modes");
      Modes (Ignore_Break) := True;
      Modes (Mark_Parity_Errors) := True;
      Modes (Perform_Output_Processing) := True;
      Modes (Echo_Kill) := True;
      Modes (No_Flush) := True;
      Define_Terminal_Modes (Tc, Modes);
      Comment ("Set_Terminal_Characteristics");
      Set_Terminal_Characteristics
        (POSIX_IO.Standard_Error, Tc, After_Output);
      Comment ("Get_Terminal_Characteristics");
      New_Tc := Get_Terminal_Characteristics (POSIX_IO.Standard_Error);
      New_Modes := Terminal_Modes_Of (New_Tc);
      Assert (New_Modes = Modes, "A003");
      Comment ("set terminal characteristics");
      Modes (Ignore_Break) := False;
      Modes (Mark_Parity_Errors) := False;
      --  Modes (Perform_Output_Processing) := False;
      --  Setting this to false can mess up the screen output;
      --  taking it out should not matter much.
      Modes (Echo_Kill) := False;
      Modes (No_Flush) := False;
      Define_Terminal_Modes (New_Tc, Modes);
      Set_Terminal_Characteristics (POSIX_IO.Standard_Error, New_Tc);
      Comment ("check terminal characteristics");
      Tc := New_Tc;
      New_Tc := Get_Terminal_Characteristics (POSIX_IO.Standard_Error);
      New_Modes := Terminal_Modes_Of (New_Tc);
      Assert (New_Modes = Modes, "A004");
      Comment ("try setting terminal characteristics");
      Define_Minimum_Input_Count (Tc, 17);
      --  Check for correct input count
      Assert (Minimum_Input_Count_Of (Tc) = 17, "A005");
      Define_Input_Time (Tc, 0.5);
      --  Check for correct input time
      Assert (Input_Time_Of (Tc) = 0.5, "A006");
      Set_Terminal_Characteristics (POSIX_IO.Standard_Error, Tc,
                   After_Output_And_Input);
      Comment ("check results");
      New_Tc := Get_Terminal_Characteristics (POSIX_IO.Standard_Error);
      New_Modes := Terminal_Modes_Of (New_Tc);
      --  Check for correct modes
      Assert (New_Modes = Modes, "A007");
      --  Check for correct count
      Assert (Minimum_Input_Count_Of (New_Tc) =
         Minimum_Input_Count_Of (Tc), "A008");
      --  Check for correct time
      Assert (Input_Time_Of (New_Tc) = Input_Time_Of (Tc), "A009");
      Comment ("set to invalid value");
      declare
         Uninitialized : Terminal_Characteristics;
      begin
         Set_Terminal_Characteristics
           (POSIX_IO.Standard_Error, Uninitialized);
         Expect_Exception ("A010");
      exception
      when POSIX_Error => Check_Error_Code (Invalid_Argument, "A011");
      end;
      Comment ("set invalid Input_Count");
      begin
         Define_Minimum_Input_Count (Tc, 30000);
         Expect_Exception ("A012");
      exception
      when POSIX_Error => Check_Error_Code (Invalid_Argument, "A013");
      end;
      Assert (Minimum_Input_Count_Of (Tc) = 17, "A014");
      Comment ("set invalid Input_Time");
      begin
         Define_Input_Time (Tc, 100.0);
         Expect_Exception ("A015");
      exception
      when POSIX_Error =>  Check_Error_Code (Invalid_Argument, "A016");
      end;
      Assert (Input_Time_Of (Tc) = 0.5, "A017");

      Comment ("try to set characteristics of regular file");
      test_Fd := Open ("The_Test_File", Read_Write);
      begin
         Set_Terminal_Characteristics (test_Fd, Tc);
         Expect_Exception ("A018");
      exception
      when POSIX_Error =>
         Check_Error_Code (Inappropriate_IO_Control_Operation, "A019");
      end;

      Comment ("try to getd characteristics of regular file");
      begin
         Tc := Get_Terminal_Characteristics (test_Fd);
         Expect_Exception ("A020");
      exception
      when POSIX_Error =>
         Check_Error_Code (Inappropriate_IO_Control_Operation, "A021");
      end;
      Close (test_Fd);

      Comment ("special control character");
      begin
         y := Special_Control_Character_Of (Tc, Interrupt_Char);
         Disable_Control_Character (Tc, Interrupt_Char);
         Assert (y
           /= Special_Control_Character_Of (Tc, Interrupt_Char), "A022");
         Define_Special_Control_Character (Tc, Interrupt_Char, y);
         Assert (y
           = Special_Control_Character_Of (Tc, Interrupt_Char), "A023");
      end;

      Comment ("input baud rate");
      begin
         Tc := Get_Terminal_Characteristics (POSIX_IO.Standard_Error);
         z := Input_Baud_Rate_Of (Tc);
         Comment ("z = " & Baud_Rate'Image (z));
         Define_Input_Baud_Rate (Tc, B9600);
         Assert (Input_Baud_Rate_Of (Tc) = B9600, "A024");
         Define_Input_Baud_Rate (Tc, z);
         Comment ("z = " & Baud_Rate'Image (z));
         Comment ("Input_Baud_Rate_Of (Tc) = "
           & Baud_Rate'Image (Input_Baud_Rate_Of (Tc)));
         --  ....
         --  The Assert below is disabled because it apparently exceeds the
         --  POSIX.1 specifications.  We asked for an interpretation on this.
         --  Apparently, the OS is allowed to treat zero as a special case.
         --  Assert (Input_Baud_Rate_Of (Tc) = z, "Baud Rate Wrong 2");
      end;
      Comment ("output baud rate");
      begin
         Tc := Get_Terminal_Characteristics (POSIX_IO.Standard_Error);
         z := Output_Baud_Rate_Of (Tc);
         Define_Output_Baud_Rate (Tc, B9600);
         Assert (Output_Baud_Rate_Of (Tc) = B9600, "A025");
         Define_Output_Baud_Rate (Tc, z);
         Assert (Output_Baud_Rate_Of (Tc) = z, "A026");
      end;
      Comment ("bits per character");
      begin
         Tc := Get_Terminal_Characteristics (POSIX_IO.Standard_Error);
         w := Bits_Per_Character_Of (Tc);
         Define_Bits_Per_Character (Tc, 8);
         Assert
           (Bits_Per_Character_Of (Tc) = 8, "A027");
         Define_Bits_Per_Character (Tc, w);
         Assert
           (Bits_Per_Character_Of (Tc) = w, "A028");
      end;
      Comment ("controlling terminal name");
      begin
         Comment ("Controlling Terminal Name is " &
           POSIX.To_String (Get_Controlling_Terminal_Name));
      end;
      Comment ("process group ID");
      declare
         GID : POSIX_Process_Identification.Process_Group_ID;
      begin
         GID := Get_Process_Group_ID (POSIX_IO.Standard_Error);
         Comment ("Process group ID is " &
           POSIX_Process_Identification.Image (GID));
      end;
      Comment ("Drain");
      begin
         Drain (POSIX_IO.Standard_Error);
      end;
      Comment ("restore characteristics");
      Set_Terminal_Characteristics (POSIX_IO.Standard_Error, original_Tc);
   exception
   when E : others => Unexpected_Exception (E, "A029");
   end;

   --  remove the file created for this test.
   Unlink ("The_Test_File");

   Done;

exception when E : others =>
   --  remove the file created for this test.
   Unlink ("The_Test_File");
   Fatal_Exception (E, "A030");
end p070200;

------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                             P 0 4 0 3 0 1                                --
--                                                                          --
--                                B o d y                                   --
--                                                                          --
--                                                                          --
--  Copyright (c) 1995-1999 Florida  State  University  (FSU).  All Rights  --
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


--  Test package POSIX_Process_Environment and operations on
--  current working directory.

--  Setup: Before running this test set PWD to the current working
--  directory, (PWD=`pwd`) and export PWD.

with POSIX,
     POSIX_Files,
     POSIX_Permissions,
     POSIX_Process_Environment,
     POSIX_Report,
     POSIX_Configurable_File_Limits,
     Ada.Command_Line;

procedure p040301 is

   use POSIX,
       POSIX_Files,
       POSIX_Permissions,
       POSIX_Process_Environment,
       POSIX_Report;

begin

   Header ("p040301");
   Test ("package POSIX_Process_Environment [4.3]");

   Test ("Argument_List [4.3.1]");
   begin
      for I in 1 .. Length (Argument_List) loop
         Comment ("Argument " & Natural'Image (I) & " = " & To_String (
             Value (Argument_List, I)));
         if I > 1 then
            Assert (To_String (Value (Argument_List, I)) = Ada.Command_Line.
               Argument (I - 1), "A001." & Natural'Image (I));
            Comment ("Ada.Command_Line.Argument" & "(" & Natural'Image (
              I - 1) & ") " & " returns " & Ada.Command_Line.Argument (I - 1));
         end if;
      end loop;
      Comment ("Verify Ada.Command_Line.Argument_Count = POSIX Length" &
               "(" & "Argument_List" & ")" & "-1");
      Comment ("Ada.Command_Line.Argument_Count=" & Natural'Image (
               Ada.Command_Line.Argument_Count));
      Comment ("POSIX Length(Argument_List)=" & Natural'Image (Length (
                Argument_List)));
      Assert (Ada.Command_Line.Argument_Count = Length (Argument_List) - 1,
                   "A002");
      Comment ("Ada.Comman_Line.Command_Name = " & Ada.Command_Line.
               Command_Name);
   end;

   -----------------------------------------------------------------------

   Test ("Environment Variables [4.3.2]");
   declare
      EnvA, EnvB : Environment;
      Test_Str : POSIX_String := "Test_Str_Value";
      --  Instantiation of generic procedure Action in the standard
      procedure Check_Variable_Value (Variable  : POSIX_String;
                                      Value     : POSIX_String;
                                      Quit      : in out Boolean);
      procedure Check_Variable_Value (Variable  : POSIX_String;
                                      Value     : POSIX_String;
                                      Quit      : in out Boolean) is
      begin
         if not (To_String (Environment_Value_Of (Variable)) =
           To_String (Value))
         then
            Fail ("for """ & To_String (Variable) & """ found """ &
             To_String (Environment_Value_Of (Variable)) & """ /= """ &
             To_String (Value) & """");
         end if;
      end Check_Variable_Value;
      procedure Check_All_Table is new For_Every_Environment_Variable (
                       Check_Variable_Value);
      procedure Check_All_Current_Table is new
         For_Every_Current_Environment_Variable (Check_Variable_Value);
      procedure Check_Action_Quit (Variable : POSIX_String;
                                   Value    : POSIX_String;
                                   Quit     : in out Boolean);
      procedure Check_Action_Quit (Variable : POSIX_String;
                                   Value    : POSIX_String;
                                   Quit     : in out Boolean) is
      begin
         if Quit then
            Fail ("Either 'Quit' is initialized as False or it does not work");
         elsif Variable = "PATH" then
            Quit := True;
         elsif Variable = "Quit_Trigger" then
            Quit := True;
         end if;
      end Check_Action_Quit;
      procedure Check_Table_Action_Quit is new For_Every_Environment_Variable
                  (Check_Action_Quit);
      procedure Check_Current_Table_Action_Quit is new
          For_Every_Current_Environment_Variable (Check_Action_Quit);
   begin
      Comment ("Testing Check_All_Current_Table");
      Check_All_Current_Table;
      Comment ("Testing Copy_From_Current_Environment(EnvA: Environment)");
      Copy_From_Current_Environment (EnvA);
      Comment ("Testing Check_All_Table_A also verify copy is done all right");
      Check_All_Table (EnvA);
      Comment ("Clear_Environment since we have a backup EnvA now");
      Clear_Environment;
      Comment ("Copy_To_Current_Environment(EnvA)");
      Copy_To_Current_Environment (EnvA);
      Comment ("After copy EnvA to current then Check_All_Table (EnvA)");
      Check_All_Table (EnvA);
      Comment ("Testing Copy_Environment(EnvA, EnvB)");
      Copy_Environment (EnvA, EnvB);
      Comment ("Testing EnvB has the right copy using Check_All_Table(EnvB)");
      Check_All_Table (EnvB);
      Comment ("Testing Environment_Value_Of(an undefined env variable)");
      Assert (Environment_Value_Of ("Undefined_Env_Var", EnvA,
              "Undefined_Value") = "Undefined_Value", "A003");
      Assert (Environment_Value_Of ("Undefined_Env_Var", EnvA) = "", "A004");
      Assert (Environment_Value_Of ("Undefined_Env_Var", "Undefined_Value")
              = "Undefined_Value", "A005");
      Assert (Environment_Value_Of ("Undefined_Env_Var") = "", "A006");
      Comment ("Testing Is_Environment_Variable(an undefined env variable)");
      Assert (Is_Environment_Variable ("Undefined_Env_Var", EnvA) = False,
                 "A007");
      Assert (Is_Environment_Variable ("Undefined_Env_Var") = False, "A008");
      Set_Environment_Variable ("Undefined_Env_Var", "Defined_Now", EnvA);
      Set_Environment_Variable ("Undefined_Env_Var", "Defined_Now");
      Assert (Is_Environment_Variable ("Undefined_Env_Var", EnvA) = True,
              "A009");
      Assert (Is_Environment_Variable ("Undefined_Env_Var") = True, "A010");
      Comment ("Testing case significance of environment variables");
      --  Test the case of characters in the environment variable name
      --   are significant
      Assert (Is_Environment_Variable ("undefined_env_var", EnvA) = False,
                  "A011");
      Assert (Is_Environment_Variable ("undefined_env_var") = False, "A012");
      Assert (Environment_Value_Of ("undefined_env_var", EnvA) =
                  "", "A013");
      Assert (Environment_Value_Of ("undefined_env_var", " ") = " ", "A014");
      Clear_Environment (EnvA);
      Comment ("Testing validity of a null environment");
      Assert (Is_Environment_Variable ("Undefined_Env_Var", EnvA) = False,
                  "A015");
      Comment ("Clear_Environment");
      Clear_Environment;
      Assert (Is_Environment_Variable ("Undefined_Env_Var") = False, "A016");
      Comment ("Copy_To_Current_Environment (EnvB)");
      Copy_To_Current_Environment (EnvB);
      Assert (Environment_Value_Of ("New_Variable_For_Test", EnvA) = "",
              "A017");
      Assert (Environment_Value_Of ("New_Variable_For_Test", EnvA,
                "Undefined_Value") = "Undefined_Value", "A018");
      Set_Environment_Variable ("New_Variable_For_Test", "New_Variable=Value",
                EnvA);
      Assert (Environment_Value_Of ("New_Variable_For_Test", EnvA) =
               "New_Variable=Value", "A019");
      Set_Environment_Variable ("New_Variable_For_Test",
           "New_Variable=Value");
      Assert (Environment_Value_Of ("New_Variable_For_Test") =
               "New_Variable=Value", "A020");
      Delete_Environment_Variable ("New_Variable_For_Test", EnvA);
      Assert (Is_Environment_Variable ("New_Variable_For_Test", EnvA) = False,
               "A021");
      Delete_Environment_Variable ("New_Variable_For_Test");
      Assert (Is_Environment_Variable ("New_Variable_For_Test") = False,
               "A022");
      Copy_Environment (EnvB, EnvA);
      Assert (Length (EnvB) = Length (EnvA), "A023");
      Assert (Length (EnvA) = Length, "A024");
      Set_Environment_Variable ("Quit_Trigger", "Trigger_Value");
      Set_Environment_Variable ("Quit_Trailer", "Trailer_Value");
      Set_Environment_Variable ("quit_trailer", "Trailer_value");
      Assert (Length (EnvA) = Length - 3, "A025");
      Copy_From_Current_Environment (EnvA);
      Check_Table_Action_Quit (EnvA);
      Check_Current_Table_Action_Quit;
      Copy_To_Current_Environment (EnvB);
      Clear_Environment (EnvA);
      Clear_Environment (EnvB);
      --  Following lines help verify whether empty env is valid
      Copy_Environment (EnvA, EnvB);
      Assert (Length (EnvB) = 0, "A026");

      ----------------------------------------------------------------


      Test ("Error Handling for Environment Variables [4.3.2]");
      Comment ("Testing Error Handling For Environment_Value_Of");
      begin
         Test_Str := Environment_Value_Of ("", EnvA);
         Assert (False, "A027");
      exception when POSIX_Error => Check_Error_Code (Invalid_Argument,
                  "A028");
      end;
      begin
         Test_Str := Environment_Value_Of ("");
         Assert (False, "A029");
      exception when POSIX_Error => Check_Error_Code (Invalid_Argument,
                       "A030");
      end;
      begin
         Test_Str := Environment_Value_Of ("Contain=Symbol", EnvA);
         Assert (False, "A031");
      exception when POSIX_Error => Check_Error_Code (Invalid_Argument,
                 "A032");
      end;
      begin
         Test_Str := Environment_Value_Of ("Contain=Symbol");
         Assert (False, "A033");
      exception when POSIX_Error => Check_Error_Code (Invalid_Argument,
                "A034");
      end;
      Comment ("Testing Error Handling for Is_Environment_Variable");
      begin
         if Is_Environment_Variable ("", EnvA) = False then
            Assert (False, "A035");
         end if;
         Assert (False, "A036");
      exception when POSIX_Error => Check_Error_Code (Invalid_Argument,
                 "A037");
      end;
      begin
         if Is_Environment_Variable ("") = False then
            Assert (False, "A038");
         end if;
         Assert (False, "A039");
      exception when POSIX_Error => Check_Error_Code (Invalid_Argument,
                "A040");
      end;
      begin
         if Is_Environment_Variable ("Contain=Symbol", EnvA) = False then
            Assert (False, "A041");
         end if;
         Assert (False, "A042");
      exception when POSIX_Error => Check_Error_Code (Invalid_Argument,
                 "A043");
      end;
      begin
         if Is_Environment_Variable ("Contain=Symbol") = False then
            Assert (False, "A044");
         end if;
         Assert (False, "A045");
      exception when POSIX_Error => Check_Error_Code (Invalid_Argument,
                       "A046");

      end;
      begin
         Set_Environment_Variable ("", "Invalid", EnvA);
         Assert (False, "A047");
      exception when POSIX_Error => Check_Error_Code (Invalid_Argument,
                       "A048");
      end;
      Comment ("Testing Error Handling for Set_Environment_Variable");
      begin
         Set_Environment_Variable ("", "Invalid");
         Assert (False, "A049");
      exception when POSIX_Error => Check_Error_Code (Invalid_Argument,
                       "A050");
      end;
      begin
         Set_Environment_Variable ("Contain=Symbol", "Invalid", EnvA);
         Assert (False, "A051");
      exception when POSIX_Error => Check_Error_Code (Invalid_Argument,
                       "A052");
      end;
      begin
         Set_Environment_Variable ("Contain=Symbol", "Invalid");
         Assert (False, "A053");
      exception when POSIX_Error => Check_Error_Code (Invalid_Argument,
                       "A054");
      end;
      declare
         Nul : POSIX_Character := POSIX_Character'Val (0);
      begin
         Set_Environment_Variable ("Contain_Null_Symbol" & Nul, "Invalid",
                       EnvA);
         Assert (False, "A055");
      exception when POSIX_Error => Check_Error_Code (Invalid_Argument,
                       "A056");
      end;
      declare
         Nul : POSIX_Character := POSIX_Character'Val (0);
      begin
         Set_Environment_Variable ("Contain_Null_Symbol" & Nul, "Invalid");
         Assert (False, "A057");
      exception when POSIX_Error => Check_Error_Code (Invalid_Argument,
                       "A058");
      end;
      Comment ("Testing Error Handling for Delete_Environment_Variable");
      begin
         Delete_Environment_Variable ("", EnvA);
         Assert (False, "A059");
      exception when POSIX_Error => Check_Error_Code (Invalid_Argument,
                       "A060");
      end;
      begin
         Delete_Environment_Variable ("");
         Assert (False, "A061");
      exception when POSIX_Error => Check_Error_Code (Invalid_Argument,
                       "A062");
      end;
      begin
         Delete_Environment_Variable ("Contain=Symbol", EnvA);
         Assert (False, "A063");
      exception when POSIX_Error => Check_Error_Code (Invalid_Argument,
                       "A064");
      end;
      begin
         Delete_Environment_Variable ("Contain=Symbol");
         Assert (False, "A065");
      exception when POSIX_Error => Check_Error_Code (Invalid_Argument,
                       "A066");
      end;

   exception when E : others => Unexpected_Exception (E, "A067");
   end;

   --------------------------------------------------------------------

   Test ("Process Working Directory [4.3.3]");
   declare
      DirA : POSIX_String := Get_Working_Directory;
   begin
      Comment ("Testing Get_Working_Directory");
      Assert (DirA'First = 1, "A068");
      Comment ("Get_Working_Directory returns " & To_String (DirA));
      Create_Directory ("Unlikely_Exist_Dir", (others => True));
      Comment ("Testing Change_Working_Directory");
      Change_Working_Directory ("Unlikely_Exist_Dir");
      Assert (Get_Working_Directory = DirA & "/Unlikely_Exist_Dir", "A069");
      Change_Working_Directory ("..");
      Assert (Get_Working_Directory = DirA, "A070");
      Remove_Directory ("Unlikely_Exist_Dir");


      --------------------------------------------------------------------

      Test ("Error Handling for Process Working Directory [4.3.3]");
      Comment ("Testing Handling of Permission_Denied");
      begin
         Create_Directory ("Unlikely_Exist_Dir", (Owner_Write => True,
                     others => False));
         Change_Working_Directory ("Unlikely_Exist_Dir");
         Assert (False, "A071");
      exception when POSIX_Error => Check_Error_Code (Permission_Denied,
                       "A072");
      end;
      Remove_Directory ("Unlikely_Exist_Dir");

      Comment ("Testing Handling of Filename_Too_Long");
      declare
         Long_Name_Limit : Integer := Integer
            (POSIX_Configurable_File_Limits.Pathname_Maximum (
              Get_Working_Directory));

         type String_Access is access String;
         Over_Long_Name : String_Access;
      begin
         Over_Long_Name := new String'("L");
         Comment ("Pathname_Maximum" & "(" & To_String (Get_Working_Directory)
              & ")" & " is " & Integer'Image (Long_Name_Limit));
         for I in 1 .. Long_Name_Limit loop
            Over_Long_Name := new String'(Over_Long_Name.all & "L");
         end loop;
         Change_Working_Directory (To_POSIX_String (Over_Long_Name.all));
         Assert (False, "A073");
      exception
      when POSIX_Error => Check_Error_Code (Filename_Too_Long, "A074");
      end;

      Comment ("Testing Handling of Not_A_Directory <try a FIFO:->");
      begin
         Create_FIFO ("FIFO_Not_A_Directory", (others => True));
         Change_Working_Directory ("FIFO_Not_A_Directory");
         Assert (False, "A075");
      exception when POSIX_Error => Check_Error_Code (Not_A_Directory,
                       "A076");
      end;
      Unlink ("FIFO_Not_A_Directory");

      Comment ("Testing Handling of No_Such_File_Or_Directory");
      begin
         Change_Working_Directory ("Unlikely_Exist_Dir");
         Assert (False, "A077");
      exception when POSIX_Error =>
         Check_Error_Code (No_Such_File_Or_Directory, "A078");
      end;
   end;

   Done;

exception when E : others => Fatal_Exception (E, "A079");
end p040301;

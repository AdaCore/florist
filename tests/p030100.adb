------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                             P 0 3 0 1 0 0                                --
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

with POSIX,
     POSIX_Configurable_System_Limits,
     POSIX_Files,
     POSIX_Process_Environment,
     POSIX_Process_Identification,
     POSIX_Process_Primitives,
     POSIX_Report,
     POSIX_Signals,
     Text_IO;

procedure p030100 is

   use POSIX,
       POSIX_Files,
       POSIX_Process_Environment,
       POSIX_Process_Identification,
       POSIX_Process_Primitives,
       POSIX_Report,
       POSIX_Signals;

   --  Cases for child process:
   Should_Not_Start     : constant := 1;
   Parents_Environment  : constant := 2;
   Explicit_Environment : constant := 3;

   Search_Path : constant POSIX_String := ".:./bin";
   Child_Filename : constant POSIX_String := "p030100b";
   Child_Pathname : constant POSIX_String := "./bin/p030100b";

begin

   Header ("p030100");

   Comment ("Pathname =" & To_String (Child_Pathname));
   Comment ("Filename =" & To_String (Child_Filename));
   Comment ("Search Path =" & To_String (Search_Path));

   ---------------------------------------------------------------------

   --  Set up file and environment variable for use in tests below.

   declare
      use Text_IO;
      Test_File : File_Type;
   begin
      Comment ("creating test file ");
      Create (Test_File, Out_File, "test_file");
      Put (Test_File, "01234");
      Close (Test_File);
      Set_Environment_Variable ("ABC", "abc");
   exception when E : others => Unexpected_Exception (E, "001");
   end;

   ---------------------------------------------------------------------

   Test ("Process_Template type [3.1.1]");
   declare
      Mask : Signal_Set;
      Template : Process_Template;
   begin

      Add_Signal (Mask, Signal_User_1);

      ------------------------------------------------------------------

      --  Assert: All operations detect an invalid template.

      begin
         Comment ("Set_Keep_Effective_IDs (invalid template)");
         Set_Keep_Effective_IDs (Template);
         --  invalid template not detected
         Assert (False, "002");
      exception
      when POSIX_Error => Check_Error_Code (Invalid_Argument, "003");
      when E : others => Unexpected_Exception (E, "004");
      end;

      begin
         Comment ("Set_Signal_Mask (invalid template)");
         Set_Signal_Mask (Template, Mask);
         --  invalid template not detected
         Assert (False, "005");
      exception
      when POSIX_Error => Check_Error_Code (Invalid_Argument, "006");
      when E : others => Unexpected_Exception (E, "007");
      end;

      begin
         Comment ("Set_Creation_Signal_Masking (invalid template)");
         Set_Creation_Signal_Masking (Template, All_Signals);
         --  invalid template not detected
         Assert (False, "008");
      exception
      when POSIX_Error => Check_Error_Code (Invalid_Argument, "009");
      when E : others => Unexpected_Exception (E, "010");
      end;

      begin
         Comment ("Set_File_Action_To_Open (invalid template)");
         Set_File_Action_To_Open (Template, 3, "test_file");
         --  invalid template not detected
         Assert (False, "011");
      exception
      when POSIX_Error => Check_Error_Code (Invalid_Argument, "012");
      when E : others => Unexpected_Exception (E, "013");
      end;

      begin
         Comment ("Set_File_Action_To_Duplicate (invalid template)");
         Set_File_Action_To_Duplicate (Template, 5, 3);
         --  invalid template not detected
         Assert (False, "014");
      exception
      when POSIX_Error => Check_Error_Code (Invalid_Argument, "015");
      when E : others => Unexpected_Exception (E, "016");
      end;

      begin
         Comment ("Set_File_Action_To_Close (invalid template)");
         Set_File_Action_To_Close (Template, 0);
         --  invalid template not detected
         Assert (False, "017");
      exception
      when POSIX_Error => Check_Error_Code (Invalid_Argument, "018");
      when E : others => Unexpected_Exception (E, "019");
      end;

      ------------------------------------------------------------------

      Comment ("Open_Template");
      Open_Template (Template);

      Comment ("Close_Template");
      Close_Template (Template);

      ------------------------------------------------------------------

      --  Assert: All operations detect a closed template.

      begin
         Comment ("Set_Keep_Effective_IDs (closed template)");
         Set_Keep_Effective_IDs (Template);
         --  closed template not detected
         Assert (False, "020");
      exception
      when POSIX_Error => Check_Error_Code (Invalid_Argument, "021");
      when E : others => Unexpected_Exception (E, "022");
      end;

      begin
         Comment ("Set_Signal_Mask (closed template)");
         Set_Signal_Mask (Template, Mask);
         --  closed template not detected
         Assert (False, "023");
      exception
      when POSIX_Error => Check_Error_Code (Invalid_Argument, "024");
      when E : others => Unexpected_Exception (E, "025");
      end;

      begin
         Comment ("Set_Creation_Signal_Masking (closed template)");
         Set_Creation_Signal_Masking (Template, All_Signals);
         --  closed template not detected
         Assert (False, "026");
      exception
      when POSIX_Error => Check_Error_Code (Invalid_Argument, "027");
      when E : others => Unexpected_Exception (E, "028");
      end;

      begin
         Comment ("Set_File_Action_To_Open (closed template)");
         Set_File_Action_To_Open (Template, 3, "test_file");
         --  closed template not detected
         Assert (False, "029");
      exception
      when POSIX_Error => Check_Error_Code (Invalid_Argument, "030");
      when E : others => Unexpected_Exception (E, "031");
      end;

      begin
         Comment ("Set_File_Action_To_Duplicate (closed template)");
         Set_File_Action_To_Duplicate (Template, 5, 3);
         --  closed template not detected
         Assert (False, "032");
      exception
      when POSIX_Error => Check_Error_Code (Invalid_Argument, "033");
      when E : others => Unexpected_Exception (E, "034");
      end;

      begin
         Comment ("Set_File_Action_To_Close (closed template)");
         Set_File_Action_To_Close (Template, 0);
         --  closed template not detected
         Assert (False, "035");
      exception
      when POSIX_Error => Check_Error_Code (Invalid_Argument, "036");
      when E : others => Unexpected_Exception (E, "037");
      end;

      ------------------------------------------------------------------

      Comment ("Open_Template");
      Open_Template (Template);

      ------------------------------------------------------------------

      --  Assert: All operations return normally for an open template.

      Comment ("Set_Keep_Effective_IDs");
      Set_Keep_Effective_IDs (Template);

      Comment ("Set_Signal_Mask");
      Set_Signal_Mask (Template, Mask);

      Comment ("Set_Creation_Signal_Masking");
      Set_Creation_Signal_Masking (Template, All_Signals);

      Comment ("Set_File_Action_To_Open");
      Set_File_Action_To_Open (Template, 3, "test_file");

      Comment ("Set_File_Action_To_Duplicate");
      Set_File_Action_To_Duplicate (Template, 5, 3);

      Comment ("Set_File_Action_To_Close");
      Set_File_Action_To_Close (Template, 0);

      ------------------------------------------------------------------

      Comment ("Close_Template");
      Close_Template (Template);

   exception when E : others => Unexpected_Exception (E, "038");
   end;

   ---------------------------------------------------------------------

   Test ("Exit_Status type [3.1.3]");
   declare
   begin
      --  Checking range
      Assert (Exit_Status'First = 0 and Exit_Status'Last = 2**8 - 1,
        "039");
      --  Checking constants
      Assert (Normal_Exit = 0 and Failed_Creation_Exit = 41 and
        Unhandled_Exception_Exit = 42, "040");
   exception when E : others => Unexpected_Exception (E, "041");
   end;

   ---------------------------------------------------------------------


   Test ("Termination_Status type [3.1.4]");
   declare
      Status : Termination_Status;
   begin
      Assert (Exited = Termination_Cause'First and
        Termination_Cause'Pos (Terminated_By_Signal) = 1 and
        Termination_Cause'Last = Stopped_By_Signal,
        "042");

      --  Checking initial value
      Assert (not Status_Available (Status), "043");

      declare
         Pid : Process_ID;
      begin
         Pid := Process_ID_Of (Status);
         --  Checking Process_ID_Of invalid status
         Assert (False, "044");
      exception
      when POSIX_Error => Check_Error_Code (Invalid_Argument, "045");
      when E : others => Unexpected_Exception (E, "046");
      end;

      declare
         Cause : Termination_Cause;
      begin
         Cause := Termination_Cause_Of (Status);
         --  Checking Termination_Cause_Of invalid status
         Assert (False, "047");
      exception
      when POSIX_Error => Check_Error_Code (Invalid_Argument, "048");
      when E : others => Unexpected_Exception (E, "049");
      end;

      declare
         E : Exit_Status;
      begin
         E := Exit_Status_Of (Status);
         --  Checking Exit_Status_Of invalid status
         Assert (False, "050");
      exception
      when POSIX_Error => Check_Error_Code (Invalid_Argument, "051");
      when E : others => Unexpected_Exception (E, "052");
      end;

      declare
         Sig : Signal;
      begin
         Sig := Termination_Signal_Of (Status);
         --  Checking Termination_Signal_Of invalid status
         Assert (False, "053");
      exception
      when POSIX_Error => Check_Error_Code (Invalid_Argument, "054");
      when E : others => Unexpected_Exception (E, "055");
      end;

      declare
         Sig : Signal;
      begin
         Sig := Stopping_Signal_Of (Status);
         --  Checking Stopping_Signal_Of invalid status
         Assert (False, "056");
      exception
      when POSIX_Error => Check_Error_Code (Invalid_Argument, "057");
      when E : others => Unexpected_Exception (E, "058");
      end;

   exception when E : others => Unexpected_Exception (E, "059");
   end;

   ---------------------------------------------------------------------

   Test ("Start_Process operations [3.1.2]");
   declare
      Template : Process_Template;
      Args : POSIX_String_List;
      Env : Environment;
      Pid : Process_ID;
      Status : Termination_Status;
   begin

      --  Checking Args
      Assert (Length (Args) = 0, "060");
      POSIX.Append (Args, "-child" &
        To_POSIX_String (Integer'Image (Should_Not_Start)));

      ------------------------------------------------------------------

      --  Assert: All operations detect an invalid template.

      begin
         Comment ("Start_Process (invalid template)");
         Start_Process (Pid, Child_Pathname, Template, Args);
         --  Checking invalid template not detected
         Assert (False, "061");
      exception
      when POSIX_Error => Check_Error_Code (Invalid_Argument, "062");
      when E : others => Unexpected_Exception (E, "063");
      end;

      ------------------------------------------------------------------

      begin
         Comment ("Start_Process with Env  (invalid template)");
         Start_Process (Pid, Child_Pathname, Template, Env, Args);
         --  Checking invalid template not detected
         Assert (False, "064");
      exception
      when POSIX_Error => Check_Error_Code (Invalid_Argument, "065");
      when E : others => Unexpected_Exception (E, "066");
      end;

      ------------------------------------------------------------------

      begin
         Comment ("Start_Process_Search (invalid template)");
         Start_Process_Search (Pid, Child_Filename, Template, Args);
         --  Checking invalid template not detected
         Assert (False, "067");
      exception
      when POSIX_Error => Check_Error_Code (Invalid_Argument, "068");
      when E : others => Unexpected_Exception (E, "069");
      end;

      ------------------------------------------------------------------

      begin
         Comment ("Start_Process_Search with Env (invalid template)");
         Start_Process_Search
          (Pid, Child_Filename, Template, Env, Args);
         --  Checking invalid template not detected
         Assert (False, "070");
      exception
      when POSIX_Error => Check_Error_Code (Invalid_Argument, "071");
      when E : others => Unexpected_Exception (E, "072");
      end;

      ------------------------------------------------------------------

      Open_Template (Template);
      Close_Template (Template);

      ------------------------------------------------------------------

      --  Assert: All operations detect a closed template.

      begin
         Comment ("Start_Process (closed template)");
         Start_Process (Pid, Child_Pathname, Template, Args);
         --  Checking closed template not detected
         Assert (False, "073");
      exception
      when POSIX_Error => Check_Error_Code (Invalid_Argument, "074");
      when E : others => Unexpected_Exception (E, "075");
      end;

      ------------------------------------------------------------------

      begin
         Comment ("Start_Process with Env  (closed template)");
         Start_Process (Pid, Child_Pathname, Template, Env, Args);
         --  Checking closed template not detected
         Assert (False, "076");
      exception
      when POSIX_Error => Check_Error_Code (Invalid_Argument, "077");
      when E : others => Unexpected_Exception (E, "078");
      end;

      ------------------------------------------------------------------

      begin
         Comment ("Start_Process_Search (closed template)");
         Start_Process_Search (Pid, Child_Filename, Template, Args);
         --  Checking closed template not detected
         Assert (False, "079");
      exception
      when POSIX_Error => Check_Error_Code (Invalid_Argument, "080");
      when E : others => Unexpected_Exception (E, "081");
      end;

      ------------------------------------------------------------------

      begin
         Comment ("Start_Process_Search with Env (closed template)");
         Start_Process_Search
          (Pid, Child_Filename, Template, Env, Args);
         --  Checking closed template not detected
         Assert (False, "082");
      exception
      when POSIX_Error => Check_Error_Code (Invalid_Argument, "083");
      when E : others => Unexpected_Exception (E, "084");
      end;

      ------------------------------------------------------------------

      begin
         Comment ("Start_Process (closed template)");
         Start_Process (Pid, Child_Pathname, Template, Args);
         --  Checking closed template not detected
         Assert (False, "085");
      exception
      when POSIX_Error => Check_Error_Code (Invalid_Argument, "086");
      when E : others => Unexpected_Exception (E, "087");
      end;

      ------------------------------------------------------------------

      Open_Template (Template);

      begin
         Comment ("Set up argument list");
         Make_Empty (Args);
         POSIX.Append (Args, "");
         POSIX.Append (Args, "-child" &
           To_POSIX_String (Integer'Image (Parents_Environment)));
         Pass_Through_Verbosity (Args);
         Comment ("Set up environment");
         Set_Environment_Variable (Child_Filename, "default");
         Set_Environment_Variable (Child_Filename, "special", Env);
         Set_Environment_Variable ("PATH", Search_Path);
         Set_Environment_Variable ("PATH", Search_Path, Env);
      exception
      when POSIX_Error => Check_Error_Code (Invalid_Argument, "088");
      when E : others => Unexpected_Exception (E, "089");
      end;

      ------------------------------------------------------------------

      --  Assert: All operations work for an open template.

      begin
         Comment ("Start_Process");
         Start_Process (Pid, Child_Pathname, Template, Args);
         Wait_For_Child_Process (Status, Pid);
         Check_Child_Status (Status, Pid, 0, "090");
      exception
      when E : others => Unexpected_Exception (E, "091");
      end;

      begin
         Comment ("Start_Process_Search with filename");
         Start_Process (Pid, Child_Filename, Template, Args);
         Wait_For_Child_Process (Status, Pid);
         Check_Child_Status (Status, Pid, 0, "092");
      exception
      when E : others => Unexpected_Exception (E, "093");
      end;

      begin
         Comment ("Start_Process_Search with pathname");
         Start_Process (Pid, Child_Pathname, Template, Args);
         Wait_For_Child_Process (Status, Pid);
         Check_Child_Status (Status, Pid, 0, "094");
      exception
      when E : others => Unexpected_Exception (E, "095");
      end;

      ------------------------------------------------------------------

      --  Set up arguments to cause child to look at environment
      --  variables this time.

      begin
         Comment ("Reset argument list");
         Make_Empty (Args);
         POSIX.Append (Args, "");
         POSIX.Append (Args, "-child" &
           To_POSIX_String (Integer'Image (Explicit_Environment)));
         Pass_Through_Verbosity (Args);
      exception
      when POSIX_Error => Check_Error_Code (Invalid_Argument, "096");
      when E : others => Unexpected_Exception (E, "097");
      end;

      ------------------------------------------------------------------

      begin
         Comment ("Start_Process with Env");
         Comment ("-1-");
         Start_Process (Pid, Child_Pathname, Template, Env, Args);
         Comment ("-2-");
         Wait_For_Child_Process (Status, Pid);
         Comment ("-3-");
         Check_Child_Status (Status, Pid, 0, "098");
         Comment ("-4-");
      exception
      when E : others => Unexpected_Exception (E, "099");
      end;

      begin
         Comment ("Start_Process_Search with Env and filename");
         Start_Process_Search
           (Pid, Child_Filename, Template, Env, Args);
         Wait_For_Child_Process (Status, Pid);
         Check_Child_Status (Status, Pid, 0, "100");
      exception
      when E : others => Unexpected_Exception (E, "101");
      end;

      begin
         Comment ("Start_Process_Search with Env and pathname");
         Start_Process_Search
           (Pid, Child_Pathname, Template, Env, Args);
         Wait_For_Child_Process (Status, Pid);
         Check_Child_Status (Status, Pid, 0, "102");
      exception
      when E : others => Unexpected_Exception (E, "103");
      end;

      -----------------------------------------------------------------

      Close_Template (Template);

   exception when E : others => Unexpected_Exception (E, "104");
   end;

   ---------------------------------------------------------------------

   Test ("Start_Process operations that fail [3.1.2]");
   declare
      Template : Process_Template;
      Args : POSIX_String_List;
      Pid : Process_ID;
      Status : Termination_Status;
   begin

      Assert (Length (Args) = 0, "105");
      POSIX.Append (Args, "-child" &
        To_POSIX_String (Integer'Image (Should_Not_Start)));

      Open_Template (Template);

      ------------------------------------------------------------------

      begin
         Comment ("Start_Process with nonexistent program");
         Start_Process (Pid, "not the name of a file", Template, Args);
         Wait_For_Child_Process (Status, Pid);
         Check_Child_Status (Status, Pid, Failed_Creation_Exit, "106");
      exception
      when E : others => Unexpected_Exception (E, "107");
      end;

      begin
         Comment ("Start_Process with nonexistent file to open");
         Set_File_Action_To_Open (Template, 3, "not the name of a file");
         Start_Process (Pid, Child_Pathname, Template, Args);
         Wait_For_Child_Process (Status, Pid);
         Check_Child_Status (Status, Pid, Failed_Creation_Exit, "108");
      exception
      when E : others => Unexpected_Exception (E, "109");
      end;

      ------------------------------------------------------------------

      Close_Template (Template);

   exception when E : others => Unexpected_Exception (E, "110");
   end;

   ---------------------------------------------------------------------

   Test ("Wait_For_Child_Process operations [3.1.5]");
   declare
      Template : Process_Template;
      Args : POSIX_String_List;
      Env : Environment;
      Pid : Process_ID;
      Status : Termination_Status;
   begin

      ------------------------------------------------------------------

      Open_Template (Template);

      begin
         Comment ("Set up argument list");
         POSIX.Append (Args, "");
         POSIX.Append (Args, "-child" &
           To_POSIX_String (Integer'Image (Parents_Environment)));
         Pass_Through_Verbosity (Args);
         Comment ("Set up environment");
         Set_Environment_Variable (Child_Filename, "default");
         Set_Environment_Variable (Child_Filename, "special", Env);
         Set_Environment_Variable ("PATH", Search_Path);
         Set_Environment_Variable ("PATH", Search_Path, Env);
         Set_Environment_Variable ("WAIT", "YES", Env);
      exception
      when POSIX_Error => Check_Error_Code (Invalid_Argument, "111");
      when E : others => Unexpected_Exception (E, "112");
      end;

      ------------------------------------------------------------------

      Comment ("Wait_For_Child_Process with no ID");
      begin
         Start_Process_Search
           (Pid, Child_Filename, Template, Env, Args);
         Comment ("check status of active child");
         Wait_For_Child_Process (Status,
           Block => False);
         --  Checking status available
         Assert (not Status_Available (Status), "113");
         if POSIX_Configurable_System_Limits.Job_Control_Supported then
            Comment ("stop child process");
            Send_Signal (Pid, Signal_Stop);
            --  first, ignore stopped jobs
            Wait_For_Child_Process (Status,
              Block => False,
              Trace_Stopped => False);
            --  Checking status available
            Assert (not Status_Available (Status), "114");
            --  now, include stopped jobs
            Wait_For_Child_Process (Status,
              Block => True,
              Trace_Stopped => True);
            --  Checking status not available
            Assert (Status_Available (Status), "115");
            --  Checking Pid
            Assert (Process_ID_Of (Status) = Pid, "116");
            --  Checking cause
            Assert (Termination_Cause_Of (Status) = Stopped_By_Signal,
              "117");
            --  Checking signal
            Assert (Stopping_Signal_Of (Status) = Signal_Stop,
              "118");
            declare
               E : Exit_Status;
            begin
               E := Exit_Status_Of (Status);
               --  Fail on exit
               Assert (False, "119");
            exception
            when POSIX_Error => Check_Error_Code (Invalid_Argument, "120");
            when E : others => Unexpected_Exception (E, "121");
            end;
            Comment ("continue child process");
            Send_Signal (Pid, Signal_Continue);
            Wait_For_Child_Process (Status,
              Block => False,
              Trace_Stopped => True);
            Assert (not Status_Available (Status), "122");
         else
            Comment ("Job control option is not supported");
         end if;
         Comment ("kill child process");
         Send_Signal (Pid, Signal_Kill);
         Wait_For_Child_Process (Status,
           Block => True,
           Trace_Stopped => False);
         Assert (Status_Available (Status), "123");
         --  Checking pid
         Assert (Process_ID_Of (Status) = Pid, "124");
         --  Checking cause
         Assert (Termination_Cause_Of (Status) = Terminated_By_Signal,
           "125");
         --  Checking signal
         Assert (Termination_Signal_Of (Status) = Signal_Kill,
           "126");
      exception
      when E : others => Unexpected_Exception (E, "127");
      end;

      ------------------------------------------------------------------

      Comment ("Wait_For_Child_Process with process ID");
      begin
         Start_Process_Search
           (Pid, Child_Filename, Template, Env, Args);
         Comment ("check status of active child");
         Wait_For_Child_Process (Status,
           Child => Pid,
           Block => False);
         Assert (not Status_Available (Status), "128");
         if POSIX_Configurable_System_Limits.Job_Control_Supported then
            Comment ("stop child process");
            Send_Signal (Pid, Signal_Stop);
            --  first, ignore stopped jobs
            Wait_For_Child_Process (Status,
              Child => Pid,
              Block => False,
              Trace_Stopped => False);
            Assert (not Status_Available (Status), "129");
            --  now, include stopped jobs
            Wait_For_Child_Process (Status,
              Child => Pid,
              Block => True,
              Trace_Stopped => True);
            Assert (Status_Available (Status), "130");
            --  Checking pid
            Assert (Process_ID_Of (Status) = Pid, "131");
            --  Checking cause
            Assert (Termination_Cause_Of (Status) = Stopped_By_Signal, "132");
            --  Checking signal
            Assert (Stopping_Signal_Of (Status) = Signal_Stop, "133");
            declare
               E : Exit_Status;
            begin
               E := Exit_Status_Of (Status);
               Assert (False, "exited");
            exception
            when POSIX_Error => Check_Error_Code (Invalid_Argument, "134");
            when E : others => Unexpected_Exception (E, "135");
            end;
            Comment ("continue child process");
            Send_Signal (Pid, Signal_Continue);
            Wait_For_Child_Process (Status,
              Child => Pid,
              Block => False,
              Trace_Stopped => True);
            Assert (not Status_Available (Status), "136");
         else
            Comment ("Job control option is not supported");
         end if;
         Comment ("kill child process");
         Send_Signal (Pid, Signal_Kill);
         Wait_For_Child_Process (Status,
           Child => Pid,
           Block => True,
           Trace_Stopped => False);
         Assert (Status_Available (Status), "137");
         --  Checking pid
         Assert (Process_ID_Of (Status) = Pid, "138");
         --  Checking cause
         Assert (Termination_Cause_Of (Status) = Terminated_By_Signal,
           "139");
         --  Checking signal
         Assert (Termination_Signal_Of (Status) = Signal_Kill,
           "140");
      exception
      when E : others => Unexpected_Exception (E, "141");
      end;


      ------------------------------------------------------------------

      Comment ("Wait_For_Child_Process with group ID");
      declare
         Gid : Process_Group_ID := Get_Process_Group_ID;
      begin
         Start_Process_Search
           (Pid, Child_Filename, Template, Env, Args);
         Comment ("check status of active child");
         Wait_For_Child_Process (Status,
           Group => Gid,
           Block => False);
         Assert (not Status_Available (Status), "142");
         if POSIX_Configurable_System_Limits.Job_Control_Supported then
            Comment ("stop child process");
            Send_Signal (Pid, Signal_Stop);
            --  first, ignore stopped jobs
            Wait_For_Child_Process (Status,
              Group => Gid,
              Block => False,
              Trace_Stopped => False);
            Assert (not Status_Available (Status), "143");
            --  now, include stopped jobs
            Wait_For_Child_Process (Status,
              Group => Gid,
              Block => True,
              Trace_Stopped => True);
            Assert (Status_Available (Status), "144");
            --  Checking pid
            Assert (Process_ID_Of (Status) = Pid, "145");
            --  Checking cause
            Assert (Termination_Cause_Of (Status) = Stopped_By_Signal,
              "146");
            --  Checking signal
            Assert (Stopping_Signal_Of (Status) = Signal_Stop,
              "147");
            declare
               E : Exit_Status;
            begin
               E := Exit_Status_Of (Status);
               Assert (False, "148");
            exception
            when POSIX_Error => Check_Error_Code (Invalid_Argument, "149");
            when E : others => Unexpected_Exception (E, "150");
            end;
            Comment ("continue child process");
            Send_Signal (Pid, Signal_Continue);
            Wait_For_Child_Process (Status,
              Group => Gid,
              Block => False,
              Trace_Stopped => True);
            Assert (not Status_Available (Status), "151");
         else
            Comment ("Job control option is not supported");
         end if;
         Comment ("kill child process");
         Send_Signal (Pid, Signal_Kill);
         Wait_For_Child_Process (Status,
           Group => Gid,
           Block => True,
           Trace_Stopped => False);
         Assert (Status_Available (Status), "152");
         --  Checking pid
         Assert (Process_ID_Of (Status) = Pid, "153");
         --  Checking cause
         Assert (Termination_Cause_Of (Status) = Terminated_By_Signal,
           "154");
         --  Checking signal
         Assert (Termination_Signal_Of (Status) = Signal_Kill,
           "155");
      exception
      when E : others => Unexpected_Exception (E, "156");
      end;

      ------------------------------------------------------------------

      Close_Template (Template);

   exception when E : others => Unexpected_Exception (E, "157");
   end;

   --------------------------------------------------------------------

   --  remove the file created for this test.
   POSIX_Files.Unlink ("test_file");

   --------------------------------------------------------------------

   Done;

exception
when E : others =>
   POSIX_Files.Unlink ("test_file");
   Fatal_Exception (E, "158");
end p030100;

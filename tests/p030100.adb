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
   exception when E : others => Unexpected_Exception (E, "A001");
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
         Assert (False, "A002");
      exception
      when POSIX_Error => Check_Error_Code (Invalid_Argument, "A003");
      when E : others => Unexpected_Exception (E, "A004");
      end;

      begin
         Comment ("Set_Signal_Mask (invalid template)");
         Set_Signal_Mask (Template, Mask);
         --  invalid template not detected
         Assert (False, "A005");
      exception
      when POSIX_Error => Check_Error_Code (Invalid_Argument, "A006");
      when E : others => Unexpected_Exception (E, "A007");
      end;

      begin
         Comment ("Set_Creation_Signal_Masking (invalid template)");
         Set_Creation_Signal_Masking (Template, All_Signals);
         --  invalid template not detected
         Assert (False, "A008");
      exception
      when POSIX_Error => Check_Error_Code (Invalid_Argument, "A009");
      when E : others => Unexpected_Exception (E, "A010");
      end;

      begin
         Comment ("Set_File_Action_To_Open (invalid template)");
         Set_File_Action_To_Open (Template, 3, "test_file");
         --  invalid template not detected
         Assert (False, "A011");
      exception
      when POSIX_Error => Check_Error_Code (Invalid_Argument, "A012");
      when E : others => Unexpected_Exception (E, "A013");
      end;

      begin
         Comment ("Set_File_Action_To_Duplicate (invalid template)");
         Set_File_Action_To_Duplicate (Template, 5, 3);
         --  invalid template not detected
         Assert (False, "A014");
      exception
      when POSIX_Error => Check_Error_Code (Invalid_Argument, "A015");
      when E : others => Unexpected_Exception (E, "A016");
      end;

      begin
         Comment ("Set_File_Action_To_Close (invalid template)");
         Set_File_Action_To_Close (Template, 0);
         --  invalid template not detected
         Assert (False, "A017");
      exception
      when POSIX_Error => Check_Error_Code (Invalid_Argument, "A018");
      when E : others => Unexpected_Exception (E, "A019");
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
         Assert (False, "A020");
      exception
      when POSIX_Error => Check_Error_Code (Invalid_Argument, "A021");
      when E : others => Unexpected_Exception (E, "A022");
      end;

      begin
         Comment ("Set_Signal_Mask (closed template)");
         Set_Signal_Mask (Template, Mask);
         --  closed template not detected
         Assert (False, "A023");
      exception
      when POSIX_Error => Check_Error_Code (Invalid_Argument, "A024");
      when E : others => Unexpected_Exception (E, "A025");
      end;

      begin
         Comment ("Set_Creation_Signal_Masking (closed template)");
         Set_Creation_Signal_Masking (Template, All_Signals);
         --  closed template not detected
         Assert (False, "A026");
      exception
      when POSIX_Error => Check_Error_Code (Invalid_Argument, "A027");
      when E : others => Unexpected_Exception (E, "A028");
      end;

      begin
         Comment ("Set_File_Action_To_Open (closed template)");
         Set_File_Action_To_Open (Template, 3, "test_file");
         --  closed template not detected
         Assert (False, "A029");
      exception
      when POSIX_Error => Check_Error_Code (Invalid_Argument, "A030");
      when E : others => Unexpected_Exception (E, "A031");
      end;

      begin
         Comment ("Set_File_Action_To_Duplicate (closed template)");
         Set_File_Action_To_Duplicate (Template, 5, 3);
         --  closed template not detected
         Assert (False, "A032");
      exception
      when POSIX_Error => Check_Error_Code (Invalid_Argument, "A033");
      when E : others => Unexpected_Exception (E, "A034");
      end;

      begin
         Comment ("Set_File_Action_To_Close (closed template)");
         Set_File_Action_To_Close (Template, 0);
         --  closed template not detected
         Assert (False, "A035");
      exception
      when POSIX_Error => Check_Error_Code (Invalid_Argument, "A036");
      when E : others => Unexpected_Exception (E, "A037");
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

   exception when E : others => Unexpected_Exception (E, "A038");
   end;

   ---------------------------------------------------------------------

   Test ("Exit_Status type [3.1.3]");
   declare
   begin
      --  Checking range
      Assert (Exit_Status'First = 0 and Exit_Status'Last = 2**8 - 1,
        "A039");
      --  Checking constants
      Assert (Normal_Exit = 0 and Failed_Creation_Exit = 41 and
        Unhandled_Exception_Exit = 42, "A040");
   exception when E : others => Unexpected_Exception (E, "A041");
   end;

   ---------------------------------------------------------------------


   Test ("Termination_Status type [3.1.4]");
   declare
      Status : Termination_Status;
   begin
      Assert (Exited = Termination_Cause'First and
        Termination_Cause'Pos (Terminated_By_Signal) = 1 and
        Termination_Cause'Last = Stopped_By_Signal,
        "A042");

      --  Checking initial value
      Assert (not Status_Available (Status), "A043");

      declare
         Pid : Process_ID;
      begin
         Pid := Process_ID_Of (Status);
         --  Checking Process_ID_Of invalid status
         Assert (False, "A044");
      exception
      when POSIX_Error => Check_Error_Code (Invalid_Argument, "A045");
      when E : others => Unexpected_Exception (E, "A046");
      end;

      declare
         Cause : Termination_Cause;
      begin
         Cause := Termination_Cause_Of (Status);
         --  Checking Termination_Cause_Of invalid status
         Assert (False, "A047");
      exception
      when POSIX_Error => Check_Error_Code (Invalid_Argument, "A048");
      when E : others => Unexpected_Exception (E, "A049");
      end;

      declare
         E : Exit_Status;
      begin
         E := Exit_Status_Of (Status);
         --  Checking Exit_Status_Of invalid status
         Assert (False, "A050");
      exception
      when POSIX_Error => Check_Error_Code (Invalid_Argument, "A051");
      when E : others => Unexpected_Exception (E, "A052");
      end;

      declare
         Sig : Signal;
      begin
         Sig := Termination_Signal_Of (Status);
         --  Checking Termination_Signal_Of invalid status
         Assert (False, "A053");
      exception
      when POSIX_Error => Check_Error_Code (Invalid_Argument, "A054");
      when E : others => Unexpected_Exception (E, "A055");
      end;

      declare
         Sig : Signal;
      begin
         Sig := Stopping_Signal_Of (Status);
         --  Checking Stopping_Signal_Of invalid status
         Assert (False, "A056");
      exception
      when POSIX_Error => Check_Error_Code (Invalid_Argument, "A057");
      when E : others => Unexpected_Exception (E, "A058");
      end;

   exception when E : others => Unexpected_Exception (E, "A059");
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
      Assert (Length (Args) = 0, "A060");
      POSIX.Append (Args, "-child" &
        To_POSIX_String (Integer'Image (Should_Not_Start)));

      ------------------------------------------------------------------

      --  Assert: All operations detect an invalid template.

      begin
         Comment ("Start_Process (invalid template)");
         Start_Process (Pid, Child_Pathname, Template, Args);
         --  Checking invalid template not detected
         Assert (False, "A061");
      exception
      when POSIX_Error => Check_Error_Code (Invalid_Argument, "A062");
      when E : others => Unexpected_Exception (E, "A063");
      end;

      ------------------------------------------------------------------

      begin
         Comment ("Start_Process with Env  (invalid template)");
         Start_Process (Pid, Child_Pathname, Template, Env, Args);
         --  Checking invalid template not detected
         Assert (False, "A064");
      exception
      when POSIX_Error => Check_Error_Code (Invalid_Argument, "A065");
      when E : others => Unexpected_Exception (E, "A066");
      end;

      ------------------------------------------------------------------

      begin
         Comment ("Start_Process_Search (invalid template)");
         Start_Process_Search (Pid, Child_Filename, Template, Args);
         --  Checking invalid template not detected
         Assert (False, "A067");
      exception
      when POSIX_Error => Check_Error_Code (Invalid_Argument, "A068");
      when E : others => Unexpected_Exception (E, "A069");
      end;

      ------------------------------------------------------------------

      begin
         Comment ("Start_Process_Search with Env (invalid template)");
         Start_Process_Search
          (Pid, Child_Filename, Template, Env, Args);
         --  Checking invalid template not detected
         Assert (False, "A070");
      exception
      when POSIX_Error => Check_Error_Code (Invalid_Argument, "A071");
      when E : others => Unexpected_Exception (E, "A072");
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
         Assert (False, "A073");
      exception
      when POSIX_Error => Check_Error_Code (Invalid_Argument, "A074");
      when E : others => Unexpected_Exception (E, "A075");
      end;

      ------------------------------------------------------------------

      begin
         Comment ("Start_Process with Env  (closed template)");
         Start_Process (Pid, Child_Pathname, Template, Env, Args);
         --  Checking closed template not detected
         Assert (False, "A076");
      exception
      when POSIX_Error => Check_Error_Code (Invalid_Argument, "A077");
      when E : others => Unexpected_Exception (E, "A078");
      end;

      ------------------------------------------------------------------

      begin
         Comment ("Start_Process_Search (closed template)");
         Start_Process_Search (Pid, Child_Filename, Template, Args);
         --  Checking closed template not detected
         Assert (False, "A079");
      exception
      when POSIX_Error => Check_Error_Code (Invalid_Argument, "A080");
      when E : others => Unexpected_Exception (E, "A081");
      end;

      ------------------------------------------------------------------

      begin
         Comment ("Start_Process_Search with Env (closed template)");
         Start_Process_Search
          (Pid, Child_Filename, Template, Env, Args);
         --  Checking closed template not detected
         Assert (False, "A082");
      exception
      when POSIX_Error => Check_Error_Code (Invalid_Argument, "A083");
      when E : others => Unexpected_Exception (E, "A084");
      end;

      ------------------------------------------------------------------

      begin
         Comment ("Start_Process (closed template)");
         Start_Process (Pid, Child_Pathname, Template, Args);
         --  Checking closed template not detected
         Assert (False, "A085");
      exception
      when POSIX_Error => Check_Error_Code (Invalid_Argument, "A086");
      when E : others => Unexpected_Exception (E, "A087");
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
      when POSIX_Error => Check_Error_Code (Invalid_Argument, "A088");
      when E : others => Unexpected_Exception (E, "A089");
      end;

      ------------------------------------------------------------------

      --  Assert: All operations work for an open template.

      begin
         Comment ("Start_Process");
         Start_Process (Pid, Child_Pathname, Template, Args);
         Wait_For_Child_Process (Status, Pid);
         Check_Child_Status (Status, Pid, 0, "A090");
      exception
      when E : others => Unexpected_Exception (E, "A091");
      end;

      begin
         Comment ("Start_Process_Search with filename");
         Start_Process (Pid, Child_Filename, Template, Args);
         Wait_For_Child_Process (Status, Pid);
         Check_Child_Status (Status, Pid, 0, "A092");
      exception
      when E : others => Unexpected_Exception (E, "A093");
      end;

      begin
         Comment ("Start_Process_Search with pathname");
         Start_Process (Pid, Child_Pathname, Template, Args);
         Wait_For_Child_Process (Status, Pid);
         Check_Child_Status (Status, Pid, 0, "A094");
      exception
      when E : others => Unexpected_Exception (E, "A095");
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
      when POSIX_Error => Check_Error_Code (Invalid_Argument, "A096");
      when E : others => Unexpected_Exception (E, "A097");
      end;

      ------------------------------------------------------------------

      begin
         Comment ("Start_Process with Env");
         Comment ("-1-");
         Start_Process (Pid, Child_Pathname, Template, Env, Args);
         Comment ("-2-");
         Wait_For_Child_Process (Status, Pid);
         Comment ("-3-");
         Check_Child_Status (Status, Pid, 0, "A098");
         Comment ("-4-");
      exception
      when E : others => Unexpected_Exception (E, "A099");
      end;

      begin
         Comment ("Start_Process_Search with Env and filename");
         Start_Process_Search
           (Pid, Child_Filename, Template, Env, Args);
         Wait_For_Child_Process (Status, Pid);
         Check_Child_Status (Status, Pid, 0, "100");
      exception
      when E : others => Unexpected_Exception (E, "A101");
      end;

      begin
         Comment ("Start_Process_Search with Env and pathname");
         Start_Process_Search
           (Pid, Child_Pathname, Template, Env, Args);
         Wait_For_Child_Process (Status, Pid);
         Check_Child_Status (Status, Pid, 0, "A102");
      exception
      when E : others => Unexpected_Exception (E, "A103");
      end;

      -----------------------------------------------------------------

      Close_Template (Template);

   exception when E : others => Unexpected_Exception (E, "A104");
   end;

   ---------------------------------------------------------------------

   Test ("Start_Process operations that fail [3.1.2]");
   declare
      Template : Process_Template;
      Args : POSIX_String_List;
      Pid : Process_ID;
      Status : Termination_Status;
   begin

      Assert (Length (Args) = 0, "A105");
      POSIX.Append (Args, "-child" &
        To_POSIX_String (Integer'Image (Should_Not_Start)));

      Open_Template (Template);

      ------------------------------------------------------------------

      begin
         Comment ("Start_Process with nonexistent program");
         Start_Process (Pid, "not the name of a file", Template, Args);
         Wait_For_Child_Process (Status, Pid);
         Check_Child_Status (Status, Pid, Failed_Creation_Exit, "A106");
      exception
      when E : others => Unexpected_Exception (E, "A107");
      end;

      begin
         Comment ("Start_Process with nonexistent file to open");
         Set_File_Action_To_Open (Template, 3, "not the name of a file");
         Start_Process (Pid, Child_Pathname, Template, Args);
         Wait_For_Child_Process (Status, Pid);
         Check_Child_Status (Status, Pid, Failed_Creation_Exit, "A108");
      exception
      when E : others => Unexpected_Exception (E, "A109");
      end;

      ------------------------------------------------------------------

      Close_Template (Template);

   exception when E : others => Unexpected_Exception (E, "A110");
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
      when POSIX_Error => Check_Error_Code (Invalid_Argument, "A111");
      when E : others => Unexpected_Exception (E, "A112");
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
         Assert (not Status_Available (Status), "A113");
         if POSIX_Configurable_System_Limits.Job_Control_Supported then
            Comment ("stop child process");
            Send_Signal (Pid, Signal_Stop);
            --  first, ignore stopped jobs
            Wait_For_Child_Process (Status,
              Block => False,
              Trace_Stopped => False);
            --  Checking status available
            Assert (not Status_Available (Status), "A114");
            --  now, include stopped jobs
            Wait_For_Child_Process (Status,
              Block => True,
              Trace_Stopped => True);
            --  Checking status not available
            Assert (Status_Available (Status), "A115");
            --  Checking Pid
            Assert (Process_ID_Of (Status) = Pid, "A116");
            --  Checking cause
            Assert (Termination_Cause_Of (Status) = Stopped_By_Signal,
              "A117");
            --  Checking signal
            Assert (Stopping_Signal_Of (Status) = Signal_Stop,
              "A118");
            declare
               E : Exit_Status;
            begin
               E := Exit_Status_Of (Status);
               --  Fail on exit
               Assert (False, "A119");
            exception
            when POSIX_Error => Check_Error_Code (Invalid_Argument, "A120");
            when E : others => Unexpected_Exception (E, "A121");
            end;
            Comment ("continue child process");
            Send_Signal (Pid, Signal_Continue);
            Wait_For_Child_Process (Status,
              Block => False,
              Trace_Stopped => True);
            Assert (not Status_Available (Status), "A122");
         else
            Comment ("Job control option is not supported");
         end if;
         Comment ("kill child process");
         Send_Signal (Pid, Signal_Kill);
         Wait_For_Child_Process (Status,
           Block => True,
           Trace_Stopped => False);
         Assert (Status_Available (Status), "A123");
         --  Checking pid
         Assert (Process_ID_Of (Status) = Pid, "A124");
         --  Checking cause
         Assert (Termination_Cause_Of (Status) = Terminated_By_Signal,
           "A125");
         --  Checking signal
         Assert (Termination_Signal_Of (Status) = Signal_Kill,
           "A126");
      exception
      when E : others => Unexpected_Exception (E, "A127");
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
         Assert (not Status_Available (Status), "A128");
         if POSIX_Configurable_System_Limits.Job_Control_Supported then
            Comment ("stop child process");
            Send_Signal (Pid, Signal_Stop);
            --  first, ignore stopped jobs
            Wait_For_Child_Process (Status,
              Child => Pid,
              Block => False,
              Trace_Stopped => False);
            Assert (not Status_Available (Status), "A129");
            --  now, include stopped jobs
            Wait_For_Child_Process (Status,
              Child => Pid,
              Block => True,
              Trace_Stopped => True);
            Assert (Status_Available (Status), "A130");
            --  Checking pid
            Assert (Process_ID_Of (Status) = Pid, "A131");
            --  Checking cause
            Assert (Termination_Cause_Of (Status) = Stopped_By_Signal, "A132");
            --  Checking signal
            Assert (Stopping_Signal_Of (Status) = Signal_Stop, "A133");
            declare
               E : Exit_Status;
            begin
               E := Exit_Status_Of (Status);
               Assert (False, "exited");
            exception
            when POSIX_Error => Check_Error_Code (Invalid_Argument, "A134");
            when E : others => Unexpected_Exception (E, "A135");
            end;
            Comment ("continue child process");
            Send_Signal (Pid, Signal_Continue);
            Wait_For_Child_Process (Status,
              Child => Pid,
              Block => False,
              Trace_Stopped => True);
            Assert (not Status_Available (Status), "A136");
         else
            Comment ("Job control option is not supported");
         end if;
         Comment ("kill child process");
         Send_Signal (Pid, Signal_Kill);
         Wait_For_Child_Process (Status,
           Child => Pid,
           Block => True,
           Trace_Stopped => False);
         Assert (Status_Available (Status), "A137");
         --  Checking pid
         Assert (Process_ID_Of (Status) = Pid, "A138");
         --  Checking cause
         Assert (Termination_Cause_Of (Status) = Terminated_By_Signal,
           "A139");
         --  Checking signal
         Assert (Termination_Signal_Of (Status) = Signal_Kill,
           "A140");
      exception
      when E : others => Unexpected_Exception (E, "A141");
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
         Assert (not Status_Available (Status), "A142");
         if POSIX_Configurable_System_Limits.Job_Control_Supported then
            Comment ("stop child process");
            Send_Signal (Pid, Signal_Stop);
            --  first, ignore stopped jobs
            Wait_For_Child_Process (Status,
              Group => Gid,
              Block => False,
              Trace_Stopped => False);
            Assert (not Status_Available (Status), "A143");
            --  now, include stopped jobs
            Wait_For_Child_Process (Status,
              Group => Gid,
              Block => True,
              Trace_Stopped => True);
            Assert (Status_Available (Status), "A144");
            --  Checking pid
            Assert (Process_ID_Of (Status) = Pid, "A145");
            --  Checking cause
            Assert (Termination_Cause_Of (Status) = Stopped_By_Signal,
              "A146");
            --  Checking signal
            Assert (Stopping_Signal_Of (Status) = Signal_Stop,
              "A147");
            declare
               E : Exit_Status;
            begin
               E := Exit_Status_Of (Status);
               Assert (False, "A148");
            exception
            when POSIX_Error => Check_Error_Code (Invalid_Argument, "A149");
            when E : others => Unexpected_Exception (E, "A150");
            end;
            Comment ("continue child process");
            Send_Signal (Pid, Signal_Continue);
            Wait_For_Child_Process (Status,
              Group => Gid,
              Block => False,
              Trace_Stopped => True);
            Assert (not Status_Available (Status), "A151");
         else
            Comment ("Job control option is not supported");
         end if;
         Comment ("kill child process");
         Send_Signal (Pid, Signal_Kill);
         Wait_For_Child_Process (Status,
           Group => Gid,
           Block => True,
           Trace_Stopped => False);
         Assert (Status_Available (Status), "A152");
         --  Checking pid
         Assert (Process_ID_Of (Status) = Pid, "A153");
         --  Checking cause
         Assert (Termination_Cause_Of (Status) = Terminated_By_Signal,
           "A154");
         --  Checking signal
         Assert (Termination_Signal_Of (Status) = Signal_Kill,
           "A155");
      exception
      when E : others => Unexpected_Exception (E, "A156");
      end;

      ------------------------------------------------------------------

      Close_Template (Template);

   exception when E : others => Unexpected_Exception (E, "A157");
   end;

   --------------------------------------------------------------------

   --  remove the file created for this test.
   POSIX_Files.Unlink ("test_file");

   --------------------------------------------------------------------

   Done;

exception
when E : others =>
   POSIX_Files.Unlink ("test_file");
   Fatal_Exception (E, "A158");
end p030100;

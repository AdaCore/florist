------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                         P O S I X _ R E P O R T                          --
--                                                                          --
--                                B o d y                                   --
--                                                                          --
--                                                                          --
--  Copyright (c) 1997-1998 Florida  State  University  (FSU).  All Rights  --
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

with Ada.Command_Line,
     Ada.Text_IO,
     POSIX_Configurable_System_Limits,
     POSIX_Options,
     POSIX_Process_Identification,
     POSIX_Process_Primitives,
     POSIX_Signals;

pragma Elaborate_All (POSIX_Process_Identification);
package body POSIX_Report is

   use Ada.Command_Line,
       Ada.Exceptions,
       Ada.Text_IO,
       POSIX,
       POSIX_Configurable_System_Limits,
       POSIX_Process_Identification,
       POSIX_Process_Primitives,
       POSIX_Options,
       POSIX_Signals;

   subtype Exit_Status is POSIX_Process_Primitives.Exit_Status;

   -------------------------
   --  Local Subprograms  --
   -------------------------

   function Option_Name (Option : POSIX_Option) return String;

   function Option_Name (Option : POSIX_Option) return String is
   begin
      case Option is
      when Asynchronous_IO_Option =>
         return "Asynchronous IO";
      when Change_Owner_Restriction_Option =>
         return "Change Owner Restriction";
      when Filename_Truncation_Option =>
         return "Filename Truncation";
      when File_Synchronization_Option =>
         return "File Synchronization";
      when Memory_Mapped_Files_Option =>
         return "Memory Mapped Files";
      when Memory_Locking_Option =>
         return "Memory Locking";
      when Memory_Range_Locking_Option =>
         return "Memory Range Locking";
      when Memory_Protection_Option =>
         return "Memory Protection";
      when Message_Queues_Option =>
         return "Message Queues";
      when Mutex_Priority_Ceiling_Option =>
         return "Mutex Priority Ceiling";
      when Mutex_Priority_Inheritance_Option =>
         return "Mutex Priority Inheritance";
      when Mutex_Option =>
         return "Mutexes";
      when Prioritized_IO_Option =>
         return "Prioritized IO";
      when Priority_Process_Scheduling_Option =>
         return "Priority Process Scheduling";
      when Priority_Task_Scheduling_Option =>
         return "Priority Task Scheduling";
      when Process_Shared_Option =>
         return "Process Shared";
      when Realtime_Signals_Option =>
         return "Realtime Signals";
      when Saved_IDs_Option =>
         return "Saved IDs";
      when Job_Control_Option =>
         return "Job Control";
      when Semaphores_Option =>
         return "Semaphores";
      when Shared_Memory_Objects_Option =>
         return "Shared Memory";
      when Signal_Entries_Option =>
         return "Signal Entries";
      when Synchronized_IO_Option =>
         return "Synchronized IO";
      when Timers_Option =>
         return "Timers";
      end case;
   end Option_Name;

   function int_to_uid
     (Id : Integer) return POSIX_Process_Identification.User_ID;

   function int_to_uid
     (Id : Integer) return POSIX_Process_Identification.User_ID is
   begin
      return Value (Integer'Image (Id));
   end int_to_uid;

   -----------------------
   --  Local Variables  --
   -----------------------

   Super_User_ID : User_ID := int_to_uid (0);

   procedure Header (Label : String; Root_OK : Boolean := False) is
      Saved_Verbose : Boolean := Verbose;
      Empty_String : String := "";
   begin
      Put_Line (",.,. " & Label & " " & Test_Identifier);
      if Get_Real_User_ID = Super_User_ID and then not Root_OK then
         Fail ("For safety reasons, the test program should not be " &
           "run as root");
         Done;
         Exit_Process (Exit_Status'Last);
      end if;
      Program_Name_Length := Label'Length;
      if Program_Name_Length > Program_Name'Length then
         Program_Name_Length := Program_Name'Length;
      end if;
      Program_Name (1 .. Program_Name_Length) :=
        Label (Program_Name'First ..
          Program_Name'First + Program_Name_Length - 1);
   exception when E : others => Fatal_Exception (E, "in Header");
   end Header;

   procedure Test (Label : String) is
      Empty_String : String := "";
   begin
      if not Terse then
         Put_Line ("---- *-Subtest: " & Label); Flush;
      end if;
      Test_Label_Length := Label'Length;
      if Test_Label_Length > Test_Label'Length then
         Test_Label_Length := Test_Label'Length;
      end if;
      Test_Label (1 .. Test_Label_Length) :=
        Label (Label'First .. Label'First + Test_Label_Length - 1);
   end Test;

   procedure Fail (Message : String) is
   begin
      Put_Line ("     !!TEST FAILED: " & Message);
      Flush;
      Error_Count := Error_Count + 1;
   end Fail;

   procedure Fail
     (E : Exception_Occurrence;
      Message : String) is
   begin
      Put ("     !!TEST FAILED: " & Exception_Name (E));
      if Message = "" then
         if Exception_Message (E) = "" then
            New_Line;
         else
            Put_Line (": " & Exception_Message (E));
         end if;
      elsif Exception_Message (E) = "" then
         Put_Line (": " & Message);
      else
         Put_Line (": " & Exception_Message (E) & ": " & Message);
      end if;
      Error_Count := Error_Count + 1;
   end Fail;

   procedure Assert (Condition : Boolean; Message : String) is
   begin
      if not Condition then
         if Message = "" then Fail ("assert");
         else Fail ("assert [" & Message & "]");
         end if;
      end if;
   end Assert;

   procedure Expect_Exception (Message : String) is
   begin
      Fail ("exception not raised [" & Message & "]");
   end Expect_Exception;

   procedure Unexpected_Exception
     (E : Ada.Exceptions.Exception_Occurrence;
      Message : String) is
   begin
      Fail ("exception " & Exception_Name (E) & " [" & Message & "]");
      if Exception_Message (E) /= "" and then Verbose then
         Put_Line ("       -- Exception message = "
           & Exception_Message (E));
         Flush;
      end if;
   end Unexpected_Exception;

   procedure Check_Error_Code
     (EC : POSIX.Error_Code;
      Message : String) is
      E : POSIX.Error_Code := POSIX.Get_Error_Code;
   begin
      if E /= EC then
         Fail ("incorrect error code: " & POSIX.Image (E));
         Comment ("Expected error code: " & POSIX.Image (EC));
         Flush;
      end if;
   end Check_Error_Code;

   procedure Check_Error_Code
     (EC : POSIX.Error_Code;
      E : Ada.Exceptions.Exception_Occurrence;
      Message : String) is
      EEC : POSIX.Error_Code := POSIX.Get_Error_Code;
   begin
      if Exception_Identity (E) /= POSIX_Error'Identity then
         Fail (E, Message);
      elsif EEC /= EC then
         if Message = "" then
            Fail ("incorrect error code " & POSIX.Image (EEC)
               & " [expected " & POSIX.Image (EC) & "]");
         else
            Fail ("incorrect error code " & POSIX.Image (EEC)
               & " [expected " & POSIX.Image (EC) & "]"
               & ":  " & Message);
         end if;
      elsif Exception_Message (E) /= "" and then
        Exception_Message (E) /= POSIX.Image (EC) then
         if Message = "" then
            Fail ("incorrect Exception_Message: "
               & Exception_Message (E));
         else
            Fail ("incorrect Exception_Message: "
               & Exception_Message (E)
               & ":  " & Message);
         end if;
      end if;
   end Check_Error_Code;

   procedure Check_Message
     (E  : Ada.Exceptions.Exception_Occurrence;
      Expected_Message : String;
      Message : String) is
   begin
      if Exception_Message (E) /= Expected_Message then
         Fail (Message & ": message is not " & Expected_Message);
      end if;
   end Check_Message;

   procedure Comment (Msg : String) is
   begin
      if Verbose then Put_Line ("       -- " & Msg); Flush;
      end if;
   end Comment;

   function Image (T : POSIX.Timespec) return String is
      S : Seconds;
      NS : Nanoseconds;
      Z : constant Integer := Character'Pos ('0');
   begin
      Split (T, S, NS);
      declare
         SBuff  : String (1 .. 9) := "000000000";
         I : Integer := SBuff'Last;
      begin
         while NS > 0 loop
            SBuff (I) := Character'Val (Z + Integer (NS rem 10));
            NS := NS / 10;
            I := I - 1;
         end loop;
         return Seconds'Image (S) & "." & SBuff & "s";
      end;
   end Image;

   procedure Comment (Msg : String; T : POSIX.Timespec) is
   begin
      Comment (Msg & " = " & Image (T));
   end Comment;

   function Is_Supported (Option : POSIX_Option) return Boolean is
      use POSIX_Configurable_System_Limits;
   begin
      case Option is
      when Asynchronous_IO_Option =>
         if True in Asynchronous_IO_Support then
            if False in Asynchronous_IO_Support then
               return Asynchronous_IO_Is_Supported;
            end if;
         else
            return False;
         end if;
      when Change_Owner_Restriction_Option =>
         raise Constraint_Error;
      when Filename_Truncation_Option =>
         raise Constraint_Error;
      when File_Synchronization_Option =>
         if True in File_Synchronization_Support then
            if False in File_Synchronization_Support then
               return File_Synchronization_Is_Supported;
            end if;
         else
            return False;
         end if;
      when Memory_Mapped_Files_Option =>
         if True in Memory_Mapped_Files_Support then
            if False in Memory_Mapped_Files_Support then
               return Memory_Mapped_Files_Are_Supported;
            end if;
         else
            return False;
         end if;
      when Memory_Locking_Option =>
         if True in Memory_Locking_Support then
            if False in Memory_Locking_Support then
               return Memory_Locking_Is_Supported;
            end if;
         else return False;
         end if;
      when Memory_Range_Locking_Option =>
         if True in Memory_Range_Locking_Support then
            if False in Memory_Range_Locking_Support then
               return Memory_Range_Locking_Is_Supported;
            end if;
         else return False;
         end if;
      when Memory_Protection_Option =>
         if True in Memory_Protection_Support then
            if False in Memory_Protection_Support then
               return Memory_Protection_Is_Supported;
            end if;
         else return False;
         end if;
      when Message_Queues_Option =>
         if True in Message_Queues_Support then
            if False in Message_Queues_Support then
               return Message_Queues_Are_Supported;
            end if;
         else return False;
         end if;
      when Mutex_Priority_Ceiling_Option =>
         if True in Mutex_Priority_Ceiling_Support then
            if False in Mutex_Priority_Ceiling_Support then
               return Mutex_Priority_Ceiling_Is_Supported;
            end if;
         else return False;
         end if;
      when Mutex_Priority_Inheritance_Option =>
         if True in Mutex_Priority_Inheritance_Support then
            if False in Mutex_Priority_Inheritance_Support then
               return Mutex_Priority_Inheritance_Is_Supported;
            end if;
         else return False;
         end if;
      when Mutex_Option =>
         if True in Mutexes_Support then
            if False in Mutexes_Support then
               return Mutexes_Are_Supported;
            end if;
         else return False;
         end if;
      when Prioritized_IO_Option =>
         if True in Prioritized_IO_Support then
            if False in Prioritized_IO_Support then
               return Prioritized_IO_Is_Supported;
            end if;
         else return False;
         end if;
      when Priority_Process_Scheduling_Option =>
         if True in Priority_Process_Scheduling_Support then
            if False in Priority_Process_Scheduling_Support then
               return Priority_Process_Scheduling_Is_Supported;
            end if;
         else return False;
         end if;
      when Priority_Task_Scheduling_Option =>
         if True in Priority_Task_Scheduling_Support then
            if False in Priority_Task_Scheduling_Support then
               return Priority_Task_Scheduling_Is_Supported;
            end if;
         else return False;
         end if;
      when Process_Shared_Option =>
         if True in Process_Shared_Support then
            if False in Process_Shared_Support then
               return Process_Shared_Is_Supported;
            end if;
         else return False;
         end if;
      when Realtime_Signals_Option =>
         if True in Realtime_Signals_Support then
            if False in Realtime_Signals_Support then
               return Realtime_Signals_Are_Supported;
            end if;
         else return False;
         end if;
      when Job_Control_Option =>
         if True in POSIX.Job_Control_Support then
            if False in POSIX.Job_Control_Support then
               return Job_Control_Is_Supported;
            end if;
         else return False;
         end if;
      when Saved_IDs_Option =>
         if True in POSIX.Saved_IDs_Support then
            if False in POSIX.Saved_IDs_Support then
               return Saved_IDs_Are_Supported;
            end if;
         else return False;
         end if;
      when Semaphores_Option =>
         if True in Semaphores_Support then
            if False in Semaphores_Support then
               return Semaphores_Are_Supported;
            end if;
         else return False;
         end if;
      when Shared_Memory_Objects_Option =>
         if True in Shared_Memory_Objects_Support then
            if False in Shared_Memory_Objects_Support then
               return Shared_Memory_Objects_Are_Supported;
            end if;
         else return False;
         end if;
      when Signal_Entries_Option =>
         if True in Signal_Entries_Support then
            if False in Signal_Entries_Support then
               return True;
            end if;
         else return False;
         end if;
      when Synchronized_IO_Option =>
         if True in Synchronized_IO_Support then
            if False in Synchronized_IO_Support then
               return Synchronized_IO_Is_Supported;
            end if;
         else return False;
         end if;
      when Timers_Option =>
         if True in Timers_Support then
            if False in Timers_Support then
               return Timers_Are_Supported;
            end if;
         else return False;
         end if;
      end case;
      return True;
   end Is_Supported;

   procedure Optional
     (Option : POSIX_Option;
      Message : String) is
   begin
      if not Is_Supported (Option) then
         if Message /= "" then
            Comment (POSIX_Option'Image (Option) & " required: "
              & Message);
         else
            Comment (POSIX_Option'Image (Option) & " required");
         end if;
         Nonsupport (Option) := True;
         for I in Nonsupport'Range loop
            if Nonsupport (I) then
               Put ("**** Nonsupport of "&POSIX_Option'Image (I)&" detected.");
               New_Line;
            end if;
         end loop;
         Put_Line ("==== Test Not Applicable.");
         Exit_Process (Normal_Exit);
      end if;
   end Optional;

   procedure Optional
     (Option   : POSIX_Option;
      Expected : POSIX.Error_Code;
      E        : Ada.Exceptions.Exception_Occurrence;
      Message  : String) is
   begin
      if Is_Supported (Option) or else POSIX.Get_Error_Code /= Expected then
         Fail (E, Message);
      elsif Message = "" then
         Comment (POSIX_Option'Image (Option) & " not supported");
         Nonsupport (Option) := True;
      else
         Comment (POSIX_Option'Image (Option) & " not supported [" &
           Message & "]");
         Nonsupport (Option) := True;
      end if;
   exception
   when E1 : others =>
      Fail (E1, "checking for support of " & POSIX_Option'Image (Option));
   end Optional;

   procedure Optional
     (Option_1 : POSIX_Option;
      Option_2 : POSIX_Option;
      Expected : POSIX.Error_Code;
      E        : Ada.Exceptions.Exception_Occurrence;
      Message  : String) is
   begin
      if (Is_Supported (Option_1) and then
         Is_Supported (Option_2)) or else
         POSIX.Get_Error_Code /= Expected then Fail (E, Message);
      else
         if not Is_Supported (Option_1) then
            if Message = "" then
               Comment (POSIX_Option'Image (Option_1)
                 & " not supported");
               Nonsupport (Option_1) := True;
            else
               Comment (POSIX_Option'Image (Option_1)
                 & " not supported [" & Message & "]");
               Nonsupport (Option_1) := True;
            end if;
         end if;
         if not Is_Supported (Option_2) then
            if Message = "" then
               Comment (POSIX_Option'Image (Option_2)
                 & " not supported");
               Nonsupport (Option_2) := True;
            else
               Comment (POSIX_Option'Image (Option_2)
                 & " not supported [" & Message & "]");
               Nonsupport (Option_2) := True;
            end if;
         end if;
      end if;
   exception
   when E1 : others =>
      Fail (E1, "checking for support of options");
   end Optional;

   procedure Optional
     (Option      : POSIX_Option;
      Expected_If_Not_Supported : POSIX.Error_Code;
      Expected_If_Supported : POSIX.Error_Code;
      E           : Ada.Exceptions.Exception_Occurrence;
      Message     : String) is
   begin
      if Is_Supported (Option) then
         if POSIX.Get_Error_Code /= Expected_If_Supported then
            Fail (E, Message);
         end if;
      elsif POSIX.Get_Error_Code /= Expected_If_Not_Supported then
         Fail (E, Message);
      else
         if Message = "" then
            Comment (POSIX_Option'Image (Option)
              & " not supported");
            Nonsupport (Option) := True;
         else
            Comment (POSIX_Option'Image (Option)
              & " not supported [" & Message & "]");
            Nonsupport (Option) := True;
         end if;
      end if;
   exception
   when E1 : others =>
      Fail (E1, "checking for support of options");
   end Optional;

   function Uid_To_Integer
     (Uid : POSIX_Process_Identification.User_ID) return Integer;
   --  .... not portable; needs configurable mechanism
   function Uid_To_Integer
     (Uid : POSIX_Process_Identification.User_ID) return Integer is
   begin
      return Integer'Value (POSIX_Process_Identification.Image (Uid));
   end Uid_To_Integer;

   procedure Privileged
     (Privilege : POSIX_Privilege;
      Option    : POSIX_Option;
      Expected  : POSIX.Error_Code;
      E         : Ada.Exceptions.Exception_Occurrence;
      Message   : String) is
      Error : constant POSIX.Error_Code := POSIX.Get_Error_Code;
   begin
      if Error = Operation_Not_Permitted then
         Fail (E, Message & " - insufficient privilege");
         Privilege_Failure := True;
         return;
      end if;
      if Is_Supported (Option) or else Error /= Expected then
         Fail (E, Message);
      elsif Message = "" then
         Comment (POSIX_Option'Image (Option) & " not supported");
         Nonsupport (Option) := True;
      else
         Comment (POSIX_Option'Image (Option) & " not supported [" &
           Message & "]");
         Nonsupport (Option) := True;
      end if;
   exception
   when E1 : others =>
      Fail (E1, "checking for support of " & POSIX_Option'Image (Option));
   end Privileged;

   procedure Privileged
     (Privilege : POSIX_Privilege;
      Message : String) is
      Error : constant POSIX.Error_Code := POSIX.Get_Error_Code;
   begin
      if Error = Operation_Not_Permitted then
         --  .... This is temporary.
         --  For the longer term, there should be a locally configurable
         --  mechanism for determining whether we have appropriate
         --  privilege for various operations.  For now, we assume that
         --  appropriate privilege is equivalent to having root user-id.
         if (Uid_To_Integer
           (POSIX_Process_Identification.Get_Effective_User_ID) = 0) then
            Fail ("should have appropriate privilege");
         end if;
      end if;
   end Privileged;

   procedure Privileged
     (Privilege : POSIX_Privilege;
      Option    : POSIX_Option;
      Expected_If_Not_Supported : POSIX.Error_Code;
      Expected_If_Supported : POSIX.Error_Code;
      E         : Ada.Exceptions.Exception_Occurrence;
      Message   : String) is
      Error : constant POSIX.Error_Code := POSIX.Get_Error_Code;
   begin
      if Error = Operation_Not_Permitted then
         Fail (E, Message & " - insufficient privilege");
         Privilege_Failure := True;
         return;
      end if;
      Optional (Option, Expected_If_Not_Supported,
        Expected_If_Supported, E, Message);
   end Privileged;

   procedure Increment_Error_Count (Number : Integer) is
   begin
      Error_Count := Error_Count + 1;
   end Increment_Error_Count;

   function Get_Error_Count return Integer is
   begin
      return Error_Count;
   end Get_Error_Count;

   procedure Done is
   begin
      if Error_Count = 0 and not Privilege_Failure then
         if Child /= 0 then
            Comment ("child process completed successfully");
         else
            Put_Line ("==== Test Completed Successfully.");
         end if;
      else
         if Privilege_Failure then
            if Child /= 0 then
               Put_Line ("**** Child failed due to insufficient privilege");
            else
               Put_Line
                 ("**** Failed some parts due to insufficient privilege");
            end if;
         end if;
         if Error_Count > 0 and Child = 0 then
            Put ("==== Failed"); Put (Natural'Image (Error_Count));
            Put (" test");
            if Error_Count /= 1 then Put ("s."); end if;
            New_Line;
         end if;
      end if;
      for I in Nonsupport'Range loop
         if Nonsupport (I) then
            Put ("**** Nonsupport of "&POSIX_Option'Image (I)&" detected.");
            New_Line;
         end if;
      end loop;
      Flush;
      if Child /= 0 then
         --  Report number of errors back to parent process.
         if Error_Count >=
            Natural (Failed_Creation_Exit) then
            Put ("==== Child error count overflowed");
            Error_Count := Natural (Failed_Creation_Exit) - 1;
         end if;
         Exit_Process (Exit_Status (Error_Count));
      end if;
   end Done;

   procedure Fatal (Msg : String) is
   begin
      Fail ("fatal error: [" & Msg & "]");
      Done;
      Exit_Process (Normal_Exit);
   end Fatal;

   procedure Fatal_Exception
     (E : Ada.Exceptions.Exception_Occurrence;
      Message : String) is
   begin
      if Message /= "" then
         Fail ("[" & Message & "] fatal exception " & Exception_Name (E));
      else Fail ("fatal exception " & Exception_Name (E));
      end if;
      Done;
      Exit_Process (Normal_Exit);
   end Fatal_Exception;

   procedure Pass_Through_Verbosity
     (Args : in out POSIX.POSIX_String_List) is
   begin
      if Verbose then Append (Args, "-v");
      elsif Terse then Append (Args, "-t");
      end if;
   end Pass_Through_Verbosity;

   procedure Check_Child_Status
     (Status   : Termination_Status;
      Child_ID : Process_ID;
      Expected : Exit_Status;
      Message  : String) is
      E : Exit_Status;
   begin
      Assert (Child_ID /= Null_Process_ID,
        Message & ": null child id");
      if not Status_Available (Status) then
         --  Fail when status not available
         Fail (Message & ": no status available");
         return;
      end if;
      Assert (Process_ID_Of (Status) = Child_ID,
        Message & ": wrong child");
      if Termination_Cause_Of (Status) /= Exited then
         --  Fail when did not exit
         Assert (False, Message & ": did not exit");
         return;
      end if;
      E := Exit_Status_Of (Status);
      if E > 0 and E < Failed_Creation_Exit then
         --  child process reports errors via exit status
         Increment_Error_Count (Integer (E));
      elsif E /= Expected then
         Assert (False, Message & ": exit status ="
           & Exit_Status'Image (E));
      end if;
      declare
         Sig : Signal;
      begin
         Sig := Stopping_Signal_Of (Status);
         --  Stopping_Signal_Of invalid status
         Expect_Exception (Message);
      exception
      when POSIX_Error => Check_Error_Code (Invalid_Argument, Message);
      when E : others => Fail (E, Message);
      end;
      declare
         Sig : Signal;
      begin
         Sig := Termination_Signal_Of (Status);
         --  Termination_Signal_Of invalid status
         Expect_Exception (Message);
      exception
      when POSIX_Error => Check_Error_Code (Invalid_Argument, Message);
      when E : others => Fail (E, Message);
      end;
   exception when E : others => Unexpected_Exception (E, Message);
   end Check_Child_Status;

begin
   for I in 1 .. Argument_Count loop
      if Argument (I) = "-v" then Verbose := True;
      elsif Argument (I) = "-t" then Terse := True;
      elsif Argument (I)'Length >= 6 and then
        Argument (I)(Argument (I)'First .. Argument (I)'First + 5)
          = "-child" then
         --  Treat this argument as value of Child.
         --  Default value is 1.
         declare
            Arg : constant String := Argument (I);
            J : Integer := Arg'First + 6;
            Tmp : Integer := 0;
         begin
            while J <= Arg'Last and then Arg (J) = ' ' loop
               J := J + 1;
            end loop;
            while J <= Arg'Last and then Arg (J) in '0' .. '9' loop
               Tmp := Tmp * 10 +
                 Character'Pos (Arg (J)) - Character'Pos ('0');
               J := J + 1;
            end loop;
            while J <= Arg'Last and then Arg (J) = ' ' loop
               J := J + 1;
            end loop;
            if J /= Arg'Last + 1 or Tmp = 0 then Child := 1;
            else Child := Tmp;
            end if;
         exception when others =>
            Fail ("bad command-line argument");
         end;
      end if;
   end loop;
end POSIX_Report;

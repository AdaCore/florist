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

with Ada.Exceptions,
     POSIX,
     POSIX_Process_Identification,
     POSIX_Process_Primitives;
package POSIX_Report is

   --   This package contains utility routines which are useful for
   --   running tests.

   Verbose : Boolean := False;
   --  set to True if "-v" is on command line.

   Terse : Boolean := False;
   --  set to True if "-t" is on command line.

   Child : Natural := 0;
   --  set to 1 if "-child" is on command line
   --  set to NNN if "-child NNN" is on command line, where NNN is a number

   Test_Identifier : String := "POSIX Ada Validation Tests, Version 1.2a";

   --  Call this once for each test program, at the beginning.
   --  It prints a message indicating that a test is about to be
   --  performed, regardless of whether verbose is turned on.
   --  User Root_OK = True if the test is one that makes sense to
   --  run as root.
   procedure Header (Label : String; Root_OK : Boolean := False);

   --  Call this once for each section of the test program.
   --  It prints a message indicating that a test is about to be
   --  performed, if verbose is turned on.
   --  The label should be unique so that if the test fails it is
   --  possible to find the code for the test by grepping through
   --  the source.
   procedure Test (Label : String);

   --  ....
   --  Fail is obsolescent. Use the more specific procedures.

   --  Call this to record the fact that a test failed.
   procedure Fail (Message : String);

   --  Call this from an exception handler for "others", to catch
   --  completion of test casem by an unhandled exception.
   procedure Fail
     (E : Ada.Exceptions.Exception_Occurrence;
      Message : String);

   --  Call this to check a condition that should be true.
   --  The test fails if the argument is false.

   procedure Assert (Condition : Boolean; Message : String);

   type POSIX_Option is
     (Asynchronous_IO_Option,
      Change_Owner_Restriction_Option,
      Filename_Truncation_Option,
      File_Synchronization_Option,
      Memory_Mapped_Files_Option,
      Memory_Locking_Option,
      Memory_Range_Locking_Option,
      Memory_Protection_Option,
      Message_Queues_Option,
      Mutex_Priority_Ceiling_Option,
      Mutex_Priority_Inheritance_Option,
      Mutex_Option,
      Prioritized_IO_Option,
      Priority_Process_Scheduling_Option,
      Priority_Task_Scheduling_Option,
      Process_Shared_Option,
      Realtime_Signals_Option,
      Job_Control_Option,
      Saved_IDs_Option,
      Semaphores_Option,
      Shared_Memory_Objects_Option,
      Signal_Entries_Option,
      Synchronized_IO_Option,
      Timers_Option);

   function Is_Supported (Option : POSIX_Option) return Boolean;

   --  Call this near the beginning of a test program
   --  that entirely depends on an option.  It will end the test
   --  if the required option is not supported.
   procedure Optional
     (Option : POSIX_Option;
      Message : String);

   --  Call this inside an exception handler for an
   --  exception that may be raised in response to an
   --  attempt to use an optional feature that is not
   --  supported, where the feature depends on one option.
   procedure Optional
     (Option : POSIX_Option;
      Expected : POSIX.Error_Code;
      E : Ada.Exceptions.Exception_Occurrence;
      Message : String);

   --  Call this inside an exception handler for an
   --  exception that may be raised in response to an
   --  attempt to use an optional feature that is not
   --  supported, where the feature depends on two options.
   procedure Optional
     (Option_1 : POSIX_Option;
      Option_2 : POSIX_Option;
      Expected : POSIX.Error_Code;
      E        : Ada.Exceptions.Exception_Occurrence;
      Message  : String);

   --  Call this inside an exception handler for an
   --  exception that may be raised in response to an
   --  attempt to use an optional feature that is not
   --  supported, where POSIX_Error should be raised
   --  with a different error code if the option is supported.
   procedure Optional
     (Option      : POSIX_Option;
      Expected_If_Not_Supported : POSIX.Error_Code;
      Expected_If_Supported : POSIX.Error_Code;
      E           : Ada.Exceptions.Exception_Occurrence;
      Message     : String);

   type POSIX_Privilege is
     (Memory_Locking_Privilege,
      Semaphore_Initialization_Privilege,
      Set_Time_Privilege);
   --  add more of these values as we discover more privileges

   --  Call this inside an exception handler for an
   --  exception that may be raised in response to an
   --  attempt to use an optional feature that is not
   --  supported, where the feature depends on one option
   --  and also depends on having appropriate privilege.
   procedure Privileged
     (Privilege : POSIX_Privilege;
      Option    : POSIX_Option;
      Expected  : POSIX.Error_Code;
      E         : Ada.Exceptions.Exception_Occurrence;
      Message   : String);

   procedure Privileged
     (Privilege : POSIX_Privilege;
      Message   : String);

   --  Call this if the test is supposed to raise an exception,
   --  other than for nonsupport or lack of privilege.
   procedure Privileged
     (Privilege : POSIX_Privilege;
      Option    : POSIX_Option;
      Expected_If_Not_Supported : POSIX.Error_Code;
      Expected_If_Supported : POSIX.Error_Code;
      E         : Ada.Exceptions.Exception_Occurrence;
      Message   : String);

   --  Call this at a point where control should not reach
   --  because an exception should have been raised.
   procedure Expect_Exception (Message : String);

   --  Call this inside an exception handler for an unexpected
   --  exception.
   procedure Unexpected_Exception
     (E : Ada.Exceptions.Exception_Occurrence;
      Message : String);

   --  Call this inside an exception handler for POSIX_Error,
   --  to validate that the expected error code is set.
   procedure Check_Error_Code
     (EC : POSIX.Error_Code;
      Message : String);

   --  Call this inside an exception handler for POSIX_Error,
   --  to validate that the expected error code is set.
   procedure Check_Error_Code
     (EC : POSIX.Error_Code;
      E  : Ada.Exceptions.Exception_Occurrence;
      Message : String);

   --  Call this inside an exception handler for any exception
   --  to check that the exception message is correct.
   procedure Check_Message
     (E  : Ada.Exceptions.Exception_Occurrence;
      Expected_Message : String;
      Message : String);

   --  Call this from an exception handler for "others", to catch
   --  completion of test program by an unhandled exception.
   procedure Fatal_Exception
    (E : Ada.Exceptions.Exception_Occurrence;
     Message : String);

   --  Call this to print out a an informational message,
   --  iff Verbose = True.
   --  It does not imply that anything went wrong.
   procedure Comment (Msg : String);

   --  Call this to get a printable string for a Timepsec value.
   function Image (T : POSIX.Timespec) return String;

   --  This is equivalent to Comment (Msg & " = " & Image (T));
   procedure Comment (Msg : String; T : POSIX.Timespec);

   --  Call this to  add to this package's internal error count.
   procedure Increment_Error_Count (Number : Integer);

   --  Call this to get this package's internal error count.
   function Get_Error_Count return Integer;

   --  Call this once, before exiting, when the testing is complete,
   --  for normal completion of a main program.
   procedure Done;

   --  Call this to terminate a test immediately.
   procedure Fatal (Msg : String);

   --  Add to the given argument list the necessary values to pass
   --  through the verbose/normal/terse state of the parent process.
   procedure Pass_Through_Verbosity (Args : in out POSIX.POSIX_String_List);

   --  Check Termination_Status value of child process, to verify
   --  that the child exited and exited with the anticipated status.
   --  Add the error count of the child process to that of the parent.
   procedure Check_Child_Status
     (Status   : POSIX_Process_Primitives.Termination_Status;
      Child_ID : POSIX_Process_Identification.Process_ID;
      Expected : POSIX_Process_Primitives.Exit_Status;
      Message  : String);

private

   --  name of the executable file for this program
   Program_Name : String (1 .. 128) := (others => ' ');
   Program_Name_Length : Integer := 0;

   --  label from the last Test subprogram call
   Test_Label : String (1 .. 128) := (others => ' ');
   Test_Label_Length : Integer := 0;

   --  an option has been found to be unsupported
   Nonsupport : array (POSIX_Option) of Boolean := (others => False);
      pragma Atomic_Components (Nonsupport);

   --  an operation has failed due to insufficient privilege
   Privilege_Failure : Boolean := False;
      pragma Atomic (Privilege_Failure);

   --  number of errors so far
   Error_Count : Natural := 0;
      pragma Atomic (Error_Count);

end POSIX_Report;

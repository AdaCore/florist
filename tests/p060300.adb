------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                             P 0 6 0 3 0 0                                --
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

--  Test for POSIX_Asynchronous_IO package

--  ... this is a superficial test
--  It does not test much more than whether the interfaces can
--  be called. A more complete test is desirable.

with Ada.Calendar,
     Ada.Streams,
     POSIX,
     POSIX_Asynchronous_IO,
     POSIX_Configurable_File_Limits,
     POSIX_IO,
     POSIX_Permissions,
     POSIX_Report,
     POSIX_Signals;
procedure p060300 is
   use Ada.Streams,
       POSIX,
       POSIX_Asynchronous_IO,
       POSIX_IO,
       POSIX_Permissions,
       POSIX_Report,
       POSIX_Signals;

   FD : File_Descriptor;
   Buf : IO_Array_Pointer := new Stream_Element_Array (1 .. 10);
   Status : AIO_Status;   --  set by Check_Status
   Valid_AIO_Filename : constant POSIX_String := "test_io_filename";

   procedure Setup (AD : in out AIO_Descriptor);
   procedure Cleanup (AD : in out AIO_Descriptor);
   procedure Check_Status (AD : AIO_Descriptor);

   procedure Setup (AD : in out AIO_Descriptor) is
      Event : Signal_Event;
   begin
      AD := Create_AIO_Control_Block;
      Set_Buffer (AD, Buf);
      Buf.all := To_Stream_Element_Array ("hello.....");
      Set_Length (AD, 6);
      Set_Notification (Event, No_Notification);
      Set_Event (AD, Event);
   exception when E : others => Fatal_Exception (E, "A001");
   end Setup;

   procedure Cleanup (AD : in out AIO_Descriptor) is
      use Ada.Calendar;
      Start_Time : constant Ada.Calendar.Time := Clock;
   begin
      while Get_AIO_Status (AD) = In_Progress loop
         if Clock - Start_Time > 1.0 then
            Fail ("A002: IO operation apparently hung");
            exit;
         end if;
      end loop;
      Destroy_AIO_Control_Block (AD);
      Close (FD);
   exception when E : others => Fatal_Exception (E, "A003");
   end Cleanup;

   procedure Check_Status (AD : AIO_Descriptor) is
      Err1, Err2 : Error_Code := No_Error;
   begin

      ---------------------------------------------------
      --  Get_AIO_Status( ) shall raise POSIX_Error, with
      --  the Status Code of the request as error code, if
      --  the request has failed and Get_Byte_Transferred
      --  has not yet been called for AD. If the request
      --  has not failed and Get_Bytes_Transferred has not
      --  yet been called for AD, the function shall return
      --  the value of type AIO_Status value corresponding
      --  to the Status Code, according to the following table:

      --  Status Code Value          AIO_Status Value

      --  Operation_In_Progress      In_Process
      --  No_Error                   Completed_Successfully
      --  Operation_Canceled         Canceled


      --  During the lifetime of an asychronous I/O request,
      --  if the operation has not failed, the Status Code
      --  shall be as In_Progress, Completed_Successfully,
      --  or Cancelled.

      --  The AIO descriptor originally specified in the call
      --  that requests initiation of an asynchronous I/O
      --  operation can be used, thereafter, as a handle
      --  for retrieving the status of the I/O request.
      --  It shall remain valid for this purpose during its
      --  lifetime. If the operation has not failed, the Status
      --  code shall be In_Progress, Completed_Successfully,
      --  or Cancelled.

      ---------------------------------------------------
      --  Get_AIO_Error_Code shall
      --  return the Status Code of asychronous I/O request
      --  specified by AD, if Get_Bytes_Transferred has not
      --  yet been called for AD.

      Err1 := Get_AIO_Error_Code (AD);
      begin

         Status := Get_AIO_Status (AD);
         if Status = In_Progress then

            Assert (Err1 = Operation_In_Progress, "A004");
            Comment ("delaying to await I/O completion");
            delay 1.0;
            Status := Get_AIO_Status (AD);

            Assert (Status /= In_Progress, "A005");

         elsif Is_Supported (Asynchronous_IO_Option) then
            Assert (Status = Completed_Successfully, "A006");
         end if;
         Comment ("Status=" & AIO_Status'Image (Status));
      exception
      when E1 : POSIX_Error =>
         Err2 := Get_Error_Code;
         Assert (Err2 /= No_Error, "A007");
         Unexpected_Exception (E1, "A008");
      when E2 : others => Unexpected_Exception (E2, "A009");
      end;
      if Err2 /= No_Error then
         Assert (Err1 = Err2, "A010");
      else
         if Err1 = Operation_Canceled then
            Assert (Status = Canceled, "A011");
         else
            Assert (Err1 = No_Error or Err1 = Operation_In_Progress, "A012");
         end if;
      end if;
   exception when E : others => Fatal_Exception (E, "A013");
   end Check_Status;

   procedure Check_Cancelation (AD : in out AIO_Descriptor);

   procedure Check_Cancelation (AD : in out AIO_Descriptor) is
      Ret : Cancelation_Status;
   begin
      Ret := Cancel (AD);
      Comment ("Cancelation_Status=" & Cancelation_Status'Image (Ret));
   exception when E : others => Fatal_Exception (E, "A014");
   end Check_Cancelation;

begin

   Header ("p060300");

   declare
      AD : AIO_Descriptor;
   begin
      Test ("Create/Destroy_AIO_Control_Block [6.3.1]");
      Setup (AD);
      FD := Open_Or_Create
        (Valid_AIO_Filename, Read_Write, Owner_Permission_Set);
      AD := Create_AIO_Control_Block;

      -------------------------------------------------------------------
      --  Create_AIO_Control_Block( ) shall allocate a control block,
      --  and return a value of type AIO_Descriptor that refers to it.
      --  Since Create_AIO_Control_Block is a function, the
      --  implementation shall be such that the value returned can be
      --  assigned to a variable of type AIO_Descriptor.
      --  Destroy_AIO_Control_Block( ) shall deallocate the control block
      --  to which the AD parameter refers, and destroy the reference,
      --  unless the object corresponds to an asynchronous I/O request
      --  that is still being processed.

      Assert (True, "A015");
      Destroy_AIO_Control_Block (AD);
   exception
   when E1 : POSIX_Error =>
      Optional (Asynchronous_IO_Option,
        Operation_Not_Implemented, E1, "A016: Synchronize_Data");
   when E2 : others => Unexpected_Exception (E2, "A017: Synchronize_Data");
   end;

   ----------------------------------------------------------------------

   declare
      AD : AIO_Descriptor;
      FD : File_Descriptor;
      FD2 : File_Descriptor;
   begin
      Test ("Set/Get_File [6.3.2]");
      Setup (AD);
      FD := Open_Or_Create
        (Valid_AIO_Filename, Read_Write, Owner_Permission_Set);
      Set_File (AD, FD);
      FD2 := Get_File (AD);

      ------------------------------------------------------------------
      --  Set_File( ) shall set the file attribute of the object specified
      --  by the first argument to the value specified by the second
      --  argument.
      --  Get_File( ) shall return the value of the file attribute of the
      --  argument.
      --  The file descriptor on which the asynchronous I/O operation
      --  is to be performed.

      Assert (FD2 = FD, "A018");
      Check_Status (AD);
   exception
   when E1 : POSIX_Error =>
         Optional (Asynchronous_IO_Option,
            Operation_Not_Implemented, E1, "A019: Synchronize_Data");
   when E2 : others => Unexpected_Exception (E2, "A020: Synchronize_Data");
   end;

   -----------------------------------------------------------------------
   --  Stream_Element'Size should be equal to POSIX_Character'Size.

   Assert (Stream_Element'Size = POSIX_Character'Size, "A021");

   ----------------------------------------------------------
   --  The Read operation allows the calling process to read
   --  the number of bytes specified by the Length attribute
   --  Of AD from the ifle specified by the File attribute,
   --  into the buffer designated by the Buffer attribute.

   ------------------------------------------------------------
   --  The Write operation allows the calling process to write Length
   --  bytes to the file associated with File from the buffer
   --  pointed to Buffer (see POSIX_IO.Write), where Length,
   --  File and Buffer are attributes of AD.

   declare
      AD : AIO_Descriptor;
   begin
      Test ("Read and Write [6.3.3] & [6.3.4]");
      Setup (AD);
      FD := Open_Or_Create
        (Valid_AIO_Filename, Read_Write, Owner_Permission_Set);
      Set_File (AD, FD);
      Comment ("Write");
      Write (AD);    -- write to file from buf
      Check_Status (AD);
      Comment ("Read");
      Read (AD);     -- read to buffer from file
      Check_Status (AD);

      Assert (Get_Buffer (AD) = Buf, "A022");
      Cleanup (AD);
   exception
   when E1 : POSIX_Error =>
      Optional (Asynchronous_IO_Option,
        Operation_Not_Implemented, E1, "A023");
   when E2 : others => Unexpected_Exception (E2, "A024");
   end;

   -----------------------------------------------------------------------

   declare
      AD : AIO_Descriptor;
      List : AIO_Descriptor_List (1 .. 1);
      Event : Signal_Event;
   begin
      Test ("List_IO_No_Wait [6.3.5]");
      Setup (AD);
      Set_Notification (Event, No_Notification);
      Set_Event (AD, Event);
      FD := Open_Or_Create
        (Valid_AIO_Filename, Read_Write, Owner_Permission_Set);
      Set_File (AD, FD);
      Set_Operation (AD, Write);
      List (1) := AD;
      List_IO_No_Wait (List, Event);
      Check_Status (List (1));
      Cleanup (AD);
   exception
   when E1 : POSIX_Error =>
      Optional (Asynchronous_IO_Option,
        Operation_Not_Implemented, E1, "A025");
   when E2 : others => Unexpected_Exception (E2, "A026");
   end;

   -----------------------------------------------------------------------

   declare
      AD : AIO_Descriptor;
      List : AIO_Descriptor_List (1 .. 1);
   begin
      Test ("List_IO_Wait [6.3.5]");
      Setup (AD);
      FD := Open_Or_Create
        (Valid_AIO_Filename, Read_Write, Owner_Permission_Set);
      Set_File (AD, FD);
      Set_Operation (AD, Write);
      List (1) := AD;
      List_IO_Wait (List);
      Check_Status (List (1));
      Cleanup (AD);
   exception
   when E1 : POSIX_Error =>
      Optional (Asynchronous_IO_Option,
        Operation_Not_Implemented, E1, "A027");
   when E2 : others => Unexpected_Exception (E2, "A028");
   end;

   -----------------------------------------------------------------------

   declare
      AD : AIO_Descriptor;
      Err1 : Error_Code := No_Error;
   begin
      Test ("Get_AIO_Status (1) [6.3.6]");
      Setup (AD);
      FD := Open_Or_Create
        (Valid_AIO_Filename, Read_Write, Owner_Permission_Set);
      Set_File (AD, FD);
      Set_Operation (AD, Write);
      Write (AD);
      Err1 := Get_AIO_Error_Code (AD);
      begin
         Status := Get_AIO_Status (AD);
         if Status = In_Progress then
            Assert (Err1 = Operation_In_Progress, "A029");
            Comment ("delaying to await I/O completion");
            delay 1.0;
            Status := Get_AIO_Status (AD);
            Assert (Status /= In_Progress, "A030");
         else Assert (Status = Completed_Successfully, "A031");
         end if;
         Comment ("Status=" & AIO_Status'Image (Status));
      end;
      Cleanup (AD);
   exception
   when E1 : POSIX_Error =>
      Optional (Asynchronous_IO_Option,
        Operation_Not_Implemented, E1, "A032");
   when E2 : others => Unexpected_Exception (E2, "A033");
   end;

   -----------------------------------------------------------------------

   declare
      AD : AIO_Descriptor;
      List : AIO_Descriptor_List (1 .. 1);
   begin
      Test ("Get_AIO_Status (2) [6.3.6]");
      Setup (AD);
      FD := Open_Or_Create
        (Valid_AIO_Filename, Read_Write, Owner_Permission_Set);
      Set_File (AD, FD);
      Set_Operation (AD, Write);
      List (1) := AD;
      Write (AD);
      Check_Status (AD);
      Cleanup (AD);
   exception
   when E1 : POSIX_Error =>
      Optional (Asynchronous_IO_Option,
        Operation_Not_Implemented, E1, "A034");
   when E2 : others => Unexpected_Exception (E2, "A035");
   end;

   -----------------------------------------------------------------------

   declare
      AD : AIO_Descriptor;
   begin
      Test ("Get_Bytes_Transferred [6.3.7]");
      Setup (AD);
      FD := Open_Or_Create
        (Valid_AIO_Filename, Read_Write, Owner_Permission_Set);
      Set_File (AD, FD);
      Write (AD);
      Check_Status (AD);
      Cleanup (AD);
   exception
   when E1 : POSIX_Error =>
      Optional (Asynchronous_IO_Option,
        Operation_Not_Implemented, E1, "A036");
   when E2 : others => Unexpected_Exception (E2, "A037");
   end;

   -----------------------------------------------------------------------

   declare
      AD : AIO_Descriptor;
   begin
      Test ("Cancel (1) [6.3.8]");
      Setup (AD);
      FD := Open_Or_Create
        (Valid_AIO_Filename, Read_Write, Owner_Permission_Set);
      Set_File (AD, FD);
      Write (AD);
      Check_Cancelation (AD);
      Cleanup (AD);
   exception
   when E1 : POSIX_Error =>
      Optional (Asynchronous_IO_Option,
        Operation_Not_Implemented, E1, "A038");
   when E2 : others => Unexpected_Exception (E2, "A039");
   end;

   -----------------------------------------------------------------------

   declare
      AD : AIO_Descriptor;
      FD : File_Descriptor;
      Ret : Cancelation_Status;
   begin
      Test ("Cancel (2) [6.3.8]");
      Setup (AD);
      FD := Open_Or_Create
        (Valid_AIO_Filename, Read_Write, Owner_Permission_Set);
      Set_File (AD, FD);
      Write (AD);
      begin
         Ret := Cancel (FD);
         Comment ("Cancelation_Status=" & Cancelation_Status'Image (Ret));
      end;
      Cleanup (AD);
   exception
   when E1 : POSIX_Error =>
      Optional (Asynchronous_IO_Option,
        Operation_Not_Implemented, E1, "A040: Synchronize_Data");
   when E2 : others => Unexpected_Exception (E2, "A041: Synchronize_Data");
   end;

   -----------------------------------------------------------------------

   declare
      AD : AIO_Descriptor;
   begin
      Test ("Await_IO (1) [6.3.9]");
      Setup (AD);
      FD := Open_Or_Create
        (Valid_AIO_Filename, Read_Write, Owner_Permission_Set);
      Set_File (AD, FD);
      Write (AD);
      Await_IO (AD);
      Check_Status (AD);
      Cleanup (AD);
   exception
   when E1 : POSIX_Error =>
      Optional (Asynchronous_IO_Option,
        Operation_Not_Implemented, E1, "A042");
   when E2 : others => Unexpected_Exception (E2, "A043");
   end;

   -----------------------------------------------------------------------

   declare
      AD : AIO_Descriptor;
      List : AIO_Descriptor_List (1 .. 1);
   begin
      Test ("Await_IO (2) [6.3.9]");
      Setup (AD);
      FD := Open_Or_Create
        (Valid_AIO_Filename, Read_Write, Owner_Permission_Set);
      Set_File (AD, FD);
      Set_Operation (AD, Write);
      List (1) := AD;
      List_IO_Wait (List);
      Await_IO (List);
      Check_Status (AD);
      Cleanup (AD);
   exception
   when E1 : POSIX_Error =>
      Optional (Asynchronous_IO_Option,
        Operation_Not_Implemented, E1, "A044");
   when E2 : others => Unexpected_Exception (E2, "A045");
   end;

   -----------------------------------------------------------------------

   declare
      AD : AIO_Descriptor;
      Timeout : Timespec;
   begin
      Test ("Await_IO_Or_Timeout (1) [6.3.9]");
      Setup (AD);
      FD := Open_Or_Create
        (Valid_AIO_Filename, Read_Write, Owner_Permission_Set);
      Set_File (AD, FD);
      Write (AD);
      Set_Seconds (Timeout, Seconds (1));
      Set_Nanoseconds (Timeout, Nanoseconds (1));
      Await_IO_Or_Timeout (AD, Timeout);
      --  If time expires, should have raised POSIX_Error
      Check_Status (AD);
      Cleanup (AD);
   exception
   when E1 : POSIX_Error =>
      Optional (Asynchronous_IO_Option,
        Operation_Not_Implemented, E1, "A046");
   when E2 : others => Unexpected_Exception (E2, "A047");
   end;

   -----------------------------------------------------------------------

   declare
      AD : AIO_Descriptor;
      List : AIO_Descriptor_List (1 .. 1);
      Timeout : Timespec;
   begin
      Test ("Await_IO_Or_Timeout (2) [6.3.9]");
      Setup (AD);
      FD := Open_Or_Create
        (Valid_AIO_Filename, Read_Write, Owner_Permission_Set);
      Set_File (AD, FD);
      Set_Operation (AD, Write);
      List (1) := AD;
      List_IO_Wait (List);
      Set_Seconds (Timeout, Seconds (1));
      Set_Nanoseconds (Timeout, Nanoseconds (1));
      Await_IO_Or_Timeout (List, Timeout);
      Check_Status (AD);
      Cleanup (AD);
   exception
   when E1 : POSIX_Error =>
      Optional (Asynchronous_IO_Option,
        Operation_Not_Implemented, E1, "A048");
   when E2 : others => Unexpected_Exception (E2, "A049");
   end;

   -----------------------------------------------------------------------
   --  Synchronize_File( ) asynchronously forces all I/O requests
   --  associated with the file specified by the AD parameter
   --  and queued at the time of the call to the synchronized
   --  completion state.  The procedure call to Synchronize_File
   --  shall return when the synchronization request has been
   --  initiated or queued to the file or device (even when the
   --  data cannot be synchronized immediately).
   --  If Synchronize_File or Synchronize_Data succeeds, then
   --  it is only the I/O that was queued at the time of
   --  the call to Synchronize_File that is guaranteed to be
   --  forced to the relevant completion state. The completion of
   --  subsequent I/O on the file descriptor is not guaranted to
   --  be completed in a synchronized fashion.
   --  If the Synchronize_File procedure fails or there is an
   --  error condition associated with AD, data is not guaranteed
   --  to have been successfulIy transferred.

   declare
      AD : AIO_Descriptor;
      AD2 : AIO_Descriptor;
   begin
      Test ("Synchronize_File [6.3.10]");
      Setup (AD);
      Setup (AD2);
      FD := Open_Or_Create
        (Valid_AIO_Filename, Read_Write, Owner_Permission_Set);
      Set_File (AD, FD);
      Write (AD);
      delay 1.0;
      Comment ("Synchronizing file");
      Synchronize_File (AD2);
      Check_Status (AD);
      Check_Status (AD2);
      Cleanup (AD2);
      Destroy_AIO_Control_Block (AD);
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code = Invalid_Argument then
         Assert (not POSIX_Configurable_File_Limits.
           Synchronized_IO_Is_Supported (Valid_AIO_Filename), "A050");
      else
         Optional (Asynchronous_IO_Option, Synchronized_IO_Option,
           Operation_Not_Implemented, E1, "A051");
      end if;
   when E2 : others => Unexpected_Exception (E2, "A052");
   end;

   -----------------------------------------------------------------------
   --  Synchronize_Data( ) asynchronously forces all I/O requests
   --  associated with the file specified by the AD parameter
   --  and queued at the time of the call to the synchronized
   --  completion state.  The procedure call to Synchronize_Data
   --  shall return when the synchronization request has been
   --  initiated or queued to the file or device (even when the
   --  data cannot be synchronized immediately).
   --  If Synchronize_Data or Synchronize_Data succeeds, then
   --  it is only the I/O that was queued at the time of
   --  the call to Synchronize_Data that is guaranteed to be
   --  forced to the relevant completion state. The completion of
   --  subsequent I/O on the file descriptor is not guaranteed to
   --  be completed in a synchronized fashion.
   --  If the Synchronize_Data procedure fails or there is an
   --  error condition associated with AD, data is not guaranteed
   --  to have been successfulIy transferred.

   declare
      AD : AIO_Descriptor;
      AD2 : AIO_Descriptor;
   begin
      Test ("Synchronize_Data [6.3.10]");
      Setup (AD);
      Setup (AD2);
      FD := Open_Or_Create
        (Valid_AIO_Filename, Read_Write, Owner_Permission_Set);
      Set_File (AD, FD);
      Write (AD);
      delay 1.0;
      Set_File (AD2, FD);
      Comment ("Synchronizing data");
      Synchronize_Data (AD2);
      Cleanup (AD2);
      Destroy_AIO_Control_Block (AD);
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code = Invalid_Argument then
         Assert (not POSIX_Configurable_File_Limits.
           Synchronized_IO_Is_Supported (Valid_AIO_Filename), "A053");
      else
         Optional (Asynchronous_IO_Option, Synchronized_IO_Option,
           Operation_Not_Implemented, E1, "A054");
      end if;
   when E2 : others => Unexpected_Exception (E2, "A055");
   end;

   --------------------------------------------------------------------------

   Test ("Set_Offset [6.3.2]");

   --  This interface is coverd by procedure Setup
   --  ....

   --------------------------------------------------------------------------



   Test ("Set_Buffer [6.3.2]");

   --  This interface is covered by procedure SetUp
   --  ....

   --------------------------------------------------------------------------

   Test ("Set_Length [6.3.2]");

   --  This interface is covered by procedure Check_Status
   --  ....

   -------------------------------------------------------------------------

   Test ("Get_AIO_Error_Code [6.3.6]");

   --  This interface is covered by procedure Check_Status
   --  ....

   -----------------------------------------------------------------------

   Done;
exception
when E : others => Fatal_Exception (E, "A056");
end p060300;

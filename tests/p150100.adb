------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5B VALIDATION TEST SUITE                      --
--                                                                          --
--                             P 1 5 0 1 0 0                                --
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

--  Test for POSIX_Message_Queues package

--  ... Baker still needs to review changes since last version.
--  A quick look suggests more care needs to be taken to "clean up"
--  after tests, e.g., to close and unlink message queues.

with Ada_Streams,
     POSIX,
     POSIX_Configurable_System_Limits,
     POSIX_File_Status,
     POSIX_IO,
     POSIX_Message_Queues,
     POSIX_Permissions,
     POSIX_Process_Identification,
     POSIX_Process_Primitives,
     POSIX_Report,
     POSIX_Signals,
     Test_Parameters;

procedure p150100 is
   use Ada_Streams,
       POSIX,
       POSIX_Configurable_System_Limits,
       POSIX_File_Status,
       POSIX_IO,
       POSIX_Message_Queues,
       POSIX_Permissions,
       POSIX_Process_Identification,
       POSIX_Process_Primitives,
       POSIX_Report,
       POSIX_Signals;

   package TP renames Test_Parameters;

   Mqd : Message_Queue_Descriptor;
   Attr : Attributes;

   Child_Pathname : constant POSIX_String := "./bin/p150100a";
   Child_Filename : constant POSIX_String := "p150100a";

begin

   Header ("p150100");
   Comment ("Parent Process Beginning.");

   -----------------------------------------------------------------------
   --  Assert: The Set/Get_Max_Messages operations on the Attributes type
   --  give consistent results.

   begin
      Test ("Set/Get_Max_Messages [15.1.1]");
      Set_Max_Messages (Attr, 10);
      Assert (Get_Max_Messages (Attr) = 10, "A001");
   exception
   when E1 : POSIX_Error =>
      Optional (Message_Queues_Option,
        Operation_Not_Implemented, E1, "A002");
   when E2 : others => Unexpected_Exception (E2, "A003");
   end;

   -----------------------------------------------------------------------
   --  Assert: The Set/Get_Message_Length operations on the Attributes type
   --  give consistent results.

   begin
      Test ("Set/Get_Message_Length [15.1.1]");
      Set_Message_Length (Attr, 100);
      Assert (Get_Message_Length (Attr) = 100, "A004");
   exception
   when E1 : POSIX_Error =>
      Optional (Message_Queues_Option,
        Operation_Not_Implemented, E1, "A005");
   when E2 : others => Unexpected_Exception (E2, "A006");
   end;

   -----------------------------------------------------------------------
   --  Assert: The Set/Get_Options operations on the Attributes type
   --  give consistent results.

   begin
      Test ("Set/Get_Options [15.1.1]");
      Set_Options (Attr, POSIX_Message_Queues.Non_Blocking);
      Assert (Get_Options (Attr) = POSIX_Message_Queues.Non_Blocking, "A007");
   exception
   when E1 : POSIX_Error =>
      Optional (Message_Queues_Option,
        Operation_Not_Implemented, E1, "A008");
   when E2 : others => Unexpected_Exception (E2, "A009");
   end;

   -----------------------------------------------------------------------
   --  Test the Get_Message_Count call on the Attributes type to see
   --  that a positive value is returned.

   begin
      Test ("Get_Message_Count [15.1.1]");
      Assert (Get_Message_Count (Attr) >= 0, "A010");
   exception
   when E1 : POSIX_Error =>
      Optional (Message_Queues_Option,
        Operation_Not_Implemented, E1, "A011");
   when E2 : others => Unexpected_Exception (E2, "A012");
   end;

   -----------------------------------------------------------------------
   --  Assert: Calls to Send shall fail if a message queue resource is
   --  temporarily unavailable (such as if the message queue is full).

   begin
      Test ("Temporarily Unavailable Error");
      Set_Options (Attr, POSIX_Message_Queues.Non_Blocking);
      Set_Max_Messages (Attr, 1);
      Mqd := Open_Or_Create (TP.Valid_MQ_Name (1),
       Read_Write, Owner_Permission_Set,
       POSIX_IO.Open_Option_Set (POSIX_Message_Queues.Get_Options (Attr)),
       Attr, POSIX.RTS_Signals);
      Send (Mqd, To_Stream_Element_Array ("Hello....."), 1);
      Send (Mqd, To_Stream_Element_Array ("Hello....."), 1);
      Assert (False, "Exception Expected");
      Close (Mqd);
      Unlink_Message_Queue (TP.Valid_MQ_Name (1));
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code /= Resource_Temporarily_Unavailable then
         Optional (Message_Queues_Option, Operation_Not_Implemented, E1,
                   "A013");
      else
         Close (Mqd);
         Unlink_Message_Queue (TP.Valid_MQ_Name (1));
      end if;
   end;

   -----------------------------------------------------------------------
   --  Assert: A Message Queue is actually created by the
   --  Open_Or_Create call, can be closed, and can be reopened in any mode.

   begin
      Test ("Open_Or_Create [15.1.2]");
      Mqd := Open_Or_Create (TP.Valid_MQ_Name (2),
        Read_Write, Owner_Permission_Set);
      begin
         Assert (Is_Message_Queue
           (Get_File_Status (TP.Valid_MQ_Name (2))), "file status wrong");
      exception
      when E : POSIX_Error =>
         if Get_Error_Code = No_Such_File_Or_Directory then
            Comment ("message queue is not in file system");
         else Unexpected_Exception (E, "get file status failed");
         end if;
      end;
      Close (Mqd);
      Mqd := Open (TP.Valid_MQ_Name (2), Read_Write);
      Close (Mqd);
      Mqd := Open (TP.Valid_MQ_Name (2), Write_Only);
      Close (Mqd);
      Unlink_Message_Queue (TP.Valid_MQ_Name (2));
   exception
   when E1 : POSIX_Error =>
      Optional (Message_Queues_Option,
        Operation_Not_Implemented, E1, "A014");
   when E2 : others => Unexpected_Exception (E2, "A015");
   end;

   -----------------------------------------------------------------------
   --  Assert: The Unlink_Message_Queue procedure works.

   begin
      Test ("Unlink_Message_Queue [15.1.4]");
      Mqd := Open_Or_Create (TP.Valid_MQ_Name (3),
       Read_Write, Owner_Permission_Set,
       POSIX_IO.Open_Option_Set (POSIX_Message_Queues.Get_Options (Attr)),
       Attr, POSIX.RTS_Signals);
      Close (Mqd);
      Unlink_Message_Queue (TP.Valid_MQ_Name (3));
   exception
   when E1 : POSIX_Error =>
      Optional (Message_Queues_Option, Operation_Not_Implemented, E1, "A016");
   when E2 : others => Unexpected_Exception (E2, "A017");
   end;


   -----------------------------------------------------------------------
   --  Assert: Error Code is No_Such_File_Or_Directory when trying to
   --  open an unlinked message queue.

   begin
      Test ("Open unlinked message queue");
      Mqd := Open (TP.Valid_MQ_Name (3), Read_Write);
      Close (Mqd);
      Assert (False, "Exception Expected");
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code /= No_Such_File_Or_Directory then
         Optional (Message_Queues_Option, Operation_Not_Implemented, E1,
                    "A018");
      end if;
   when E2 : others => Unexpected_Exception (E2, "A019");
   end;

   -----------------------------------------------------------------------
   --  Assert: Error Code is No_Such_File_Or_Directory when trying to
   --  open a message queue which has not been created.

   begin
      Test ("Open never-created message queue");
      Mqd := Open (TP.Valid_MQ_Name (4), Read_Write);
      Close (Mqd);
      Assert (False, "Exception Expected");
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code /= No_Such_File_Or_Directory then
         Optional (Message_Queues_Option, Operation_Not_Implemented, E1,
                   "A020");
      end if;
   when E2 : others => Unexpected_Exception (E2, "A021");
   end;

   -----------------------------------------------------------------------
   --  Assert: The Open_Or_Create procedure works when attributes are
   --  passed as a parameter, and that attributes remain consistent after
   --  creation of message queue.

   begin
      Test ("Open_Or_Create w/ Attributes");
      Set_Max_Messages (Attr, 10);
      Set_Message_Length (Attr, 10);
      Set_Options (Attr, POSIX_Message_Queues.Non_Blocking);
      Mqd := Open_Or_Create (TP.Valid_MQ_Name (4),
       Read_Write, Owner_Permission_Set,
       POSIX_IO.Open_Option_Set (POSIX_Message_Queues.Get_Options (Attr)),
       Attr, POSIX.RTS_Signals);
      Attr := POSIX_Message_Queues.Get_Attributes (Mqd);
      Comment ("Message_Length set to " &
       Integer'Image (Get_Message_Length (Attr)));
      Assert (Get_Max_Messages (Attr) = 10, "max messages");
      Assert (Get_Message_Length (Attr) = 10, "message length");
      Close (Mqd);
      Unlink_Message_Queue (TP.Valid_MQ_Name (4));
   exception
   when E1 : POSIX_Error =>
      Optional (Message_Queues_Option,
        Operation_Not_Implemented, E1, "A022");
   when E2 : others => Unexpected_Exception (E2, "A023");
   end;

   -----------------------------------------------------------------------
   --  Assert: Message can be sent to a queue and then received from the
   --  queue.

   declare
      Last : Ada_Streams.Stream_Element_Offset;
      Prio : Message_Priority;
      Msg  : Ada_Streams.Stream_Element_Array (1 .. 10);
   begin
      Test ("Send [15.1.5]");
      Comment ("Message Length set to " &
       Integer'Image (Get_Message_Length (Attr)));
      Mqd := Open_Or_Create (TP.Valid_MQ_Name (5),
       Read_Write, Owner_Permission_Set,
       POSIX_IO.Open_Option_Set (POSIX_IO.Empty_Set), Attr);
      Send (Mqd, To_Stream_Element_Array ("Hello....."), 1);
      Receive (Mqd, Msg, Last, Prio);
      Assert (Prio = 1, "priority");
      Assert (Last = 10 and then Msg = To_Stream_Element_Array ("Hello....."),
           "A024");
      Close (Mqd);
      Unlink_Message_Queue (TP.Valid_MQ_Name (5));
   exception
   when E1 : POSIX_Error =>
      Optional (Message_Queues_Option,
        Operation_Not_Implemented, E1, "A025");
   when E2 : others => Unexpected_Exception (E2, "A026");
   end;

   -----------------------------------------------------------------------
   --  Assert: A message can be received and that the Priority is set
   --  properly.

   declare
      Last : Ada_Streams.Stream_Element_Offset;
      Prio : Message_Priority;
      Msg  : Ada_Streams.Stream_Element_Array (1 .. 5);
      ATR  : Attributes;
   begin
      Test ("Receive [15.1.6]");
      Set_Message_Length (ATR, 5);
      Set_Max_Messages (ATR, 1);
      Mqd := Open_Or_Create (TP.Valid_MQ_Name (6),
       Read_Write, Owner_Permission_Set,
       POSIX_IO.Open_Option_Set (POSIX_IO.Empty_Set), ATR);
      Send (Mqd, To_Stream_Element_Array ("Hello"), 1);
      Receive (Mqd, Msg, Last, Prio);
      Assert (Prio = 1, "priority");
      Assert (Last = 5 and then Msg = To_Stream_Element_Array ("Hello"),
              "Message Data Corrupted");
      Close (Mqd);
      Unlink_Message_Queue (TP.Valid_MQ_Name (6));
   exception
   when E1 : POSIX_Error =>
      Optional (Message_Queues_Option,
        Operation_Not_Implemented, E1, "A027");
   when E2 : others => Unexpected_Exception (E2, "A028");
   end;

   -----------------------------------------------------------------------
   --  Assert: The Request_Notify and Remove_Notify functions can be
   --  called.

   declare
      Event : Signal_Event;
   begin
      Test ("Request/Remove_Notify [15.1.8]");
      Mqd := Open (TP.Valid_MQ_Name (6), Read_Write);
      Set_Notification (Event, No_Notification);
      Set_Signal (Event, Signal_Kill);
      Request_Notify (Mqd, Event);
      --  Not notify the process,
      --  should return no error messages if implemented
      Remove_Notify (Mqd);
      Close (Mqd);
      Unlink_Message_Queue (TP.Valid_MQ_Name (6));
   exception
   when E1 : POSIX_Error =>
      Optional (Message_Queues_Option, Realtime_Signals_Option,
        Operation_Not_Implemented, E1, "A029");
   when E2 : others => Unexpected_Exception (E2, "A030");
   end;

   -----------------------------------------------------------------------
   --  Assert: Communication is possible between two processes
   --  using Message_queues.

   declare
      Child_PID : Process_ID;
      Child_Status : Termination_Status;
      Template : Process_Template;
      Arg_List : POSIX_String_List;
      ATR : Attributes;
   begin
      Test ("Two-process communication");
      POSIX.Append (Arg_List, Child_Filename);
      Open_Template (Template);
      Start_Process (Child_PID, Child_Pathname, Template, Arg_List);
      Comment ("Message Receiver process started.");
      Set_Message_Length (ATR, 10);
      Set_Max_Messages (ATR, 1);
      Mqd := Open_Or_Create (TP.Valid_MQ_Name (7),
       Read_Write, Owner_Permission_Set,
       POSIX_IO.Open_Option_Set (POSIX_IO.Empty_Set), ATR);
      Send (Mqd, To_Stream_Element_Array ("Hello....."), 1);
      Wait_For_Child_Process (Child_Status, Child_PID);
      if Exit_Status_Of (Child_Status) /= Normal_Exit then
         Fail ("Message not received by child process.");
      end if;
      Unlink_Message_Queue (TP.Valid_MQ_Name (7));
   exception
   when E1 : POSIX_Error =>
      Optional (Message_Queues_Option,
        Operation_Not_Implemented, E1, "A031");
   when E2 : others => Unexpected_Exception (E2, "A032");
   end;

   ------------------------------------------------------------------------
   --  Assert: The Bad_File_Descriptor error code is given when
   --  attempting to access a node with improper permissions.

   begin
      Test ("Bad_File_Descriptor Error");
      Mqd := Open_Or_Create
        (TP.Valid_MQ_Name (8),
         POSIX_IO.Read_Only, Owner_Permission_Set);
      Send (Mqd, To_Stream_Element_Array ("Hello....."), 1);
      Unlink_Message_Queue (TP.Valid_MQ_Name (8));
      Assert (False,
              "Expected POSIX_ERROR with Error Code Bad_File_Descriptor");
   exception
   when E1 : POSIX_Error =>
      Optional (Message_Queues_Option,
        Operation_Not_Implemented, Bad_File_Descriptor, E1, "A033");
   when E2 : others => Unexpected_Exception (E2, "A034");
   end;

   ------------------------------------------------------------------------
   --  Assert: The File_Exists error code is given when attempting to
   --  Open_Or_Create a message queue with the Exclusive option.
   declare
      ATR : Attributes;
   begin
      Test ("File_Exists Error");
      Set_Max_Messages (ATR, 1);
      Set_Message_Length (ATR, 10);
      Set_Options (ATR, Message_Queue_Options (POSIX_IO.Exclusive));
      Mqd := Open_Or_Create (TP.Valid_MQ_Name (9),
        POSIX_IO.Read_Only, Owner_Permission_Set,
        POSIX_IO.Open_Option_Set (Get_Options (ATR)),
        ATR);
      Mqd := Open_Or_Create (TP.Valid_MQ_Name (9),
       POSIX_IO.Read_Only,
       Owner_Permission_Set, POSIX_IO.Open_Option_Set (Exclusive));
      Assert (False, "Expected POSIX_ERROR with Error Code File_Exists");
   exception
   when E1 : POSIX_Error =>
      Optional (Message_Queues_Option,
        Operation_Not_Implemented, File_Exists, E1, "A035");
   when E2 : others => Unexpected_Exception (E2, "A036");
   end;

   ------------------------------------------------------------------------
   --  Assert: The Bad_File_Descriptor error code is given when
   --  attempting to close an invalid message queue.

   declare
      QD : Message_Queue_Descriptor;
   begin
      Test ("Bad File Descriptor Error");
      QD := POSIX_Message_Queues.Open (TP.Valid_MQ_Name (9),
       POSIX_IO.Read_Only,
       POSIX_IO.Open_Option_Set (POSIX_IO.Empty_Set));
      Close (QD);
      POSIX_Message_Queues.Close (QD);
      Assert (False,
       "Expected POSIX_ERROR with Error Code Bad_File_Descriptor");
   exception
   when E1 : POSIX_Error =>
      Optional (Message_Queues_Option,
        Operation_Not_Implemented, Bad_File_Descriptor, E1, "A037");
   when E2 : others => Unexpected_Exception (E2, "A038");
   end;

   ------------------------------------------------------------------------
   --  Assert: The No_Such_File_Or_Directory error code is given when
   --  attempting to unlink a nonexistent message queue.

   begin
      Test ("No_Such_File_Or_Directory Error");
      Unlink_Message_Queue (TP.Valid_MQ_Name (12));
      Assert (False,
       "Expected POSIX_ERROR with Error Code No_Such_File_Or_Directory");
   exception
   when E1 : POSIX_Error =>
      Optional (Message_Queues_Option,
        Operation_Not_Implemented, No_Such_File_Or_Directory, E1, "A039");
   when E2 : others => Unexpected_Exception (E2, "A040");
   end;

   ------------------------------------------------------------------------
   --  Assert: The Bad_File_Descriptor error code is given when
   --  attempting to send to an invalid message queue.

   declare
      Uninitialized_QD : Message_Queue_Descriptor;
      pragma Warnings (Off, Uninitialized_QD);
      --  Let this variable uninitialized.
   begin
      Test ("Send to Invalid Message Queue");
      Send (Uninitialized_QD, To_Stream_Element_Array ("Hello....."), 0);
      Assert (False,
       "Expected POSIX_ERROR with Error Code Bad_File_Descriptor");
   exception
   when E1 : POSIX_Error =>
      Optional (Message_Queues_Option,
        Operation_Not_Implemented, Bad_File_Descriptor, E1, "A041");
   when E2 : others => Unexpected_Exception (E2, "A042");
   end;

   ------------------------------------------------------------------------
   --  Assert: The Invalid_Argument error code is given when
   --  attempting to send a message with a priority that is too high.

   --  ??????
   --  This may need checking in POSIX.5b.
   --  Florist rases Constraint_Error for this case,
   --  Is that legal?

   declare
      Mqd : Message_Queue_Descriptor;
   begin
      Test ("Invalid_Argument Error");
      Mqd := Open_Or_Create (TP.Valid_MQ_Name (9),
       Write_Only, Owner_Permission_Set,
       POSIX_IO.Open_Option_Set (POSIX_IO.Empty_Set), Attr);
      Send (Mqd, To_Stream_Element_Array ("Hello....."),
       POSIX_Configurable_System_Limits.Message_Priority_Maximum + 1);
      Assert (False,
       "Expected POSIX_ERROR with Error Code Invalid_Argument");
   exception
   when E1 : POSIX_Error =>
      Optional (Message_Queues_Option,
        Operation_Not_Implemented, Invalid_Argument, E1, "A043");
   when Constraint_Error =>
      null;
   when E2 : others => Unexpected_Exception (E2, "A044");
   end;

   ------------------------------------------------------------------------
   --  Assert: The Message_Too_Long error code is given when attempting
   --  to send a message that is longer than the maximum length.

   begin
      Test ("Message_Too_Long Error");
      Set_Message_Length (Attr, 1);
      Mqd := Open_Or_Create (TP.Valid_MQ_Name (9),
       Write_Only, Owner_Permission_Set,
       POSIX_IO.Open_Option_Set (POSIX_IO.Empty_Set), Attr);
      Send (Mqd, To_Stream_Element_Array ("Hello....."), 1);
      Assert (False,
       "Expected POSIX_ERROR with Error Code Message_Too_Long");
   exception
   when E1 : POSIX_Error =>
      Optional (Message_Queues_Option,
        Operation_Not_Implemented, Message_Too_Long, E1, "A045");
   when E2 : others => Unexpected_Exception (E2, "A046");
   end;

   ------------------------------------------------------------------------
   --  Assert: The Bad_File_Descriptor error code is given when
   --  attempting to receive an invalid message queue.

   declare
      QD : Message_Queue_Descriptor;
      AR : Stream_Element_Array (1 .. 10);
      Last : Ada_Streams.Stream_Element_Offset;
      Prio : Integer;
   begin
      Test ("Receive Invalid Message Queue");
      QD := POSIX_Message_Queues.Open (TP.Valid_MQ_Name (10),
       POSIX_IO.Read_Only,
       POSIX_IO.Open_Option_Set (POSIX_IO.Empty_Set));
      Close (QD);
      Receive (QD, AR, Last, Prio);
      Assert (False,
       "Expected POSIX_ERROR with Error Code Bad_File_Descriptor");
   exception
   when E1 : POSIX_Error =>
      Optional (Message_Queues_Option,
        Operation_Not_Implemented, Bad_File_Descriptor, E1, "A047");
   when E2 : others => Unexpected_Exception (E2, "A048");
   end;

   ------------------------------------------------------------------------
   --  Assert: The Message_Too_Long error code is given when attempting
   --  to receive a message into an array that is shorter than the
   --  Message Length attribute of the message queue.

   declare
      AR : Stream_Element_Array (1 .. 2);
      Last : Ada_Streams.Stream_Element_Offset;
      Prio : Integer;
   begin
      Test ("Receive to Short Array");
      Set_Message_Length (Attr, 10);
      Mqd := Open_Or_Create (TP.Valid_MQ_Name (10),
       Read_Write, Owner_Permission_Set,
       POSIX_IO.Open_Option_Set (POSIX_IO.Empty_Set), Attr);
      Send (Mqd, To_Stream_Element_Array ("Hello....."), 1);
      Receive (Mqd, AR, Last, Prio);
      Assert (False,
       "Expected POSIX_ERROR with Error Code Message_Too_Long");
   exception
   when E1 : POSIX_Error =>
      Optional (Message_Queues_Option,
        Operation_Not_Implemented, Message_Too_Long, E1, "A049");
   when E2 : others => Unexpected_Exception (E2, "A050");
   end;

   ------------------------------------------------------------------------
   --  Assert: The Generic_Message_Passing package can be used.

   declare
      package Int_Queues is new Generic_Message_Queues (Integer);
      GQD : Message_Queue_Descriptor;
      ATR : Attributes;
      SI : Integer;
      RI : Integer;
      Prio : Integer;
   begin
      Test ("Generic Message Queues [15.1.7]");
      SI := 2;
      Prio := 1;
      Set_Max_Messages (ATR, 10);
      Set_Message_Length (ATR, 4);
      GQD := Open_Or_Create (TP.Valid_MQ_Name (11),
       Read_Write,
       Owner_Permission_Set, POSIX_IO.Open_Option_Set (POSIX_IO.Empty_Set),
       ATR);
      ATR := Get_Attributes (GQD);
      Comment ("Max Messages = " & Integer'Image (Get_Max_Messages (ATR)));
      Comment ("Message Length = " & Integer'Image (Get_Message_Length (ATR)));
      Int_Queues.Send (GQD, SI, 1);
      Comment ("Message sent");
      Int_Queues.Receive (GQD, RI, Prio);
      Comment ("Message received");
      if RI /= SI then
         Fail ("Integer corrupted during transmission.");
      end if;
      Unlink_Message_Queue (TP.Valid_MQ_Name (11));
   exception
   when E1 : POSIX_Error =>
      Optional (Message_Queues_Option,
        Operation_Not_Implemented, E1, "A051");
   when E2 : others => Unexpected_Exception (E2, "A052");
   end;

   ------------------------------------------------------------------------

   --  .... Should add a loop here, with an exception handler inside,
   --  to loop over the array of MQ names used, and unlink them all.

   ------------------------------------------------------------------------

   Done;

exception
when E : others => Fatal_Exception (E, "A053");
end p150100;

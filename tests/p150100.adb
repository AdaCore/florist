------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5B VALIDATION TEST SUITE                      --
--                                                                          --
--                             P 1 5 0 1 0 0                                --
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

--  Test for POSIX_Message_Queues package.

--  Setup:  When this test is run the executable program p150100b
--  must be accessible via the pathname "./bin/p150100b".

with Ada_Streams,
     POSIX,
     POSIX_Configurable_System_Limits,
     POSIX_File_Status,
     POSIX_IO,
     POSIX_Limits,
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

   Child_Pathname : constant POSIX_String := "./bin/p150100b";
   Child_Filename : constant POSIX_String := "p150100b";

begin

   Header ("p150100", Root_OK => True);
   Comment ("Parent Process Beginning.");

   -----------------------------------------------------------------------
   --  The Set/Get_Max_Messages operations on the Attributes type
   --  give consistent results.

   begin
      Test ("Set/Get_Max_Messages [15.1.1]");
      Set_Max_Messages (Attr, 10);
      Assert (Get_Max_Messages (Attr) = 10, "A001: get_max_messages");
   exception
   when E1 : POSIX_Error =>
      Optional (Message_Queues_Option,
        Operation_Not_Implemented, E1, "A002");
   when E2 : others => Unexpected_Exception (E2, "A003");
   end;

   -----------------------------------------------------------------------
   --  The Set/Get_Message_Length operations on the Attributes type
   --  give consistent results.

   begin
      Test ("Set/Get_Message_Length [15.1.1]");
      Set_Message_Length (Attr, 100);
      Assert (Get_Message_Length (Attr) = 100, "A004: get_message_length");
   exception
   when E1 : POSIX_Error =>
      Optional (Message_Queues_Option,
        Operation_Not_Implemented, E1, "A005");
   when E2 : others => Unexpected_Exception (E2, "A006");
   end;

   -----------------------------------------------------------------------
   --  The Set/Get_Options operations on the Attributes type
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
   --  Calls to Send shall fail if a message queue resource is
   --  temporarily unavailable (such as if the message queue is full).

   begin
      Test ("Temporarily Unavailable Error");
      Set_Options (Attr, POSIX_Message_Queues.Non_Blocking);
      Set_Message_Length (Attr, 100);
      Set_Max_Messages (Attr, 1);
      Mqd := Open_Or_Create (TP.Valid_MQ_Name (1),
       Read_Write, Owner_Permission_Set,
       POSIX_IO.Open_Option_Set (POSIX_Message_Queues.Get_Options (Attr)),
       Attr, POSIX.RTS_Signals);
      Send (Mqd, To_Stream_Element_Array ("Hello....."), 1);
      Send (Mqd, To_Stream_Element_Array ("Hello....."), 1);
      Expect_Exception ("A013: POSIX_Error");
      Close (Mqd);
      Unlink_Message_Queue (TP.Valid_MQ_Name (1));
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code /= Resource_Temporarily_Unavailable then
         Optional (Message_Queues_Option, Operation_Not_Implemented, E1,
                   "A014");
      else
         Close (Mqd);
         Unlink_Message_Queue (TP.Valid_MQ_Name (1));
      end if;
   end;

   -----------------------------------------------------------------------
   --  A Message Queue is actually created by the
   --  Open_Or_Create call, can be closed, and can be reopened in any mode.

   begin
      Test ("Open_Or_Create [15.1.2]");
      Mqd := Open_Or_Create (TP.Valid_MQ_Name (2),
        Read_Write, Owner_Permission_Set);
      begin
         Assert (Is_Message_Queue
           (Get_File_Status (TP.Valid_MQ_Name (2))), "A015: file status");
      exception
      when E : POSIX_Error =>
         if Get_Error_Code = No_Such_File_Or_Directory then
            Comment ("message queue is not in file system");
         else Unexpected_Exception (E, "A016: get file status failed");
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
        Operation_Not_Implemented, E1, "A017");
   when E2 : others => Unexpected_Exception (E2, "A018");
   end;

   -----------------------------------------------------------------------
   --  The Unlink_Message_Queue procedure works.

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
      Optional (Message_Queues_Option, Operation_Not_Implemented, E1, "A019");
   when E2 : others => Unexpected_Exception (E2, "A020");
   end;


   -----------------------------------------------------------------------
   --  Error Code is No_Such_File_Or_Directory when trying to
   --  open an unlinked message queue.

   begin
      Test ("Open unlinked message queue");
      Mqd := Open (TP.Valid_MQ_Name (3), Read_Write);
      Expect_Exception ("A021: POSIX_Error");
      Close (Mqd);
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code /= No_Such_File_Or_Directory then
         Optional (Message_Queues_Option, Operation_Not_Implemented, E1,
                    "A022");
      end if;
   when E2 : others => Unexpected_Exception (E2, "A023");
   end;

   -----------------------------------------------------------------------
   --  Error Code is No_Such_File_Or_Directory when trying to
   --  open a message queue which has not been created.

   begin
      Test ("Open never-created message queue");
      Mqd := Open (TP.Valid_MQ_Name (4), Read_Write);
      Expect_Exception ("A024: POSIX_Error");
      Close (Mqd);
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code /= No_Such_File_Or_Directory then
         Optional (Message_Queues_Option, Operation_Not_Implemented, E1,
                   "A025");
      end if;
   when E2 : others => Unexpected_Exception (E2, "A026");
   end;

   -----------------------------------------------------------------------
   --  The Open_Or_Create procedure works when attributes are
   --  passed as a parameter, and that attributes remain consistent after
   --  creation of message queue.

   declare
      Local_Attr : Attributes;
   begin
      Test ("Open_Or_Create w/ Attributes");
      Set_Max_Messages (Attr, 10);
      Set_Message_Length (Attr, 10);
      Set_Options (Attr, POSIX_Message_Queues.Non_Blocking);
      Mqd := Open_Or_Create (TP.Valid_MQ_Name (4),
       Read_Write, Owner_Permission_Set,
       POSIX_IO.Open_Option_Set (POSIX_Message_Queues.Get_Options (Attr)),
       Attr, POSIX.RTS_Signals);
      Local_Attr := POSIX_Message_Queues.Get_Attributes (Mqd);
      Comment ("Message_Length set to " &
       Integer'Image (Get_Message_Length (Local_Attr)));
      Assert (Get_Max_Messages (Local_Attr) = 10, "A027: max messages");
      Assert (Get_Message_Length (Local_Attr) = 10, "A028: message length");
      Close (Mqd);
      Unlink_Message_Queue (TP.Valid_MQ_Name (4));
   exception
   when E1 : POSIX_Error =>
      Optional (Message_Queues_Option,
        Operation_Not_Implemented, E1, "A029");
   when E2 : others => Unexpected_Exception (E2, "A030");
   end;

   -----------------------------------------------------------------------
   --  Message can be sent to a queue and then received from the
   --  queue.

   declare
      Last : Ada_Streams.Stream_Element_Offset;
      Prio : Message_Priority;
      Msg  : Ada_Streams.Stream_Element_Array (1 .. 10);
   begin
      Test ("Send [15.1.5]");
      Set_Max_Messages (Attr, 10);
      Set_Message_Length (Attr, 10);
      Set_Options (Attr, POSIX_Message_Queues.Non_Blocking);
      Mqd := Open_Or_Create (TP.Valid_MQ_Name (5),
       Read_Write, Owner_Permission_Set, Empty_Set, Attr);
      Send (Mqd, To_Stream_Element_Array ("Hello....."), 1);
      Receive (Mqd, Msg, Last, Prio);
      Assert (Prio = 1, "A031: incorrect priority");
      Assert (Last = 10 and then Msg = To_Stream_Element_Array ("Hello....."),
           "A032: wrong message");
      Close (Mqd);
      Unlink_Message_Queue (TP.Valid_MQ_Name (5));
   exception
   when E1 : POSIX_Error =>
      Optional (Message_Queues_Option,
        Operation_Not_Implemented, E1, "A033");
   when E2 : others => Unexpected_Exception (E2, "A034");
   end;

   -----------------------------------------------------------------------
   --  A message can be received and that the Priority is set
   --  properly.

   declare
      Last : Ada_Streams.Stream_Element_Offset;
      Prio : Message_Priority;
      Msg  : Ada_Streams.Stream_Element_Array (1 .. 5);
   begin
      Test ("Receive [15.1.6]");
      Set_Options (Attr, POSIX_Message_Queues.Non_Blocking);
      Set_Message_Length (Attr, 5);
      Set_Max_Messages (Attr, 1);
      Mqd := Open_Or_Create (TP.Valid_MQ_Name (6),
       Read_Write, Owner_Permission_Set, Empty_Set, Attr);
      Send (Mqd, To_Stream_Element_Array ("Hello"), 1);
      Receive (Mqd, Msg, Last, Prio);
      Assert (Prio = 1, "A035: incorrect priority");
      Assert (Last = 5 and then Msg = To_Stream_Element_Array ("Hello"),
              "A036: message data corrupted");
      Close (Mqd);
      Unlink_Message_Queue (TP.Valid_MQ_Name (6));
   exception
   when E1 : POSIX_Error =>
      Optional (Message_Queues_Option,
        Operation_Not_Implemented, E1, "A037");
   when E2 : others => Unexpected_Exception (E2, "A038");
   end;

   -----------------------------------------------------------------------
   --  The Request_Notify and Remove_Notify functions can be
   --  called.

   declare
      Event : Signal_Event;
   begin
      Test ("Request/Remove_Notify [15.1.8]");
      Mqd := Open_Or_Create (TP.Valid_MQ_Name (6),
       Read_Write, Owner_Permission_Set, Empty_Set, Attr);
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
        Operation_Not_Implemented, E1, "A039");
   when E2 : others => Unexpected_Exception (E2, "A040");
   end;

   -----------------------------------------------------------------------
   --  Communication is possible between two processes
   --  using Message_queues.

   declare
      Child_PID : Process_ID;
      Child_Status : Termination_Status;
      Template : Process_Template;
      Arg_List : POSIX_String_List;
   begin
      Test ("Two-process communication");
      POSIX.Append (Arg_List, Child_Filename);
      Open_Template (Template);
      Start_Process (Child_PID, Child_Pathname, Template, Arg_List);
      Comment ("Message Receiver process started.");
      Set_Options (Attr, POSIX_Message_Queues.Non_Blocking);
      Set_Message_Length (Attr, 10);
      Set_Max_Messages (Attr, 1);
      Mqd := Open_Or_Create (TP.Valid_MQ_Name (7),
       Read_Write, Owner_Permission_Set, Empty_Set, Attr);
      Send (Mqd, To_Stream_Element_Array ("Hello....."), 1);
      Wait_For_Child_Process (Child_Status, Child_PID);
      Check_Child_Status (Child_Status, Child_PID, Normal_Exit, "A041");
      Unlink_Message_Queue (TP.Valid_MQ_Name (7));
   exception
   when E1 : POSIX_Error =>
      Optional (Message_Queues_Option,
        Operation_Not_Implemented, E1, "A042");
   when E2 : others => Unexpected_Exception (E2, "A043");
   end;

   ------------------------------------------------------------------------
   --  The Bad_File_Descriptor error code is given when
   --  attempting to access a node with improper permissions.

   begin
      Test ("Bad_File_Descriptor Error");
      Mqd := Open_Or_Create
        (TP.Valid_MQ_Name (8),
         POSIX_IO.Read_Only, Owner_Permission_Set);
      Send (Mqd, To_Stream_Element_Array ("Hello....."), 1);
      Unlink_Message_Queue (TP.Valid_MQ_Name (8));
      Expect_Exception ("A044: POSIX_Error, Bad_File_Descriptor");
   exception
   when E1 : POSIX_Error =>
      Optional (Message_Queues_Option,
        Operation_Not_Implemented, Bad_File_Descriptor, E1, "A045");
   when E2 : others => Unexpected_Exception (E2, "A046");
   end;

   ------------------------------------------------------------------------
   --  The File_Exists error code is given when attempting to
   --  Open_Or_Create a message queue with the Exclusive option,
   --  if there already exists a message queue with the same name.
   --  Retest for each of the two overloaded versions.

   begin
      Test ("File_Exists Error");
      Set_Max_Messages (Attr, 1);
      Set_Message_Length (Attr, 10);
      Mqd := Open_Or_Create (TP.Valid_MQ_Name (9),
        POSIX_IO.Read_Only, Owner_Permission_Set,
        POSIX_IO.Empty_Set, Attr);
      Mqd := Open_Or_Create (TP.Valid_MQ_Name (9),
       POSIX_IO.Read_Only,
       Owner_Permission_Set, POSIX_IO.Exclusive);
      Expect_Exception ("A047: POSIX_ERROR, File_Exists");
   exception
   when E1 : POSIX_Error =>
      Optional (Message_Queues_Option,
        Operation_Not_Implemented, File_Exists, E1, "A048");
   when E2 : others => Unexpected_Exception (E2, "A049");
   end;

   begin
      Test ("File_Exists Error, second version");
      Mqd := Open_Or_Create (TP.Valid_MQ_Name (9),
       POSIX_IO.Read_Only,
       Owner_Permission_Set, POSIX_IO.Exclusive, Attr);
      Expect_Exception ("A050: POSIX_ERROR, File_Exists");
   exception
   when E1 : POSIX_Error =>
      Optional (Message_Queues_Option,
        Operation_Not_Implemented, File_Exists, E1, "A051");
   when E2 : others => Unexpected_Exception (E2, "A052");
   end;

   ------------------------------------------------------------------------
   --  The Bad_File_Descriptor error code is given when
   --  attempting to close an invalid message queue.

   declare
      QD : Message_Queue_Descriptor;
   begin
      Test ("Bad File Descriptor Error");
      QD := POSIX_Message_Queues.Open (TP.Valid_MQ_Name (9),
       POSIX_IO.Read_Only, Empty_Set);
      Close (QD);
      POSIX_Message_Queues.Close (QD);
      Expect_Exception ("A053: POSIX_ERROR, Bad_File_Descriptor");
   exception
   when E1 : POSIX_Error =>
      Optional (Message_Queues_Option,
        Operation_Not_Implemented, Bad_File_Descriptor, E1, "A054");
   when E2 : others => Unexpected_Exception (E2, "A055");
   end;

   ------------------------------------------------------------------------
   --  The No_Such_File_Or_Directory error code is given when
   --  attempting to unlink a nonexistent message queue.

   begin
      Test ("No_Such_File_Or_Directory Error");
      Unlink_Message_Queue (TP.Valid_MQ_Name (10));
      Expect_Exception ("A056: POSIX_Error, No_Such_File_Or_Directory");
   exception
   when E1 : POSIX_Error =>
      Optional (Message_Queues_Option,
        Operation_Not_Implemented, No_Such_File_Or_Directory, E1, "A057");
   when E2 : others => Unexpected_Exception (E2, "A058");
   end;

   ------------------------------------------------------------------------
   --  The Bad_File_Descriptor error code is given when
   --  attempting to send to an invalid message queue.

   declare
      Uninitialized_QD : Message_Queue_Descriptor;
      pragma Warnings (Off, Uninitialized_QD);
   begin
      Test ("Send to Invalid Message Queue");
      Send (Uninitialized_QD, To_Stream_Element_Array ("Hello....."), 0);
      Expect_Exception ("A059: POSIX_ERROR, Bad_File_Descriptor");
   exception
   when E1 : POSIX_Error =>
      Optional (Message_Queues_Option,
        Operation_Not_Implemented, Bad_File_Descriptor, E1, "A060");
   when E2 : others => Unexpected_Exception (E2, "A061");
   end;

   ------------------------------------------------------------------------
   --  The Invalid_Argument error code is given when
   --  attempting to send a message with a priority that is too high.

   declare
      Mqd : Message_Queue_Descriptor;
   begin
      Test ("Invalid_Argument Error");
      Mqd := Open_Or_Create (TP.Valid_MQ_Name (9),
       Write_Only, Owner_Permission_Set, Empty_Set, Attr);
      Send (Mqd, To_Stream_Element_Array ("Hello....."),
       POSIX_Configurable_System_Limits.Message_Priority_Maximum + 1);
      Expect_Exception ("A062: POSIX_ERROR, Invalid_Argument");
   exception
   when E1 : POSIX_Error =>
      Optional (Message_Queues_Option,
        Operation_Not_Implemented, Invalid_Argument, E1, "A063");
   when Constraint_Error =>
      Assert (POSIX_Configurable_System_Limits.Message_Priority_Maximum <=
        POSIX_Limits.Message_Priority_Maxima'Last, "A064");
   when E2 : others => Unexpected_Exception (E2, "A065");
   end;

   ------------------------------------------------------------------------
   --  The Message_Too_Long error code is given when attempting
   --  to send a message that is longer than the maximum length.

   begin
      Test ("Message_Too_Long Error");
      Set_Options (Attr, POSIX_Message_Queues.Non_Blocking);
      Set_Max_Messages (Attr, 10);
      Set_Message_Length (Attr, 1);
      Mqd := Open_Or_Create (TP.Valid_MQ_Name (10),
       Write_Only, Owner_Permission_Set,
       POSIX_IO.Non_Blocking, Attr);
      Comment ("Sending excessively long message");
      Send (Mqd, To_Stream_Element_Array ("Hello....."), 1);
      Expect_Exception ("A066: POSIX_ERROR, Message_Too_Long");
   exception
   when E1 : POSIX_Error =>
      Optional (Message_Queues_Option,
        Operation_Not_Implemented, Message_Too_Long, E1, "A067");
   when E2 : others => Unexpected_Exception (E2, "A068");
   end;

   ------------------------------------------------------------------------
   --  The Bad_File_Descriptor error code is given when
   --  attempting to receive from an invalid message queue.

   declare
      QD : Message_Queue_Descriptor;
      AR : Stream_Element_Array (1 .. 10);
      Last : Ada_Streams.Stream_Element_Offset;
      Prio : Integer;
   begin
      Test ("Receive Invalid Message Queue");
      Mqd := Open (TP.Valid_MQ_Name (10), Write_Only);
      Close (QD);
      Receive (QD, AR, Last, Prio);
      Expect_Exception ("A069: POSIX_ERROR, Bad_File_Descriptor");
   exception
   when E1 : POSIX_Error =>
      Optional (Message_Queues_Option,
        Operation_Not_Implemented, Bad_File_Descriptor, E1, "A070");
   when E2 : others => Unexpected_Exception (E2, "A071");
   end;

   ------------------------------------------------------------------------
   --  The Message_Too_Long error code is given when attempting
   --  to receive a message into an array that is shorter than the
   --  Message Length attribute of the message queue.

   declare
      AR : Stream_Element_Array (1 .. 2);
      Last : Ada_Streams.Stream_Element_Offset;
      Prio : Integer;
   begin
      Test ("Receive to Short Array");
      Set_Message_Length (Attr, 10);
      Mqd := Open_Or_Create (TP.Valid_MQ_Name (11),
       Read_Write, Owner_Permission_Set, Empty_Set, Attr);
      Send (Mqd, To_Stream_Element_Array ("Hello....."), 1);
      Receive (Mqd, AR, Last, Prio);
      Expect_Exception ("A072: POSIX_Error, Message_Too_Long");
   exception
   when E1 : POSIX_Error =>
      Optional (Message_Queues_Option,
        Operation_Not_Implemented, Message_Too_Long, E1, "A073");
   when E2 : others => Unexpected_Exception (E2, "A074");
   end;

   ------------------------------------------------------------------------
   --  The Generic_Message_Passing package can be used.

   declare
      package Int_Queues is new Generic_Message_Queues (Integer);
      GQD : Message_Queue_Descriptor;
      Local_Attr : Attributes;
      SI : Integer;
      RI : Integer;
      Prio : Integer;
   begin
      Test ("Generic Message Queues [15.1.7]");
      SI := 2;
      Prio := 1;
      Set_Max_Messages (Local_Attr, 10);
      Set_Message_Length (Local_Attr, 4);
      GQD := Open_Or_Create (TP.Valid_MQ_Name (12),
       Read_Write,
       Owner_Permission_Set, Empty_Set,
       Local_Attr);
      Local_Attr := Get_Attributes (GQD);
      Comment ("Max Messages = " &
        Integer'Image (Get_Max_Messages (Local_Attr)));
      Comment ("Message Length = " &
        Integer'Image (Get_Message_Length (Local_Attr)));
      Int_Queues.Send (GQD, SI, 1);
      Comment ("Message sent");
      Int_Queues.Receive (GQD, RI, Prio);
      Comment ("Message received");
      Assert (RI = SI, "A075: integer corrupted during transmission");
      Unlink_Message_Queue (TP.Valid_MQ_Name (12));
   exception
   when E1 : POSIX_Error =>
      Optional (Message_Queues_Option,
        Operation_Not_Implemented, E1, "A076");
   when E2 : others => Unexpected_Exception (E2, "A077");
   end;

   ------------------------------------------------------------------------

   --  Loop over the array of MQ names used, and unlink them all.

   for I in 1 .. 12 loop
      begin
         Unlink_Message_Queue (TP.Valid_MQ_Name (I));
      exception
      when E1 : POSIX_Error =>
         Optional (Message_Queues_Option,
           Operation_Not_Implemented, No_Such_File_Or_Directory, E1, "A078");
      when E2 : others => Unexpected_Exception (E2, "A079");
      end;
   end loop;

   ------------------------------------------------------------------------

   Done;

exception
when E : others => Fatal_Exception (E, "A080");
end p150100;

------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                             P 1 5 0 1 0 1                                --
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

--  Test package POSIX_Message_Queues
--  in IEEE Std 1003.5b Section 15.1.

--  This is an test of Message Queues based on a possible
--  application usage model.  It does not try to exercise the whole
--  package.  It just checks that Message Queues can be used to
--  communicate between tasks.

--  The basic concept of the test is to simulate a bank,
--  via a set of "customer" tasks (i.e., clients) and a
--  smaller set of "teller" tasks (i.e., servers).
--  Each customer requires one service.
--  (The customers and tellers are collectively termed the "players".)
--  Message Queues are used to enforce an orderly service discipline,
--  so that each teller serves one customer at a time.

--  .... This test is currently not working with Florist.
--  The behavior is that messages seem to be lost.
--  We have not yet determined whether the fault is in Florist
--  or in the test.

with Ada_Streams,
     POSIX,
     POSIX_IO,
     POSIX_Limits,
     POSIX_Message_Queues,
     POSIX_Permissions,
     POSIX_Report,
     POSIX_Semaphores,
     System,
     Test_Parameters,
     Unchecked_Conversion;

procedure p150101 is

   use Ada_Streams,
       POSIX,
       POSIX_IO,
       POSIX_Message_Queues,
       POSIX_Permissions,
       POSIX_Report,
       POSIX_Semaphores;

   package TP renames Test_Parameters;

   Attr    : Attributes;
   Prio    : Message_Priority;
   Last    : Stream_Element_Offset;
   Buffer  : Stream_Element_Array (1 .. 8);

   Num_Customers : constant := 100;
   Num_Tellers   : constant := 5;

   Null_Player : constant := 0;
   Num_Players : constant := Num_Customers + Num_Tellers + 3;
   type Player_ID is range Null_Player .. Num_Players;

   Main_Program : constant Player_ID := 1;
   subtype Teller_ID is Player_ID
     range  Main_Program + 1 .. Num_Tellers;
   subtype Customer_ID is Player_ID
     range Teller_ID'Last + 1 .. Player_ID'Last;

   task type Customer is
      entry Start (ID : Customer_ID);
   end Customer;

   task type Teller is
      entry Start (ID : Teller_ID);
   end Teller;

   Tellers : array (Teller_ID) of Teller;
   Customers : array (Customer_ID) of Customer;
   Customer_Count : Integer := 0;

   --  In case we can't open enough queues for all the
   --  players, we scale back to Last_Queue.
   Last_Queue : Player_ID;

   Teller_Queue : Message_Queue_Descriptor;
   Exit_Queue   : Message_Queue_Descriptor;
   Player_Waits :
    array (Teller_ID'First .. Customer_ID'Last) of Message_Queue_Descriptor;

   Wait_QID         : constant Integer := 1;
   Finish_QID       : constant Integer := 4;

   -----------------------------------
   -- Player_ID_Message_Conversions --
   -----------------------------------

   package Player_ID_MSG_Conversions is
      Player_ID_Length : constant Stream_Element_Offset;
      function To_Player_ID
        (Buffer : in Stream_Element_Array)
        return Player_ID;
      function To_Stream_Element_Array
        (ID : in Player_ID)
        return Stream_Element_Array;
      procedure Put_Player_ID
        (Buffer : out Stream_Element_Array;
         ID : in Player_ID);
   private
      Truncation : constant Boolean :=
        (Player_ID'Size / Stream_Element'Size) * Stream_Element'Size
         /= Player_ID'Size;
      Player_ID_Length : constant Stream_Element_Offset :=
         Player_ID'Size / Stream_Element'Size + Boolean'Pos (Truncation);
   end Player_ID_MSG_Conversions;

   package body Player_ID_MSG_Conversions is

      type PID_Ptr is access Player_ID;
      function To_PID_Ptr is
        new Unchecked_Conversion (System.Address, PID_Ptr);

      function To_Player_ID
        (Buffer : in Stream_Element_Array)
        return Player_ID is
      begin
         Assert (Buffer'Length = Player_ID_Length,
           "A001: Buffer'Length ="
           & Stream_Element_Offset'Image (Buffer'Length));
         return To_PID_Ptr (Buffer (Buffer'First)'Address).all;
      end To_Player_ID;

      procedure Put_Player_ID
        (Buffer : out Stream_Element_Array;
         ID : in Player_ID) is
      begin
         Assert (Buffer'Length = Player_ID_Length, "A002");
         To_PID_Ptr (Buffer (Buffer'First)'Address).all := ID;
      end Put_Player_ID;

      function To_Stream_Element_Array
        (ID : in Player_ID)
        return Stream_Element_Array is
         Buffer : Stream_Element_Array (1 .. Player_ID_Length);
      begin
         To_PID_Ptr (Buffer (Buffer'First)'Address).all := ID;
         return Buffer;
      end To_Stream_Element_Array;

   end Player_ID_MSG_Conversions;

   use Player_ID_MSG_Conversions;

   type Teller_Player_Array is array (Teller_ID) of Player_ID;

   -------
   -- P --
   -------

   --  Checks for duplicate messages.

   protected P is
      procedure Serve (CID : Customer_ID; TID : Teller_ID);
      procedure End_Serve (CID : Customer_ID; TID : Teller_ID);
      procedure Claim (CID : Customer_ID; TID : Teller_ID);
   private
      Serving : Teller_Player_Array := (others => Null_Player);
   end P;

   protected body P is

      procedure Serve (CID : Customer_ID; TID : Teller_ID) is
      begin
         Assert (Serving (TID) = CID, "A003: wrong customer");
      end Serve;

      procedure End_Serve (CID : Customer_ID; TID : Teller_ID) is
      begin
         Assert (Serving (TID) = CID, "A004: wrong customer");
         Serving (TID) := Null_Player;
      end End_Serve;

      procedure Claim (CID : Customer_ID; TID : Teller_ID) is
      begin
         Assert (Serving (TID) = Null_Player, "A005: double claim");
         Serving (TID) := CID;
      end Claim;

   end P;

   -----------
   -- Cmmnt --
   -----------

   procedure Cmmnt (ID : Player_ID; Message : String);

   procedure Cmmnt (ID : Player_ID; Message : String) is
   begin
      if ID in Teller_ID then
         Comment ("Teller" & Player_ID'Image (ID) & " " & Message);
      else
         Comment ("Customer" & Player_ID'Image (ID) & " " & Message);
      end if;
   end Cmmnt;

   --------------
   -- Shutdown --
   --------------

   --  Shut down all the Customer and Teller tasks.

   procedure Shutdown (Self : Player_ID);

   procedure Shutdown (Self : Player_ID) is
   begin
      for I in Customer_ID loop
         if Self /= I then abort Customers (I);
         end if;
      end loop;
      for I in Teller_ID loop
         if Self /= I then abort Tellers (I);
         end if;
      end loop;
      if Self in Teller_ID then abort Tellers (Self);
      elsif Self in Customer_ID then abort Customers (Self);
      end if;
   end Shutdown;

   ------------
   -- Customer --
   ------------

   task body Customer is
      Self      : Customer_ID;
      My_Teller : Teller_ID;
      Prio      : Message_Priority;
      Last      : Stream_Element_Offset;
      Buffer    : Stream_Element_Array (1 .. 8);
      My_Wait_Queue : Message_Queue_Descriptor;
   begin
      --  Customer waits to be assigned an ID
      accept Start (ID : Customer_ID) do
         Self := ID;
      end Start;
      My_Wait_Queue := Player_Waits (Self);
      Cmmnt (Self, "waits for available teller");
      Receive (Teller_Queue, Buffer, Last, Prio);
      My_Teller := To_Player_ID (Buffer (1 .. Last));
      Cmmnt (Self, "wakes up Teller"
        & Player_ID'Image (My_Teller));
      P.Claim (Self, My_Teller);
      Send (Player_Waits (My_Teller), To_Stream_Element_Array (Self), 1);
      Cmmnt (Self, "waits for teller to perform service");
      Receive (My_Wait_Queue, Buffer, Last, Prio);
      Cmmnt (Self, "leaves the bank");
      Send (Exit_Queue, To_Stream_Element_Array (Self), 1);
   exception
   when E : others =>
      Shutdown (Self);
      Fatal_Exception (E, "A006: in Customer" & Player_ID'Image (Self));
   end Customer;

   ------------
   -- Teller --
   ------------

   task body Teller is
      Self        : Teller_ID;
      My_Customer : Player_ID;
      Prio   : Message_Priority;
      Last   : Stream_Element_Offset;
      Buffer : Stream_Element_Array (1 .. 8);
      My_Wait_Queue : Message_Queue_Descriptor;
   begin
      --  Teller waits to be assigned an ID
      accept Start (ID : Teller_ID) do
         Self := ID;
      end Start;
      My_Wait_Queue := Player_Waits (Self);
      loop
         Cmmnt (Self, "opens for business");
         Send (Teller_Queue, To_Stream_Element_Array (Self), 1);
         Cmmnt (Self, "waits for a customer to show up");
         Receive (My_Wait_Queue, Buffer, Last, Prio);
         My_Customer := To_Player_ID (Buffer (1 .. Last));
         exit when My_Customer = Null_Player;
         P.Serve (Self, My_Customer);
         Cmmnt (Self, "delays, serving Customer"
           & Player_ID'Image (My_Customer));
         delay Duration (Self) * Duration'(0.001);
         P.End_Serve (Self, My_Customer);
         Cmmnt (Self, "wakes up the customer");
         Send (Player_Waits (My_Customer),
          To_Stream_Element_Array ("Go Ahead"), 1);
      end loop;
   exception
   when E : others =>
      Shutdown (Self);
      Fatal_Exception (E, "A007: in Teller" & Player_ID'Image (Self));
   end Teller;

   --------------
   -- Watchdog --
   --------------

   task Watchdog;

   task body Watchdog is
   begin
      delay 15.0;
      Fatal ("A008: watchdog timeout");
   end Watchdog;

begin

   Header ("p150101");

   -----------------------------------------------------------------------

   Test ("Use Message Queues to synchronize Ada tasks.");

   declare
      EC : Error_Code;
   begin
      Comment ("Initialize message queues");
      Set_Message_Length (Attr, 8);
--      Set_Max_Messages (Attr, Integer (Num_Tellers));
      Set_Max_Messages (Attr, 200);
      Teller_Queue := Open_Or_Create (TP.Valid_MQ_Name (Wait_QID),
        Read_Write, Owner_Permission_Set,
        POSIX_IO.Empty_Set,
        Attr, POSIX.RTS_Signals);
--      Set_Max_Messages (Attr, Integer (Num_Customers));
      Exit_Queue := Open_Or_Create (TP.Valid_MQ_Name (Finish_QID),
        Read_Write,
        Owner_Permission_Set,
        POSIX_IO.Empty_Set,
        Attr, POSIX.RTS_Signals);
--      Set_Max_Messages (Attr, 1);
      begin
         Last_Queue := Player_Waits'First - 1;
         while Last_Queue < Player_Waits'Last loop
            Last_Queue := Last_Queue + 1;
            Player_Waits (Last_Queue) := Open_Or_Create
             (TP.Valid_MQ_Name (Finish_QID + Integer (Last_Queue)),
              Read_Write, Owner_Permission_Set,
              POSIX_IO.Empty_Set,
              Attr, POSIX.RTS_Signals);
         end loop;
      exception
      when POSIX_Error =>
         EC := Get_Error_Code;
         if EC = Too_Many_Open_Files_In_System or
            EC = Too_Many_Open_Files or
            EC = No_Space_Left_On_Device then
            Comment ("Failed to create queue");
            Assert (Integer (Last_Queue) + 2 >
              POSIX_Limits.Portable_Open_Message_Queues_Maximum, "A009");
         else raise;
         end if;
      end;
   exception
   when E1 : POSIX_Error =>
      Optional (Message_Queues_Option, Operation_Not_Implemented,
       E1, "A010");
   when E2 : others =>
      Shutdown (Main_Program);
      Fatal_Exception (E2, "A011: Queue creation");
   end;

   -----------------------------------------------------------------------

   begin
      Comment ("Start all players");
      for I in Teller_ID'Range loop
         Tellers (I).Start (I);
      end loop;
      for I in Customer_ID'First .. Last_Queue loop
         Customers (I).Start (I);
         Customer_Count := Customer_Count + 1;
         Comment ("Customer arrived. Customer count ="
          & Integer'Image (Customer_Count));
      end loop;
      if Last_Queue < Customer_ID'Last then
         Comment ("Send away" & Player_ID'Image (Customer_ID'Last - Last_Queue)
           & " extra customers");
         for I in Last_Queue + 1 .. Customer_ID'Last loop
            abort Customers (I);
            Customer_Count := Customer_Count - 1;
            Comment ("Customer gave up. Customer count ="
             & Integer'Image (Customer_Count));
         end loop;
      end if;
   exception
   when E1 : POSIX_Error =>
      Optional (Message_Queues_Option, Operation_Not_Implemented,
        E1, "A012");
   when E2 : others =>
      Shutdown (Main_Program);
      Fatal_Exception (E2, "A013 : Startup");
   end;

   -----------------------------------------------------------------------

   begin
      Comment ("Wait for all customers to finish");
      for I in Customer_ID'First .. Last_Queue loop
         Receive (Exit_Queue, Buffer, Last, Prio);
         Assert (Last = Player_ID_Length, "A014");
         Customer_Count := Customer_Count - 1;
         Comment ("Customer"
           & Player_ID'Image
             (To_Player_ID (Buffer (1 .. Player_ID_Length)))
           & " left. Customers count ="
           & Integer'Image (Customer_Count));
      end loop;
   exception
   when E1 : POSIX_Error =>
      Optional (Message_Queues_Option, Operation_Not_Implemented,
        E1, "A015");
   when E2 : others =>
      Shutdown (Main_Program);
      Fatal_Exception (E2, "A016: Waiting for finish");
   end;

   -----------------------------------------------------------------------

   begin
      Comment ("Wake up tellers to exit");
      for I in Tellers'Range loop
         Send (Player_Waits (I), To_Stream_Element_Array (Null_Player), 1);
      end loop;
   exception
   when E1 : POSIX_Error =>
      Optional (Message_Queues_Option, Operation_Not_Implemented,
        E1, "A017");
   when E2 : others =>
      Shutdown (Main_Program);
      Fatal_Exception (E2, "A018: Waking up tellers to exit");
   end;

   -----------------------------------------------------------------------

   begin
      Comment ("Wait for all players to terminate");
      for I in Tellers'Range loop
         while not Tellers (I)'Terminated loop
            delay 0.01;
         end loop;
      end loop;
      for I in Customers'Range loop
         while not Customers (I)'Terminated loop
            delay 0.01;
         end loop;
      end loop;
   exception
   when E1 : POSIX_Error =>
      Optional (Message_Queues_Option, Operation_Not_Implemented,
        E1, "A019");
   when E2 : others =>
      Shutdown (Main_Program);
      Fatal_Exception (E2, "A020: Waking up tellers to exit");
   end;

   -----------------------------------------------------------------------

   begin
      Comment ("Unlink message queues");
      for I in Wait_QID .. Integer (Last_Queue) loop
         Unlink_Message_Queue (TP.Valid_MQ_Name (I));
      end loop;
   exception
   when E1 : POSIX_Error =>
      Optional (Message_Queues_Option, Operation_Not_Implemented,
        E1, "A021");
   when E2 : others =>
      Shutdown (Main_Program);
      Fatal_Exception (E2, "A022: Unlinking message queues");
   end;

   -----------------------------------------------------------------------

   abort Watchdog;
   Done;

exception
when E : others =>
   Shutdown (Main_Program);
   Fatal_Exception (E, "A023");
end p150101;

------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5c VALIDATION TEST SUITE                      --
--                                                                          --
--                             P 1 8 0 4 0 2                                --
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

--  Multiprocess integrated test for package POSIX_Sockets
--  in IEEE Std 1003.5c Section 18.4 with package
--  POSIX_Sockets_Interent.

--  This test covers only features that depend only on
--  the packages (POSIX_Sockets/_Interent) and features from
--  other packages that are required to be supported.

with POSIX,
     POSIX_Sockets,
     POSIX_Sockets_Internet,
     POSIX_Report,
     POSIX_IO,
     POSIX_Process_Primitives,
     Unchecked_Conversion,
--     Test_Parameters,
     System,
     POSIX_Process_Environment,
     POSIX_Process_Identification;

--  with Ada.Integer_Text_IO;

--  with Ada.Text_IO; use Ada.Text_IO;


procedure p180402 is

   use POSIX,
       POSIX_Sockets,
       POSIX_Sockets_Internet,
       POSIX_IO,
       POSIX_Process_Primitives,
       POSIX_Process_Identification,
       POSIX_Process_Environment,
--       Test_Parameters,
       POSIX_Report;

   --  These are declared in Posix.C but not intended to be used from there
   PF_MAX    : constant := 25;
   PF_UNSPEC : constant := 0;
   PF_LOCAL  : constant := 1;
   PF_UNIX   : constant := 1;
   PF_INET   : constant := 2;
   PF_OSI    : constant := 19;
   --  *** MISSING: PF_ISO ***  --
   PF_ISO    : constant := 0;

   IPPROTO_IP   : constant := 0;
   IPPROTO_ICMP : constant := 1;
   IPPROTO_TCP  : constant := 6;
   IPPROTO_UDP  : constant := 17;
   IPPROTO_RAW  : constant := 255;


   procedure Accept_Connection_Procedure (
      Socket       : in POSIX_IO.File_Descriptor;
      Address      : in Socket_Address_Pointer
                   := Null_Socket_Address;
      Expected     : in Error_Code := No_Error;
      Er1, Er2, Er3, Er4 : in String);

   procedure Accept_Connection_Function (
      Socket       : in POSIX_IO.File_Descriptor;
      Expected     : in Error_Code := No_Error;
      Er1, Er2, Er3, Er4 : in String);

   procedure Receive1_Tests (
      Socket   : in POSIX_IO.File_Descriptor;
      Options  : in Message_Option_Set := Empty_Set;
      Expected : in Error_Code := No_Error;
      Er1, Er2, Er3, Er4 : in String);

   procedure Receive2_Tests (
      Socket   : in POSIX_IO.File_Descriptor;
      Options  : in Message_Option_Set := Empty_Set;
      Expected : in Error_Code := No_Error;
      Er1, Er2, Er3, Er4 : in String);

   procedure Receive3_Tests (
      Socket   : in POSIX_IO.File_Descriptor;
      Options  : in Message_Option_Set := Empty_Set;
      From     : in Socket_Address_Pointer
               := Null_Socket_Address;
      Expected : in Error_Code := No_Error;
      Er1, Er2, Er3, Er4 : in String);

   procedure Receive4_Tests (
      Socket   : in POSIX_IO.File_Descriptor;
      Options  : in Message_Option_Set := Empty_Set;
      From     : in Socket_Address_Pointer
               := Null_Socket_Address;
      Expected : in Error_Code := No_Error;
      Er1, Er2, Er3, Er4 : in String);

   procedure Receive_Message1_Tests (
      Socket   : in POSIX_IO.File_Descriptor;
      Message  : in out Socket_Message;
      Options  : in Message_Option_Set := Empty_Set;
      Expected : in Error_Code := No_Error;
      Er1, Er2, Er3, Er4 : in String);

   procedure Receive_Message2_Tests (
      Socket   : in POSIX_IO.File_Descriptor;
      Message  : in out Socket_Message;
      Options  : in Message_Option_Set := Empty_Set;
      Expected : in Error_Code := No_Error;
      Er1, Er2, Er3, Er4 : in String);

   procedure Accept_Connection_Procedure (
      Socket   : in POSIX_IO.File_Descriptor;
      Address  : in Socket_Address_Pointer
               := Null_Socket_Address;
      Expected : in Error_Code := No_Error;
      Er1, Er2, Er3, Er4 : in String)
   is
      Connection_Socket : POSIX_IO.File_Descriptor := 0;
      Int_Add           : aliased Internet_Socket_Address;
      Connect_Name      : Socket_Address_Pointer :=
         +(Int_Add'Unchecked_Access);
   begin
      if Address /= Null_Socket_Address then
         Connect_Name := Address;
      end if;
      Accept_Connection (Socket, Connection_Socket, Connect_Name);
      if Er1 /= "A000" then
         Expect_Exception (Er1);
      else
         Assert (Connection_Socket /= 0, Er2);
      end if;
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code /= Expected then
         Unexpected_Exception (E1, Er3);
      end if;
   when E2 : others => Unexpected_Exception (E2, Er4);
   end Accept_Connection_Procedure;

   procedure Accept_Connection_Function (
      Socket   : in POSIX_IO.File_Descriptor;
      Expected : in Error_Code := No_Error;
      Er1, Er2, Er3, Er4 : in String)
   is
      Connection_Socket : POSIX_IO.File_Descriptor := 0;
   begin
      Connection_Socket := Accept_Connection (Socket);
      if Er1 /= "A000" then
         Expect_Exception (Er1);
      else
         Assert (Connection_Socket /= 0, Er2);
      end if;
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code /= Expected then
         Unexpected_Exception (E1, Er3);
      end if;
   when E2 : others => Unexpected_Exception (E2, Er4);
   end Accept_Connection_Function;

   procedure Receive1_Tests (
      Socket    : in POSIX_IO.File_Descriptor;
      Options   : in Message_Option_Set := Empty_Set;
      Expected  : in Error_Code := No_Error;
      Er1, Er2, Er3, Er4 : in String)
   is
      Octets    : POSIX_String (1 .. 40);
      Buffer    : System.Address := Octets'Address;
      Requested : POSIX.IO_Count := 40;
      Received  : POSIX.IO_Count := 0;
      Mask      : POSIX.Signal_Masking := All_Signals;
   begin
      Comment ("Receive 1");
      Receive (Socket, Buffer, Requested, Received, Mask, Options);
      Comment ("received octets = " &
         Integer'Image (Integer (Received)));
      if Er1 = "A000" then
         Assert (Received = 11 and Octets (1 .. 11) = "Send 1 test", Er2);
      else
         Expect_Exception (Er1);
      end if;
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code /= Expected then
         Unexpected_Exception (E1, Er3);
      end if;
   when E2 : others => Unexpected_Exception (E2, Er4);
   end Receive1_Tests;

   procedure Receive2_Tests (
      Socket    : in POSIX_IO.File_Descriptor;
      Options   : in Message_Option_Set := Empty_Set;
      Expected  : in Error_Code := No_Error;
      Er1, Er2, Er3, Er4 : in String)
   is
      Octets    : POSIX_String (1 .. 40);
      Buffer    : System.Address := Octets'Address;
      Requested : POSIX.IO_Count := 40;
      Received  : POSIX.IO_Count := 0;
   begin
      Comment ("Receive 2");
      Receive (Socket, Buffer, Requested, Received, Options);
      Comment ("received octets = " &
         Integer'Image (Integer (Received)));
      if Er1 = "A000" then
         Assert (Received = 11 and Octets (1 .. 11) = "Send 2 test", Er2);
      else
         Expect_Exception (Er1);
      end if;
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code /= Expected then
         Unexpected_Exception (E1, Er3);
      end if;
   when E2 : others => Unexpected_Exception (E2, Er4);
   end Receive2_Tests;

   procedure Receive3_Tests (
      Socket   : in POSIX_IO.File_Descriptor;
      Options  : in Message_Option_Set := Empty_Set;
      From     : in Socket_Address_Pointer
               := Null_Socket_Address;
      Expected : in Error_Code := No_Error;
      Er1, Er2, Er3, Er4 : in String)
   is
      Octets    : POSIX_String (1 .. 40);
      Buffer    : System.Address := Octets'Address;
      Requested : POSIX.IO_Count := 40;
      Received  : POSIX.IO_Count := 0;
      Mask      : POSIX.Signal_Masking := All_Signals;
   begin
      Comment ("Receive 3");
      Receive (Socket, Buffer, Requested, Received, From,
         Mask, Options);
      Comment ("received octets = " &
         Integer'Image (Integer (Received)));
      if Er1 = "A000" then
         Assert (Received = 11 and Octets (1 .. 11) = "Send 3 test", Er2);
      else
         Expect_Exception (Er1);
      end if;
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code /= Expected then
         Unexpected_Exception (E1, Er3);
      end if;
   when E2 : others => Unexpected_Exception (E2, Er4);
   end Receive3_Tests;

   procedure Receive4_Tests (
      Socket   : in POSIX_IO.File_Descriptor;
      Options  : in Message_Option_Set := Empty_Set;
      From     : in Socket_Address_Pointer
               := Null_Socket_Address;
      Expected : in Error_Code := No_Error;
      Er1, Er2, Er3, Er4 : in String)
   is
      Octets    : POSIX_String (1 .. 40);
      Buffer    : System.Address := Octets'Address;
      Requested : POSIX.IO_Count := 40;
      Received  : POSIX.IO_Count := 0;
   begin
      Comment ("Receive 4");
      Receive (Socket, Buffer, Requested, Received, From,
         Options);
      Comment ("received octets = " &
         Integer'Image (Integer (Received)));
      if Er1 = "A000" then
         Assert (Received = 11 and Octets (1 .. 11) = "Send 4 test", Er2);
      else
         Expect_Exception (Er1);
      end if;
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code /= Expected then
         Unexpected_Exception (E1, Er3);
      end if;
   when E2 : others => Unexpected_Exception (E2, Er4);
   end Receive4_Tests;

   procedure Receive_Message1_Tests (
      Socket   : in POSIX_IO.File_Descriptor;
      Message  : in out Socket_Message;
      Options  : in Message_Option_Set := Empty_Set;
      Expected : in Error_Code := No_Error;
      Er1, Er2, Er3, Er4 : in String)
   is
      Received : POSIX.IO_Count := 0;
      Mask     : POSIX.Signal_Masking := All_Signals;
   begin
      Comment ("Receive Message 1");
      Receive_Message (Socket, Message, Received, Mask, Options);
      Comment ("received octets = " &
         Integer'Image (Integer (Received)));
      if Er1 = "A000" then
         Assert (Received /= 0, Er2);
      else
         Expect_Exception (Er1);
      end if;
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code /= Expected then
         Unexpected_Exception (E1, Er3);
      end if;
   when E2 : others => Unexpected_Exception (E2, Er4);
   end Receive_Message1_Tests;

   procedure Receive_Message2_Tests (
      Socket   : in POSIX_IO.File_Descriptor;
      Message  : in out Socket_Message;
      Options  : in Message_Option_Set := Empty_Set;
      Expected : in Error_Code := No_Error;
      Er1, Er2, Er3, Er4 : in String)
   is
      Received : POSIX.IO_Count := 0;
   begin
      Comment ("Receive Message 1");
      Receive_Message (Socket, Message, Received, Options);
      Comment ("received octets = " &
         Integer'Image (Integer (Received)));
      if Er1 = "A000" then
         Assert (Received /= 0, Er2);
      else
         Expect_Exception (Er1);
      end if;
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code /= Expected then
         Unexpected_Exception (E1, Er3);
      end if;
   when E2 : others => Unexpected_Exception (E2, Er4);
   end Receive_Message2_Tests;


--------------------------------------------------------------------------
--  Begin Tests

begin
   Header ("p180402");
   Test ("package POSIX.Sockets");

   -----------------------------------------------------------------------
   --  Accept an incoming connection

   Test ("Accept_Connection Procedure [18.4.2]");
   declare
      Socket   : POSIX_IO.File_Descriptor;
      Port     : Positive;
      Template : Process_Template;
      Child    : Process_ID;
      List     : POSIX_String_List;
      Int_Add  : aliased Internet_Socket_Address;
      Name     : Internet_Socket_Address_Pointer :=
         Int_Add'Unchecked_Access;
   begin
      Set_Internet_Port (Int_Add, Unspecified_Internet_Port);
      Set_Internet_Address (Int_Add,
         String_To_Internet_Address ("127.0.0.1"));
      Socket := Create (PF_INET, Stream_Socket, IPPROTO_TCP);
      Bind (Socket, +Name);
      Port := Integer (Get_Internet_Port (Get_Socket_Name (Socket)));
      Listen (Socket);
      POSIX.Append (List, "p180402a");

      --  Have to send the second argument to the secondary program
      --  exeception simple takes care of the case where no argument
      --  was entered.
      begin
         POSIX.Append (List, Value (Argument_List, 2));
      exception
         when Constraint_Error => POSIX.Append (List, " ");
      end;

      POSIX.Append (List, To_POSIX_String (Integer'Image
         (Port)));
      Comment ("port is " & Integer'Image (Port));
      Open_Template (Template);
      Start_Process (Child, "./p180402a", Template, List);
      Close_Template (Template);
      Accept_Connection_Procedure (Socket, Er1 => "A000",
         Er2 =>  "A001", Er3 => "A002", Er4 => "A003");
      Close (Socket);
   exception
       when E : others => Unexpected_Exception (E, "A004");
   end;

   -----------------------------------------------------------------------
   --  An address object of an incorrect type for the address format of
   --  this socket raises the Incorrect_Address_Type error code.

   --  ... Shoud be able to get the correct error when other protocols
   --  ... can be used.

   Test ("  Incorrect_Address_Type [18.4.2.3]");
   declare
      Socket  : POSIX_IO.File_Descriptor;
      Junk    : Internet_Address;
      Address : Socket_Address_Pointer;
      function To_Socket_Address_Pointer is new
         Unchecked_Conversion (System.Address,
         Socket_Address_Pointer);
      Int_Add : aliased Internet_Socket_Address;
      Name    : Internet_Socket_Address_Pointer :=
         Int_Add'Unchecked_Access;
   begin
      Address := To_Socket_Address_Pointer (Junk'Address);
      Set_Internet_Port (Int_Add, Unspecified_Internet_Port);
      Set_Internet_Address (Int_Add,
         String_To_Internet_Address ("127.0.0.1"));
      Socket := Create (PF_INET, Stream_Socket, IPPROTO_TCP);
      Bind (Socket, +Name);
      Listen (Socket);
      Accept_Connection_Procedure (Socket, Address => Address,
--         Expected => Incorrect_Address_Type,
         Er1 => "A005", Er2 =>  "A006", Er3 => "A007", Er4 => "A008");
      Close (Socket);
   end;

   -----------------------------------------------------------------------
   --  Trying to accept a connection on a closed socket results in the
   --  Bad_File_Descriptor error code.

   Test ("  Bad_File_Descriptor [18.4.2.3]");
   declare
      Socket  : POSIX_IO.File_Descriptor;
   begin
      Socket := Create (PF_INET, Stream_Socket, IPPROTO_TCP);
      Close (Socket);
      Accept_Connection_Procedure (Socket,
         Expected => Bad_File_Descriptor,
         Er1 => "A009", Er2 =>  "A0010", Er3 => "A011", Er4 => "A012");
   end;

   -----------------------------------------------------------------------
   --  Trying to accept a connection from a non listening socket
   --  raises the Invalid_Argument error code.

   Test ("  Invalid_Argument [18.4.2.3]");
   declare
      Socket  : POSIX_IO.File_Descriptor;
      Int_Add : aliased Internet_Socket_Address;
      Name    : Internet_Socket_Address_Pointer :=
         Int_Add'Unchecked_Access;
   begin
      Set_Internet_Port (Int_Add, Unspecified_Internet_Port);
      Set_Internet_Address (Int_Add,
         String_To_Internet_Address ("127.0.0.1"));
      Socket := Create (PF_INET, Stream_Socket, IPPROTO_TCP);
      Bind (Socket, +Name);
      Accept_Connection_Procedure (Socket,
         Expected => Invalid_Argument,
         Er1 => "A013", Er2 =>  "A014", Er3 => "A015", Er4 => "A016");
      Close (Socket);
   end;

   -----------------------------------------------------------------------
   --  Trying to accept a connection from a non-socket
   --  raises the Not_A_Socket error code.

   Test ("  Not_A_Socket [18.4.2.3]");
   declare
      Socket  : POSIX_IO.File_Descriptor := 0;
   begin
      Accept_Connection_Procedure (Socket,
         Expected => Not_A_Socket,
         Er1 => "A017", Er2 =>  "A018", Er3 => "A019", Er4 => "A020");
   end;

   -----------------------------------------------------------------------
   --  Trying to accept a connection from an socket that does not support
   --  the Accept_Connection operation raises the
   --  Option_Not_Supported error code.

   Test ("  Option_Not_Supported [18.4.2.3]");
   declare
      Socket  : POSIX_IO.File_Descriptor;
   begin
      Socket := Create (PF_INET, Datagram_Socket, IPPROTO_UDP);
      Accept_Connection_Procedure (Socket,
         Expected => Option_Not_Supported,
         Er1 => "A021", Er2 =>  "A022", Er3 => "A023", Er4 => "A024");
      Close (Socket);
   end;

   -----------------------------------------------------------------------
   --  A non-blocking socket will cause the Would_Block error code
   --  when Accept_Connection would normaly block.

   Test ("  Would_Block [18.4.2.3]");
   declare
      Socket   : POSIX_IO.File_Descriptor;
      Int_Add  : aliased Internet_Socket_Address;
      Name     : Internet_Socket_Address_Pointer :=
         Int_Add'Unchecked_Access;
   begin
      Set_Internet_Port (Int_Add, Unspecified_Internet_Port);
      Set_Internet_Address (Int_Add,
         String_To_Internet_Address ("127.0.0.1"));
      Socket := Create (PF_INET, Stream_Socket, IPPROTO_TCP);
      Set_File_Control (Socket, Non_Blocking);
      Bind (Socket, +Name);
      Listen (Socket);
      Accept_Connection_Procedure (Socket,
         Expected => Would_Block,
         Er1 => "A025", Er2 =>  "A026",
         Er3 => "A027", Er4 => "A028");
      Close (Socket);
   end;

   -----------------------------------------------------------------------
   --  Accept an incoming connection

   Test ("Accept_Connection Function [18.4.2]");
   declare
      Socket   : POSIX_IO.File_Descriptor;
      Port     : Positive;
      Template : Process_Template;
      Child    : Process_ID;
      List     : POSIX_String_List;
      Int_Add  : aliased Internet_Socket_Address;
      Name     : Internet_Socket_Address_Pointer :=
         Int_Add'Unchecked_Access;
   begin
      Set_Internet_Port (Int_Add, Unspecified_Internet_Port);
      Set_Internet_Address (Int_Add,
         String_To_Internet_Address ("127.0.0.1"));
      Socket := Create (PF_INET, Stream_Socket, IPPROTO_TCP);
      Bind (Socket, +Name);
      Port := Integer (Get_Internet_Port (Get_Socket_Name (Socket)));
      Listen (Socket);
      POSIX.Append (List, "p180402a");

      begin
         POSIX.Append (List, Value (Argument_List, 2));
      exception
         when Constraint_Error => POSIX.Append (List, " ");
      end;

      POSIX.Append (List, To_POSIX_String (Integer'Image
         (Port)));
      Comment ("port is " & Integer'Image (Port));
      Open_Template (Template);
      Start_Process (Child, "./p180402a", Template, List);
      Close_Template (Template);
      Accept_Connection_Function (Socket, Er1 => "A000",
         Er2 =>  "A029", Er3 => "A030", Er4 => "A031");
      Close (Socket);
   end;

   -----------------------------------------------------------------------
   --  Trying to accept a connection on a closed socket results in the
   --  Bad_File_Descriptor error code.

   Test ("  Bad_File_Descriptor [18.4.2.3]");
   declare
      Socket  : POSIX_IO.File_Descriptor;
   begin
      Socket := Create (PF_INET, Stream_Socket, IPPROTO_TCP);
      Close (Socket);
      Accept_Connection_Function (Socket,
         Expected => Bad_File_Descriptor,
         Er1 => "A032", Er2 =>  "A033", Er3 => "A034", Er4 => "A035");
   end;

   -----------------------------------------------------------------------
   --  Trying to accept a connection from a non listening socket
   --  raises the Invalid_Argument error code.

   Test ("  Invalid_Argument [18.4.2.3]");
   declare
      Socket  : POSIX_IO.File_Descriptor;
      Int_Add : aliased Internet_Socket_Address;
      Name    : Internet_Socket_Address_Pointer :=
         Int_Add'Unchecked_Access;
   begin
      Set_Internet_Port (Int_Add, Unspecified_Internet_Port);
      Set_Internet_Address (Int_Add,
         String_To_Internet_Address ("127.0.0.1"));
      Socket := Create (PF_INET, Stream_Socket, IPPROTO_TCP);
      Bind (Socket, +Name);
      Accept_Connection_Function (Socket,
         Expected => Invalid_Argument,
         Er1 => "A036", Er2 =>  "A037", Er3 => "A038", Er4 => "A039");
      Close (Socket);
   end;

   -----------------------------------------------------------------------
   --  Trying to accept a connection from a non-socket
   --  raises the Not_A_Socket error code.

   Test ("  Not_A_Socket [18.4.2.3]");
   declare
      Socket  : POSIX_IO.File_Descriptor := 0;
   begin
      Accept_Connection_Function (Socket,
         Expected => Not_A_Socket,
         Er1 => "A040", Er2 =>  "A041", Er3 => "A042", Er4 => "A043");
   end;

   -----------------------------------------------------------------------
   --  Trying to accept a connection from an socket that does not support
   --  the Accept_Connection operation raises the
   --  Option_Not_Supported error code.

   Test ("  Option_Not_Supported [18.4.2.3]");
   declare
      Socket  : POSIX_IO.File_Descriptor;
   begin
      Socket := Create (PF_INET, Datagram_Socket, IPPROTO_UDP);
      Accept_Connection_Function (Socket,
         Expected => Option_Not_Supported,
         Er1 => "A044", Er2 =>  "A045", Er3 => "A046", Er4 => "A047");
      Close (Socket);
   end;

   -----------------------------------------------------------------------
   --  A non-blocking socket will cause the Would_Block error code
   --  when Accept_Connection would normaly block.

   Test ("  Would_Block [18.4.2.3]");
   declare
      Socket   : POSIX_IO.File_Descriptor;
      Int_Add  : aliased Internet_Socket_Address;
      Name     : Internet_Socket_Address_Pointer :=
         Int_Add'Unchecked_Access;
   begin
      Set_Internet_Port (Int_Add, Unspecified_Internet_Port);
      Set_Internet_Address (Int_Add,
         String_To_Internet_Address ("127.0.0.1"));
      Socket := Create (PF_INET, Stream_Socket, IPPROTO_TCP);
      Set_File_Control (Socket, Non_Blocking);
      Bind (Socket, +Name);
      Listen (Socket);
      Accept_Connection_Function (Socket,
         Expected => Would_Block,
         Er1 => "A048", Er2 =>  "A049",
         Er3 => "A050", Er4 => "A051");
      Close (Socket);
   end;

   --  ===============================================================  --
   --  ==                                                           ==  --
   --  ==  The remaining Accept_Connection errors cannont be tested ==  --
   --  ==  due to their dependence on either system resources or    ==  --
   --  ==  race conditions.                                         ==  --
   --  ==                                                           ==  --
   --  ===============================================================  --

   -----------------------------------------------------------------------
   --  Receive a message sent to local socket

   Test ("Receive 1 [18.4.12] (<-)");
   declare
      Socket    : POSIX_IO.File_Descriptor;
      Port      : Positive;
      Template  : Process_Template;
      Child     : Process_ID;
      List      : POSIX_String_List;
      Int_Add   : aliased Internet_Socket_Address;
      Name      : Internet_Socket_Address_Pointer :=
         Int_Add'Unchecked_Access;
      Connection_Socket : POSIX_IO.File_Descriptor := 0;
   begin
      Set_Internet_Port (Int_Add, Unspecified_Internet_Port);
      Set_Internet_Address (Int_Add,
         String_To_Internet_Address ("127.0.0.1"));
      Socket := Create (PF_INET, Stream_Socket, IPPROTO_TCP);
      Bind (Socket, +Name);
      Port := Integer (Get_Internet_Port (Get_Socket_Name (Socket)));
      Listen (Socket);
      POSIX.Append (List, "p180402b");

      begin
         POSIX.Append (List, Value (Argument_List, 2));
      exception
         when Constraint_Error => POSIX.Append (List, " ");
      end;

      POSIX.Append (List, To_POSIX_String (Integer'Image
         (Port)));
      Comment ("port is " & Integer'Image (Port));
      Open_Template (Template);
      Start_Process (Child, "./p180402b", Template, List);
      Close_Template (Template);
      Connection_Socket := Accept_Connection (Socket);
      Receive1_Tests (Connection_Socket, Er1 => "A000",
         Er2 => "A052", Er3 => "A053", Er4 => "A054");
      Close (Socket);
   end;

   -----------------------------------------------------------------------
   --  A non-blocking socket that should block on a Receive generates
   --  the Would_Block error code

   Test ("<- Would_Block [18.4.12.3]");
   declare
      Socket    : POSIX_IO.File_Descriptor;
      Int_Add   : aliased Internet_Socket_Address;
      Name      : Internet_Socket_Address_Pointer :=
         Int_Add'Unchecked_Access;
   begin
      Set_Internet_Port (Int_Add, 9);
      Set_Internet_Address (Int_Add,
         String_To_Internet_Address ("127.0.0.1"));
      Socket := Create (PF_INET, Stream_Socket, IPPROTO_TCP);
      Set_File_Control (Socket, Non_Blocking);
      Connect (Socket, +Name);
      Receive1_Tests (Socket, Expected => Would_Block,
         Er1 => "A055", Er2 => "A056",
         Er3 => "A057", Er4 => "A058");
   end;

   -----------------------------------------------------------------------
   --  Trying to receive on a closed file descriptor generates
   --  the Bad_File_Desriptor error code.

   Test ("<- Bad_File_Destriptor [18.4.12.3]");
   declare
      Socket : POSIX_IO.File_Descriptor := 0;
   begin
      Close (Socket);
      Receive1_Tests (Socket, Expected => Bad_File_Descriptor,
         Er1 => "A059", Er2 => "A060",
         Er3 => "A061", Er4 => "A062");
   end;

   -----------------------------------------------------------------------
   --  Trying to receive on a non-connected socket generates
   --  the Not_Connected error code.

   Test ("<- Not_Connected [18.4.12.3]");
   declare
      Socket    : POSIX_IO.File_Descriptor;
   begin
      Socket := Create (PF_INET, Stream_Socket, IPPROTO_TCP);
      Receive1_Tests (Socket, Expected => Not_Connected,
         Er1 => "A063", Er2 => "A064",
         Er3 => "A065", Er4 => "A066");
   end;

   -----------------------------------------------------------------------
   --  Trying to receive on a non-socket generates
   --  the Not_A_Socket error code.

   Test ("<- Not_A_Socket [18.4.12.3]");
   declare
      Socket : POSIX_IO.File_Descriptor := 1;
   begin
      Receive1_Tests (Socket, Expected => Not_A_Socket,
         Er1 => "A067", Er2 => "A068",
         Er3 => "A069", Er4 => "A070");
   end;

   -----------------------------------------------------------------------
   --  Trying to receive using the Process_OOB_Data option on a socket
   --  type that doesn't allow it generate the
   --  Option_Not_Supported error code.

   Test ("<- Option_Not_Supported [18.4.12.3]");
   declare
      Socket    : POSIX_IO.File_Descriptor;
   begin
      Socket := Create (PF_INET, Datagram_Socket, IPPROTO_UDP);
      Receive1_Tests (Socket, Options => Process_OOB_Data,
         Expected => Option_Not_Supported,
         Er1 => "A080", Er2 => "A081",
         Er3 => "A082", Er4 => "A083");
   end;

   -----------------------------------------------------------------------
   --  Receive a message sent to local socket

   Test ("Receive 2 [18.4.12] (<-)");
   declare
      Socket    : POSIX_IO.File_Descriptor;
      Port      : Positive;
      Template  : Process_Template;
      Child     : Process_ID;
      List      : POSIX_String_List;
      Int_Add   : aliased Internet_Socket_Address;
      Name      : Internet_Socket_Address_Pointer :=
         Int_Add'Unchecked_Access;
      Connection_Socket : POSIX_IO.File_Descriptor := 0;
   begin
      Set_Internet_Port (Int_Add, Unspecified_Internet_Port);
      Set_Internet_Address (Int_Add,
         String_To_Internet_Address ("127.0.0.1"));
      Socket := Create (PF_INET, Stream_Socket, IPPROTO_TCP);
      Bind (Socket, +Name);
      Port := Integer (Get_Internet_Port (Get_Socket_Name (Socket)));
      Listen (Socket);
      POSIX.Append (List, "p180402e");

      begin
         POSIX.Append (List, Value (Argument_List, 2));
      exception
         when Constraint_Error => POSIX.Append (List, " ");
      end;

      POSIX.Append (List, To_POSIX_String (Integer'Image
         (Port)));
      Comment ("port is " & Integer'Image (Port));
      Open_Template (Template);
      Start_Process (Child, "./p180402e", Template, List);
      Close_Template (Template);
      Connection_Socket := Accept_Connection (Socket);
      Receive2_Tests (Connection_Socket, Er1 => "A000",
         Er2 => "A084", Er3 => "A085", Er4 => "A086");
   end;

   -----------------------------------------------------------------------
   --  A non-blocking socket that should block on a Receive generates
   --  the Would_Block error code

   Test ("<- Would_Block [18.4.12.3]");
   declare
      Socket    : POSIX_IO.File_Descriptor;
      Int_Add   : aliased Internet_Socket_Address;
      Name      : Internet_Socket_Address_Pointer :=
         Int_Add'Unchecked_Access;
   begin
      Set_Internet_Port (Int_Add, 9);
      Set_Internet_Address (Int_Add,
         String_To_Internet_Address ("127.0.0.1"));
      Socket := Create (PF_INET, Stream_Socket, IPPROTO_TCP);
      Set_File_Control (Socket, Non_Blocking);
      Connect (Socket, +Name);
      Receive2_Tests (Socket, Expected => Would_Block,
         Er1 => "A087", Er2 => "A088",
         Er3 => "A089", Er4 => "A090");
   end;

   -----------------------------------------------------------------------
   --  Trying to receive on a closed file descriptor generates
   --  the Bad_File_Desriptor error code.

   Test ("<- Bad_File_Destriptor [18.4.12.3]");
   declare
      Socket : POSIX_IO.File_Descriptor := 0;
   begin
      Close (Socket);
      Receive2_Tests (Socket, Expected => Bad_File_Descriptor,
         Er1 => "A091", Er2 => "A092",
         Er3 => "A093", Er4 => "A094");
   end;

   -----------------------------------------------------------------------
   --  Trying to receive on a non-connected socket generates
   --  the Not_Connected error code.

   Test ("<- Not_Connected [18.4.12.3]");
   declare
      Socket    : POSIX_IO.File_Descriptor;
   begin
      Socket := Create (PF_INET, Stream_Socket, IPPROTO_TCP);
      Receive2_Tests (Socket, Expected => Not_Connected,
         Er1 => "A095", Er2 => "A096",
         Er3 => "A097", Er4 => "A098");
   end;

   -----------------------------------------------------------------------
   --  Trying to receive on a non-socket generates
   --  the Not_A_Socket error code.

   Test ("<- Not_A_Socket [18.4.12.3]");
   declare
      Socket : POSIX_IO.File_Descriptor := 1;
   begin
      Receive2_Tests (Socket, Expected => Not_A_Socket,
         Er1 => "A099", Er2 => "A100",
         Er3 => "A101", Er4 => "A102");
   end;

   -----------------------------------------------------------------------
   --  Trying to receive using the Process_OOB_Data option on a socket
   --  type that doesn't allow it generate the
   --  Option_Not_Supported error code.

   Test ("<- Option_Not_Supported [18.4.12.3]");
   declare
      Socket    : POSIX_IO.File_Descriptor;
   begin
      Socket := Create (PF_INET, Datagram_Socket, IPPROTO_UDP);
      Receive2_Tests (Socket, Options => Process_OOB_Data,
         Expected => Option_Not_Supported,
         Er1 => "A103", Er2 => "A104",
         Er3 => "A105", Er4 => "A106");
   end;

   -----------------------------------------------------------------------
   --  Receive a message sent to local socket

   Test ("Receive 3 [18.4.12] (<-)");
   declare
      Socket    : POSIX_IO.File_Descriptor;
      Port      : Positive;
      Template  : Process_Template;
      Child     : Process_ID;
      List      : POSIX_String_List;
      Int_Add   : aliased Internet_Socket_Address;
      Name      : Internet_Socket_Address_Pointer :=
         Int_Add'Unchecked_Access;
   begin
      Set_Internet_Port (Int_Add, Unspecified_Internet_Port);
      Set_Internet_Address (Int_Add,
         String_To_Internet_Address ("127.0.0.1"));
      Socket := Create (PF_INET, Datagram_Socket, IPPROTO_UDP);
      Bind (Socket, +Name);
      Port := Integer (Get_Internet_Port (Get_Socket_Name (Socket)));
      POSIX.Append (List, "p180402c");

      begin
         POSIX.Append (List, Value (Argument_List, 2));
      exception
         when Constraint_Error => POSIX.Append (List, " ");
      end;

      POSIX.Append (List, To_POSIX_String (Integer'Image
         (Port)));
      Comment ("port is " & Integer'Image (Port));
      Open_Template (Template);
      Start_Process (Child, "./p180402c", Template, List);
      Close_Template (Template);
      Receive3_Tests (Socket, From => +Name,
         Er1 => "A000", Er2 => "A107",
         Er3 => "A108", Er4 => "A109");
   end;

   -----------------------------------------------------------------------
   --  A non-blocking socket that should block on a Receive generates
   --  the Would_Block error code

   Test ("<- Would_Block [18.4.12.3]");
   declare
      Socket    : POSIX_IO.File_Descriptor;
      Int_Add   : aliased Internet_Socket_Address;
      Name      : Internet_Socket_Address_Pointer :=
         Int_Add'Unchecked_Access;
   begin
      Set_Internet_Port (Int_Add, 9);
      Set_Internet_Address (Int_Add,
         String_To_Internet_Address ("127.0.0.1"));
      Socket := Create (PF_INET, Datagram_Socket, IPPROTO_UDP);
      Set_File_Control (Socket, Non_Blocking);
      Connect (Socket, +Name);
      Receive3_Tests (Socket, From => +Name, Expected => Would_Block,
         Er1 => "A110", Er2 => "A111",
         Er3 => "A112", Er4 => "A113");
   end;

   -----------------------------------------------------------------------
   --  Trying to receive on a closed file descriptor generates
   --  the Bad_File_Desriptor error code.

   Test ("<- Bad_File_Destriptor [18.4.12.3]");
   declare
      Socket : POSIX_IO.File_Descriptor := 0;
      Int_Add   : aliased Internet_Socket_Address;
      Name      : Internet_Socket_Address_Pointer :=
         Int_Add'Unchecked_Access;
   begin
      Set_Internet_Port (Int_Add, 9);
      Set_Internet_Address (Int_Add,
         String_To_Internet_Address ("127.0.0.1"));
      Close (Socket);
      Receive3_Tests (Socket, From => +Name, Expected => Bad_File_Descriptor,
         Er1 => "A114", Er2 => "115",
         Er3 => "A116", Er4 => "117");
   end;

   -----------------------------------------------------------------------
   --  Trying to receive on a non-connected socket generates
   --  the Not_Connected error code.

   Test ("<- Not_Connected [18.4.12.3]");
   declare
      Socket    : POSIX_IO.File_Descriptor;
      Int_Add   : aliased Internet_Socket_Address;
      Name      : Internet_Socket_Address_Pointer :=
         Int_Add'Unchecked_Access;
   begin
      Set_Internet_Address (Int_Add,
         String_To_Internet_Address ("127.0.0.1"));
      Socket := Create (PF_INET, Stream_Socket, IPPROTO_TCP);
      Receive3_Tests (Socket, From => +Name, Expected => Not_Connected,
         Er1 => "A118", Er2 => "A119",
         Er3 => "A120", Er4 => "A121");
   end;

   -----------------------------------------------------------------------
   --  Trying to receive on a non-socket generates
   --  the Not_A_Socket error code.

   Test ("<- Not_A_Socket [18.4.12.3]");
   declare
      Socket : POSIX_IO.File_Descriptor := 1;
      Int_Add   : aliased Internet_Socket_Address;
      Name      : Internet_Socket_Address_Pointer :=
         Int_Add'Unchecked_Access;
   begin
      Set_Internet_Port (Int_Add, 9);
      Set_Internet_Address (Int_Add,
         String_To_Internet_Address ("127.0.0.1"));
      Receive3_Tests (Socket, From => +Name, Expected => Not_A_Socket,
         Er1 => "A122", Er2 => "A123",
         Er3 => "A124", Er4 => "A125");
   end;

   -----------------------------------------------------------------------
   --  Trying to receive using the Process_OOB_Data option on a socket
   --  type that doesn't allow it generate the
   --  Option_Not_Supported error code.

   Test ("<- Option_Not_Supported [18.4.12.3]");
   declare
      Socket    : POSIX_IO.File_Descriptor;
      Int_Add   : aliased Internet_Socket_Address;
      Name      : Internet_Socket_Address_Pointer :=
         Int_Add'Unchecked_Access;
   begin
      Set_Internet_Port (Int_Add, 9);
      Set_Internet_Address (Int_Add,
         String_To_Internet_Address ("127.0.0.1"));
      Socket := Create (PF_INET, Datagram_Socket, IPPROTO_UDP);
      Receive3_Tests (Socket, From => +Name, Options => Process_OOB_Data,
         Expected => Option_Not_Supported,
         Er1 => "A126", Er2 => "A127",
         Er3 => "A128", Er4 => "A129");
   end;

   --  ... Add test for Incorrect_Address_Type here when testing of
   --  ... a non-internet protocol is completed

   -----------------------------------------------------------------------
   --  Receive a message sent to local socket

   Test ("Receive 4 [18.4.12] (<-)");
   declare
      Socket    : POSIX_IO.File_Descriptor;
      Port      : Positive;
      Template  : Process_Template;
      Child     : Process_ID;
      List      : POSIX_String_List;
      Int_Add   : aliased Internet_Socket_Address;
      Name      : Internet_Socket_Address_Pointer :=
         Int_Add'Unchecked_Access;
   begin
      Set_Internet_Port (Int_Add, Unspecified_Internet_Port);
      Set_Internet_Address (Int_Add,
         String_To_Internet_Address ("127.0.0.1"));
      Socket := Create (PF_INET, Datagram_Socket, IPPROTO_UDP);
      Bind (Socket, +Name);
      Port := Integer (Get_Internet_Port (Get_Socket_Name (Socket)));
      POSIX.Append (List, "p180402g");

      begin
         POSIX.Append (List, Value (Argument_List, 2));
      exception
         when Constraint_Error => POSIX.Append (List, " ");
      end;

      POSIX.Append (List, To_POSIX_String (Integer'Image
         (Port)));
      Comment ("port is " & Integer'Image (Port));
      Open_Template (Template);
      Start_Process (Child, "./p180402g", Template, List);
      Close_Template (Template);
      Receive4_Tests (Socket, From => +Name,
         Er1 => "A000", Er2 => "A130",
         Er3 => "A131", Er4 => "A132");
   end;

   -----------------------------------------------------------------------
   --  A non-blocking socket that should block on a Receive generates
   --  the Would_Block error code

   Test ("<- Would_Block [18.4.12.3]");
   declare
      Socket    : POSIX_IO.File_Descriptor;
      Int_Add   : aliased Internet_Socket_Address;
      Name      : Internet_Socket_Address_Pointer :=
         Int_Add'Unchecked_Access;
   begin
      Set_Internet_Port (Int_Add, 9);
      Set_Internet_Address (Int_Add,
         String_To_Internet_Address ("127.0.0.1"));
      Socket := Create (PF_INET, Datagram_Socket, IPPROTO_UDP);
      Set_File_Control (Socket, Non_Blocking);
      Connect (Socket, +Name);
      Receive4_Tests (Socket, From => +Name, Expected => Would_Block,
         Er1 => "A133", Er2 => "A134",
         Er3 => "A135", Er4 => "A136");
   end;

   -----------------------------------------------------------------------
   --  Trying to receive on a closed file descriptor generates
   --  the Bad_File_Desriptor error code.

   Test ("<- Bad_File_Destriptor [18.4.12.3]");
   declare
      Socket : POSIX_IO.File_Descriptor := 0;
      Int_Add   : aliased Internet_Socket_Address;
      Name      : Internet_Socket_Address_Pointer :=
         Int_Add'Unchecked_Access;
   begin
      Set_Internet_Address (Int_Add,
         String_To_Internet_Address ("127.0.0.1"));
      Close (Socket);
      Receive4_Tests (Socket, From => +Name, Expected => Bad_File_Descriptor,
         Er1 => "A137", Er2 => "A138",
         Er3 => "A139", Er4 => "A140");
   end;

   -----------------------------------------------------------------------
   --  Trying to receive on a non-connected socket generates
   --  the Not_Connected error code.

   Test ("<- Not_Connected [18.4.12.3]");
   declare
      Socket    : POSIX_IO.File_Descriptor;
      Int_Add   : aliased Internet_Socket_Address;
      Name      : Internet_Socket_Address_Pointer :=
         Int_Add'Unchecked_Access;
   begin
      Set_Internet_Address (Int_Add,
         String_To_Internet_Address ("127.0.0.1"));
      Socket := Create (PF_INET, Stream_Socket, IPPROTO_TCP);
      Receive4_Tests (Socket, From => +Name, Expected => Not_Connected,
         Er1 => "A141", Er2 => "A142",
         Er3 => "A143", Er4 => "A144");
   end;

   -----------------------------------------------------------------------
   --  Trying to receive on a non-socket generates
   --  the Not_A_Socket error code.

   Test ("<- Not_A_Socket [18.4.12.3]");
   declare
      Socket : POSIX_IO.File_Descriptor := 1;
      Int_Add   : aliased Internet_Socket_Address;
      Name      : Internet_Socket_Address_Pointer :=
         Int_Add'Unchecked_Access;
   begin
      Set_Internet_Address (Int_Add,
         String_To_Internet_Address ("127.0.0.1"));
      Receive4_Tests (Socket, From => +Name, Expected => Not_A_Socket,
         Er1 => "A145", Er2 => "A146",
         Er3 => "A147", Er4 => "A148");
   end;

   -----------------------------------------------------------------------
   --  Trying to receive using the Process_OOB_Data option on a socket
   --  type that doesn't allow it generates the
   --  Option_Not_Supported error code.

   Test ("<- Option_Not_Supported [18.4.12.3]");
   declare
      Socket    : POSIX_IO.File_Descriptor;
      Int_Add   : aliased Internet_Socket_Address;
      Name      : Internet_Socket_Address_Pointer :=
         Int_Add'Unchecked_Access;
   begin
      Set_Internet_Address (Int_Add,
         String_To_Internet_Address ("127.0.0.1"));
      Socket := Create (PF_INET, Datagram_Socket, IPPROTO_UDP);
      Receive4_Tests (Socket, From => +Name, Options => Process_OOB_Data,
         Expected => Option_Not_Supported,
         Er1 => "A149", Er2 => "A150",
         Er3 => "A151", Er4 => "A152");
   end;

   --  ... Add test for Incorrect_Address_Type here when testing of
   --  ... a non-internet protocol is completed

   -----------------------------------------------------------------------
   --  Receive a message sent to local socket

   Test ("Receive_Message 1 [18.4.12] (<-)");
   declare
      Socket    : POSIX_IO.File_Descriptor;
      Port      : Positive;
      Template  : Process_Template;
      Child     : Process_ID;
      List      : POSIX_String_List;
      Int_Add   : aliased Internet_Socket_Address;
      Name      : Internet_Socket_Address_Pointer :=
         Int_Add'Unchecked_Access;
      Message   : Socket_Message;
      Buffer    : POSIX_String (1 .. 80);
      IOV_Array : IO_Vector_Array_Pointer :=
          new IO_Vector_Array (1 .. 1);

   begin
      Set_Buffer (IOV_Array (1), Buffer (Buffer'First)'Address,
                  Buffer'Length);
      Set_IO_Vector_Array (Message, IOV_Array);
      Set_Internet_Port (Int_Add, Unspecified_Internet_Port);
      Set_Internet_Address (Int_Add,
         String_To_Internet_Address ("127.0.0.1"));
      Socket := Create (PF_INET, Datagram_Socket, IPPROTO_UDP);
      Bind (Socket, +Name);
      Port := Integer (Get_Internet_Port (Get_Socket_Name (Socket)));
      POSIX.Append (List, "p180402d");

      begin
         POSIX.Append (List, Value (Argument_List, 2));
      exception
         when Constraint_Error => POSIX.Append (List, " ");
      end;

      POSIX.Append (List, To_POSIX_String (Integer'Image
         (Port)));
      Comment ("port is " & Integer'Image (Port));
      Open_Template (Template);
      Start_Process (Child, "./p180402d", Template, List);
      Close_Template (Template);
      Receive_Message1_Tests (Socket, Message,
         Er1 => "A000", Er2 => "A153",
         Er3 => "A154", Er4 => "A155");
      Comment (To_String (Buffer));

   end;

   -----------------------------------------------------------------------
   --  A non-blocking socket that should block on a Receive generates
   --  the Would_Block error code

   Test ("<- Would_Block [18.4.12.3]");
   declare
      Socket    : POSIX_IO.File_Descriptor;
      Int_Add   : aliased Internet_Socket_Address;
      Message   : Socket_Message;
      Buffer    : POSIX_String (1 .. 80);
      IOV_Array : IO_Vector_Array_Pointer :=
          new IO_Vector_Array (1 .. 1);
   begin
      Set_Buffer (IOV_Array (1), Buffer (Buffer'First)'Address,
                  Buffer'Length);
      Set_IO_Vector_Array (Message, IOV_Array);
      Set_Internet_Port (Int_Add, 9);
      Set_Internet_Address (Int_Add,
         String_To_Internet_Address ("127.0.0.1"));
      Socket := Create (PF_INET, Datagram_Socket, IPPROTO_UDP);
      Set_File_Control (Socket, Non_Blocking);
      Receive_Message1_Tests (Socket, Message, Expected => Would_Block,
         Er1 => "A156", Er2 => "A157",
         Er3 => "A158", Er4 => "A159");
   end;

   -----------------------------------------------------------------------
   --  Trying to receive on a closed file descriptor generates
   --  the Bad_File_Desriptor error code.

   Test ("<- Bad_File_Destriptor [18.4.12.3]");
   declare
      Socket : POSIX_IO.File_Descriptor := 0;
      Int_Add   : aliased Internet_Socket_Address;
      Message   : Socket_Message;
      Buffer    : POSIX_String (1 .. 80);
      IOV_Array : IO_Vector_Array_Pointer :=
          new IO_Vector_Array (1 .. 1);
   begin
      Set_Buffer (IOV_Array (1), Buffer (Buffer'First)'Address,
                  Buffer'Length);
      Set_IO_Vector_Array (Message, IOV_Array);
      Set_Internet_Address (Int_Add,
         String_To_Internet_Address ("127.0.0.1"));
      Close (Socket);
      Receive_Message1_Tests (Socket, Message, Expected => Bad_File_Descriptor,
         Er1 => "A160", Er2 => "A161",
         Er3 => "A162", Er4 => "A163");
   end;

   -----------------------------------------------------------------------
   --  Trying to receive on a non-connected socket generates
   --  the Not_Connected error code.

   Test ("<- Not_Connected [18.4.12.3]");
   declare
      Socket    : POSIX_IO.File_Descriptor;
      Int_Add   : aliased Internet_Socket_Address;
      Message   : Socket_Message;
      Buffer    : POSIX_String (1 .. 80);
      IOV_Array : IO_Vector_Array_Pointer :=
          new IO_Vector_Array (1 .. 1);
   begin
      Set_Buffer (IOV_Array (1), Buffer (Buffer'First)'Address,
                  Buffer'Length);
      Set_IO_Vector_Array (Message, IOV_Array);
      Set_Internet_Address (Int_Add,
         String_To_Internet_Address ("127.0.0.1"));
      Socket := Create (PF_INET, Stream_Socket, IPPROTO_TCP);
      Receive_Message1_Tests (Socket, Message, Expected => Not_Connected,
         Er1 => "A164", Er2 => "A165",
         Er3 => "A166", Er4 => "A167");
   end;

   -----------------------------------------------------------------------
   --  Trying to receive on a non-socket generates
   --  the Not_A_Socket error code.

   Test ("<- Not_A_Socket [18.4.12.3]");
   declare
      Socket : POSIX_IO.File_Descriptor := 1;
      Int_Add   : aliased Internet_Socket_Address;
      Message   : Socket_Message;
      Buffer    : POSIX_String (1 .. 80);
      IOV_Array : IO_Vector_Array_Pointer :=
          new IO_Vector_Array (1 .. 1);
   begin
      Set_Buffer (IOV_Array (1), Buffer (Buffer'First)'Address,
                  Buffer'Length);
      Set_IO_Vector_Array (Message, IOV_Array);
      Set_Internet_Address (Int_Add,
         String_To_Internet_Address ("127.0.0.1"));
      Receive_Message1_Tests (Socket, Message, Expected => Not_A_Socket,
         Er1 => "A168", Er2 => "A169",
         Er3 => "A170", Er4 => "A171");
   end;

   -----------------------------------------------------------------------
   --  Trying to receive using the Process_OOB_Data option on a socket
   --  type that doesn't allow it generates the
   --  Option_Not_Supported error code.

   Test ("<- Option_Not_Supported [18.4.12.3]");
   declare
      Socket    : POSIX_IO.File_Descriptor;
      Int_Add   : aliased Internet_Socket_Address;
      Message   : Socket_Message;
      Buffer    : POSIX_String (1 .. 80);
      IOV_Array : IO_Vector_Array_Pointer :=
          new IO_Vector_Array (1 .. 1);
   begin
      Set_Buffer (IOV_Array (1), Buffer (Buffer'First)'Address,
                  Buffer'Length);
      Set_IO_Vector_Array (Message, IOV_Array);
      Set_Internet_Address (Int_Add,
         String_To_Internet_Address ("127.0.0.1"));
      Socket := Create (PF_INET, Datagram_Socket, IPPROTO_UDP);
      Receive_Message1_Tests (Socket, Message, Options => Process_OOB_Data,
         Expected => Option_Not_Supported,
         Er1 => "A172", Er2 => "A173",
         Er3 => "A174", Er4 => "A175");
   end;

   --  ... Add test for Incorrect_Address_Type here when testing of
   --  ... a non-internet protocol is completed

   -----------------------------------------------------------------------
   --  Receive a message sent to local socket

   Test ("Receive_Message 2 [18.4.12] (<-)");
   declare
      Socket    : POSIX_IO.File_Descriptor;
      Port      : Positive;
      Template  : Process_Template;
      Child     : Process_ID;
      List      : POSIX_String_List;
      Int_Add   : aliased Internet_Socket_Address;
      Name      : Internet_Socket_Address_Pointer :=
         Int_Add'Unchecked_Access;
      Message   : Socket_Message;
      Buffer    : POSIX_String (1 .. 80);
      IOV_Array : IO_Vector_Array_Pointer :=
          new IO_Vector_Array (1 .. 1);

   begin
      Set_Buffer (IOV_Array (1), Buffer (Buffer'First)'Address,
                  Buffer'Length);
      Set_IO_Vector_Array (Message, IOV_Array);
      Set_Internet_Port (Int_Add, Unspecified_Internet_Port);
      Set_Internet_Address (Int_Add,
         String_To_Internet_Address ("127.0.0.1"));
      Socket := Create (PF_INET, Datagram_Socket, IPPROTO_UDP);
      Bind (Socket, +Name);
      Port := Integer (Get_Internet_Port (Get_Socket_Name (Socket)));
      POSIX.Append (List, "p180402f");

      begin
         POSIX.Append (List, Value (Argument_List, 2));
      exception
         when Constraint_Error => POSIX.Append (List, " ");
      end;

      POSIX.Append (List, To_POSIX_String (Integer'Image
         (Port)));
      Comment ("port is " & Integer'Image (Port));
      Open_Template (Template);
      Start_Process (Child, "./p180402f", Template, List);
      Close_Template (Template);
      Receive_Message2_Tests (Socket, Message,
         Er1 => "A000", Er2 => "A176",
         Er3 => "A177", Er4 => "A178");
      Comment (To_String (Buffer));

   end;

   -----------------------------------------------------------------------
   --  A non-blocking socket that should block on a Receive generates
   --  the Would_Block error code

   Test ("<- Would_Block [18.4.12.3]");
   declare
      Socket    : POSIX_IO.File_Descriptor;
      Int_Add   : aliased Internet_Socket_Address;
      Message   : Socket_Message;
      Buffer    : POSIX_String (1 .. 80);
      IOV_Array : IO_Vector_Array_Pointer :=
          new IO_Vector_Array (1 .. 1);
   begin
      Set_Buffer (IOV_Array (1), Buffer (Buffer'First)'Address,
                  Buffer'Length);
      Set_IO_Vector_Array (Message, IOV_Array);
      Set_Internet_Port (Int_Add, 9);
      Set_Internet_Address (Int_Add,
         String_To_Internet_Address ("127.0.0.1"));
      Socket := Create (PF_INET, Datagram_Socket, IPPROTO_UDP);
      Set_File_Control (Socket, Non_Blocking);
      Receive_Message2_Tests (Socket, Message, Expected => Would_Block,
         Er1 => "A179", Er2 => "A180",
         Er3 => "A181", Er4 => "A182");
   end;

   -----------------------------------------------------------------------
   --  Trying to receive on a closed file descriptor generates
   --  the Bad_File_Desriptor error code.

   Test ("<- Bad_File_Destriptor [18.4.12.3]");
   declare
      Socket : POSIX_IO.File_Descriptor := 0;
      Int_Add   : aliased Internet_Socket_Address;
      Message   : Socket_Message;
      Buffer    : POSIX_String (1 .. 80);
      IOV_Array : IO_Vector_Array_Pointer :=
          new IO_Vector_Array (1 .. 1);
   begin
      Set_Buffer (IOV_Array (1), Buffer (Buffer'First)'Address,
                  Buffer'Length);
      Set_IO_Vector_Array (Message, IOV_Array);
      Set_Internet_Address (Int_Add,
         String_To_Internet_Address ("127.0.0.1"));
      Close (Socket);
      Receive_Message2_Tests (Socket, Message, Expected => Bad_File_Descriptor,
         Er1 => "A183", Er2 => "A184",
         Er3 => "A185", Er4 => "A186");
   end;

   -----------------------------------------------------------------------
   --  Trying to receive on a non-connected socket generates
   --  the Not_Connected error code.

   Test ("<- Not_Connected [18.4.12.3]");
   declare
      Socket    : POSIX_IO.File_Descriptor;
      Int_Add   : aliased Internet_Socket_Address;
      Message   : Socket_Message;
      Buffer    : POSIX_String (1 .. 80);
      IOV_Array : IO_Vector_Array_Pointer :=
          new IO_Vector_Array (1 .. 1);
   begin
      Set_Buffer (IOV_Array (1), Buffer (Buffer'First)'Address,
                  Buffer'Length);
      Set_IO_Vector_Array (Message, IOV_Array);
      Set_Internet_Address (Int_Add,
         String_To_Internet_Address ("127.0.0.1"));
      Socket := Create (PF_INET, Stream_Socket, IPPROTO_TCP);
      Receive_Message2_Tests (Socket, Message, Expected => Not_Connected,
         Er1 => "A187", Er2 => "A188",
         Er3 => "A189", Er4 => "A190");
   end;

   -----------------------------------------------------------------------
   --  Trying to receive on a non-socket generates
   --  the Not_A_Socket error code.

   Test ("<- Not_A_Socket [18.4.12.3]");
   declare
      Socket : POSIX_IO.File_Descriptor := 1;
      Int_Add   : aliased Internet_Socket_Address;
      Message   : Socket_Message;
      Buffer    : POSIX_String (1 .. 80);
      IOV_Array : IO_Vector_Array_Pointer :=
          new IO_Vector_Array (1 .. 1);
   begin
      Set_Buffer (IOV_Array (1), Buffer (Buffer'First)'Address,
                  Buffer'Length);
      Set_IO_Vector_Array (Message, IOV_Array);
      Set_Internet_Address (Int_Add,
         String_To_Internet_Address ("127.0.0.1"));
      Receive_Message2_Tests (Socket, Message, Expected => Not_A_Socket,
         Er1 => "A191", Er2 => "A192",
         Er3 => "A193", Er4 => "A194");
   end;

   -----------------------------------------------------------------------
   --  Trying to receive using the Process_OOB_Data option on a socket
   --  type that doesn't allow it generates the
   --  Option_Not_Supported error code.

   Test ("<- Option_Not_Supported [18.4.12.3]");
   declare
      Socket    : POSIX_IO.File_Descriptor;
      Int_Add   : aliased Internet_Socket_Address;
      Message   : Socket_Message;
      Buffer    : POSIX_String (1 .. 80);
      IOV_Array : IO_Vector_Array_Pointer :=
          new IO_Vector_Array (1 .. 1);
   begin
      Set_Buffer (IOV_Array (1), Buffer (Buffer'First)'Address,
                  Buffer'Length);
      Set_IO_Vector_Array (Message, IOV_Array);
      Set_Internet_Address (Int_Add,
         String_To_Internet_Address ("127.0.0.1"));
      Socket := Create (PF_INET, Datagram_Socket, IPPROTO_UDP);
      Receive_Message2_Tests (Socket, Message, Options => Process_OOB_Data,
         Expected => Option_Not_Supported,
         Er1 => "A195", Er2 => "A196",
         Er3 => "A197", Er4 => "A198");
   end;

   --  ... Add test for Incorrect_Address_Type here when testing of
   --  ... a non-internet protocol is completed

   Done;
exception when E : others => Fatal_Exception (E, "A999");
end p180402;

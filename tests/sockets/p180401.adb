------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5c VALIDATION TEST SUITE                      --
--                                                                          --
--                             P 1 8 0 4 0 1                                --
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
--  AVAILABLE OR DISCLOSED,  OR THE OWNERSHIP,  MERCHANTABILITY, OR FITNESS  --
--  FOR A PARTICULAR PURPOSE OF SAID MATERIAL.                              --
--                                                                          --
------------------------------------------------------------------------------

--  Integrated test for package POSIX_Sockets
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
     Unchecked_Conversion,
     System;

procedure p180401 is

   use POSIX,
       POSIX_Sockets,
       POSIX_Sockets_Internet,
       POSIX_IO,
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

   procedure Bind_Tests (
      Socket   : in POSIX_IO.File_Descriptor;
      Port     : in Internet_Port
               := Unspecified_Internet_Port;
      Address  : in Internet_Address
               := Unspecified_Internet_Address;
      Expected : in Error_Code := No_Error;
      Error1, Error2, Error3 : in String);

   procedure Connect_Tests (
      Socket   : in POSIX_IO.File_Descriptor;
      Port     : in Internet_Port := 23;
      Address  : in Internet_Address
         := String_To_Internet_Address ("127.0.0.1");
      Expected : in Error_Code := No_Error;
      Error1, Error2, Error3 : in String);

   procedure Specify_Peer_Tests (
      Socket   : in POSIX_IO.File_Descriptor;
      Port     : in Internet_Port := 23;
      Address  : in Internet_Address
         := String_To_Internet_Address ("127.0.0.1");
      Expected : in Error_Code := No_Error;
      Error1, Error2, Error3 : in String);

   procedure Unspecify_Peer_Tests (
      Socket   : in POSIX_IO.File_Descriptor;
      Expected : in Error_Code := No_Error;
      Error1, Error2, Error3 : in String);

   procedure Listen_Tests (
      Socket   : in POSIX_IO.File_Descriptor;
      Backlog  : in Connection_Queue_Length
         := Connection_Queue_Length'Last;
      Expected : in Error_Code := No_Error;
      Error1, Error2, Error3 : in String;
      Close_Socket : in boolean := false);

   procedure Shutdown_Tests (
      Socket   : in POSIX_IO.File_Descriptor;
      Mode     : in Shutdown_Mode;
      Expected : in Error_Code := No_Error;
      Error1, Error2, Error3 : in String;
      Close_Socket : in boolean := false;
      Should_Connect : in boolean := true);

   procedure Bind_Tests (
      Socket   : in POSIX_IO.File_Descriptor;
      Port     : in Internet_Port
               := Unspecified_Internet_Port;
      Address  : in Internet_Address
               := Unspecified_Internet_Address;
      Expected : in Error_Code := No_Error;
      Error1, Error2, Error3 : in String)
   is
      Int_Add : aliased Internet_Socket_Address;
      Name    : Internet_Socket_Address_Pointer :=
         Int_Add'Unchecked_Access;
   begin
      Set_Internet_Port (Int_Add, Port);
      Set_Internet_Address (Int_Add, Address);
      Bind (Socket, +Name);
      if Error1 /= "A000" then
         Expect_Exception (Error1);
      end if;
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code /= Expected then
         Unexpected_Exception (E1, Error2);
      end if;
   when E2 : others => Unexpected_Exception (E2, Error3);
   end Bind_Tests;

   procedure Connect_Tests (
      Socket   : in POSIX_IO.File_Descriptor;
      Port     : in Internet_Port := 23;
      Address  : in Internet_Address
         := String_To_Internet_Address ("127.0.0.1");
      Expected : in Error_Code := No_Error;
      Error1, Error2, Error3 : in String)
   is
      Int_Add : aliased Internet_Socket_Address;
      Name    : Internet_Socket_Address_Pointer :=
         Int_Add'Unchecked_Access;
   begin
      Set_Internet_Port (Int_Add, Port);
      Set_Internet_Address (Int_Add, Address);
      Connect (Socket, +Name);
      if Error1 /= "A000" then
         Expect_Exception (Error1);
      end if;
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code /= Expected then
         Unexpected_Exception (E1, Error2);
      end if;
   when E2 : others => Unexpected_Exception (E2, Error3);
   end Connect_Tests;

   procedure Specify_Peer_Tests (
      Socket   : in POSIX_IO.File_Descriptor;
      Port     : in Internet_Port := 23;
      Address  : in Internet_Address
         := String_To_Internet_Address ("127.0.0.1");
      Expected : in Error_Code := No_Error;
      Error1, Error2, Error3 : in String)
   is
      Int_Add : aliased Internet_Socket_Address;
      Name    : Internet_Socket_Address_Pointer :=
         Int_Add'Unchecked_Access;
   begin
      Set_Internet_Port (Int_Add, Port);
      Set_Internet_Address (Int_Add, Address);
      Specify_Peer (Socket, +Name);
      if Error1 /= "A000" then
         Expect_Exception (Error1);
      end if;
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code /= Expected then
         Unexpected_Exception (E1, Error2);
      end if;
   when E2 : others => Unexpected_Exception (E2, Error3);
   end Specify_Peer_Tests;

   procedure Unspecify_Peer_Tests (
      Socket   : in POSIX_IO.File_Descriptor;
      Expected : in Error_Code := No_Error;
      Error1, Error2, Error3 : in String)
   is
   begin
      Unspecify_Peer (Socket);
      if Error1 /= "A000" then
         Expect_Exception (Error1);
      end if;
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code /= Expected then
         Unexpected_Exception (E1, Error2);
      end if;
   when E2 : others => Unexpected_Exception (E2, Error3);
   end Unspecify_Peer_Tests;

   procedure Listen_Tests (
      Socket   : in POSIX_IO.File_Descriptor;
      Backlog  : in Connection_Queue_Length
         := Connection_Queue_Length'Last;
      Expected : in Error_Code := No_Error;
      Error1, Error2, Error3 : in String;
      Close_Socket : in boolean := false)
   is
      Int_Add : aliased Internet_Socket_Address;
      Name    : Internet_Socket_Address_Pointer :=
         Int_Add'Unchecked_Access;
   begin
      Set_Internet_Port (Int_Add, Unspecified_Internet_Port);
      Set_Internet_Address (Int_Add,
         String_To_Internet_Address ("127.0.0.1"));
      Bind (Socket, +Name);
      if Close_Socket = true then
         Close (Socket);
      end if;
      Listen (Socket, Backlog);
      if Error1 /= "A000" then
         Expect_Exception (Error1);
      end if;
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code /= Expected then
         Unexpected_Exception (E1, Error2);
      end if;
   when E2 : others => Unexpected_Exception (E2, Error3);
   end Listen_Tests;

   procedure Shutdown_Tests (
      Socket   : in POSIX_IO.File_Descriptor;
      Mode     : in Shutdown_Mode;
      Expected : in Error_Code := No_Error;
      Error1, Error2, Error3 : in String;
      Close_Socket : in boolean := false;
      Should_Connect : in boolean := true)
   is
      Int_Add : aliased Internet_Socket_Address;
      Name    : Internet_Socket_Address_Pointer :=
         Int_Add'Unchecked_Access;
   begin
      Set_Internet_Port (Int_Add, 23);
      Set_Internet_Address (Int_Add,
         String_To_Internet_Address ("127.0.0.1"));
      if Should_Connect = true then
         Connect (Socket, +Name);
      end if;
      if Close_Socket = true then
         Close (Socket);
      end if;
      Shutdown (Socket, Mode);
      if Error1 /= "A000" then
         Expect_Exception (Error1);
      end if;
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code /= Expected then
         Unexpected_Exception (E1, Error2);
      end if;
   when E2 : others => Unexpected_Exception (E2, Error3);
   end Shutdown_Tests;


--------------------------------------------------------------------------
--  Begin Tests

begin
   Header ("p180401");
   Test ("package POSIX.Sockets");

   -----------------------------------------------------------------------
   --  A Socket can be bound

   Test ("Bind [18.4.3]");
   declare
      Socket : POSIX_IO.File_Descriptor;
   begin
      Socket := Create (PF_INET, Stream_Socket, IPPROTO_TCP);
      Bind_Tests (Socket, Error1 => "A000", Error2 => "A001",
         Error3 => "A002");
      Close (Socket);
   end;

   -----------------------------------------------------------------------
   --  Trying to bind a socket to a port that is reserved results in the
   --  Permission_Denied error code.

   Test ("Permission_Denied [18.4.3.3]");
   declare
      Socket : POSIX_IO.File_Descriptor;
   begin
      Socket := Create (PF_INET, Stream_Socket, IPPROTO_TCP);
      Bind_Tests (Socket, Port => 23, Expected => Permission_Denied,
         Error1 => "A003", Error2 => "A004", Error3 => "A005");
      Close (Socket);
   end;

   -----------------------------------------------------------------------
   --  Trying to bind a socket to a port already in use results in the
   --  Already_In_Use error code.

   Test ("Already_In_Use [18.4.3.3]");
   declare
      Socket1 : POSIX_IO.File_Descriptor;
      Socket2 : POSIX_IO.File_Descriptor;
   begin
      Socket1 := Create (PF_INET, Stream_Socket, IPPROTO_TCP);
      Bind_Tests (Socket1, Port => 1530, Error1 => "A000",
         Error2 => "A006", Error3 => "A007");
      Socket2 := Create (PF_INET, Stream_Socket, IPPROTO_TCP);
      Bind_Tests (Socket2, Port => 1530, Expected => Address_In_Use,
         Error1 => "A008", Error2 => "A009", Error3 => "A010");
      Close (Socket1);
      Close (Socket2);
   end;

   -----------------------------------------------------------------------
   --  Trying to bind a socket to an address that doesn't exist results
   --  in the Address_Not_Available error code.

   Test ("Address_Not_Available [18.4.3.3]");
   declare
      Socket : POSIX_IO.File_Descriptor;
   begin
      Socket := Create (PF_INET, Stream_Socket, IPPROTO_TCP);
      Bind_Tests (Socket,
         Address => String_To_Internet_Address ("@#$%^&*()"),
         Expected => Address_Not_Available, Error1 => "A011",
         Error2 => "A012", Error3 => "A013");
      Close (Socket);
   end;


   -----------------------------------------------------------------------
   --  Trying to bind a closed socket results
   --  in the Bad_File_Descriptor error code.

   Test ("Bad_File_Descriptor [18.4.3.3]");
   declare
      Socket  : POSIX_IO.File_Descriptor;
   begin
      Socket := Create (PF_INET, Stream_Socket, IPPROTO_TCP);
      Close (Socket);
      Bind_Tests (Socket, Expected => Bad_File_Descriptor,
         Error1 => "A014", Error2 => "A015", Error3 => "A016");
   end;

   -----------------------------------------------------------------------
   --  Trying to bind a non socket results
   --  in the Not_A_Socket error code.

   Test ("Not_A_Socket [18.4.3.3]");
   declare
      Socket  : POSIX_IO.File_Descriptor := 0;
   begin
      Bind_Tests (Socket, Expected => Not_A_Socket,
         Error1 => "A017", Error2 => "A018", Error3 => "A019");
   end;

   -----------------------------------------------------------------------
   --  Trying to bind to a socket with a different type of object results
   --  in the Inappropriate_Family error code.  This error is not in the
   --  the standard but I get it in what I think is the place of
   --  Incorrect_Address_Type.

   Test ("Inappropriate_Family [18.4.3.3]");
   declare
      Socket  : POSIX_IO.File_Descriptor := 0;
   begin
      Socket := Create (PF_UNIX, Stream_Socket, IPPROTO_IP);
      Bind_Tests (Socket, Expected => Inappropriate_Family,
         Error1 => "A020", Error2 => "A021", Error3 => "A022");
   end;

   -----------------------------------------------------------------------
   --  Trying to bind a socket with a bad Address pointer results
   --  in the Invalid_Argument error code.

   Test ("Invalid_Argument [18.4.3.3]");
   declare
      Socket  : POSIX_IO.File_Descriptor;
      Name    : Internet_Socket_Address_Pointer;
      Junk    : Internet_Address;
      function To_Internet_Socket_Address_Pointer is new
         Unchecked_Conversion (System.Address,
            Internet_Socket_Address_Pointer);
   begin
      Name := To_Internet_Socket_Address_Pointer (Junk'Address);
      Socket := Create (PF_INET, Stream_Socket, IPPROTO_TCP);
      Bind (Socket, +Name);
      Expect_Exception ("A023");
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code /= Invalid_Argument then
         Unexpected_Exception (E1, "A024");
      end if;
   when E2 : others => Unexpected_Exception (E2, "A025");
   end;

   -----------------------------------------------------------------------
   --  Connecting a socket to local machines telnet port

   Test ("Connect [18.4.4]");
   declare
      Socket  : POSIX_IO.File_Descriptor;
   begin
      Socket := Create (PF_INET, Stream_Socket, IPPROTO_TCP);
      Connect_Tests (Socket, 23, String_To_Internet_Address ("127.0.0.1"),
         No_Error, "A000", "A026", "A027");
      Close (Socket);
   end;

   -----------------------------------------------------------------------
   --  Trying to bind a socket to a port that is reserved results in the
   --  Permission_Denied error code.  Since connect choses the port this
   --  error message cannot be systematicaly created.
   -----------------------------------------------------------------------

   -----------------------------------------------------------------------
   --  Trying to bind a socket already in use results in the
   --  Already_In_Use error code.  Since connect choses the port to bind
   --  to this error message cannot by systematicaly created.
   -----------------------------------------------------------------------

   -----------------------------------------------------------------------
   --  Trying to connect a socket already in connected results in the
   --  Is_Already_Connected error code.

   Test ("Is_Already_Connected [18.4.4.3]");
   declare
      Socket : POSIX_IO.File_Descriptor;
   begin
      Socket := Create (PF_INET, Stream_Socket, IPPROTO_TCP);
      Connect_Tests (Socket, Error1 => "A000",
         Error2 => "A028", Error3 => "A029");
      Connect_Tests (Socket, Expected => Is_Already_Connected,
         Error1 => "A030", Error2 => "A031", Error3 => "A032");
      Close (Socket);
   end;

   -----------------------------------------------------------------------
   --  Trying to connect a socket to an address that doesn't exist results
   --  in the Network_Unreachable error code.

   Test ("Network_Unreachable [18.4.4.3]");
   declare
      Socket : POSIX_IO.File_Descriptor;
   begin
      Socket := Create (PF_INET, Stream_Socket, IPPROTO_TCP);
      Connect_Tests (Socket,
         Address => String_To_Internet_Address ("@#$%^&*()"),
         Expected => Network_Unreachable, Error1 => "A033",
         Error2 => "A034", Error3 => "A035");
      Close (Socket);
   end;

   -----------------------------------------------------------------------
   --  Address_Not_Available is an error code generated by the bind
   --  portion of connect.  In all createable cases this error is
   --  supercided by the Network_Unreachabe error code.
   -----------------------------------------------------------------------

   -----------------------------------------------------------------------
   --  Trying to connect a closed socket results
   --  in the Bad_File_Descriptor error code.

   Test ("Bad_File_Descriptor [18.4.4.3]");
   declare
      Socket  : POSIX_IO.File_Descriptor;
   begin
      Socket := Create (PF_INET, Stream_Socket, IPPROTO_TCP);
      Close (Socket);
      Connect_Tests (Socket, Expected => Bad_File_Descriptor,
         Error1 => "A036", Error2 => "A037", Error3 => "A038");
   end;

   -----------------------------------------------------------------------
   --  Trying to connect a non socket results
   --  in the Not_A_Socket error code.

   Test ("Not_A_Socket [18.4.3.3]");
   declare
      Socket  : POSIX_IO.File_Descriptor := 0;
   begin
      Connect_Tests (Socket, Expected => Not_A_Socket,
         Error1 => "A039", Error2 => "A040", Error3 => "A041");
   end;

   -----------------------------------------------------------------------
   --  Trying to connect to a socket with a different type of object
   --  results in the Inappropriate_Family error code.  This error is not
   --  in the the standard but I get it in what I think is the place of
   --  Incorrect_Address_Type.

   Test ("Inappropriate_Family [18.4.4.3]");
   declare
      Socket  : POSIX_IO.File_Descriptor := 0;
   begin
      Socket := Create (PF_UNIX, Stream_Socket, IPPROTO_IP);
      Connect_Tests (Socket, Expected => Inappropriate_Family,
         Error1 => "A042", Error2 => "A043", Error3 => "A044");
   end;

   -----------------------------------------------------------------------
   --  Trying to connect with a bad Address pointer results
   --  in the Invalid_Argument error code.

   Test ("Invalid_Argument [18.4.4.3]");
   declare
      Socket  : POSIX_IO.File_Descriptor;
      Name    : Internet_Socket_Address_Pointer;
      Junk    : Internet_Address;
      function To_Internet_Socket_Address_Pointer is new
         Unchecked_Conversion (System.Address,
            Internet_Socket_Address_Pointer);
   begin
      Name := To_Internet_Socket_Address_Pointer (Junk'Address);
      Socket := Create (PF_INET, Stream_Socket, IPPROTO_TCP);
      Connect (Socket, +Name);
      Expect_Exception ("A045");
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code /= Invalid_Argument then
         Unexpected_Exception (E1, "A046");
      end if;
   when E2 : others => Unexpected_Exception (E2, "A047");
   end;

   -----------------------------------------------------------------------
   --  Trying to connect twice to a non-reponding address causes the
   --  Operation_In_Progress error code.

   Test ("Operation_In_Progress [18.4.4.3]");
   declare
      Socket  : POSIX_IO.File_Descriptor;
      Int_Add : aliased Internet_Socket_Address;
      Name    : Internet_Socket_Address_Pointer := Int_Add'Unchecked_Access;
   begin
      Set_Internet_Port (Int_Add, 23);
      --  www.microsoft.com should take forever to make connection
      Set_Internet_Address (Int_Add,
         String_To_Internet_Address ("207.46.130.149"));
      Socket := Create (PF_INET, Stream_Socket, IPPROTO_TCP);
      Set_File_Control (Socket, Non_Blocking);
      Connect (Socket, +Name);
      Connect (Socket, +Name);
      Expect_Exception ("A048");
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code /= Operation_In_Progress then
         Unexpected_Exception (E1, "A049");
      end if;
   when E2 : others => Unexpected_Exception (E2, "A050");
   end;

   -----------------------------------------------------------------------
   --  The Operation_In_Progress error code seem to supercede the
   --  Already_Awaiting_Connection error code.  Therefore there is no
   --  way to create an Alredy_Awaiting_Connection error code.
   -----------------------------------------------------------------------

   -----------------------------------------------------------------------
   --  Trying to connect to a restricted port causes
   --  the Connection_Refused error code.

   --  ... port 53 is a common Internet service (domain) that is often
   --  ... restricted.

   Test ("Connection_Refused [18.4.4.3]");
   declare
      Socket  : POSIX_IO.File_Descriptor := 0;
   begin
      Socket := Create (PF_INET, Stream_Socket, IPPROTO_TCP);
      Connect_Tests (Socket, Port => 53, Expected => Connection_Refused,
         Error1 => "A051", Error2 => "A052", Error3 => "A053");
   end;

   -----------------------------------------------------------------------
   --  ??? Host_Unreachable and Network_Unreachable appear to be the
   --  ??? same thing.  Either that or connect would simply block.
   -----------------------------------------------------------------------

   -----------------------------------------------------------------------
   --  Any signal that would cause Interrupted_Argument on a single
   --  threaded process will stop the process.  Therefore this case
   --  cannot be recreated.
   -----------------------------------------------------------------------

   -----------------------------------------------------------------------
   --  The Network_Down error code cannot accuratly be created due to
   --  it the unreliability of a network being down.
   -----------------------------------------------------------------------

   -----------------------------------------------------------------------
   --  The No_Buffer_Space error code cannot be accuratly created due to
   --  the need of buffer space for the rest of the test.
   -----------------------------------------------------------------------

   -----------------------------------------------------------------------
   --  Trying to connect to a non-reponding address causes the
   --  Timed_Out error code after a period of time.

   Test ("Timed_Out (This may take a few minutes) [18.4.4.3]");
   declare
      Socket  : POSIX_IO.File_Descriptor;
   begin
      --  www.microsoft.com should take forever to make connection
      Socket := Create (PF_INET, Stream_Socket, IPPROTO_TCP);
      Connect_Tests (Socket, Address =>
         String_To_Internet_Address ("207.46.130.149"),
         Expected => Timed_Out, Error1 => "A054", Error2 => "A055",
         Error3 => "A056");
   end;

   -----------------------------------------------------------------------
   --  Specifying Peer as local machines telnet port

   Test ("Specify_Peer [18.4.4]");
   declare
      Socket  : POSIX_IO.File_Descriptor;
   begin
      Socket := Create (PF_INET, Datagram_Socket, IPPROTO_UDP);
      Specify_Peer_Tests (Socket, 23, String_To_Internet_Address ("127.0.0.1"),
         No_Error, "A000", "A057", "A058");
      Close (Socket);
   end;

   -----------------------------------------------------------------------
   --  Trying to specify a peer using a closed socket results
   --  in the Bad_File_Descriptor error code.

   Test ("Bad_File_Descriptor [18.4.4.3]");
   declare
      Socket  : POSIX_IO.File_Descriptor;
   begin
      Socket := Create (PF_INET, Datagram_Socket, IPPROTO_UDP);
      Close (Socket);
      Specify_Peer_Tests (Socket, Expected => Bad_File_Descriptor,
         Error1 => "A059", Error2 => "A060", Error3 => "A061");
   end;

   -----------------------------------------------------------------------
   --  Trying to specify a peer using a non socket results
   --  in the Not_A_Socket error code.

   Test ("Not_A_Socket [18.4.3.3]");
   declare
      Socket  : POSIX_IO.File_Descriptor := 0;
   begin
      Specify_Peer_Tests (Socket, Expected => Not_A_Socket,
         Error1 => "A062", Error2 => "A063", Error3 => "A064");
   end;

   -----------------------------------------------------------------------
   --  Trying to specify a peer useing a socket with a different type of
   --  object results in the Inappropriate_Family error code.  This error
   --  is not in the the standard but I get it in what I think is the
   --  place of Incorrect_Address_Type.

   Test ("Inappropriate_Family [18.4.4.3]");
   declare
      Socket  : POSIX_IO.File_Descriptor := 0;
   begin
      Socket := Create (PF_UNIX, Stream_Socket, IPPROTO_IP);
      Specify_Peer_Tests (Socket, Expected => Inappropriate_Family,
         Error1 => "A065", Error2 => "A066", Error3 => "A067");
   end;

   -----------------------------------------------------------------------
   --  Trying to specify a peer with a bad Address pointer results
   --  in the Invalid_Argument error code.

   Test ("Invalid_Argument [18.4.4.3]");
   declare
      Socket  : POSIX_IO.File_Descriptor;
      Name    : Internet_Socket_Address_Pointer;
      Junk    : Internet_Address;
      function To_Internet_Socket_Address_Pointer is new
         Unchecked_Conversion (System.Address,
            Internet_Socket_Address_Pointer);
   begin
      Name := To_Internet_Socket_Address_Pointer (Junk'Address);
      Socket := Create (PF_INET, Datagram_Socket, IPPROTO_UDP);
      Specify_Peer (Socket, +Name);
      Expect_Exception ("A068");
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code /= Invalid_Argument then
         Unexpected_Exception (E1, "A069");
      end if;
   when E2 : others => Unexpected_Exception (E2, "A070");
   end;

   -----------------------------------------------------------------------
   --  Unspecify a peer that is the local machines telnet port

   Test ("Unpecify_Peer [18.4.4]");
   declare
      Socket  : POSIX_IO.File_Descriptor;
   begin
      Socket := Create (PF_INET, Datagram_Socket, IPPROTO_UDP);
      Specify_Peer_Tests (Socket, 23, String_To_Internet_Address ("127.0.0.1"),
         No_Error, "A000", "A071", "A072");
      Unspecify_Peer_Tests (Socket, Error1 => "A000", Error2 => "A073",
         Error3 => "A074");
      Close (Socket);
   end;

   -----------------------------------------------------------------------
   --  Unspecify a peer that hasn't been specified give the
   --  Invalid_Argument error code.

   Test ("Invalid_Argument [18.4.4]");
   declare
      Socket  : POSIX_IO.File_Descriptor;
   begin
      Socket := Create (PF_INET, Datagram_Socket, IPPROTO_UDP);
      Unspecify_Peer_Tests (Socket, Expected => Invalid_Argument,
         Error1 => "A000", Error2 => "A075", Error3 => "A076");
      Close (Socket);
   end;

   -----------------------------------------------------------------------
   --  Set socket to listen on system chosen port

   Test ("Listen [18.4.11]");
   declare
      Socket  : POSIX_IO.File_Descriptor;
   begin
      Socket := Create (PF_INET, Stream_Socket, IPPROTO_TCP);
      Listen_Tests (Socket, Error1 => "A000",
         Error2 =>  "A077", Error3 => "A078");
      Close (Socket);
   end;

   -----------------------------------------------------------------------
   --  Trying to listen on a closes socket results in the
   --  Bad_File_Desriptor error code

   Test ("Bad_File_Desriptor [18.4.11.3]");
   declare
      Socket  : POSIX_IO.File_Descriptor;
   begin
      Socket := Create (PF_INET, Stream_Socket, IPPROTO_TCP);
      Listen_Tests (Socket, Expected => Bad_File_Descriptor,
         Error1 => "A079", Error2 =>  "A080", Error3 => "A081",
         Close_Socket => true);
   end;

   -----------------------------------------------------------------------
   --  Trying to listen on a non-socket results in the
   --  Not_A_Socket error code

   Test ("Not_A_Socket [18.4.11.3]");
   declare
      Socket  : POSIX_IO.File_Descriptor := 0;
   begin
      Listen_Tests (Socket, Expected => Not_A_Socket,
         Error1 => "A082", Error2 =>  "A083", Error3 => "A084",
         Close_Socket => true);
   end;

   -----------------------------------------------------------------------
   --  Trying to listen on a UDP socket results in the
   --  Option_Not_Supported error code

   Test ("Option_Not_Supported [18.4.11.3]");
   declare
      Socket  : POSIX_IO.File_Descriptor;
   begin
      Socket := Create (PF_INET, Datagram_Socket, IPPROTO_UDP);
      Listen_Tests (Socket, Expected => Option_Not_Supported,
         Error1 => "A085", Error2 =>  "A086", Error3 => "A087");
   end;

   -----------------------------------------------------------------------
   --  Shutdown a connection

   Test ("Shutdown [18.4.14]");
   declare
      Socket  : POSIX_IO.File_Descriptor;
   begin
      Socket := Create (PF_INET, Stream_Socket, IPPROTO_TCP);
      Shutdown_Tests (Socket,
         Mode => Further_Sends_And_Receives_Disallowed,
         Error1 => "A000", Error2 =>  "A088", Error3 => "A089");
      Close (Socket);
   end;

   -----------------------------------------------------------------------
   --  Trying to shutdown a closed socket results in the
   --  Bad_File_Desriptor error code.

   Test ("Bad_File_Descriptor [18.4.14.3]");
   declare
      Socket  : POSIX_IO.File_Descriptor;
   begin
      Socket := Create (PF_INET, Stream_Socket, IPPROTO_TCP);
      Shutdown_Tests (Socket, Expected => Bad_File_Descriptor,
         Mode => Further_Sends_And_Receives_Disallowed,
         Close_Socket => true, Error1 => "A090", Error2 =>  "A091",
         Error3 => "A092");
   end;

   -----------------------------------------------------------------------
   --  Trying to shutdown using a non-connected socket results in the
   --  Not_Connected error code.

   Test ("Not_Connected [18.4.14.3]");
   declare
      Socket  : POSIX_IO.File_Descriptor;
   begin
      Socket := Create (PF_INET, Stream_Socket, IPPROTO_TCP);
      Shutdown_Tests (Socket, Mode => Further_Sends_Disallowed,
         Error1 => "A093", Error2 =>  "A094", Error3 => "A095",
         Expected => Not_Connected, Should_Connect => false);
   end;

   -----------------------------------------------------------------------
   --  Trying to shutdown using a non-socket results in the
   --  Not_A_Socket error code.

   Test ("Not_A_Socket [18.4.14.3]");
   declare
      Socket  : POSIX_IO.File_Descriptor := 0;
   begin
      Shutdown_Tests (Socket, Mode => Further_Sends_Disallowed,
         Error1 => "A096", Error2 =>  "A097", Error3 => "A098",
         Expected => Not_A_Socket, Should_Connect => false);
   end;


   --  ===============================================================  --
   --  ==                                                           ==  --
   --  ==  The few remaining functions require multiple processes   ==  --
   --  ==  to be used to test them.  Please refer to the p180402    ==  --
   --  ==  test files.                                              ==  --
   --  ==                                                           ==  --
   --  ===============================================================  --

   Done;
exception when E : others => Fatal_Exception (E, "A999");
end p180401;

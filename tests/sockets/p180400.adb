------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5c VALIDATION TEST SUITE                      --
--                                                                          --
--                             P 1 8 0 4 0 0                                --
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

--  Basic test for package POSIX_Sockets,
--  in IEEE Std 1003.5c Section 18.4.

--  This test covers only features that depend only on
--  the package itself and features from other packages
--  that are required to be supported.

with POSIX,
     POSIX_Sockets,
     POSIX_Report,
     POSIX_IO,
     Test_Parameters;

--  with Ada.Integer_Text_IO;
--  with Ada.Text_IO;


procedure p180400 is

   use POSIX,
       POSIX_Sockets,
       POSIX_IO,
       POSIX_Report,
       Test_Parameters;

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

   procedure Test_Get_Socket_Address_Info_4Param
      (Name     : in POSIX_String    := Valid_Internet_Name;
       Service  : in POSIX_String    := "telnet";
       Flags    : in Address_Flags   := Use_For_Binding;
       Family   : in Protocol_Family := PF_INET;
       To       : in Socket_Type     := Stream_Socket;
       Protocol : in Protocol_Number := IPPROTO_TCP;
       Expected : in Error_Code      := Operation_Not_Implemented;
       Error1   : in String;
       Error2   : in String;
       Error3   : in String          := "A000");


   procedure Test_Create
      (Family   : in Protocol_Family := PF_INET;
       Soc_Type : in Socket_Type     := Stream_Socket;
       Protocol : in Protocol_Number := IPPROTO_TCP;
       Expected : in Error_Code      := No_Error;
       Error1   : in String;
       Error2   : in String          := "A000";
       Error3   : in String;
       Error4   : in String);

   procedure Test_Create_Pair
      (Family   : in Protocol_Family := PF_UNIX;
       Soc_Type : in Socket_Type     := Stream_Socket;
       Protocol : in Protocol_Number := IPPROTO_IP;
       Expected : in Error_Code      := No_Error;
       Error1   : in String;
       Error2   : in String          := "A000";
       Error3   : in String;
       Error4   : in String);

   procedure Test_Get_Socket_Address_Info_3Param
      (Name     : in POSIX_String    := Valid_Internet_Name;
       Service  : in POSIX_String    := "telnet";
       Expected : in Error_Code      := No_Error;
       Error1   : in String          := "A000";
       Error2   : in String;
       Error3   : in String);

   generic
      type T is (<>);
         with function Get (Socket : in POSIX_IO.File_Descriptor)
            return T;
         with procedure Set
            (Socket : in POSIX_IO.File_Descriptor;
             To     : in T);
   procedure Testit
      (Name     : in String;
       Init     : in T;
       Value    : in T;
       Er1, Er2, Er3, Er4, Er5, Er6, Er7, Er8, Er9,
          Er10, Er11, Er12 : in String);


   -----------------------------------------------------------------------
   --  Procedure used to test Get_Socket_Address_Info 4-parameter version

   procedure Test_Get_Socket_Address_Info_4Param
      (Name     : in POSIX_String    := Valid_Internet_Name;
       Service  : in POSIX_String    := "telnet";
       Flags    : in Address_Flags   := Use_For_Binding;
       Family   : in Protocol_Family := PF_INET;
       To       : in Socket_Type     := Stream_Socket;
       Protocol : in Protocol_Number := IPPROTO_TCP;
       Expected : in Error_Code      := Operation_Not_Implemented;
       Error1   : in String;
       Error2   : in String;
       Error3   : in String          := "A000")
   is
      Info    : Socket_Address_Info_List;
      Request : Socket_Address_Info;
   begin
      Set_Flags (Request, Flags);
      Set_Family (Request, Family);
      Set_Socket_Type (Request, To);
      Set_Protocol_Number (Request, Protocol);
      Get_Socket_Address_Info (Name, Service, Request, Info);
      if Error3 /= "A000" then
         Expect_Exception (Error3);
      end if;
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code /= Expected then
         Optional (Network_Management_Option,
            Unknown_Protocol_Option, E1, Error1);
      end if;
   when E2 : others => Unexpected_Exception (E2, Error2);
   end Test_Get_Socket_Address_Info_4Param;


   -----------------------------------------------------------------------
   --  Procedure used to test Create method

   procedure Test_Create
      (Family   : in Protocol_Family := PF_INET;
       Soc_Type : in Socket_Type     := Stream_Socket;
       Protocol : in Protocol_Number := IPPROTO_TCP;
       Expected : in Error_Code      := No_Error;
       Error1   : in String;
       Error2   : in String          := "A000";
       Error3   : in String;
       Error4   : in String)
   is
      Sock : POSIX_IO.File_Descriptor := 0;
   begin
      Sock := Create (Family, Soc_Type, Protocol);
      Assert (Sock /= 0, Error1);
      if Error2 /= "A000" then
         Expect_Exception (Error2);
      end if;
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code /= Expected then
         Unexpected_Exception (E1, Error3);
      end if;
   when E2 : others => Unexpected_Exception (E2, Error4);
   end Test_Create;

   -----------------------------------------------------------------------
   --  Procedure used to test Create_Pair method

   procedure Test_Create_Pair
      (Family   : in Protocol_Family := PF_UNIX;
       Soc_Type : in Socket_Type     := Stream_Socket;
       Protocol : in Protocol_Number := IPPROTO_IP;
       Expected : in Error_Code      := No_Error;
       Error1   : in String;
       Error2   : in String          := "A000";
       Error3   : in String;
       Error4   : in String)
   is
      Peer1, Peer2 : POSIX_IO.File_Descriptor := 0;
   begin
      Create_Pair (Peer1, Peer2, Family, Soc_Type, Protocol);
      Assert (Peer1 /= 0 and Peer2 /= 0,
         Error1 & ": Socket Pair not Created");
      if Error2 /= "A000" then
         Expect_Exception (Error2);
      end if;
      POSIX_IO.Close (Peer1);
      POSIX_IO.Close (Peer2);
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code /= Expected then
         Unexpected_Exception (E1, Error3);
      end if;
   when E2 : others => Unexpected_Exception (E2, Error4);
   end Test_Create_Pair;



   -----------------------------------------------------------------------
   --  Procedure used to test Get_Socket_Address_Info 3-parameter version

   procedure Test_Get_Socket_Address_Info_3Param
      (Name     : in POSIX_String    := Valid_Internet_Name;
       Service  : in POSIX_String    := "telnet";
       Expected : in Error_Code      := No_Error;
       Error1   : in String          := "A000";
       Error2   : in String;
       Error3   : in String)
   is
      Info : Socket_Address_Info_List;
   begin
      Get_Socket_Address_Info (Name, Service, Info);
      if Error1 /= "A000" then
         Expect_Exception (Error1);
      end if;
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code /= Expected then
         Optional (Network_Management_Option,
            Unknown_Protocol_Option, E1, Error2);
      end if;
   when E2 : others => Unexpected_Exception (E2, Error3);
   end Test_Get_Socket_Address_Info_3Param;


   -----------------------------------------------------------------------
   --  Procedure used to test Get and Set methods used with protocol
   --  independent Socket information

   procedure Testit
      (Name     : in String;
       Init     : in T;
       Value    : in T;
       Er1, Er2, Er3, Er4, Er5, Er6, Er7, Er8, Er9,
          Er10, Er11, Er12 : in String)

   is
   begin
      --------------------------------------------------------------------
      --  The Set and Get methods are consistent

      Test ("Set/Get_" & Name & " [18.4.9]");
      declare
         Socket : POSIX_IO.File_Descriptor := 0;
         To     : T                        := Value;
      begin
         Socket := Create (PF_INET, Stream_Socket, IPPROTO_TCP);
         Set (Socket, Init);
         Comment ("Socket Set_" & Name);
         To := Get (Socket);
         Assert (To = Init, Er1);
      exception when E : others => Unexpected_Exception (E, Er2);
      end;

      --------------------------------------------------------------------
      --  The Get method returns a default value

      Test ("Get_" & Name & " (Default) [18.4.9.2]");
      declare
         Socket : POSIX_IO.File_Descriptor := 0;
         To     : T                        := Init;
      begin
         Socket := Create (PF_INET, Datagram_Socket, IPPROTO_UDP);
         To := Get (Socket);
         Assert (To = Value, Er3);
      exception when E : others => Unexpected_Exception (E, Er4);
      end;

      --------------------------------------------------------------------
      --  The Set causes the Not_A_Socket POSIX error to
      --  be raised when the socket has not been created

      Test ("Not_A_Socket (" & Name & ") [18.4.9.3]");
      declare
         Socket : POSIX_IO.File_Descriptor := 0;
         To     : T                        := Init;
      begin
         Set  (Socket, To);
         Expect_Exception (Er5);
      exception
      when E1 : POSIX_Error =>
         if Get_Error_Code /= Not_A_Socket then
            Unexpected_Exception (E1, Er6);
         end if;
      when E2 : others => Unexpected_Exception (E2, Er7);
      end;

      --------------------------------------------------------------------
      --  The Get  causes the Not_A_Socket POSIX error to
      --  be raised when the socket has not been created

      Test ("Not_A_Socket (" & Name & ") [18.4.9.3]");
      declare
         Socket : POSIX_IO.File_Descriptor := 0;
         To     : T                        := Init;
      begin
         To := Get (Socket);
         Expect_Exception (Er8);
      exception
      when E1 : POSIX_Error =>
         if Get_Error_Code /= Not_A_Socket then
            Unexpected_Exception (E1, Er9);
         end if;
      when E2 : others => Unexpected_Exception (E2, Er10);
      end;

      --------------------------------------------------------------------
      --  The Set  may cause the Is_Already_Connected
      --  POSIX error to be raised when the socket is already connected

      Test ("Is_Already_Connected (" & Name & ") [18.4.9.3]");
      declare
         Peer1, Peer2 : POSIX_IO.File_Descriptor := 0;
         To           : T                        := Init;
      begin
         Create_Pair (Peer1, Peer2, PF_UNIX,
            Stream_Socket, IPPROTO_IP);
         Set (Peer1, To);
         POSIX_IO.Close (Peer1);
         POSIX_IO.Close (Peer2);
      exception
      when E1 : POSIX_Error =>
         if Get_Error_Code /= Is_Already_Connected then
            Comment ("Connected Socket caused an error");
            Unexpected_Exception (E1, Er11);
         end if;
      when E2 : others => Unexpected_Exception (E2, Er12);
      end;
   end Testit;


--------------------------------------------------------------------------
--  Begin Tests

begin
   Header ("p180400");
   Test ("package POSIX.Sockets");

   -----------------------------------------------------------------------
   --  Socket Types are in the proper positions

   Test ("Socket Types Position [18.4.1.1]");
   declare
      Type_of_Socket : Socket_Type;
   begin
      Type_of_Socket := 0;
      Assert (Socket_Type'First = 0 and
              Socket_Type'Pos (Datagram_Socket) = 1 and
              Socket_Type'Pos (Stream_Socket) = 2 and
              Socket_Type'Pos (Raw_Socket) = 4 and
              Socket_Type'Last = 6, "A001");
   exception when E : others => Unexpected_Exception (E, "A002");
   end;

   -----------------------------------------------------------------------
   --  Socket Types are unique

   Test ("Socket Types Uniqueness [18.4.1.1]");
   declare
      A : constant array (1 .. 5) of Socket_Type :=
        (Stream_Socket,
         Datagram_Socket,
         Raw_Socket,
         Sequenced_Packet_Socket,
         Unspecified_Socket_Type);
   begin
      for I in A'Range loop
         for J in A'Range loop
            Assert (A (I) /= A (J) or I = J, "A003");
         end loop;
      end loop;
   exception when E : others => Unexpected_Exception (E, "A004");
   end;

   -----------------------------------------------------------------------
   --  Protocol Family has the appropriate range

   Test ("Protocol Family  [18.4.1.1]");
   declare
      Proto_Family   : Protocol_Family;
   begin
      Proto_Family := 0;
      Assert (Protocol_Family'First = 0 and
              Protocol_Family'Last = 25, "A005");
   exception when E : others => Unexpected_Exception (E, "A006");
   end;

   -----------------------------------------------------------------------
   --  Unspecified Protocol has been declared with the proper value

   Test ("Unspecified Protocol  [18.4.1.1]");
   begin
      Assert (Unspecified_Protocol = 0, "A007");
   exception when E : others => Unexpected_Exception (E, "A008");
   end;

   -----------------------------------------------------------------------
   --  Protocol_Number has the appropriate range

   Test ("Protocol Number  [18.4.1.1]");
   declare
      Proto_Number   : Protocol_Number;
   begin
      Proto_Number := 0;
      Assert (Protocol_Number'First = 0 and
              Protocol_Number'Last = 65535, "A009");
   exception when E : others => Unexpected_Exception (E, "A010");
   end;

   -----------------------------------------------------------------------
   --  Default_Protocol has been declared with the proper value

   Test ("Default Protocol  [18.4.1.1]");
   begin
      Assert (Default_Protocol = 0, "A011");
   exception when E : others => Unexpected_Exception (E, "A012");
   end;

   -----------------------------------------------------------------------
   --  Socket_Address_Pointer type has been propely declared

   Test ("Socket Addresses  [18.4.1.2]");
   declare
      SocAddrPtr : Socket_Address_Pointer;
   begin
      SocAddrPtr := Null_Socket_Address;
   exception when E : others => Unexpected_Exception (E, "A013");
   end;

   -----------------------------------------------------------------------
   --  IO_Vector_Range has the proper range

   Test ("Socket Messages : IO_Vector_Range  [18.4.1.3]");
   declare
      IOVRange : IO_Vector_Range;
   begin
      IOVRange := 12;
      Assert (IO_Vector_Range'First = 1, "A014");
      Assert (IO_Vector_Range'Last = Natural'Last, "A015");
   exception when E : others => Unexpected_Exception (E, "A016");
   end;

   -----------------------------------------------------------------------
   --  All options in Message_Option_Set are unique

   Test ("Message_Option_Set [18.4.1.3]");
   declare
      A : constant array (1 .. 4) of Message_Option_Set :=
         (Peek_Only,
          Process_OOB_Data,
          Wait_For_All_Data,
          Do_Not_Route);
   begin
      for I in A'Range loop
         for J in A'Range loop
            Assert (A (I) /= A (J) or I = J, "A017");
         end loop;
      end loop;
   exception when E : others => Unexpected_Exception (E, "A018");
   end;

   -----------------------------------------------------------------------
   --  All Message_Status_Set options are unique

   Test ("Message_Status_Set [18.4.1.3]");
   declare
      A : constant array (1 .. 4) of Message_Status_Set :=
         (Received_OOB_Data,
          End_Of_Message,
          Message_Truncated,
          Ancillary_Data_Lost);
   begin
      for I in A'Range loop
         for J in A'Range loop
            Assert (A (I) /= A (J) or I = J, "A019");
         end loop;
      end loop;
   exception when E : others => Unexpected_Exception (E, "A020");
   end;

   -----------------------------------------------------------------------
   --  A Socket properly filled in socket is created

   Test ("Create [18.4.5]");
   begin
      Test_Create (Error1 => "A021", Error3 => "A022",
         Error4 => "A023");
   end;

   -----------------------------------------------------------------------
   --  A Socket that uses the default parameter is created

   Test ("Create, useing default parameter [18.4.5]");
   declare
      Sock : POSIX_IO.File_Descriptor := 0;
   begin
      Sock := Create (PF_INET, Stream_Socket);
      Assert (Sock /= 0, "A024");
   exception when E : others => Unexpected_Exception (E, "A025");
   end;

   -----------------------------------------------------------------------
   --  A bad protocol family creates the Protocol_Not_Supported
   --  POSIX_Error code

   Test ("Create Error Checking : PNS_PF [18.4.5.3]");
   begin
      Test_Create (Family => PF_OSI,
      Expected => Protocol_Not_Supported, Error1 => "A026",
      Error2 => "A027", Error3 => "A028", Error4 => "A029");
   end;

   -----------------------------------------------------------------------
   --  A bad protocol number creates the Protocol_Not_Supported
   --  POSIX_Error code

   Test ("Create Error Checking : PNS_PN [18.4.5.3]");
   begin
      Test_Create (Protocol => 12,
      Expected => Protocol_Not_Supported, Error1 => "A030",
      Error2 => "A031", Error3 => "A032", Error4 => "A033");
   end;

   -----------------------------------------------------------------------
   --  A bad socket type causes a Socket_Not_Supported POSOIX_Error code

   Test ("Create Error Checking : STNS [18.4.5.3]");
   begin
      Test_Create (Soc_Type => 5,
      Expected => Socket_Not_Supported, Error1 => "A034",
      Error2 => "A035", Error3 => "A036", Error4 => "A037");
   end;

   -----------------------------------------------------------------------
   --  A pair of connected sockets is created

   Test ("Create_Pair [18.4.6]");
   begin
      Test_Create_Pair (Error1 => "A038", Error3 => "A039",
         Error4 => "A040");
   end;

   -----------------------------------------------------------------------
   --  A pair of connected sockets is created with a default
   --  protocol number

   Test ("Create_Pair, useing default parameter [18.4.6]");
   declare
      Peer1, Peer2 : POSIX_IO.File_Descriptor := 0;
   begin
      Create_Pair (Peer1, Peer2, PF_UNIX, Stream_Socket);
      Assert (Peer1 /= 0 and Peer2 /= 0, "A041");
      POSIX_IO.Close (Peer1);
      POSIX_IO.Close (Peer2);
   exception when E : others => Unexpected_Exception (E, "A042");
   end;

   -----------------------------------------------------------------------
   --  Trying to create a socket pair with a protocol family that doesn't
   --  support pair creation gives an Option_Not_Supported
   --  POSIX_Error code

   Test ("Create_Pair Error Checking : ONS [18.4.6.3]");
   begin
      Test_Create_Pair (Family => PF_INET,
      Expected => Option_Not_Supported,
      Error1 => "A043", Error2 => "A044", Error3 => "A045",
      Error4 => "A046");
   end;

   -----------------------------------------------------------------------
   --  Trying to create a socket pair with an invalid protocol number
   --  gives a Protocol_Not_Supported POSIX_Error code

   Test ("Create_Pair Error Checking : PNS_PN [18.4.5.3]");
   begin
      Test_Create_Pair (Protocol => 18,
      Expected => Protocol_Not_Supported,
      Error1 => "A047", Error2 => "A048", Error3 => "A049",
      Error4 => "A050");
   end;

   -----------------------------------------------------------------------
   --  Trying to create a socket pair with an invalid protocol family
   --  gives a Protocol_Not_Supported POSIX_Error code

   Test ("Create_Pair Error Checking : PNS_PF [18.4.5.3]");
   begin
      Test_Create_Pair (Family => PF_ISO,
      Expected => Protocol_Not_Supported,
      Error1 => "A051", Error2 => "A052", Error3 => "A053",
      Error4 => "A054");
   end;

   -----------------------------------------------------------------------
   --  The Address_Flags options are all unique

   Test ("Address_Flags type [18.4.7]");
   declare
      A : constant array (1 .. 2) of Address_Flags :=
        (Use_For_Binding,
         Canonical_Name);
   begin
      for I in A'Range loop
         for J in A'Range loop
            Assert (A (I) /= A (J) or I = J, "A055");
         end loop;
      end loop;
   exception when E : others => Unexpected_Exception (E, "A056");
   end;

   -----------------------------------------------------------------------
   --  If the Network Management option is supported then the get and set
   --  methods are consistent

   Test ("Set/Get_Flags Test [18.4.7]");
   declare
      Info_Item : Socket_Address_Info;
      Flags     : Address_Flags;
   begin
      Flags := Canonical_Name;
      Set_Flags (Info_Item, Flags);
      Flags := Use_For_Binding;

      --  The Assert statement is used to garauntee that the optimizer
      --  doesn't remove the previous assignment due to it being dead code.
      Assert (Flags = Use_For_Binding, "A057");
      Flags := Get_Flags (Info_Item);
      Assert (Flags = Canonical_Name, "A058");
   exception
   when E1 : POSIX_Error =>
      Optional (Network_Management_Option,
         Unknown_Protocol_Option, E1, "A059");
   when E2 : others => Unexpected_Exception (E2, "A060");
   end;

   -----------------------------------------------------------------------
   --  If the Network Management option is supported then the get and set
   --  methods are consistent

   Test ("Set/Get_Family Test [18.4.7]");
   declare
      Info_Item : Socket_Address_Info;
      Family    : Protocol_Family;
   begin
      Family := PF_INET;
      Set_Family (Info_Item, Family);
      Family := PF_UNIX;

      --  The Assert statement is used to garauntee that the optimizer
      --  doesn't remove the previous assignment due to it being dead code.
      Assert (Family = PF_UNIX, "A061");
      Family := Get_Family (Info_Item);
      Assert (Family = PF_INET, "A062");
   exception
   when E1 : POSIX_Error =>
      Optional (Network_Management_Option,
         Unknown_Protocol_Option, E1, "A063");
   when E2 : others => Unexpected_Exception (E2, "A064");
   end;

   -----------------------------------------------------------------------
   --  If the Network Management option is supported then the Set and Get
   --  methods are consistent

   Test ("Set/Get_Socket_Type Test [18.4.7]");
   declare
      Info_Item : Socket_Address_Info;
      To        : Socket_Type;
   begin
      To := Stream_Socket;
      Set_Socket_Type (Info_Item, To);
      To := Raw_Socket;

      --  The Assert statement is used to garauntee that the optimizer
      --  doesn't remove the previous assignment due to it being dead code.
      Assert (To = Raw_Socket, "A065");
      To := Get_Socket_Type (Info_Item);
      Assert (To = Stream_Socket, "A066");
   exception
   when E1 : POSIX_Error =>
      Optional (Network_Management_Option,
         Unknown_Protocol_Option, E1, "A067");
   when E2 : others => Unexpected_Exception (E2, "A068");
   end;

   -----------------------------------------------------------------------
   --  If the Network Management option is supported then the Get and Set
   --  methods are consistent

   Test ("Set/Get_Protocol_Number Test [18.4.7]");
   declare
      Info_Item : Socket_Address_Info;
      Protocol  : Protocol_Number;
   begin
      Protocol := IPPROTO_TCP;
      Set_Protocol_Number (Info_Item, Protocol);
      Protocol := IPPROTO_UDP;

      --  The Assert statement is used to garauntee that the optimizer
      --  doesn't remove the previous assignment due to it being dead code.
      Assert (Protocol = 17, "A069");
      Protocol := Get_Protocol_Number (Info_Item);
      Assert (Protocol = 6, "A070");
   exception
   when E1 : POSIX_Error =>
      Optional (Network_Management_Option,
         Unknown_Protocol_Option, E1, "A071");
   when E2 : others => Unexpected_Exception (E2, "A072");
   end;

   -----------------------------------------------------------------------
   --  If the Network Management option is supported then Info is filled
   --  up with the appropriate information

   Test ("Get_Socket_Address_Info (3-param version) [18.4.7]");
   begin
      Test_Get_Socket_Address_Info_3Param (Error2 => "A073",
         Error3 => "A074");
   end;

   -----------------------------------------------------------------------
   --  If the Network Management option is supported then Info is filled
   --  up with the appropriate information

   Test ("Get_Socket_Address_Info (3-param with null Name) [18.4.7]");
   begin
      Test_Get_Socket_Address_Info_3Param (Name => "",
         Error2 => "A075", Error3 => "A076");
   end;

   -----------------------------------------------------------------------
   --  If the Network Management option is supported then Info is filled
   --  up with the appropriate information

   Test ("Get_Socket_Address_Info (3-param with null Service) [18.4.7]");
   begin
      Test_Get_Socket_Address_Info_3Param (Service => "",
         Error2 => "A077", Error3 => "A078");
   end;

   -----------------------------------------------------------------------
   --  If the Network Management option is supported then the
   --  Name_Not_Known POSIX_Error code will be raised

   Test ("Get_Socket_Address_Info (3-param) Error NNK_Nam [18.4.7.3]");
   begin
      Test_Get_Socket_Address_Info_3Param (Name => "xi.csfsu.edu",
         Expected => Name_Not_Known, Error1 => "A079",
         Error2 => "A080", Error3 => "A081");
   end;

   -----------------------------------------------------------------------
   --  If the Network Management option is supported then the
   --  Name_Not_Known POSIX_Error code will be raised

   Test ("Get_Socket_Address_Info (3-param) Error NNK_Ser [18.4.7.3]");
   begin
      Test_Get_Socket_Address_Info_3Param (Service => "retelnet",
         Expected => Name_Not_Known, Error1 => "A082",
         Error2 => "A083", Error3 => "A084");
   end;

   -----------------------------------------------------------------------
   --  If the Network Management option is supported then the
   --  Name_Not_Known POSIX_Error code will be raised

   Test ("Get_Socket_Address_Info (3-param) Error NNK_Nul [18.4.7.3]");
   begin
      Test_Get_Socket_Address_Info_3Param (Name => "", Service => "",
         Expected => Name_Not_Known, Error1 => "A085",
         Error2 => "A086", Error3 => "A087");
   end;

   -----------------------------------------------------------------------
   --  If the Network Management option is supported then the Info object
   --  will be emptied

   Test ("Make_Empty [18.4.7]");
   declare
      Info : Socket_Address_Info_List;
   begin
      Get_Socket_Address_Info (Valid_Internet_Name, "telnet", Info);
      Make_Empty (Info);
   exception
   when E1 : POSIX_Error =>
      Optional (Network_Management_Option,
         Operation_Not_Implemented, E1, "A088");
   when E2 : others => Unexpected_Exception (E2, "A089");
   end;

   -----------------------------------------------------------------------
   --  If the Network Management option is supported then Info is filled
   --  up with the appropriate information

   Test ("Get_Socket_Address_Info (4-param version) [18.4.7]");
   begin
      Test_Get_Socket_Address_Info_4Param
         (Error1 => "A090", Error2 => "A091");
   end;

   -----------------------------------------------------------------------
   --  If the Network Management option is supported then Info is filled
   --  up with the appropriate information

   Test ("Get_Socket_Address_Info (4-param with null Name) [18.4.7]");
   begin
      Test_Get_Socket_Address_Info_4Param
         ("", Error1 => "A092", Error2 => "A093");
   end;

   -----------------------------------------------------------------------
   --  If the Network Management option is supported then Info is filled
   --  up with the appropriate information

   Test ("Get_Socket_Address_Info (4-param with null Service) [18.4.7]");
   begin
      Test_Get_Socket_Address_Info_4Param
         (Service => "", Error1 => "A094", Error2 => "A095");
   end;

   -----------------------------------------------------------------------
   --  If the Network Management option is supported then the
   --  Name_Not_Known POSIX_Error code will be raised

   Test ("Get_Socket_Address_Info (4-param) Error NNK_Nul [18.4.7]");
   begin
      Test_Get_Socket_Address_Info_4Param
         (Name => "", Service => "", Expected => Name_Not_Known,
           Error1 => "A096", Error2 => "A097", Error3 => "A098");
   end;

   -----------------------------------------------------------------------
   --  If the Network Management option is supported then the
   --  Service_Not_Supported POSIX_Error code will be raised since the
   --  socket type doesn't have the requested service

   Test ("Get_Socket_Address_Info (4-param) Error SNS [18.4.7]");
   begin
      Test_Get_Socket_Address_Info_4Param
         (Valid_Internet_Name, "retelnet",
           Expected => Service_Not_Supported,
           Error1 => "A099", Error2 => "A100", Error3 => "A101");
   end;

   -----------------------------------------------------------------------
   --  If the Network Management option is supported then the
   --  Name_Not_Known POSIX_Error code will be raised

   Test ("Get_Socket_Address_Info (4-param) Error NNK_Nam [18.4.7]");
   begin
      Test_Get_Socket_Address_Info_4Param
         ("@#$%^&*()", Expected => Name_Not_Known,
           Error1 => "A102", Error2 => "A103", Error3 => "A104");
   end;

   -----------------------------------------------------------------------
   --  If the Network Management option is supported then the
   --  Invalid_Flags POSIX_Error code should be raised

   Test ("Ivalid_Flags Error  [18.4.7.3]");
   begin
      Test_Get_Socket_Address_Info_4Param
         (Flags => Empty_Set - Canonical_Name,
          Expected => Invalid_Flags,
          Error1 => "A105", Error2 => "A106", Error3 => "A107");
   end;

   -----------------------------------------------------------------------
   --  If the Network Management option is supported then the
   --  Unknown_Protocol_Family POSIX_Error code will be raised

   Test ("Unknown_Protocol_Family  [18.4.7.3]");
   begin
      Test_Get_Socket_Address_Info_4Param
         (Family => PF_OSI, Expected => Unknown_Protocol_Family,
          Error1 => "A108", Error2 => "A109", Error3 => "A110");
   end;

   -----------------------------------------------------------------------
   --  If the Network Management option is supported then the
   --  Unknown_Socket_type POSIX_Error code will be raised

   --  ... This test is not working at the moment.  The error occurs in
   --  ... the getaddrinfo.c file under a bad socket type.  The problem
   --  ... occurs when dereferencing the ai_addr pointer.  Not for sure
   --  ... what the exact cause is.  Raises STRORAGE Errror.

--   Test ("Unknown_Socket_Type  [18.4.7.3]");
--   begin
--      Test_Get_Socket_Address_Info_4Param
--         (To => 5, Expected => Unknown_Socket_Type,
--          Error1 => "A111", Error2 => "A112", Error3 => "A113");
--   end;

   -----------------------------------------------------------------------
   --  If the socket has a pending error it will be returned and the
   --  error status shall be set to 0

   Test ("Get_Socket_Error_Status  [18.4.8]");
   declare
      Sock  : POSIX_IO.File_Descriptor := 0;
      Error : POSIX.Error_Code := 0;
   begin
      Sock := Create (PF_INET, Stream_Socket, IPPROTO_TCP);
      Error := Get_Socket_Error_Status (Sock);
      if Error /= 0 then
         Comment ("Socket returned and error; Expected");
      end if;
   exception when E : others => Unexpected_Exception (E, "A114");
   end;

   -----------------------------------------------------------------------
   --  Since the socket was never created Get_Socket_Error_Status will
   --  return Not_A_Socket

   Test ("Not_A_Socket  [18.4.8.3]");
   declare
      Sock  : POSIX_IO.File_Descriptor := 0;
      Error : POSIX.Error_Code := 0;
   begin
      Error := Get_Socket_Error_Status (Sock);
      Expect_Exception ("A115");
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code /= Not_A_Socket then
         Unexpected_Exception (E1, "A116");
      end if;
   when E2 : others => Unexpected_Exception (E2, "A117");
   end;

   -----------------------------------------------------------------------
   --  A socket type of Stream_Socket will be returned

   Test ("Get_Socket_Type  [18.4.8]");
   declare
      Sock            : POSIX_IO.File_Descriptor := 0;
      Type_Of_Socket  : Socket_Type := Datagram_Socket;
   begin
      Sock := Create (PF_INET, Stream_Socket, IPPROTO_TCP);
      Type_Of_Socket := Get_Socket_Type (Sock);
      Assert (Type_Of_Socket = Stream_Socket, "A118");
   exception when E : others => Unexpected_Exception (E, "A119");
   end;

   -----------------------------------------------------------------------
   --  Since the socket was never created an error of Not_A_Socket will
   --  be raised

   Test ("Get_Socket_Type  [18.4.8]");
   declare
      Sock            : POSIX_IO.File_Descriptor := 0;
      Type_Of_Socket  : Socket_Type := Datagram_Socket;
   begin
      Type_Of_Socket := Get_Socket_Type (Sock);
      Assert (Type_Of_Socket = Stream_Socket, "A120");
      Expect_Exception ("A121");
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code /= Not_A_Socket then
         Unexpected_Exception (E1, "A122");
      end if;
   when E2 : others => Unexpected_Exception (E2, "A123");
   end;

   -----------------------------------------------------------------------
   --  Socket_Option_Values are set and in the proper order

   Test ("Socket_Option_Values [18.4.9]");
   declare
      Option_Value : Socket_Option_Value := Enabled;
   begin
      Option_Value := Disabled;
      Assert (Socket_Option_Value'First = Enabled and
         Socket_Option_Value'Last = Disabled, "A124");
   exception when E : others => Unexpected_Exception (E, "A125");
   end;

   -----------------------------------------------------------------------
   --  The Set and Get_Socket_Broadcast method tests

   declare
      procedure Test_Socket_Broadcast is new Testit
         (Socket_Option_Value, Get_Socket_Broadcast,
          Set_Socket_Broadcast);
   begin
      Test_Socket_Broadcast ("Socket_Broadcast", Enabled, Disabled,
         "A126", "A127", "A128", "A129", "A130", "A131", "A132",
         "A133", "A134", "A135", "A136", "A137");
   end;

   -----------------------------------------------------------------------
   --  The Set and Get_Socket_Debugging method tests

   declare
      procedure Test_Socket_Debugging is new Testit
         (Socket_Option_Value, Get_Socket_Debugging,
          Set_Socket_Debugging);
   begin
      Test_Socket_Debugging ("Socket_Debugging", Enabled, Disabled,
         "A138", "A139", "A140", "A141", "A142", "A143", "A144",
         "A145", "A146", "A147", "A148", "A149");
   end;

   -----------------------------------------------------------------------
   --  The Set and Get_Socket_Routing method tests

   declare
      procedure Test_Socket_Routing is new Testit
         (Socket_Option_Value, Get_Socket_Routing,
          Set_Socket_Routing);
   begin
      Test_Socket_Routing ("Socket_Routing", Disabled, Enabled,
         "A150", "A151", "A152", "A153", "A154", "A155", "A156",
         "A157", "A158", "A159", "A160", "A161");
   end;

   -----------------------------------------------------------------------
   --  The Set and Get_Socket_Keep_Alive method tests

   declare
      procedure Test_Socket_Keep_Alive is new Testit
         (Socket_Option_Value, Get_Socket_Keep_Alive,
          Set_Socket_Keep_Alive);
   begin
      Test_Socket_Keep_Alive ("Socket_Keep_Alive", Enabled, Disabled,
         "A162", "A163", "A164", "A165", "A166", "A167", "A168",
         "A169", "A170", "A171", "A172", "A173");
   end;

   -----------------------------------------------------------------------
   --  Subtype Linger_Time has the appropriate bounds


   Test ("Linger_Time subtype bounds [18.4.9]");
   declare
      L_Time : Linger_Time;
   begin
      L_Time := 2;
      Assert (Linger_Time'First = 0, "A174");
      Assert (Linger_Time'Last = POSIX.Seconds'Last, "A175");
   exception when E : others => Unexpected_Exception (E, "A176");
   end;

   -----------------------------------------------------------------------
   --  The Set and Get_Socket_Linger_Time method tests

   declare
      procedure Test_Socket_Linger_Time is new Testit
         (Linger_Time, Get_Socket_Linger_Time,
          Set_Socket_Linger_Time);
   begin
      Test_Socket_Linger_Time ("Socket_Linger_Time", 2, 0,
         "A177", "A178", "A179", "A180", "A181", "A182", "A183",
         "A184", "A185", "A186", "A187", "A188");
   end;

   -----------------------------------------------------------------------
   --  The Set and Get_Socket_OOB_Data_Inline method tests

   declare
      procedure Test_Socket_OOB_Data_Inline is new Testit
         (Socket_Option_Value, Get_Socket_OOB_Data_Inline,
          Set_Socket_OOB_Data_Inline);
   begin
      Test_Socket_OOB_Data_Inline ("Socket_OOB_Data_Inline",
         Enabled, Disabled, "A189", "A190", "A191", "A192", "A193",
         "A194", "A195", "A196", "A197", "A198", "A199", "A200");
   end;

   -----------------------------------------------------------------------
   --  The Set and Get_Socket_Receive_Buffer_Size methods are consistent
   --  Doesn't work with a conenction mode socket
   --  Has to many special cases to use generic

   Test ("Set/Get_Socket_Receive_Buffer_Size [18.4.9]");
   declare
      Socket : POSIX_IO.File_Descriptor := 0;
      To     : POSIX.IO_Count := 16;
   begin
      Socket := Create (PF_INET, Datagram_Socket, IPPROTO_UDP);
      Set_Socket_Receive_Buffer_Size (Socket, 32);
      Comment ("Socket receive buffer size set to 32");
      To := Get_Socket_Receive_Buffer_Size (Socket);
      Assert (To = 32, "A201");
   exception when E : others => Unexpected_Exception (E, "A202");
   end;

   -----------------------------------------------------------------------
   --  The Get_Socket_Receive_Buffer_Size method has a protocol defined
   --  default value, wich is verly unlikly to be 0

   Test ("Get_Socket_Receive_Buffer_Size (Default) [18.4.9.2]");
   declare
      Socket : POSIX_IO.File_Descriptor := 0;
      To     : POSIX.IO_Count := 0;
   begin
      Socket := Create (PF_INET, Datagram_Socket, IPPROTO_UDP);
      To := Get_Socket_Receive_Buffer_Size (Socket);
      Assert (To /= 0, "A203");
   exception when E : others => Unexpected_Exception (E, "A204");
   end;

   -----------------------------------------------------------------------
   --  The Set_Socket_Receive_Buffer_Size causes the Not_A_Socket
   --  POSIX error to be raised when the socket has not been created

   Test ("Not_A_Socket (Set_Socket_Receive_Buffer_Size) [18.4.9.3]");
   declare
      Socket : POSIX_IO.File_Descriptor := 0;
      To     : POSIX.IO_Count           := 256;
   begin
      Set_Socket_Receive_Buffer_Size (Socket, To);
      Expect_Exception ("A205");
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code /= Not_A_Socket then
         Unexpected_Exception (E1, "A206");
      end if;
   when E2 : others => Unexpected_Exception (E2, "A207");
   end;

   -----------------------------------------------------------------------
   --  The Get_Socket_Receive_Buffer_Size causes the Not_A_Socket
   --  POSIX error to be raised when the socket has not been created

   Test ("Not_A_Socket (Get_Socket_Receive_Buffer_Size) [18.4.9.3]");
   declare
      Socket : POSIX_IO.File_Descriptor := 0;
      To     : POSIX.IO_Count           := 256;
   begin
      To := Get_Socket_Receive_Buffer_Size (Socket);
      Expect_Exception ("A208");
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code /= Not_A_Socket then
         Unexpected_Exception (E1, "A209");
      end if;
   when E2 : others => Unexpected_Exception (E2, "A210");
   end;

   -----------------------------------------------------------------------
   --  The Set_Socket_Receive_Buffer_Size may cause the
   --  Is_Already_Connected POSIX error to be raised when the socket
   --  is already connected

   Test ("Is_Already_Connected (Set_Socket_Receive_Buffer_Size) [18.4.9.3]");
   declare
      Peer1, Peer2 : POSIX_IO.File_Descriptor := 0;
      To           : POSIX.IO_Count           := 256;
   begin
      Create_Pair (Peer1, Peer2, PF_UNIX,
         Stream_Socket, IPPROTO_IP);
      Set_Socket_Receive_Buffer_Size (Peer1, To);
      POSIX_IO.Close (Peer1);
      POSIX_IO.Close (Peer2);
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code /= Is_Already_Connected then
         Unexpected_Exception (E1, "A211");
      else
         Comment ("Connected Socket caused an error");
      end if;
   when E2 : others => Unexpected_Exception (E2, "A212");
   end;

   -----------------------------------------------------------------------
   --  The Set_Socket_Receive_Buffer_Size causes a No_Buffer_Space
   --  Error when the requested buffer is too big

   Test ("No_Buffer_Space (Set_Socket_Receive_Buffer_Size) [18.4.9.3]");
   declare
      Socket : POSIX_IO.File_Descriptor;
      To     : POSIX.IO_Count           := 10000000;
   begin
      Socket := Create (PF_INET, Stream_Socket, IPPROTO_TCP);
      Set_Socket_Receive_Buffer_Size (Socket, To);
      Expect_Exception ("A213");
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code /= No_Buffer_Space then
         Unexpected_Exception (E1, "A214");
      end if;
   when E2 : others => Unexpected_Exception (E2, "A215");
   end;

   -----------------------------------------------------------------------
   --  The Set and Get_Socket_Receive_Low_Water_Mark methods are
   --  consistent if the implementation allows the value to be set.
   --  Otherwise the Permission_Denied Error will be raised
   --  If a system does not even support the Receive_Low_Water_Mark
   --  then the Unknown_Protocol_Option error will be raised

   Test ("Set/Get_Socket_Receive_Low_Water_Mark [18.4.9]");
   declare
      Socket : POSIX_IO.File_Descriptor := 0;
      To     : POSIX.IO_Count := 2;
   begin
      Socket := Create (PF_INET, Datagram_Socket, IPPROTO_UDP);
      Set_Socket_Receive_Low_Water_Mark (Socket, 3);
      Comment ("Socket receive buffer size set to 3");
      To := Get_Socket_Receive_Low_Water_Mark (Socket);
      Assert (To = 3, "A216");
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code = Permission_Denied then
         Comment ("Set not allowed -- Test not valid");
      else
         Unexpected_Exception (E1, "A217");
      end if;
   when E2 : others => Unexpected_Exception (E2, "A218");
   end;

   -----------------------------------------------------------------------
   --  The Get_Socket_Receive_Low_Water_Mark method has a default value
   --  of 1

   Test ("Get_Socket_Receive_Low_Water_Mark (Default) [18.4.9.2]");
   declare
      Socket : POSIX_IO.File_Descriptor := 0;
      To     : POSIX.IO_Count := 0;
   begin
      Socket := Create (PF_INET, Stream_Socket, IPPROTO_TCP);
      To := Get_Socket_Receive_Low_Water_Mark (Socket);
      Assert (To = 1, "A219");
   exception
   when E1 : others => Unexpected_Exception (E1, "A220");
   end;

   -----------------------------------------------------------------------
   --  The Set_Socket_Receive_Low_Water_Mark method causes the
   --  Not_A_Socket POSIX error to be raised when the socket
   --  has not been created (See above for other cases)

   Test ("Not_A_Socket (Set_Socket_Receive_Low_Water_Mark) [18.4.9.3]");
   declare
      Socket : POSIX_IO.File_Descriptor := 0;
      To     : POSIX.IO_Count           := 2;
   begin
      Set_Socket_Receive_Low_Water_Mark (Socket, To);
      Expect_Exception ("A221");
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code = Permission_Denied then
         Comment ("Set not allowed -- Test not valid");
      elsif Get_Error_Code /= Not_A_Socket then
         Unexpected_Exception (E1, "A222");
      end if;
   when E2 : others => Unexpected_Exception (E2, "A223");
   end;

   -----------------------------------------------------------------------
   --  The Get_Socket_Receive_Low_Water_Mark method causes the
   --  Not_A_Socket POSIX error to be raised when the socket has not
   --  been created (see above for other cases)

   Test ("Not_A_Socket (Get_Socket_Receive_Low_Water_Mark) [18.4.9.3]");
   declare
      Socket : POSIX_IO.File_Descriptor := 0;
      To     : POSIX.IO_Count           := 2;
   begin
      To := Get_Socket_Receive_Buffer_Size (Socket);
      Expect_Exception ("A224");
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code = Permission_Denied then
         Comment ("Set not allowed -- Test not valid");
      elsif Get_Error_Code /= Not_A_Socket then
         Unexpected_Exception (E1, "A225");
      end if;
   when E2 : others => Unexpected_Exception (E2, "A226");
   end;

   -----------------------------------------------------------------------
   --  The Set_Socket_Receive_Low_Water_Mark may cause the
   --  Is_Already_Connected POSIX error to be raised when the socket
   --  is already connected (see above for other cases)

   Test
      ("Is_Already_Connected (Set_Socket_Receive_Low_Water_Mark) [18.4.9.3]");
   declare
      Peer1, Peer2 : POSIX_IO.File_Descriptor := 0;
      To           : POSIX.IO_Count           := 2;
   begin
      Create_Pair (Peer1, Peer2, PF_UNIX,
         Stream_Socket, IPPROTO_IP);
      Set_Socket_Receive_Low_Water_Mark (Peer1, To);
      POSIX_IO.Close (Peer1);
      POSIX_IO.Close (Peer2);
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code = Permission_Denied then
         Comment ("Set not allowed -- Test not valid");
      elsif Get_Error_Code /= Is_Already_Connected then
         Unexpected_Exception (E1, "A227");
      else
         Comment ("Connected Socket caused an error");
      end if;
   when E2 : others => Unexpected_Exception (E2, "A228");
   end;

   -----------------------------------------------------------------------
   --  The Set and Get_Socket_Receive_Timeout methods are consistent

   Test ("Set/Get_Socket_Receive_Timeout [18.4.9]");
   declare
      Socket : POSIX_IO.File_Descriptor := 0;
      To     : Duration := 16.0;
   begin
      Socket := Create (PF_INET, Datagram_Socket, IPPROTO_UDP);
      Set_Socket_Receive_Timeout (Socket, 32.0);
      Comment ("Socket receive timout set to 32");
      To := Get_Socket_Receive_Timeout (Socket);
      Assert (To = 32.0, "A229");
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code = Permission_Denied then
         Comment ("Set not allowed -- Test not valid");
      else
         Unexpected_Exception (E1, "A230");
      end if;
   when E2 : others => Unexpected_Exception (E2, "A231");
   end;

   -----------------------------------------------------------------------
   --  The Get_Socket_Receive_Timeout method has a defined
   --  default value of 0

   Test ("Get_Socket_Receive_Timeout (Default) [18.4.9.2]");
   declare
      Socket : POSIX_IO.File_Descriptor := 0;
      To     : Duration                 := 2.0;
   begin
      Socket := Create (PF_INET, Stream_Socket, IPPROTO_TCP);
      To := Get_Socket_Receive_Timeout (Socket);
      Assert (To = 0.0, "A232");
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code = Permission_Denied then
         Comment ("Set not allowed -- Test not valid");
      else
         Unexpected_Exception (E1, "A233");
      end if;
   when E2 : others => Unexpected_Exception (E2, "A234");
   end;

   -----------------------------------------------------------------------
   --  The Set_Socket_Receive_Timeout causes the Not_A_Socket
   --  POSIX error to be raised when the socket has not been created

   Test ("Not_A_Socket (Set_Socket_Receive_Timout) [18.4.9.3]");
   declare
      Socket : POSIX_IO.File_Descriptor := 1;
      To     : Duration                 := 3.0;
   begin
      Set_Socket_Receive_Timeout (Socket, To);
      Expect_Exception ("A235");
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code = Permission_Denied then
         Comment ("Set not allowed -- Test not valid");
      elsif Get_Error_Code /= Not_A_Socket then
         Unexpected_Exception (E1, "A236");
      end if;
   when E2 : others => Unexpected_Exception (E2, "A237");
   end;

   -----------------------------------------------------------------------
   --  The Get_Socket_Receive_Timeout causes the Not_A_Socket
   --  POSIX error to be raised when the socket has not been created

   Test ("Not_A_Socket (Get_Socket_Receive_Timeout) [18.4.9.3]");
   declare
      Socket : POSIX_IO.File_Descriptor := 3;
      To     : Duration                 := 2.0;
   begin
      To := Get_Socket_Receive_Timeout (Socket);
      Expect_Exception ("A238");
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code = Permission_Denied then
         Comment ("Set not allowed -- Test not valid");
      elsif Get_Error_Code /= Not_A_Socket then
         Unexpected_Exception (E1, "A239");
      end if;
   when E2 : others => Unexpected_Exception (E2, "A240");
   end;

   -----------------------------------------------------------------------
   --  The Set_Socket_Receive_Timout may cause the
   --  Is_Already_Connected POSIX error to be raised when the socket
   --  is already connected

   Test ("Is_Already_Connected (Set_Socket_Receive_Timeout) [18.4.9.3]");
   declare
      Peer1, Peer2 : POSIX_IO.File_Descriptor := 0;
      To           : Duration                 := 3.0;
   begin
      Create_Pair (Peer1, Peer2, PF_UNIX,
         Stream_Socket, IPPROTO_IP);
      Set_Socket_Receive_Timeout (Peer1, To);
      POSIX_IO.Close (Peer1);
      POSIX_IO.Close (Peer2);
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code = Permission_Denied then
         Comment ("Set not allowed -- Test not valid");
      elsif Get_Error_Code /= Is_Already_Connected then
         Unexpected_Exception (E1, "A241");
      else
         Comment ("Connected Socket caused an error");
      end if;
   when E2 : others => Unexpected_Exception (E2, "A242");
   end;

   -----------------------------------------------------------------------
   --  The Set_Socket_Receive_Timout causes a Domain_Error
   --  Error when the requested buffer is too big

   Test ("Domain_Error (Set_Socket_Receive_Buffer_Size) [18.4.9.3]");
   declare
      Socket : POSIX_IO.File_Descriptor;
      To     : Duration := 10000000.0;
   begin
      Socket := Create (PF_INET, Datagram_Socket, IPPROTO_UDP);
      Set_Socket_Receive_Timeout (Socket, To);
      Expect_Exception ("A243");
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code = Permission_Denied then
         Comment ("Set not allowed -- Test not valid");
      elsif Get_Error_Code /= No_Buffer_Space then
         Unexpected_Exception (E1, "A244");
      end if;
   when E2 : others => Unexpected_Exception (E2, "A245");
   end;

   -----------------------------------------------------------------------
   --  The Set and Get_Socket_Reuse_Addresses method tests

   declare
      procedure Test_Socket_Reuse_Addresses is new Testit
         (Socket_Option_Value, Get_Socket_Reuse_Addresses,
          Set_Socket_Reuse_Addresses);
   begin
      Test_Socket_Reuse_Addresses ("Reuse_Addresses",
         Enabled, Disabled, "A246", "A247", "A248", "A249", "A250",
         "A251", "A252", "A253", "A254", "A255", "A256", "A257");
   end;

   -----------------------------------------------------------------------
   --  The Set and Get_Socket_Send_Buffer_Size methods are consistent
   --  Doesn't work with a conenction mode sockete

   Test ("Set/Get_Socket_Send_Buffer_Size [18.4.9]");
   declare
      Socket : POSIX_IO.File_Descriptor := 0;
      To     : POSIX.IO_Count := 16;
   begin
      Socket := Create (PF_INET, Datagram_Socket, IPPROTO_UDP);
      Set_Socket_Send_Buffer_Size (Socket, 32);
      Comment ("Socket Send buffer size set to 32");
      To := Get_Socket_Send_Buffer_Size (Socket);
      Assert (To = 32, "A258");
   exception when E : others => Unexpected_Exception (E, "A259");
   end;

   -----------------------------------------------------------------------
   --  The Get_Socket_Send_Buffer_Size method has a protocol defined
   --  default value, wich is verly unlikly to be 0

   Test ("Get_Socket_Send_Buffer_Size (Default) [18.4.9.2]");
   declare
      Socket : POSIX_IO.File_Descriptor := 0;
      To     : POSIX.IO_Count := 0;
   begin
      Socket := Create (PF_INET, Datagram_Socket, IPPROTO_UDP);
      To := Get_Socket_Send_Buffer_Size (Socket);
      Assert (To /= 0, "A260");
   exception when E : others => Unexpected_Exception (E, "A261");
   end;

   -----------------------------------------------------------------------
   --  The Set_Socket_Send_Buffer_Size causes the Not_A_Socket
   --  POSIX error to be raised when the socket has not been created

   Test ("Not_A_Socket (Set_Socket_Send_Buffer_Size) [18.4.9.3]");
   declare
      Socket : POSIX_IO.File_Descriptor := 0;
      To     : POSIX.IO_Count           := 256;
   begin
      Set_Socket_Send_Buffer_Size (Socket, To);
      Expect_Exception ("A262");
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code /= Not_A_Socket then
         Unexpected_Exception (E1, "A263");
      end if;
   when E2 : others => Unexpected_Exception (E2, "A264");
   end;

   -----------------------------------------------------------------------
   --  The Get_Socket_Send_Buffer_Size causes the Not_A_Socket
   --  POSIX error to be raised when the socket has not been created

   Test ("Not_A_Socket (Get_Socket_Send_Buffer_Size) [18.4.9.3]");
   declare
      Socket : POSIX_IO.File_Descriptor := 0;
      To     : POSIX.IO_Count           := 256;
   begin
      To := Get_Socket_Send_Buffer_Size (Socket);
      Expect_Exception ("A265");
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code /= Not_A_Socket then
         Unexpected_Exception (E1, "A266");
      end if;
   when E2 : others => Unexpected_Exception (E2, "A267");
   end;

   -----------------------------------------------------------------------
   --  The Set_Socket_Send_Buffer_Size may cause the
   --  Is_Already_Connected POSIX error to be raised when the socket
   --  is already connected

   Test ("Is_Already_Connected (Set_Socket_Send_Buffer_Size) [18.4.9.3]");
   declare
      Peer1, Peer2 : POSIX_IO.File_Descriptor := 0;
      To           : POSIX.IO_Count           := 256;
   begin
      Create_Pair (Peer1, Peer2, PF_UNIX,
         Stream_Socket, IPPROTO_IP);
      Set_Socket_Send_Buffer_Size (Peer1, To);
      POSIX_IO.Close (Peer1);
      POSIX_IO.Close (Peer2);
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code /= Is_Already_Connected then
         Unexpected_Exception (E1, "A268");
      else
         Comment ("Connected Socket caused an error");
      end if;
   when E2 : others => Unexpected_Exception (E2, "A269");
   end;

   -----------------------------------------------------------------------
   --  The Set_Socket_Send_Buffer_Size causes a No_Buffer_Space
   --  Error when the requested buffer is too big

   Test ("No_Buffer_Space (Set_Socket_Send_Buffer_Size) [18.4.9.3]");
   declare
      Socket : POSIX_IO.File_Descriptor;
      To     : POSIX.IO_Count           := 10000000;
   begin
      Socket := Create (PF_INET, Stream_Socket, IPPROTO_TCP);
      Set_Socket_Send_Buffer_Size (Socket, To);
      Expect_Exception ("A270");
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code /= No_Buffer_Space then
         Unexpected_Exception (E1, "A271");
      end if;
   when E2 : others => Unexpected_Exception (E2, "A272");
   end;

   -----------------------------------------------------------------------
   --  The Set and Get_Socket_Send_Low_Water_Mark methods are
   --  consistent if the implementation allows the value to be set.
   --  Otherwise the Permission_Denied Error will be raised
   --  If a system does not even support the Send_Low_Water_Mark
   --  then the Unknown_Protocol_Option error will be raised

   Test ("Set/Get_Socket_Send_Low_Water_Mark [18.4.9]");
   declare
      Socket : POSIX_IO.File_Descriptor := 0;
      To     : POSIX.IO_Count := 2;
   begin
      Socket := Create (PF_INET, Datagram_Socket, IPPROTO_UDP);
      Set_Socket_Send_Low_Water_Mark (Socket, 3);
      Comment ("Socket Send buffer size set to 3");
      To := Get_Socket_Send_Low_Water_Mark (Socket);
      Assert (To = 3, "A273");
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code = Permission_Denied then
         Comment ("Set not allowed -- Test not valid");
      else
         Unexpected_Exception (E1, "A274");
      end if;
   when E2 : others => Unexpected_Exception (E2, "A275");
   end;

   -----------------------------------------------------------------------
   --  The Get_Socket_Send_Low_Water_Mark method has a default value
   --  of 1

   Test ("Get_Socket_Send_Low_Water_Mark (Default) [18.4.9.2]");
   declare
      Socket : POSIX_IO.File_Descriptor := 0;
      To     : POSIX.IO_Count := 0;
   begin
      Socket := Create (PF_INET, Stream_Socket, IPPROTO_TCP);
      To := Get_Socket_Send_Low_Water_Mark (Socket);
      Assert (To = 1, "A276");
   exception
   when E1 : others => Unexpected_Exception (E1, "A277");
   end;

   -----------------------------------------------------------------------
   --  The Set_Socket_Send_Low_Water_Mark method causes the
   --  Not_A_Socket POSIX error to be raised when the socket
   --  has not been created (See above for other cases)

   Test ("Not_A_Socket (Set_Socket_Send_Low_Water_Mark) [18.4.9.3]");
   declare
      Socket : POSIX_IO.File_Descriptor := 0;
      To     : POSIX.IO_Count           := 2;
   begin
      Set_Socket_Send_Low_Water_Mark (Socket, To);
      Expect_Exception ("A278");
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code = Permission_Denied then
         Comment ("Set not allowed -- Test not valid");
      elsif Get_Error_Code /= Not_A_Socket then
         Unexpected_Exception (E1, "A279");
      end if;
   when E2 : others => Unexpected_Exception (E2, "A280");
   end;

   -----------------------------------------------------------------------
   --  The Get_Socket_Send_Low_Water_Mark method causes the
   --  Not_A_Socket POSIX error to be raised when the socket has not
   --  been created (see above for other cases)

   Test ("Not_A_Socket (Get_Socket_Send_Low_Water_Mark) [18.4.9.3]");
   declare
      Socket : POSIX_IO.File_Descriptor := 0;
      To     : POSIX.IO_Count           := 2;
   begin
      To := Get_Socket_Send_Buffer_Size (Socket);
      Expect_Exception ("A281");
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code = Permission_Denied then
         Comment ("Set not allowed -- Test not valid");
      elsif Get_Error_Code /= Not_A_Socket then
         Unexpected_Exception (E1, "A282");
      end if;
   when E2 : others => Unexpected_Exception (E2, "A283");
   end;

   -----------------------------------------------------------------------
   --  The Set_Socket_Send_Low_Water_Mark may cause the
   --  Is_Already_Connected POSIX error to be raised when the socket
   --  is already connected (see above for other cases)

   Test
      ("Is_Already_Connected (Set_Socket_Send_Low_Water_Mark) [18.4.9.3]");
   declare
      Peer1, Peer2 : POSIX_IO.File_Descriptor := 0;
      To           : POSIX.IO_Count           := 2;
   begin
      Create_Pair (Peer1, Peer2, PF_UNIX,
         Stream_Socket, IPPROTO_IP);
      Set_Socket_Send_Low_Water_Mark (Peer1, To);
      POSIX_IO.Close (Peer1);
      POSIX_IO.Close (Peer2);
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code = Permission_Denied then
         Comment ("Set not allowed -- Test not valid");
      elsif Get_Error_Code /= Is_Already_Connected then
         Unexpected_Exception (E1, "A284");
      else
         Comment ("Connected Socket caused an error");
      end if;
   when E2 : others => Unexpected_Exception (E2, "A285");
   end;

   -----------------------------------------------------------------------
   --  The Set and Get_Socket_Send_Timeout methods are consistent

   Test ("Set/Get_Socket_Send_Timeout [18.4.9]");
   declare
      Socket : POSIX_IO.File_Descriptor := 0;
      To     : Duration := 16.0;
   begin
      Socket := Create (PF_INET, Datagram_Socket, IPPROTO_UDP);
      Set_Socket_Send_Timeout (Socket, 32.0);
      Comment ("Socket Send timout set to 32");
      To := Get_Socket_Send_Timeout (Socket);
      Assert (To = 32.0, "A286");
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code = Permission_Denied then
         Comment ("Set not allowed -- Test not valid");
      else
         Unexpected_Exception (E1, "A287");
      end if;
   when E2 : others => Unexpected_Exception (E2, "A288");
   end;

   -----------------------------------------------------------------------
   --  The Get_Socket_Send_Timeout method has a defined
   --  default value of 0

   Test ("Get_Socket_Send_Timeout (Default) [18.4.9.2]");
   declare
      Socket : POSIX_IO.File_Descriptor := 0;
      To     : Duration                 := 2.0;
   begin
      Socket := Create (PF_INET, Stream_Socket, IPPROTO_TCP);
      To := Get_Socket_Send_Timeout (Socket);
      Assert (To = 0.0, "A289");
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code = Permission_Denied then
         Comment ("Set not allowed -- Test not valid");
      else
         Unexpected_Exception (E1, "A290");
      end if;
   when E2 : others => Unexpected_Exception (E2, "A291");
   end;

   -----------------------------------------------------------------------
   --  The Set_Socket_Send_Timeout causes the Not_A_Socket
   --  POSIX error to be raised when the socket has not been created

   Test ("Not_A_Socket (Set_Socket_Send_Timout) [18.4.9.3]");
   declare
      Socket : POSIX_IO.File_Descriptor := 1;
      To     : Duration                 := 3.0;
   begin
      Set_Socket_Send_Timeout (Socket, To);
      Expect_Exception ("A292");
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code = Permission_Denied then
         Comment ("Set not allowed -- Test not valid");
      elsif Get_Error_Code /= Not_A_Socket then
         Unexpected_Exception (E1, "A293");
      end if;
   when E2 : others => Unexpected_Exception (E2, "A294");
   end;

   -----------------------------------------------------------------------
   --  The Get_Socket_Send_Timeout causes the Not_A_Socket
   --  POSIX error to be raised when the socket has not been created

   Test ("Not_A_Socket (Get_Socket_Send_Timeout) [18.4.9.3]");
   declare
      Socket : POSIX_IO.File_Descriptor := 3;
      To     : Duration                 := 2.0;
   begin
      To := Get_Socket_Send_Timeout (Socket);
      Expect_Exception ("A295");
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code = Permission_Denied then
         Comment ("Set not allowed -- Test not valid");
      elsif Get_Error_Code /= Not_A_Socket then
         Unexpected_Exception (E1, "A296");
      end if;
   when E2 : others => Unexpected_Exception (E2, "A297");
   end;

   -----------------------------------------------------------------------
   --  The Set_Socket_Send_Timout may cause the
   --  Is_Already_Connected POSIX error to be raised when the socket
   --  is already connected

   Test ("Is_Already_Connected (Set_Socket_Send_Timeout) [18.4.9.3]");
   declare
      Peer1, Peer2 : POSIX_IO.File_Descriptor := 0;
      To           : Duration                 := 3.0;
   begin
      Create_Pair (Peer1, Peer2, PF_UNIX,
         Stream_Socket, IPPROTO_IP);
      Set_Socket_Send_Timeout (Peer1, To);
      POSIX_IO.Close (Peer1);
      POSIX_IO.Close (Peer2);
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code = Permission_Denied then
         Comment ("Set not allowed -- Test not valid");
      elsif Get_Error_Code /= Is_Already_Connected then
         Unexpected_Exception (E1, "A298");
      else
         Comment ("Connected Socket caused an error");
      end if;
   when E2 : others => Unexpected_Exception (E2, "A299");
   end;

   -----------------------------------------------------------------------
   --  The Set_Socket_Send_Timout causes a Domain_Error
   --  Error when the requested buffer is too big

   Test ("Domain_Error (Set_Socket_Send_Buffer_Size) [18.4.9.3]");
   declare
      Socket : POSIX_IO.File_Descriptor;
      To     : Duration := 10000000.0;
   begin
      Socket := Create (PF_INET, Datagram_Socket, IPPROTO_UDP);
      Set_Socket_Send_Timeout (Socket, To);
      Expect_Exception ("A300");
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code = Permission_Denied then
         Comment ("Set not allowed -- Test not valid");
      elsif Get_Error_Code /= No_Buffer_Space then
         Unexpected_Exception (E1, "A301");
      end if;
   when E2 : others => Unexpected_Exception (E2, "A302");
   end;

   -----------------------------------------------------------------------
   --  The Is_A_Socket function returns a value of True if the input is
   --  a socket and False otherwise

   Test ("Is_A_Socket [18.4.10]");
   declare
      Socket : POSIX_IO.File_Descriptor := 0;
      Is_A   : Boolean;
   begin
      Is_A := Is_A_Socket (Socket);
      Assert (Is_A = FALSE, "A303");
      Socket := Create (PF_INET, Stream_Socket, IPPROTO_TCP);
      Is_A := Is_A_Socket (Socket);
      Assert (Is_A = TRUE, "A304");
   exception when E : others => Unexpected_Exception (E, "A305");
   end;

   -----------------------------------------------------------------------
   --  A closed socket will cause Is_A_Socket to raise the
   --  Bad_File_Desriptor error code.

   Test ("Bad_File_Descriptor [18.4.10]");
   declare
      Socket : POSIX_IO.File_Descriptor := 0;
      Is_A   : Boolean;
   begin
      Socket := Create (PF_INET, Stream_Socket, IPPROTO_TCP);
      Close (Socket);
      Is_A := Is_A_Socket (Socket);
      Expect_Exception ("A306");
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code /= Bad_File_Descriptor then
         Unexpected_Exception (E1, "A307");
      end if;
   when E2 : others => Unexpected_Exception (E2, "A308");
   end;

   -----------------------------------------------------------------------
   --  Connection_Queue_Length/_Maximum have the appropriate values

   Test ("Connection_Queue_Length/_Maximum [18.4.11]");
   declare
      Queue_Length : Connection_Queue_Length;
   begin
      Queue_Length := 3;
      Assert (Connection_Queue_Length_Maximum = 5, "A309");
      Assert (Connection_Queue_Length'First = 0, "A310");
      Assert (Connection_Queue_Length'Last = 5, "A311");
   exception when E : others => Unexpected_Exception (E, "A312");
   end;

   -----------------------------------------------------------------------
   --  Shutdown_Mode has the appropriate values

   Test ("Shutdown_Mode [18.4.11]");
   declare
      Mode : Shutdown_Mode;
   begin
      Mode := Further_Receives_Disallowed;
      Assert (Further_Receives_Disallowed = Shutdown_Mode'First and
         Shutdown_Mode'Pos (Further_Sends_Disallowed) = 1 and
         Further_Sends_And_Receives_Disallowed = Shutdown_Mode'Last,
         "A313");
   exception when E : others => Unexpected_Exception (E, "A314");
   end;

   -----------------------------------------------------------------------
   --  The other functions are protocol dependent and will be tested in
   --  the appropriate child package
   -----------------------------------------------------------------------

   Done;
exception when E : others => Fatal_Exception (E, "A999");
end p180400;

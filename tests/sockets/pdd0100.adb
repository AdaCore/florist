------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5c VALIDATION TEST SUITE                      --
--                                                                          --
--                             P D D 0 1 0 0                                --
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

--  Basic test for package POSIX_Sockets_Internet,
--  in IEEE Std 1003.5c Section D.1.3

--  This test covers only features that depend only on
--  the package itself and features from other packages
--  that are required to be supported.

with POSIX,
     POSIX_Sockets,
     POSIX_Sockets_Internet,
     POSIX_Report,
     POSIX_IO,
     System,
     Unchecked_Conversion,
     Test_Parameters;

procedure pdd0100 is

   use POSIX,
       POSIX_Sockets,
       POSIX_Sockets_Internet,
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


--  ===== Procedure and Function Prototypes ==========================  --

   procedure Action
      (Alias_Name : in     POSIX_String;
       Quit       : in out Boolean);

   generic
      type T is (<>);
         with function Get (Socket : POSIX_IO.File_Descriptor)
            return T;
         with procedure Set (Socket : in POSIX_IO.File_Descriptor;
            To : in T);
   procedure TCP_Tests (Name : in String; To : in T; Default : in T;
      Er0 : in String := "A000"; Er1, Er2, Er3 : in String);

   generic
      type T is (<>);
         with function Get (Socket : POSIX_IO.File_Descriptor)
            return T;
         with procedure Set (Socket : in POSIX_IO.File_Descriptor;
            To : in T);
   procedure IP_Tests (Name : in String; To : in T; Default : in T;
      Er0 : in String := "A000"; Er1, Er2 : in String);

--  ===== Procedure and Function Body ================================  --

   procedure Action
      (Alias_Name : in     POSIX_String;
       Quit       : in out Boolean)
   is
   begin
      if Alias_Name = "" then
         Quit := True;
      end if;
      Comment ("Alias name is : " & To_String (Alias_Name));
   end Action;

   procedure TCP_Tests (Name : in String; To : in T; Default : in T;
      Er0 : in String := "A000"; Er1, Er2, Er3 : in String)
   is
      Socket : POSIX_IO.File_Descriptor;
      Result : T;
   begin
      Test ("Get/Set_" & Name &" [D.1.3.4]");
      Socket := Create (PF_INET, Stream_Socket, IPPROTO_TCP);
      if Er0 /= "A000" then
         Assert (Get (Socket) = Default, Er0);
      end if;
      Set (Socket, To);
      Comment ("Socket's " & Name & " has been set");
      Result := Get (Socket);
      Assert (Result = To, Er1);
   exception
   when E1 : POSIX_Error =>
      Optional (Internet_Stream_Option, Unknown_Protocol_Option,
         E1, Er2);
   when E2 : others => Unexpected_Exception (E2, Er3);
   end TCP_Tests;

   procedure IP_Tests (Name : in String; To : in T; Default : in T;
      Er0 : in String := "A000"; Er1, Er2 : in String)
   is
      Socket : POSIX_IO.File_Descriptor;
      Result : T;
   begin
      Test ("Get/Set_" & Name &" [D.1.3.6]");
      Socket := Create (PF_INET, Datagram_Socket, IPPROTO_IP);
      if Er0 /= "A000" then
         Assert (Get (Socket) = Default, Er0);
      end if;
      Set (Socket, To);
      Comment ("Socket's " & Name & " has been set");
      Result := Get (Socket);
      Assert (Result = To, Er1);
   exception
   when E : others => Unexpected_Exception (E, Er2);
   end IP_Tests;

--------------------------------------------------------------------------
--  Begin Tests

begin
   Header ("pdd0100");
   Test ("package POSIX.Sockets.Internet");

   -----------------------------------------------------------------------
   --  Constants

   Test ("Constants [D.1.3.1]");
   begin
      Assert (Internet_Protocol = PF_INET, "A001");
      Assert (ICMP = IPPROTO_ICMP, "A002");
      Assert (TCP = IPPROTO_TCP, "A003");
      Assert (UDP = IPPROTO_UDP, "A004");
      Assert (Raw = IPPROTO_RAW, "A005");
   exception when E : others => Unexpected_Exception (E, "A006");
   end;

   -----------------------------------------------------------------------
   --  Internet Socket Address Pointer arithmatic + operators return
   --  the proper types.

   Test ("Internet Socket Address [D.1.3.1]");
   declare
      Int_Ptr : Internet_Socket_Address_Pointer := null;
      Soc_Ptr : Socket_Address_Pointer := Null_Socket_Address;
   begin
      Soc_Ptr := +Int_Ptr;
      Int_Ptr := +Soc_Ptr;
      Assert (Int_Ptr = null, "A007");
   exception
   when E1 : POSIX_Error =>
      Optional (Sockets_DNI_Option, Internet_Protocol_Option,
         No_Error, E1, "A008");
   when E2 : others => Unexpected_Exception (E2, "A009");
   end;
  
   -----------------------------------------------------------------------
   --  Internet Port consants are correct and the Get/Set Internet_Port
   --  methods are consistent.

   Test ("Internet Port (Get/Set) [D.1.3.1]");
   declare
      Name : Internet_Socket_Address;
      Port : Internet_Port := 21; --  ftp port
   begin
      Assert (Unspecified_Internet_Port = 0, "A010");
      Set_Internet_Port (Name, 23); -- telnet port
      Comment ("Internet port set to 23");
      Port := Get_Internet_Port (Name);
      Assert (Port = 23, "A011");
   exception
   when E1 : POSIX_Error =>
      Optional (Sockets_DNI_Option, Internet_Protocol_Option,
         No_Error, E1, "A012");
   when E2 : others => Unexpected_Exception (E2, "A013");
   end;

   -----------------------------------------------------------------------
   --  String_To_Internet_Address and Internet_Address_To_String are
   --  consistent.  Is_Internet_Address returns the appropriate boolean
   --  value.
   --  This section is out of order because it is needed to test methods
   --  that occur in an earlier section.

   Test ("String/Internet_Address conversions [D.1.3.2]");
   declare
      type POSIX_String_Ptr is access POSIX_String;
      Name    : POSIX_String_Ptr;
      Address : Internet_Address;
   begin
      Assert (Is_Internet_Address ("555.1280.367.666") = False,
         "A014");
      Assert (Is_Internet_Address (Valid_Internet_Address) = True,
         "A015");
      Address := String_To_Internet_Address (Valid_Internet_Address);
      Comment ("Internet address set to " &
         To_String (Valid_Internet_Address));
      Name := new POSIX_String'(Internet_Address_To_String (Address));
      Comment ("Name = " & To_String (Name.all));
      Assert (Name.all = Valid_Internet_Address, "A016");
   exception
   when E1 : POSIX_Error =>
      Optional (Network_Management_Option,
         No_Error, E1, "A017");
   when E2 : others => Unexpected_Exception (E2, "A018");
   end;

   -----------------------------------------------------------------------
   --  Get/Set Internet_Address methods are consistent.

   Test ("Internet Port (Get/Set) [D.1.3.1]");
   declare
      Name    : Internet_Socket_Address;
      Address : Internet_Address;
   begin
      Address := String_To_Internet_Address (Valid_Internet_Address);
      Set_Internet_Address (Name, Address);
      Comment ("Internet address set to "
         & To_String (Valid_Internet_Address));
      Address := Get_Internet_Address (Name);
      Assert (Internet_Address_To_String (Address) =
         Valid_Internet_Address, "A019");
   exception
   when E1 : POSIX_Error =>
      Optional (Sockets_DNI_Option, Internet_Protocol_Option,
         No_Error, E1, "A020");
   when E2 : others => Unexpected_Exception (E2, "A021");
   end;
  
   -----------------------------------------------------------------------
   --  Is_Internet_Socket_Address returns true if the object is an
   --  Internet_Socket_Address.  The false case cannot be tested without
   --  the use of other protocols (will be done in a sperate test program)

   Test ("Is_Internet_Socket_Address [D.1.3.1]");
   declare
      Name    : aliased Internet_Socket_Address;
      Address : Internet_Address;
   begin
      Address := String_To_Internet_Address (Valid_Internet_Address);
      Set_Internet_Address (Name, Address);
      Assert (Is_Internet_Socket_Address (+(Name'Unchecked_Access)),
         "A022");
   exception
   when E1 : POSIX_Error =>
      Optional (Sockets_DNI_Option, Internet_Protocol_Option,
         No_Error, E1, "A023");
   when E2 : others => Unexpected_Exception (E2, "A024");
   end;
  
   -----------------------------------------------------------------------
   --  Get_Socket_Name return the correct value.

   Test ("Get_Socket_Name [D.1.3.1]");
   declare
      Name_Ptr : Internet_Socket_Address_Pointer;
      Address  : Internet_Address;
      Message  : Socket_Message;
      Name     : Internet_Socket_Address;
   begin
      Name_Ptr := new Internet_Socket_Address;
      Address := String_To_Internet_Address (Valid_Internet_Address);
      Set_Internet_Address (Name_Ptr.all, Address);
      Set_Socket_Name (Message, +Name_Ptr);
      Comment ("Socket Name Set");
      Name := Get_Socket_Name (Message);
      Address := Get_Internet_Address (Name);
      Assert (Internet_Address_To_String (Address) =
         Valid_Internet_Address, "A025");
   exception
   when E1 : POSIX_Error =>
      Optional (Sockets_DNI_Option, Internet_Protocol_Option,
         No_Error, E1, "A026");
   when E2 : others => Unexpected_Exception (E2, "A027");
   end;
  
   -----------------------------------------------------------------------
   --  Get_Address return the correct value.

   Test ("Get_Address [D.1.3.1]");
   declare
      Name      : Internet_Address;
      Address   : Internet_Socket_Address;
      Info_Item : Socket_Address_Info;
      Info      : Socket_Address_Info_List;
      type Socket_Address_Info_Ptr is access all Socket_Address_Info;
      Info_Ptr  : Socket_Address_Info_Ptr;
      function To_sock_addr_info_ptr is new Unchecked_Conversion
         (System.Address, Socket_Address_Info_Ptr);
   begin
      begin
         Set_Flags (Info_Item, Canonical_Name);
         Set_Family (Info_Item, PF_INET);
         Set_Socket_Type (Info_Item, Stream_Socket);
         Set_Protocol_Number (Info_Item, IPPROTO_TCP);
         Get_Socket_Address_Info (Valid_Internet_Address,
            "telnet", Info_Item, Info);
      exception
      when E : POSIX_Error => Optional (Network_Management_Option,
         No_Error, E, "A028");
      end;

      Info_Ptr := To_sock_addr_info_ptr (Info'Address);
      Address := Get_Address (Info_Ptr.all);
      Name := Get_Internet_Address (Address);
      Assert (Internet_Address_To_String (Name) =
         Valid_Internet_Address, "A029");
   exception
   when E1 : POSIX_Error =>
      Optional (Sockets_DNI_Option, Internet_Protocol_Option,
         No_Error, E1, "A030");
   when E2 : others => Unexpected_Exception (E2, "A031");
   end;
  
   -----------------------------------------------------------------------
   --  String_To_Internet_Address accepts all the proper dot notations
   --  and raises the appropriate error messages when an inproper
   --  string is used

   Test ("String_To_Internet_Address [D.1.3.2]");
   declare
      type POSIX_String_Ptr is access POSIX_String;
      Name    : POSIX_String_Ptr;
      Address : Internet_Address;
   begin
      --  3 dot notation tested previously, see above
      Address := String_To_Internet_Address ("128.186.31017");
      Name := new POSIX_String'(Internet_Address_To_String (Address));
      Assert (Name.all = "128.186.121.41", "A032");
      Address := String_To_Internet_Address ("128.12220713");
      Name := new POSIX_String'(Internet_Address_To_String (Address));
      Assert (Name.all = "128.186.121.41", "A033");
      Address := String_To_Internet_Address ("2159704361");
      Name := new POSIX_String'(Internet_Address_To_String (Address));
      Assert (Name.all = "128.186.121.41", "A034");
   exception
   when E1 : POSIX_Error =>
      Optional (Network_Management_Option, No_Error, E1, "A035");
   when E2 : others => Unexpected_Exception (E2, "A036");
   end;

   -----------------------------------------------------------------------
   --  Get_Network_Info_By_Name/By_Address and
   --  Get_Name/Family/Network_Number are consitent.

   Test ("Get_Network_Info [D.1.3.2]");
   declare
      Info    : Network_Info;
      Storage : Database_Array_Pointer := new Database_Array (1 .. 50);
   begin
      Info := Get_Network_Info_By_Name ("arpanet", Storage);
      Assert (Get_Name (Info) = "arpanet", "A037");
      Assert (Get_Family (Info) = PF_INET, "A038");
      Assert (Get_Network_Number (Info) = 10, "A039");
      Info := Get_Network_Info_By_Address (10, PF_INET, Storage);
      Assert (Get_Name (Info) = "arpanet", "A040");
      Assert (Get_Family (Info) = PF_INET, "A041");
      Assert (Get_Network_Number (Info) = 10, "A042");
      Info := Get_Network_Info_By_Name ("@#$%^&*()", Storage);
      Assert (Get_Name (Info) = "", "A043");
      Info := Get_Network_Info_By_Address (999, PF_INET, Storage);
      Assert (Get_Name (Info) = "", "A044");
      begin
         Storage := new Database_Array (1 .. 2);
         Info := Get_Network_Info_By_Name ("arpanet", Storage);
         Expect_Exception ("A045");
      exception when Constraint_Error => null;
      end;
      begin
         Info := Get_Network_Info_By_Address (10, PF_INET, Storage);
         Expect_Exception ("A046");
      exception when Constraint_Error => null;
      end;
   exception
   when E1 : POSIX_Error =>
      Optional (Network_Management_Option, Unknown_Protocol_Option,
         E1, "A047");
   when E2 : others => Unexpected_Exception (E2, "A048");
   end;

   -----------------------------------------------------------------------
   --  For_Every_Network_Alias calls Action for every alias.
   --  Action places the alias name in a Comment.

   Test ("For_Every_Network_Alias [D.1.3.2]");
   declare
      Info    : Network_Info;
      Storage : Database_Array_Pointer := new Database_Array (1 .. 50);
      procedure Every_Net_Alias is
         new For_Every_Network_Alias (Action);
   begin
      Info := Get_Network_Info_By_Name ("arpanet", Storage);
      Every_Net_Alias (Info);
   exception
   when E1 : POSIX_Error =>
      Optional (Network_Management_Option, Unknown_Protocol_Option,
         E1, "A049");
   when E2 : others => Unexpected_Exception (E2, "A050");
   end;
         

   -----------------------------------------------------------------------
   --  Open/Close_Network_Database_Connection open and closes a
   --  connection to the network database.

   Test ("Open/Close_Network_Database_Connection [D.1.3.2]");
   declare
      Info    : Network_Info;
      Storage : Database_Array_Pointer := new Database_Array (1 .. 50);
   begin
      Open_Network_Database_Connection (False);
      Info := Get_Network_Info_By_Name ("arpanet", Storage);
      Assert (Get_Name (Info) = "arpanet", "A051");
      Close_Network_Database_Connection;
      Open_Network_Database_Connection (False);
      Info := Get_Network_Info_By_Name ("loopback", Storage);
      Info := Get_Network_Info_By_Name ("arpanet", Storage);
      Assert (Get_Name (Info) = "arpanet", "A052");
      Close_Network_Database_Connection;
      Open_Network_Database_Connection (True);
      Info := Get_Network_Info_By_Name ("loopback", Storage);
      Info := Get_Network_Info_By_Name ("arpanet", Storage);
      Assert (Get_Name (Info) = "arpanet", "A053");
      Close_Network_Database_Connection;
   exception
   when E1 : POSIX_Error =>
      Optional (Network_Management_Option, Unknown_Protocol_Option,
         E1, "A054");
   when E2 : others => Unexpected_Exception (E2, "A055");
   end;

   -----------------------------------------------------------------------
   --  Get_Protocol_Info_By_Name/Number and
   --  Get_Name/Protocol_Number are consitent.

   Test ("Get_Protocol_Info [D.1.3.2]");
   declare
      Info    : Protocol_Info;
      Storage : Database_Array_Pointer := new Database_Array (1 .. 50);
   begin
      Info := Get_Protocol_Info_By_Name ("TCP", Storage);
      Assert (Get_Name (Info) = "tcp", "A056");
      Assert (Get_Protocol_Number (Info) = 6, "A057");
      Info := Get_Protocol_Info_By_Number (6, Storage);
      Assert (Get_Name (Info) = "tcp", "A058");
      Assert (Get_Protocol_Number (Info) = 6, "A059");
      Info := Get_Protocol_Info_By_Name ("@#$%^&*()", Storage);
      Assert (Get_Name (Info) = "", "A060");
      Info := Get_Protocol_Info_By_Number (999, Storage);
      Assert (Get_Name (Info) = "", "A061");
      begin
         Storage := new Database_Array (1 .. 2);
         Info := Get_Protocol_Info_By_Name ("tcp", Storage);
         Expect_Exception ("A062");
      exception when Constraint_Error => null;
      end;
      begin
         Info := Get_Protocol_Info_By_Number (6, Storage);
         Expect_Exception ("A063");
      exception when Constraint_Error => null;
      end;
   exception
   when E1 : POSIX_Error =>
      Optional (Network_Management_Option, Unknown_Protocol_Option,
         E1, "A064");
   when E2 : others => Unexpected_Exception (E2, "A065");
   end;

   -----------------------------------------------------------------------
   --  For_Every_Protocol_Alias calls Action for every alias.
   --  Action places the alias name in a Comment.

   Test ("For_Every_Protocol_Alias [D.1.3.2]");
   declare
      Info    : Protocol_Info;
      Storage : Database_Array_Pointer := new Database_Array (1 .. 50);
      procedure Every_Proto_Alias is
         new For_Every_Protocol_Alias (Action);
   begin
      Info := Get_Protocol_Info_By_Name ("tcp", Storage);
      Every_Proto_Alias (Info);
   exception
   when E1 : POSIX_Error =>
      Optional (Network_Management_Option, Unknown_Protocol_Option,
         E1, "A066");
   when E2 : others => Unexpected_Exception (E2, "A067");
   end;
         

   -----------------------------------------------------------------------
   --  Open/Close_Protocol_Database_Connection open and closes a
   --  connection to the protocol database.

   Test ("Open/Close_Protocol_Database_Connection [D.1.3.2]");
   declare
      Info    : Protocol_Info;
      Storage : Database_Array_Pointer := new Database_Array (1 .. 50);
   begin
      Open_Protocol_Database_Connection (False);
      Info := Get_Protocol_Info_By_Name ("tcp", Storage);
      Assert (Get_Name (Info) = "tcp", "A068");
      Close_Protocol_Database_Connection;
      Open_Protocol_Database_Connection (False);
      Info := Get_Protocol_Info_By_Name ("upd", Storage);
      Info := Get_Protocol_Info_By_Name ("tcp", Storage);
      Assert (Get_Name (Info) = "tcp", "A069");
      Close_Protocol_Database_Connection;
      Open_Protocol_Database_Connection (True);
      Info := Get_Protocol_Info_By_Name ("udp", Storage);
      Info := Get_Protocol_Info_By_Name ("tcp", Storage);
      Assert (Get_Name (Info) = "tcp", "A070");
      Close_Protocol_Database_Connection;
   exception
   when E1 : POSIX_Error =>
      Optional (Network_Management_Option, Unknown_Protocol_Option,
         E1, "A071");
   when E2 : others => Unexpected_Exception (E2, "A072");
   end;

   -----------------------------------------------------------------------
   --  TCP Constants and types

   Test ("TCP Constants and types [D.1.3.4]");
   begin
      Assert (Keep_Alive_Time'First = 1, "A073");
      Assert (Keep_Alive_Time'Last = POSIX.Seconds'Last, "A074");
      Assert (Socket_Retransmit_Time'First = -1, "A075");
      Assert (Socket_Retransmit_Time'Last = POSIX.Seconds'Last, "A076");
      Assert (Wait_Forever = -1, "A077");
      Assert (Retransmit_Time_Default = 0, "A078");
   exception when E : others => Unexpected_Exception (E, "A079");
   end;

   -----------------------------------------------------------------------
   --  Get/Set_Keep_Alive_Interval are consistent.

   declare
      procedure Keep_Alive_Interval_Test is new TCP_Tests
         (Keep_Alive_Time, Get_Keep_Alive_Interval,
          Set_Keep_Alive_Interval);
   begin
      --  Default must be 7200 or greater, but don't know exact value
      Keep_Alive_Interval_Test ("Keep_Alive_Interval", 5, 7200,
         "A000", "A080", "A081", "A082");
   end;

   -----------------------------------------------------------------------
   --  Get/Set_No_Delay are consistent.

   declare
      procedure No_Delay_Test is new TCP_Tests
         (Socket_Option_Value, Get_No_Delay, Set_No_Delay);
   begin
      No_Delay_Test ("No_Delay", Enabled, Disabled,
         "A083", "A084", "A085", "A086");
   end;

   -----------------------------------------------------------------------
   --  Get/Set_Retransmit_Time_Maximum are consistent.

   declare
      procedure Retransmit_Time_Maximum_Test is new TCP_Tests
         (Socket_Retransmit_Time, Get_Retransmit_Time_Maximum,
          Set_Retransmit_Time_Maximum);
   begin
      Retransmit_Time_Maximum_Test ("Retransmit_Time_Maxmimum", 5,
         Retransmit_Time_Default,
         "A087", "A088", "A089", "A090");
   end;

   -----------------------------------------------------------------------
   --  Get/Set_Standardized_Urgent_Data are consistent.

   declare
      procedure Standardized_Urgent_Data_Test is new TCP_Tests
         (Socket_Option_Value, Get_Standardized_Urgent_Data,
          Set_Standardized_Urgent_Data);
   begin
      Standardized_Urgent_Data_Test ("Standardized_Urgent_Data",
         Enabled, Disabled, "A091", "A092", "A093", "A094");
   end;

   -----------------------------------------------------------------------
   --  Get_Segment_Size_Maximum returns the current segement size, in
   --  octets, of the TCP connection.

   Test ("Get_Segment_Size_Maximum [D.1.3.4]");
   declare
      Socket : POSIX_IO.File_Descriptor;
   begin
      Socket := Create (PF_INET, Stream_Socket, IPPROTO_TCP);
      Assert (Get_Segment_Size_Maximum (Socket) /= 0, "A095");
   exception
   when E1 : POSIX_Error =>
      Optional (Internet_Stream_Option, Unknown_Protocol_Option,
         E1, "A096");
   when E2 : others => Unexpected_Exception (E2, "A097");
   end;


   -- ================================================================ --
   -- ==                                                            == --
   -- ==  Section D.1.3.5 does not contain any information that is  == --
   -- ==  protocol dependend.                                       == --
   -- ==                                                            == --
   -- ================================================================ --


   -----------------------------------------------------------------------
   --  IP Constants and types

   Test ("IP Constants and types [D.1.3.6]");
   begin
      Assert (Time_To_Live'First = 0, "A098");
      Assert (Time_To_Live'Last = 255, "A099");
   exception when E : others => Unexpected_Exception (E, "A100");
   end;

   -----------------------------------------------------------------------
   --  Get/Set_First_Hop are consistent.

   Test ("Set/Get_First_Hop [D.1.3.6]");
   declare
      Options : IP_Options_Buffer;
   begin
      Set_First_Hop (Options,
         String_To_Internet_Address (Valid_Internet_Address));
      Comment ("First Hop Set");
      Assert (Get_First_Hop (Options) =
         String_To_Internet_Address (Valid_Internet_Address), "A101");
   exception
   when E : others => Unexpected_Exception (E, "A102");
   end;

   -----------------------------------------------------------------------
   --  Get/Set_IP_Options are consistent.

   Test ("Set/Get_IP_Options [D.1.3.6]");
   declare
      Options : IP_Options_Buffer;
      Buffer  : POSIX.Octet_Array (1 .. 40);
   begin
      Buffer (1 .. 4) := (116, 101, 115, 116);
      Set_IP_Options (Options, Buffer);
      Comment ("IP Options Set");
      Assert (Get_IP_Options (Options) = Buffer, "A103");
   exception
   when E : others => Unexpected_Exception (E, "A104");
   end;

   -----------------------------------------------------------------------
   --  Get/Set_IP_Header_Options are consistent.
   --  IP_Header_Options_In_Use returns true when header options are in
   --    use and flase otherwise.
   --  Reset_IP_Header_Options removes any header options that are in
   --    effect.

   Test ("IP_Header_Options [D.1.3.6]");
   declare
      Options, Result : IP_Options_Buffer;
      Buffer  : POSIX.Octet_Array (1 .. 40);
      Socket  : POSIX_IO.File_Descriptor;
   begin
      Buffer (1 .. 4) := (116, 101, 115, 116);
      Set_IP_Options (Options, Buffer);
      Set_First_Hop (Options,
         String_To_Internet_Address (Valid_Internet_Address));
      Socket := Create (PF_INET, Stream_Socket, IPPROTO_TCP);
      Assert (IP_Header_Options_In_Use (Socket) = False, "A105");
      Set_IP_Header_Options (Socket, Options);
      Comment ("IP Options Set");
      Result := Get_IP_Header_Options (Socket);
      Assert (Options = Result, "A106");
      Assert (IP_Header_Options_In_Use (Socket) = True, "A107");
      Reset_IP_Header_Options (Socket);
      Assert (IP_Header_Options_In_Use (Socket) = False, "A108");
   exception
   when E : others => Unexpected_Exception (E, "A109");
   end;

   -----------------------------------------------------------------------
   --  Get/Set_Type_Of_Service are consistent.

   Test ("Get/Set_Type_Of_Service [D.1.3.6]");
   declare
      Socket : POSIX_IO.File_Descriptor;
      Result : IP_Type_Of_Service;
   begin
      Socket := Create (PF_INET, Datagram_Socket, IPPROTO_IP);
      --  Default value is Unspecified
      Assert (Get_Type_Of_Service (Socket) =
         Unspecified, "A110");
      Set_Type_Of_Service (Socket, High_Throughput);
      Comment ("Socket's Type_Of_Service has been set");
      Result := Get_Type_Of_Service (Socket);
      Assert (Result = High_Throughput, "A111");
   exception
   when E : others => Unexpected_Exception (E, "A112");
   end;

   -----------------------------------------------------------------------
   --  Get/Set_Initial_Time_To_Live are consistent.

   declare
      procedure Initial_Time_To_Live_Test is new IP_Tests
         (Time_To_Live, Get_Initial_Time_To_Live,
          Set_Initial_Time_To_Live);
   begin
      --  Default is implementation specific
      Initial_Time_To_Live_Test ("Intial_Time_To_Live", 5, 0,
         "A000", "A113", "A114");
   end;

   -----------------------------------------------------------------------
   --  Get/Set_Receive_Destination_Address are consistent.

   declare
      procedure Receive_Destination_Address_Test is new IP_Tests
         (Socket_Option_Value, Get_Receive_Destination_Address,
          Set_Receive_Destination_Address);
   begin
      --  Default value is Disabled
      Receive_Destination_Address_Test ("Receive_Destination_Address",
         Enabled, Disabled, "A115", "A116", "A117");
   end;

--  ==================================================================  --
--  ==                                                              ==  --
--  ==  Set_Ancillary_Data and Get_Destination_Address cannont be   ==  --
--  ==  properly tested without an application.  Therefore their    ==  --
--  ==  testing will be done in one of the p1804 series of tests.   ==  --
--  ==                                                              ==  --
--  ==================================================================  --

   -----------------------------------------------------------------------
   --  Get/Set_Type_Of_Service are consistent.

   Test ("Get/Set_Header_Included [D.1.3.6]");
   declare
      Socket : POSIX_IO.File_Descriptor;
      Result : Socket_Option_Value;
   begin
      --  Currently cannot create a socket of type Raw
      Socket := Create (PF_INET, Raw_Socket, IPPROTO_IP);
      --  Default value is Disabled
      Assert (Get_Header_Included (Socket) = Disabled, "A118");
      Set_Header_Included (Socket, Enabled);
      Comment ("Socket's Header_Included has been enabled");
      Result := Get_Header_Included (Socket);
      Assert (Result = Enabled, "A119");
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code /= Option_Not_Supported then
         Unexpected_Exception (E1, "A120");
      end if;
   when E2 : others => Unexpected_Exception (E2, "A121");
   end;

   Done;
exception when E : others => Fatal_Exception (E, "A122");
end pdd0100;

------------------------------------------------------------------------------
--                                                                          --
--   POSIX Ada95 Bindings for Protocol Independent Interfaces (P1003.5c)    --
--                                                                          --
--                P O S I X . S o c k e t s . I n t e r n e t               --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                                                                          --
--  This file is part of an implementation of an Ada95 API for the sockets  --
--  and network support services found in P1003.1g -- Protocol Independent  --
--  Interfaces.  It is integrated with the  FSU Implementation of POSIX.5b  --
--  (FLORIST), an Ada API for  POSIX OS services for use with the GNAT Ada  --
--  compiler and the FSU Gnu Ada Runtime Library (GNARL).                   --
--                                                                          --
--  This package specification contains some text extracted from  IEEE STD  --
--  1003.5: 1990, Information Technology --  POSIX Ada Language Interfaces  --
--  Part 1:  Binding for System Application Program Interface,  as amended  --
--  by IEEE STD 1003.5b: 1996,  Amendment 1: Realtime  Extensions,  and as  --
--  further amended by IEEE Draft STD 1003.5c: 1997, Amendment 2: Protocol  --
--  Independent Interfaces,  copyright 1997 by the Institute of Electrical  --
--  and Electronics Engineers, Inc.                                         --
--                                                                          --
--  The package specifications in the IEEE standards cited above represent  --
--  only a  portion  of  the  documents  and  are  not to be interpreteted  --
--  outside the context  of  the documents.  The standards must be used in  --
--  conjunction  with  the  package   specifications  in  order  to  claim  --
--  conformance.   The IEEE takes no responsibility for and will assume no  --
--  liability for damages resulting from the reader's misinterpretation of  --
--  said  information resulting from its out-of-context nature.   To order  --
--  copies of the IEEE standards,  please contact the  IEEE Service Center  --
--  at 445 Hoes Lane, PO Box 1331, Piscataway, NJ 08855-1331; via phone at  --
--  1-800-678-IEEE, 908-981-1393; or via fax at 908-981-9667.               --
--                                                                          --
--  These  package  specifications are  distributed in  the hope that they  --
--  will  be useful, but  WITHOUT  ANY  WARRANTY; without even the implied  --
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.        --
--                                                                          --
------------------------------------------------------------------------------

with POSIX.C,
     POSIX.Sockets;
package POSIX.Sockets.Internet is

   use POSIX.C.Netinet,
       POSIX.C.NetDB,
       POSIX.C.Sockets;

   Internet_Protocol : constant Protocol_Family := PF_INET;
   ICMP              : constant Protocol_Number := IPPROTO_ICMP;
   TCP               : constant Protocol_Number := IPPROTO_TCP;
   UDP               : constant Protocol_Number := IPPROTO_UDP;
   Raw               : constant Protocol_Number := IPPROTO_RAW;

   type Internet_Socket_Address is private;

   type Internet_Socket_Address_Pointer is
     access all Internet_Socket_Address;

   function "+" (Ptr : Internet_Socket_Address_Pointer)
     return Socket_Address_Pointer;
   function "+" (Ptr : Socket_Address_Pointer)
     return Internet_Socket_Address_Pointer;
   function Is_Internet_Socket_Address
     (Ptr : Socket_Address_Pointer)
     return Boolean;

   type Internet_Port is new in_port_t;
   Unspecified_Internet_Port : constant Internet_Port;

   function Get_Internet_Port (Name : Internet_Socket_Address)
     return Internet_Port;
   procedure Set_Internet_Port
      (Name  : in out Internet_Socket_Address;
       Port  : in     Internet_Port);

   type Internet_Address is private;
   Unspecified_Internet_Address : constant Internet_Address;
   Loopback_Internet_Address    : constant Internet_Address;
   Broadcast_Internet_Address   : constant Internet_Address;

   function Get_Internet_Address (Name : Internet_Socket_Address)
     return Internet_Address;
   procedure Set_Internet_Address
      (Name    : in out Internet_Socket_Address;
       Address : in     Internet_Address);
   function Get_Socket_Name (Handle : Socket_Message)
     return Internet_Socket_Address;
   function Get_Address (Info_Item : Socket_Address_Info)
     return Internet_Socket_Address;
   function Get_Peer_Name (Socket : POSIX.IO.File_Descriptor)
     return Internet_Socket_Address;
   function Get_Socket_Name (Socket : POSIX.IO.File_Descriptor)
     return Internet_Socket_Address;

   --  Internet Address Manipulation
   function String_To_Internet_Address (Address : POSIX.POSIX_String)
     return Internet_Address;
   function Is_Internet_Address (Address : POSIX.POSIX_String)
     return Boolean;
   function Internet_Address_To_String (Address : Internet_Address)
     return POSIX.POSIX_String;
   --  Network Database Functions
   type Network_Info is private;
   type Network_Number is range 0 .. (2**32 -1);
   Unspecified_Network_Number : constant Network_Number;
   type Database_Array is new POSIX.Octet_Array;
   type Database_Array_Pointer is access all Database_Array;
   function Get_Name (Info_Item : Network_Info)
     return POSIX.POSIX_String;
   generic
      with procedure Action
         (Alias_Name : in     POSIX.POSIX_String;
          Quit       : in out Boolean);
   procedure For_Every_Network_Alias (Info_Item : Network_Info);
   function Get_Family (Info_Item : Network_Info)
     return Protocol_Family;
   function Get_Network_Number (Info_Item : Network_Info)
     return Network_Number;
   function Get_Network_Info_By_Address
      (Number  : Network_Number;
       Family  : Protocol_Family;
       Storage : Database_Array_Pointer)
     return Network_Info;
   function Get_Network_Info_By_Name
      (Name    : POSIX.POSIX_String;
       Storage : Database_Array_Pointer)
     return Network_Info;
   procedure Open_Network_Database_Connection
      (Stay_Open : in Boolean);
   procedure Close_Network_Database_Connection;
   --  Network Protocol Database Functions
   type Protocol_Info is private;
   function Get_Name (Info_Item : Protocol_Info)
     return POSIX.POSIX_String;
   generic
      with procedure Action
         (Alias_Name : in     POSIX.POSIX_String;
          Quit       : in out Boolean);
   procedure For_Every_Protocol_Alias (Info_Item : Protocol_Info);
   function Get_Protocol_Number (Info_Item : Protocol_Info)
     return Protocol_Number;
   function Get_Protocol_Info_By_Number
      (Number  : Protocol_Number;
       Storage : Database_Array_Pointer)
     return Protocol_Info;
   function Get_Protocol_Info_By_Name
      (Name    : POSIX.POSIX_String;
       Storage : Database_Array_Pointer)
     return Protocol_Info;
   procedure Open_Protocol_Database_Connection
      (Stay_Open : in Boolean);
   procedure Close_Protocol_Database_Connection;

   subtype Keep_Alive_Time is Seconds range 1 .. Seconds'Last;
   function Get_Keep_Alive_Interval
      (Socket : POSIX.IO.File_Descriptor)
     return Keep_Alive_Time;
   procedure Set_Keep_Alive_Interval
      (Socket : in POSIX.IO.File_Descriptor;
       To     : in Keep_Alive_Time);
   function Get_No_Delay
      (Socket : POSIX.IO.File_Descriptor)
     return Socket_Option_Value;
   procedure Set_No_Delay
      (Socket : in POSIX.IO.File_Descriptor;
       To     : in Socket_Option_Value);
   subtype Socket_Retransmit_Time is Seconds range -1 .. Seconds'Last;
   Wait_Forever : constant Socket_Retransmit_Time := -1;
   Retransmit_Time_Default
                : constant Socket_Retransmit_Time := 0;
   function Get_Retransmit_Time_Maximum
      (Socket : POSIX.IO.File_Descriptor)
     return Socket_Retransmit_Time;
   procedure Set_Retransmit_Time_Maximum
      (Socket : in POSIX.IO.File_Descriptor;
       To     : in Socket_Retransmit_Time);
   function Get_Segment_Size_Maximum
      (Socket : POSIX.IO.File_Descriptor)
     return Positive;
   function Get_Standardized_Urgent_Data
      (Socket : POSIX.IO.File_Descriptor)
     return Socket_Option_Value;
   procedure Set_Standardized_Urgent_Data
      (Socket : in POSIX.IO.File_Descriptor;
       To     : in Socket_Option_Value);

   function IP_Header_Options_In_Use
      (Socket : POSIX.IO.File_Descriptor)
     return Boolean;
   procedure Reset_IP_Header_Options
      (Socket : in POSIX.IO.File_Descriptor);
   type IP_Options_Buffer is private;
   function Get_IP_Header_Options
      (Socket : POSIX.IO.File_Descriptor)
     return IP_Options_Buffer;
   procedure Set_IP_Header_Options
      (Socket : in POSIX.IO.File_Descriptor;
       To     : in IP_Options_Buffer);
   function Get_First_Hop
      (Options : IP_Options_Buffer)
     return Internet_Address;
   procedure Set_First_Hop
      (Options : in out IP_Options_Buffer;
       Address : in     Internet_Address);
   function Get_IP_Options
      (Options : IP_Options_Buffer)
     return POSIX.Octet_Array;
   procedure Set_IP_Options
      (Options : in out IP_Options_Buffer;
       Buffer  : in     POSIX.Octet_Array);
   type IP_Type_Of_Service is private;
   Low_Delay        : constant IP_Type_Of_Service;
   High_Throughput  : constant IP_Type_Of_Service;
   High_Reliability : constant IP_Type_Of_Service;
   Unspecified      : constant IP_Type_Of_Service;
   type Time_To_Live is range 0 .. 255;
   function Get_Type_Of_Service
      (Socket : POSIX.IO.File_Descriptor)
     return IP_Type_Of_Service;
   procedure Set_Type_Of_Service
      (Socket : in POSIX.IO.File_Descriptor;
       To     : in IP_Type_Of_Service);
   function Get_Initial_Time_To_Live
      (Socket : POSIX.IO.File_Descriptor)
     return Time_To_Live;
   procedure Set_Initial_Time_To_Live
      (Socket : in POSIX.IO.File_Descriptor;
       To     : in Time_To_Live);
   function Get_Receive_Destination_Address
      (Socket : POSIX.IO.File_Descriptor)
     return Socket_Option_Value;
   procedure Set_Receive_Destination_Address
      (Socket : in POSIX.IO.File_Descriptor;
       To     : in Socket_Option_Value);

   type IP_Ancillary_Data is private;
   type IP_Ancillary_Data_Pointer is access all IP_Ancillary_Data;
   procedure Set_Ancillary_Data
      (Message : in out Socket_Message;
       Data    : in     IP_Ancillary_Data_Pointer);
   function Get_Destination_Address
      (Data : IP_Ancillary_Data)
     return Internet_Address;
   function Get_Header_Included
      (Socket : POSIX.IO.File_Descriptor)
     return Socket_Option_Value;
   procedure Set_Header_Included
      (Socket : in POSIX.IO.File_Descriptor;
       To     : in Socket_Option_Value);

private

   type Internet_Socket_Address is record
      Length : POSIX.C.size_t := POSIX.C.Sockets.struct_sockaddr_in'Size /
                                 System.Storage_Unit;
      C : aliased POSIX.C.Sockets.struct_sockaddr_in :=
        struct_sockaddr_in' (sin_family => AF_INET,
                             sin_port   => 0,
                             sin_addr   => (s_addr => 0),
                             sin_zero   => (others => NUL));
   end record;

   type Internet_Address is record
      C : aliased POSIX.C.Sockets.struct_in_addr;
   end record;
   Unspecified_Internet_Address : constant Internet_Address
      := (C => (s_addr => INADDR_ANY));
   Loopback_Internet_Address    : constant Internet_Address
      := (C => (s_addr => INADDR_LOOPBACK));
   Broadcast_Internet_Address   : constant Internet_Address
      := (C => (s_addr => INADDR_BROADCAST));

   Unspecified_Internet_Port : constant Internet_Port := 0;
   Unspecified_Network_Number : constant Network_Number := 0;

   type Network_Info is record
      C : aliased POSIX.C.NetDB.struct_netent;
   end record;

   type Protocol_Info is record
      C : aliased POSIX.C.NetDB.struct_protoent;
   end record;

   type IP_Options_Buffer is record
      C : aliased POSIX.C.Sockets.struct_ip_opts;
   end record;

   type IP_Ancillary_Data is record
      C1 : aliased POSIX.C.Sockets.struct_cmsghdr :=
         struct_cmsghdr ' (cmsg_len   => 16,
                           cmsg_level => IPPROTO_IP,
                           cmsg_type  => IP_RECVDSTADDR);
      C2 : POSIX.C.Sockets.struct_in_addr;
   end record;

   type IP_Type_Of_Service is new integer;
   Low_Delay        : constant IP_Type_Of_Service
     := POSIX.C.Netinet.IPTOS_LOWDELAY;
   High_Throughput  : constant IP_Type_Of_Service
     := POSIX.C.Netinet.IPTOS_THROUGHPUT;
   High_Reliability : constant IP_Type_Of_Service
     := POSIX.C.Netinet.IPTOS_RELIABILITY;
   Unspecified      : constant IP_Type_Of_Service := 0;

end POSIX.Sockets.Internet;

------------------------------------------------------------------------------
--                                                                          --
--   POSIX Ada95 Bindings for Protocol Independent Interfaces (P1003.5c)    --
--                                                                          --
--                         P O S I X . S o c k e t s                        --
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
--  only a  portion  of  the  documents  and  are  not  to  be interpreted  --
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

with POSIX,
     POSIX.C,
     POSIX.IO,
     POSIX.Limits,
     System;
package POSIX.Sockets is

   use POSIX.C,
       POSIX.C.Sockets;

   type Socket_Type is range 0 .. SOCK_SEQPACKET;
   Stream_Socket           : constant Socket_Type := SOCK_STREAM;
   Datagram_Socket         : constant Socket_Type := SOCK_DGRAM;
   Raw_Socket              : constant Socket_Type := SOCK_RAW;
   Sequenced_Packet_Socket : constant Socket_Type := SOCK_SEQPACKET;
   Unspecified_Socket_Type : constant Socket_Type := 0;
   type Protocol_Family is range PF_UNSPEC .. PF_MAX;
   Unspecified_Protocol : constant Protocol_Family := PF_UNSPEC;
   type Protocol_Number is range 0 .. 65535;
   Default_Protocol : constant Protocol_Number := 0;

   type Socket_Address_Pointer is private;
   Null_Socket_Address : constant Socket_Address_Pointer;

   type Socket_Message is private;
   subtype IO_Vector_Range is Positive range
     1 .. POSIX.Limits.Socket_IO_Vector_Maxima'Last;

   type IO_Vector_Array is array
     (IO_Vector_Range range <>) of POSIX.IO.IO_Vector;
   procedure Set_Socket_Name
     (Message : in out Socket_Message;
      Name    : in     Socket_Address_Pointer);

   type IO_Vector_Array_Pointer is access all IO_Vector_Array;
   procedure Set_IO_Vector_Array
     (Message : in out Socket_Message;
      Ptr     : in     IO_Vector_Array_Pointer);
   function Get_IO_Vector_Array (Message : Socket_Message)
     return IO_Vector_Array_Pointer;

   type Message_Option_Set is new POSIX.Option_Set;
   Peek_Only           : constant Message_Option_Set;
   Process_OOB_Data    : constant Message_Option_Set;
   Wait_For_All_Data   : constant Message_Option_Set;
   Do_Not_Route        : constant Message_Option_Set;

   type Message_Status_Set is new POSIX.Option_Set;
   Received_OOB_Data   : constant Message_Status_Set;
   End_Of_Message      : constant Message_Status_Set;
   Message_Truncated   : constant Message_Status_Set;
   Ancillary_Data_Lost : constant Message_Status_Set;

   procedure Set_Message_Options
     (Message : in out Socket_Message;
      Options : in     Message_Option_Set);
   function Get_Message_Status (Message : Socket_Message)
     return Message_Status_Set;

   procedure Set_Ancillary_Data
     (Message : in out Socket_Message;
      Data    : in     System.Address;
      Length  : in     POSIX.IO_Count);
   procedure Get_Ancillary_Data
     (Message : in  Socket_Message;
      Data    : out System.Address;
      Length  : out POSIX.IO_Count);

   procedure Accept_Connection
     (Socket            : in  POSIX.IO.File_Descriptor;
      Connection_Socket : out POSIX.IO.File_Descriptor;
      Name              : in  Socket_Address_Pointer);
   function Accept_Connection (Socket : POSIX.IO.File_Descriptor)
     return POSIX.IO.File_Descriptor;

   procedure Bind
     (Socket : in POSIX.IO.File_Descriptor;
      Name   : in Socket_Address_Pointer);

   procedure Connect
     (Socket : in POSIX.IO.File_Descriptor;
      Peer   : in Socket_Address_Pointer);
   procedure Specify_Peer
     (Socket : in POSIX.IO.File_Descriptor;
      Peer   : in Socket_Address_Pointer);
   procedure Unspecify_Peer
     (Socket : in POSIX.IO.File_Descriptor);

   function Create
     (Domain   : Protocol_Family;
      Of_Type  : Socket_Type;
      Protocol : Protocol_Number := Default_Protocol)
     return POSIX.IO.File_Descriptor;

   procedure Create_Pair
     (Peer1    : out POSIX.IO.File_Descriptor;
      Peer2    : out POSIX.IO.File_Descriptor;
      Domain   : in  Protocol_Family;
      Of_Type  : in  Socket_Type;
      Protocol : in  Protocol_Number := Default_Protocol);

   type Socket_Address_Info is limited private;
   type Socket_Address_Info_List is limited private;
   procedure Make_Empty
     (Info_Item : in out Socket_Address_Info_List);
   type Address_Flags is new POSIX.Option_Set;
   Use_For_Binding : constant Address_Flags;
   Canonical_Name  : constant Address_Flags;
   procedure Set_Flags
     (Info_Item : in out Socket_Address_Info;
      Flags     : in     Address_Flags);
   function Get_Flags (Info_Item : Socket_Address_Info)
     return Address_Flags;
   procedure Set_Family
     (Info_Item : in out Socket_Address_Info;
      Family    : in     Protocol_Family);
   function Get_Family (Info_Item : Socket_Address_Info)
     return Protocol_Family;
   procedure Set_Socket_Type
     (Info_Item : in out Socket_Address_Info;
      To        : in     Socket_Type);
   function Get_Socket_Type (Info_Item : Socket_Address_Info)
     return Socket_Type;
   procedure Set_Protocol_Number
     (Info_Item : in out Socket_Address_Info;
      Protocol  : in     Protocol_Number);
   function Get_Protocol_Number (Info_Item : Socket_Address_Info)
     return Protocol_Number;
   function Get_Canonical_Name (Info_Item : Socket_Address_Info)
     return POSIX.POSIX_String;
   procedure Get_Socket_Address_Info
     (Name    : in     POSIX.POSIX_String;
      Service : in     POSIX.POSIX_String;
      Info    : in out Socket_Address_Info_List);
   procedure Get_Socket_Address_Info
     (Name    : in     POSIX.POSIX_String;
      Service : in     POSIX.POSIX_String;
      Request : in     Socket_Address_Info;
      Info    : in out Socket_Address_Info_List);
   generic
      with procedure Action
        (Info : in     Socket_Address_Info;
         Quit : in out Boolean);
   procedure For_Every_Item (List : in Socket_Address_Info_List);

   function Get_Socket_Error_Status (Socket : POSIX.IO.File_Descriptor)
     return POSIX.Error_Code;

   function Get_Socket_Type (Socket : POSIX.IO.File_Descriptor)
     return Socket_Type;

   type Socket_Option_Value is (Enabled, Disabled);
   function Get_Socket_Broadcast (Socket : POSIX.IO.File_Descriptor)
     return Socket_Option_Value;
   procedure Set_Socket_Broadcast
     (Socket : in POSIX.IO.File_Descriptor;
      To     : in Socket_Option_Value);
   function Get_Socket_Debugging (Socket : POSIX.IO.File_Descriptor)
     return Socket_Option_Value;
   procedure Set_Socket_Debugging
     (Socket : in POSIX.IO.File_Descriptor;
      To     : in Socket_Option_Value);
   function Get_Socket_Routing (Socket : POSIX.IO.File_Descriptor)
     return Socket_Option_Value;
   procedure Set_Socket_Routing
     (Socket : in POSIX.IO.File_Descriptor;
      To     : in Socket_Option_Value);
   function Get_Socket_Keep_Alive (Socket : POSIX.IO.File_Descriptor)
     return Socket_Option_Value;
   procedure Set_Socket_Keep_Alive
     (Socket : in POSIX.IO.File_Descriptor;
      To     : in Socket_Option_Value);
   subtype Linger_Time is Seconds range 0 .. Seconds'Last;
   function Get_Socket_Linger_Time (Socket : POSIX.IO.File_Descriptor)
     return Linger_Time;
   procedure Set_Socket_Linger_Time
     (Socket : in POSIX.IO.File_Descriptor;
      To     : in Linger_Time);
   function Get_Socket_OOB_Data_Inline (Socket : POSIX.IO.File_Descriptor)
     return Socket_Option_Value;
   procedure Set_Socket_OOB_Data_Inline
     (Socket : in POSIX.IO.File_Descriptor;
      To     : in Socket_Option_Value);
   function Get_Socket_Receive_Buffer_Size (Socket : POSIX.IO.File_Descriptor)
     return POSIX.IO_Count;
   procedure Set_Socket_Receive_Buffer_Size
     (Socket : in POSIX.IO.File_Descriptor;
      To     : in POSIX.IO_Count);
   function Get_Socket_Receive_Low_Water_Mark
     (Socket : POSIX.IO.File_Descriptor) return POSIX.IO_Count;
   procedure Set_Socket_Receive_Low_Water_Mark
     (Socket : in POSIX.IO.File_Descriptor;
      To     : in POSIX.IO_Count);
   function Get_Socket_Receive_Timeout (Socket : POSIX.IO.File_Descriptor)
     return Duration;
   procedure Set_Socket_Receive_Timeout
     (Socket : in POSIX.IO.File_Descriptor;
      To     : in Duration);
   function Get_Socket_Reuse_Addresses (Socket : POSIX.IO.File_Descriptor)
     return Socket_Option_Value;
   procedure Set_Socket_Reuse_Addresses
     (Socket : in POSIX.IO.File_Descriptor;
      To     : in Socket_Option_Value);
   function Get_Socket_Send_Buffer_Size (Socket : POSIX.IO.File_Descriptor)
     return POSIX.IO_Count;
   procedure Set_Socket_Send_Buffer_Size
     (Socket : in POSIX.IO.File_Descriptor;
      To     : in POSIX.IO_Count);
   function Get_Socket_Send_Low_Water_Mark (Socket : POSIX.IO.File_Descriptor)
     return POSIX.IO_Count;
   procedure Set_Socket_Send_Low_Water_Mark
     (Socket : in POSIX.IO.File_Descriptor;
      To     : in POSIX.IO_Count);
   function Get_Socket_Send_Timeout (Socket : POSIX.IO.File_Descriptor)
     return Duration;
   procedure Set_Socket_Send_Timeout
     (Socket : in POSIX.IO.File_Descriptor;
      To     : in Duration);

   function Is_A_Socket (File : POSIX.IO.File_Descriptor)
     return Boolean;

   Connection_Queue_Length_Maximum : constant := SOMAXCONN;
   subtype Connection_Queue_Length is natural
     range 0 .. Connection_Queue_Length_Maximum;
   procedure Listen
     (Socket  : in POSIX.IO.File_Descriptor;
      Backlog : in Connection_Queue_Length := Connection_Queue_Length'Last);

   procedure Receive
     (Socket           : in  POSIX.IO.File_Descriptor;
      Buffer           : in  System.Address;
      Octets_Requested : in  POSIX.IO_Count;
      Octets_Received  : out POSIX.IO_Count;
      Masked_Signals   : in  POSIX.Signal_Masking;
      Options          : in  Message_Option_Set
        := Message_Option_Set (POSIX.Empty_Set));
   procedure Receive
     (Socket           : in  POSIX.IO.File_Descriptor;
      Buffer           : in  System.Address;
      Octets_Requested : in  POSIX.IO_Count;
      Octets_Received  : out POSIX.IO_Count;
      Options          : in  Message_Option_Set
        := Message_Option_Set (POSIX.Empty_Set));
   procedure Receive
     (Socket           : in  POSIX.IO.File_Descriptor;
      Buffer           : in  System.Address;
      Octets_Requested : in  POSIX.IO_Count;
      Octets_Received  : out POSIX.IO_Count;
      From             : in  Socket_Address_Pointer;
      Masked_Signals   : in  POSIX.Signal_Masking;
      Options          : in  Message_Option_Set
        := Message_Option_Set (POSIX.Empty_Set));
   procedure Receive
     (Socket           : in  POSIX.IO.File_Descriptor;
      Buffer           : in  System.Address;
      Octets_Requested : in  POSIX.IO_Count;
      Octets_Received  : out POSIX.IO_Count;
      From             : in  Socket_Address_Pointer;
      Options          : in  Message_Option_Set
        := Message_Option_Set (POSIX.Empty_Set));
   procedure Receive_Message
     (Socket          : in     POSIX.IO.File_Descriptor;
      Message         : in out Socket_Message;
      Octets_Received : out    POSIX.IO_Count;
      Masked_Signals  : in     POSIX.Signal_Masking;
      Options         : in     Message_Option_Set
        := Message_Option_Set (POSIX.Empty_Set));
   procedure Receive_Message
     (Socket          : in     POSIX.IO.File_Descriptor;
      Message         : in out Socket_Message;
      Octets_Received : out    POSIX.IO_Count;
      Options         : in     Message_Option_Set
        := Message_Option_Set (POSIX.Empty_Set));

   procedure Send
     (Socket         : in  POSIX.IO.File_Descriptor;
      Buffer         : in  System.Address;
      Octets_To_Send : in  POSIX.IO_Count;
      Octets_Sent    : out POSIX.IO_Count;
      Masked_Signals : in  POSIX.Signal_Masking;
      Options        : in  Message_Option_Set
        := Message_Option_Set (POSIX.Empty_Set));
   procedure Send
     (Socket         : in  POSIX.IO.File_Descriptor;
      Buffer         : in  System.Address;
      Octets_To_Send : in  POSIX.IO_Count;
      Octets_Sent    : out POSIX.IO_Count;
      Options        : in  Message_Option_Set
        := Message_Option_Set (POSIX.Empty_Set));
   procedure Send
     (Socket         : in  POSIX.IO.File_Descriptor;
      Buffer         : in  System.Address;
      Octets_To_Send : in  POSIX.IO_Count;
      Octets_Sent    : out POSIX.IO_Count;
      To             : in  Socket_Address_Pointer;
      Masked_Signals : in  POSIX.Signal_Masking;
      Options        : in  Message_Option_Set
        := Message_Option_Set (POSIX.Empty_Set));
   procedure Send
     (Socket         : in  POSIX.IO.File_Descriptor;
      Buffer         : in  System.Address;
      Octets_To_Send : in  POSIX.IO_Count;
      Octets_Sent    : out POSIX.IO_Count;
      To             : in  Socket_Address_Pointer;
      Options        : in  Message_Option_Set
        := Message_Option_Set (POSIX.Empty_Set));
   procedure Send_Message
     (Socket         : in  POSIX.IO.File_Descriptor;
      Message        : in  Socket_Message;
      Octets_Sent    : out POSIX.IO_Count;
      Masked_Signals : in  POSIX.Signal_Masking;
      Options        : in  Message_Option_Set
        := Message_Option_Set (POSIX.Empty_Set));
   procedure Send_Message
     (Socket      : in  POSIX.IO.File_Descriptor;
      Message     : in  Socket_Message;
      Octets_Sent : out POSIX.IO_Count;
      Options     : in  Message_Option_Set
        := Message_Option_Set (POSIX.Empty_Set));

   type Shutdown_Mode is
     (Further_Receives_Disallowed,
      Further_Sends_Disallowed,
      Further_Sends_And_Receives_Disallowed);
   procedure Shutdown
     (Socket : in POSIX.IO.File_Descriptor;
      Mode   : in Shutdown_Mode);

   function Socket_Is_At_OOB_Mark (Socket : POSIX.IO.File_Descriptor)
     return Boolean;

private

   type Socket_Address is record
      Length : POSIX.C.size_t := 0;
      C      : aliased POSIX.C.Sockets.struct_sockaddr;
   end record;

   type Socket_Address_Pointer is access all Socket_Address;
   Null_Socket_Address : constant Socket_Address_Pointer := null;

   type Socket_Address_Info is record
      C : aliased POSIX.C.Sockets.struct_addrinfo;
   end record;

   type Socket_Message is record
      C : aliased POSIX.C.Sockets.struct_msghdr :=
         struct_msghdr ' (msg_name       => null,
                          msg_namelen    => 0,
                          msg_iov        => null,
                          msg_iovlen     => 0,
#     if BSD4_3 then
                          msg_accrights  => 0,
                          msg_accrightslen => 0);
#     else
                          msg_control    => null,
                          msg_controllen => 0,
                          msg_flags      => 0);
#     end if;
      io_vector_array_ptr : IO_Vector_Array_Pointer := null;
   end record;

   type Socket_Address_Info_List is record
      C : aliased POSIX.C.Sockets.struct_addrinfo;
   end record;

   Peek_Only           : constant Message_Option_Set :=
     Message_Option_Set (POSIX.Option_Set'(Option => MSG_PEEK));
   Process_OOB_Data    : constant Message_Option_Set :=
     Message_Option_Set (POSIX.Option_Set'(Option => MSG_OOB));
   Wait_For_All_Data   : constant Message_Option_Set :=
     Message_Option_Set (POSIX.Option_Set'(Option => MSG_WAITALL));
   Do_Not_Route        : constant Message_Option_Set :=
     Message_Option_Set (POSIX.Option_Set'(Option => MSG_DONTROUTE));
   Received_OOB_Data   : constant Message_Status_Set :=
     Message_Status_Set (POSIX.Option_Set'(Option => MSG_OOB));
   End_Of_Message      : constant Message_Status_Set :=
     Message_Status_Set (POSIX.Option_Set'(Option => MSG_EOR));
   Message_Truncated   : constant Message_Status_Set :=
     Message_Status_Set (POSIX.Option_Set'(Option => MSG_TRUNC));
   Ancillary_Data_Lost : constant Message_Status_Set :=
     Message_Status_Set (POSIX.Option_Set'(Option => MSG_CTRUNC));
   Use_For_Binding : constant Address_Flags :=
     Address_Flags   (POSIX.Option_Set'(Option => AI_PASSIVE));
   Canonical_Name  : constant Address_Flags :=
     Address_Flags   (POSIX.Option_Set'(Option => AI_CANONNAME));

end POSIX.Sockets;

--  This code extracted from file dot5c.tex by Latex.

------------------------------------------------------------------------------
--                                                                          --
--   POSIX Ada95 Bindings for Protocol Independent Interfaces (P1003.5c)    --
--                                                                          --
--                     P O S I X . S o c k e t s . I S O                    --
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

with POSIX.C;
package POSIX.Sockets.ISO is

   ISO_Protocol           : constant Protocol_Family := 0;
   ISO_Transport_Protocol : constant Protocol_Number := 0;
   Connectionless_Mode_Transport_Protocol
                          : constant Protocol_Number := 0;
   Connectionless_Mode_Network_Protocol
                          : constant Protocol_Number := 0;
   type ISO_Socket_Address is new Socket_Address with private;
   type ISO_Address is new POSIX.Octet_Array;
   type Presentation_Selector is new POSIX.Octet_Array;
   type Session_Selector is new POSIX.Octet_Array;
   type Transport_Selector is new POSIX.Octet_Array;
   type GOSIP_Selector is new POSIX.Octet_Array;
   function Get_ISO_Address (Name : ISO_Socket_Address)
     return ISO_Address;
   procedure Set_ISO_Address
      (Name          : in out ISO_Socket_Address;
       Address_Value : in     ISO_Address);
   function Get_Presentation_Selector (Name : ISO_Socket_Address)
     return Presentation_Selector;
   procedure Set_Presentation_Selector
      (Name     : in out ISO_Socket_Address;
       Selector : in     Presentation_Selector);
   function Get_Session_Selector (Name : ISO_Socket_Address)
     return Session_Selector;
   procedure Set_Session_Selector
      (Name     : in out ISO_Socket_Address;
       Selector : in     Session_Selector);
   function Get_Transport_Selector (Name : ISO_Socket_Address)
     return Transport_Selector;
   procedure Set_Transport_Selector
      (Name     : in out ISO_Socket_Address;
       Selector : in     Transport_Selector);
   function Get_GOSIP_Selector (Name : ISO_Socket_Address)
     return GOSIP_Selector;
   procedure Set_GOSIP_Selector
      (Name     : in out ISO_Socket_Address;
       Selector : in     GOSIP_Selector);
   --  Dispatching operations for ISO_Socket_Address
   function Get_Socket_Name (Handle : Socket_Message)
     return ISO_Socket_Address;
   function Get_Address (Info_Item : Socket_Address_Information)
     return ISO_Socket_Address;
   function Get_Peer_Name (Socket : POSIX.IO.File_Descriptor)
     return ISO_Socket_Address;
   function Get_Socket_Name (Socket : POSIX.IO.File_Descriptor)
     return ISO_Socket_Address;

   type CL_Options is new POSIX.Octet_Array;
   type CL_Flags is new POSIX.Option_Set;
   No_Segmentation     : constant CL_Flags := Empty_Set;
   Suppress_Error_PDUs : constant CL_Flags := Empty_Set;
   No_Checksum         : constant CL_Flags := Empty_Set;
   function Get_CL_Options
      (Socket : POSIX.IO.File_Descriptor)
     return CL_Options;
   procedure Set_CL_Options
      (Socket : in POSIX.IO.File_Descriptor;
       To     : in CL_Options);
   function Get_CL_Flags
      (Socket : POSIX.IO.File_Descriptor)
     return CL_Flags;
   procedure Set_CL_Flags
      (Socket : in POSIX.IO.File_Descriptor;
       To     : in CL_Flags);

   type TP_Flags is new POSIX.Option_Set;
   Public_Data_Network_Quality_Of_Service
                               : constant TP_Flags := Empty_Set;
   Peer_On_Same_Network        : constant TP_Flags := Empty_Set;
   Expedited_Data_Present      : constant TP_Flags := Empty_Set;
   function Get_TP_Flags
      (Socket : POSIX.IO.File_Descriptor)
     return TP_Flags;
   function Get_Connection_Data
      (Socket : POSIX.IO.File_Descriptor)
     return POSIX.Octet_Array;
   procedure Set_Connection_Data
      (Socket : in POSIX.IO.File_Descriptor;
       To     : in POSIX.Octet_Array);
   function Get_Disconnect_Data
      (Socket : POSIX.IO.File_Descriptor)
     return POSIX.Octet_Array;
   procedure Set_Disconnect_Data
      (Socket : in POSIX.IO.File_Descriptor;
       To     : in POSIX.Octet_Array);
   function Get_Confirmation_Data
      (Socket : POSIX.IO.File_Descriptor)
     return POSIX.Octet_Array;
   procedure Set_Confirmation_Data
      (Socket : in POSIX.IO.File_Descriptor;
       To     : in POSIX.Octet_Array);
   type TP_Ancillary_Data_Type is
      (Connection_Data, Disconnect_Data, Confirmation_Data);
   type TP_Ancillary_Data (Kind : TP_Ancillary_Data_Type;
                           Size : Positive) is private;
   procedure Set_Ancillary_Data
      (Message : in out Socket_Message;
       Object  : in     TP_Ancillary_Data);
   function Get_Ancillary_Data
      (Message : Socket_Message)
     return TP_Ancillary_Data;
   procedure Set_Ancillary_Data_Array
      (Object : in out TP_Ancillary_Data;
       Data   : in     POSIX.Octet_Array);
   function Get_Ancillary_Data_Array
      (Object : TP_Ancillary_Data)
     return POSIX.Octet_Array;
   type Connection_Parameters is private;
   function Get_Connection_Parameters
      (Socket : POSIX.IO.File_Descriptor)
     return Connection_Parameters;
   procedure Set_Connection_Parameters
      (Socket : in POSIX.IO.File_Descriptor;
       To     : in Connection_Parameters);
   function Get_Retransmit_Number
      (Object : Connection_Parameters)
     return Natural;
   procedure Set_Retransmit_Number
      (Object : in out Connection_Parameters;
       To     : in     Natural);
   type Window_Size is range 128 .. 16384;
   function Get_Window_Size
      (Object : Connection_Parameters)
     return Window_Size;
   procedure Set_Window_Size
      (Object : in out Connection_Parameters;
       To     : in     Window_Size);
   type TPDU_Size is range 7 .. 13;
   function Get_TPDU_Size
      (Object : Connection_Parameters)
     return TPDU_Size;
   procedure Set_TPDU_Size
      (Object : in out Connection_Parameters;
       To     : in     TPDU_Size);
   type TP_Acknowledgment_Strategy is
      (Acknowledge_Window, Acknowledge_Each);
   function Get_Acknowledgment_Strategy
      (Object : Connection_Parameters)
     return TP_Acknowledgment_Strategy;
   procedure Set_Acknowledgment_Strategy
      (Object : in out Connection_Parameters;
       To     : in     TP_Acknowledgment_Strategy);
   type TP_Retransmit_Strategy is
      (Retransmit_Each_Packet, Use_Congestion_Window, Fast_Start);
   function Get_Retransmit_Strategy
      (Object : Connection_Parameters)
     return TP_Retransmit_Strategy;
   procedure Set_Retransmit_Strategy
      (Object : in out Connection_Parameters;
       To     : in     TP_Retransmit_Strategy);
   type TP_Class is new POSIX.Option_Set;
   TP_Class_0 : constant TP_Class := Empty_Set;
   TP_Class_1 : constant TP_Class := Empty_Set;
   TP_Class_2 : constant TP_Class := Empty_Set;
   TP_Class_3 : constant TP_Class := Empty_Set;
   TP_Class_4 : constant TP_Class := Empty_Set;
   function Get_TP_Class
      (Object : Connection_Parameters)
     return TP_Class;
   procedure Set_TP_Class
      (Object : in out Connection_Parameters;
       To     : in     TP_Class);
   function Get_Extended_Format
      (Object : Connection_Parameters)
     return Boolean;
   procedure Set_Extended_Format
      (Object : in out Connection_Parameters;
       To     : in     Boolean);
   function Get_Expedited_Service
      (Object : Connection_Parameters)
     return Boolean;
   procedure Set_Expedited_Service
      (Object : in out Connection_Parameters;
       To     : in     Boolean);
   function Get_Negotiate_Checksums
      (Object : Connection_Parameters)
     return Boolean;
   procedure Set_Negotiate_Checksums
      (Object : in out Connection_Parameters;
       To     : in     Boolean);
   function Get_Signal_Disconnections
      (Object : Connection_Parameters)
     return Boolean;
   procedure Set_Signal_Disconnections
      (Object : in out Connection_Parameters;
       To     : in     Boolean);
   function Get_Protect_Parameters
      (Object : Connection_Parameters)
     return Boolean;
   procedure Set_Protect_Parameters
      (Object : in out Connection_Parameters;
       To     : in     Boolean);
   type TP_Network_Service is new Integer;
   ISO_Connectionless          : constant TP_Network_Service
      := 0;
   ISO_Connection              : constant TP_Network_Service
      := 0;
   ISO_Connectionless_Over_X25 : constant TP_Network_Service
      := 0;
   IP_Connectionless           : constant TP_Network_Service
      := 0;
   function Get_Network_Service
      (Object : Connection_Parameters)
     return TP_Network_Service;
   procedure Set_Network_Service
      (Object : in out Connection_Parameters;
       To     : in     TP_Network_Service);

private
   
   type ISO_Socket_Address is new Socket_Address with record
      C : aliased POSIX.C.Sockets.struct_sockaddr_iso :=
        POSIX.C.Sockets.struct_sockaddr_iso ' (
          siso_family => AF_ISO,
          siso_plen   => 0,
          siso_slen   => 0,
          siso_tlen   => 0,
          siso_addr   => (isoa_len     => 0,
                          isoa_genaddr => (others => NUL)),
          siso_pad    => (others => NUL));
   end record;

   function Address (Name : ISO_Socket_Address)
     return POSIX.C.Sockets.sockaddr_var_ptr;

   function Length (Name : ISO_Socket_Address)
     return POSIX.C.size_t;

   type Connection_Parameters is new Integer;

   type TP_Ancillary_Data (Kind : TP_Ancillary_Data_Type;
                           Size : Positive) is
   record
      case Kind is
         when Connection_Data   =>
            C1    : aliased POSIX.C.Sockets.struct_cmsghdr :=
               struct_cmsghdr ' (cmsg_len   => size_t (12 + Size),
                                 cmsg_level => 0,  -- SOL_TRANSPORT
                                 cmsg_type  => 0); -- TPOPT_CONN_DATA)
            Data1 : POSIX.Octet_Array (1 .. Size);
         when Disconnect_Data   =>
            C2    : aliased POSIX.C.Sockets.struct_cmsghdr :=
               struct_cmsghdr ' (cmsg_len   => size_t (12 + Size),
                                 cmsg_level => 0,  -- SOL_TRANSPORT
                                 cmsg_type  => 0); -- TPOPT_DISC_DATA)
            Data2 : POSIX.Octet_Array (1 .. Size);
         when Confirmation_Data   =>
            C3    : aliased POSIX.C.Sockets.struct_cmsghdr :=
               struct_cmsghdr ' (cmsg_len   => size_t (12 + Size),
                                 cmsg_level => 0,  -- SOL_TRANSPORT
                                 cmsg_type  => 0); -- TPOPT_CFRM_DATA)
            Data3 : POSIX.Octet_Array (1 .. Size);
      end case;
   end record;

end POSIX.Sockets.ISO;

------------------------------------------------------------------------------
--                                                                          --
--   POSIX Ada95 Bindings for Protocol Independent Interfaces (P1003.5c)    --
--                                                                          --
--                    P O S I X . X T I . I n t e r n e t                   --
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

with POSIX.C; use POSIX.C;
package POSIX.XTI.Internet is

   TCP_Level : constant Option_Level := POSIX.C.XTI.INET_TCP;
   UDP_Level : constant Option_Level := POSIX.C.XTI.INET_UDP;
   IP_Level  : constant Option_Level := POSIX.C.XTI.INET_IP;
   type XTI_Option is (Enabled, Disabled);
   function Get_Value (Option_Item : Protocol_Option)
     return XTI_Option;
   procedure Set_Option
      (Option_Item : in out Protocol_Option;
       Level       : in     Option_Level;
       Name        : in     Option_Name;
       To          : in     XTI_Option);
   type Internet_XTI_Address is private;

   type Internet_XTI_Address_Pointer is
     access all Internet_XTI_Address;

   function "+" (Ptr : Internet_XTI_Address_Pointer)
     return XTI_Address_Pointer;
   function "+" (Ptr : XTI_Address_Pointer)
     return Internet_XTI_Address_Pointer;
   function Is_Internet_XTI_Address (Ptr : XTI_Address_Pointer)
     return Boolean;

   type Internet_Port is mod 2 ** 16;
   Unspecified_Internet_Port : constant Internet_Port;

   function Get_Internet_Port (Name : Internet_XTI_Address)
     return Internet_Port;
   procedure Set_Internet_Port
      (Name       : in out Internet_XTI_Address;
       Port_Value : in     Internet_Port);

   type Internet_Address is private;
   Unspecified_Internet_Address : constant Internet_Address;
   Loopback_Internet_Address    : constant Internet_Address;
   Broadcast_Internet_Address   : constant Internet_Address;

   function Get_Internet_Address (Name : Internet_XTI_Address)
     return Internet_Address;
   procedure Set_Internet_Address
      (Name          : in out Internet_XTI_Address;
       Address_Value : in     Internet_Address);
   --  Dispatching operations for Internet_XTI_Address
   procedure Get_Address
      (Info_Item : in     Connection_Info;
       Address   : in out Internet_XTI_Address);

   --  Internet Address Manipulation
   function String_To_Internet_Address (Address : POSIX.POSIX_String)
     return Internet_Address;
   function Is_Internet_Address (Address : POSIX.POSIX_String)
     return Boolean;
   function Internet_Address_To_String (Address : Internet_Address)
     return POSIX.POSIX_String;

   --  Network Database Functions
   type Network_Info is private;
   type Network_Number is range 0 .. 65535;
   type Protocol_Family is range 0 .. 65535;
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
   type Protocol_Number is range 0 .. 65535;
   function Get_Name (Info_Item : Protocol_Info)
     return POSIX.POSIX_String;
   generic
      with procedure Action
         (Alias_Name : in     POSIX.POSIX_String;
          Quit       : in out Boolean);
   procedure For_Every_Protocol_Alias (Info_Item : Protocol_Info);
   function Get_Protocol_Number (Info_Item : Protocol_Info)
     return Protocol_Number;
   function Get_Protocol_Info_By_Number (Number : Protocol_Number)
     return Protocol_Info;
   function Get_Protocol_Info_By_Name (Name : POSIX.POSIX_String)
     return Protocol_Info;
   procedure Open_Protocol_Database_Connection
      (Stay_Open : in Boolean);
   procedure Close_Protocol_Database_Connection;

   TCP_Keep_Alive_Interval  : constant Option_Name
                                 := POSIX.C.XTI.TCP_KEEPALIVE;
   TCP_Segment_Size_Maximum : constant Option_Name := POSIX.C.XTI.TCP_MAXSEG;
   TCP_No_Delay             : constant Option_Name := POSIX.C.XTI.TCP_NODELAY;
   type Keep_Alive_Info is private;
   type Keep_Alive_Status is (Keep_Alive_On, Keep_Alive_Off, Send_Garbage);
   function Get_Status (Info_Item : Keep_Alive_Info)
     return Keep_Alive_Status;
   procedure Set_Status
      (Info_Item : in out Keep_Alive_Info;
       To        : in     Keep_Alive_Status);
   procedure Set_Keep_Alive_Interval_Default
      (Info_Item : in out Keep_Alive_Info);
   procedure Set_Keep_Alive_Timeout
      (Info_Item : in out Keep_Alive_Info;
       Minutes   : in     Positive);
   function Get_Keep_Alive_Timeout
      (Info_Item : Keep_Alive_Info)
     return Positive;
   function Get_Value (Option_Item : Protocol_Option)
     return Keep_Alive_Info;
   procedure Set_Option
      (Option_Item : in out Protocol_Option;
       Level       : in     Option_Level;
       Name        : in     Option_Name;
       Value       : in     Keep_Alive_Info);

   UDP_Checksum : constant Option_Name := POSIX.C.XTI.UDP_CHECKSUM;

   IP_Options          : constant Option_Name := POSIX.C.XTI.IP_OPTIONS;
   IP_Type_Of_Service  : constant Option_Name := POSIX.C.XTI.IP_TOS;
   IP_Time_To_Live     : constant Option_Name := POSIX.C.XTI.IP_TTL;
   IP_Reuse_Address    : constant Option_Name := POSIX.C.XTI.IP_REUSEADDR;
   IP_Dont_Route       : constant Option_Name := POSIX.C.XTI.IP_DONTROUTE;
   IP_Permit_Broadcast : constant Option_Name := POSIX.C.XTI.IP_BROADCAST;
   type IP_Option_List is new POSIX.Octet_Array;
   procedure Get_Value
      (Option_Item : Protocol_Option;
       IP_Option   : out IP_Option_List;
       Count       : out Natural);
   procedure Set_Option
      (Option_Item : in out Protocol_Option;
       Level       : in     Option_Level;
       Name        : in     Option_Name;
       To          : in     IP_Option_List);
   type IP_Service_Type is
      (Normal, Low_Delay, High_Throughput, High_Reliability, Low_Cost);
   type IP_Precedence_Level is
      (Routine, Priority, Immediate, Flash, Flash_Override,
       Critic_ECP, Internetwork_Control, Network_Control);
   function Get_Value (Option_Item : Protocol_Option)
     return IP_Service_Type;
   function Get_Value (Option_Item : Protocol_Option)
     return IP_Precedence_Level;
   procedure Set_Option
      (Option_Item : in out Protocol_Option;
       Level       : in     Option_Level;
       Name        : in     Option_Name;
       Service     : in     IP_Service_Type;
       Precedence  : in     IP_Precedence_Level);

private

   type Internet_XTI_Address is record
      netbuf : aliased POSIX.C.XTI.struct_netbuf :=
        POSIX.C.XTI.struct_netbuf ' (len     => 0,
                                     maxlen  => 0,
                                     buf     => null);
      buf_ptr : aliased POSIX.C.char_ptr;
      sockaddr_in : POSIX.C.Sockets.struct_sockaddr_in :=
        POSIX.C.Sockets.struct_sockaddr_in '
                 (sin_family => POSIX.C.Sockets.AF_INET,
                  sin_port   => 0,
                  sin_addr   => (s_addr => 0),
                  sin_zero   => (others => NUL));
   end record;

   type Keep_Alive_Info is record
      C : aliased POSIX.C.XTI.struct_t_kpalive :=
        POSIX.C.XTI.struct_t_kpalive ' (kp_onoff => POSIX.C.XTI.T_NO,
                                        kp_timeout => 0);
   end record;

   type Network_Info is record
      C : aliased POSIX.C.NetDB.struct_netent;
   end record;

   type Protocol_Info is record
      C : aliased POSIX.C.NetDB.struct_protoent;
   end record;

   type Internet_Address is record
      C : aliased POSIX.C.Sockets.struct_in_addr;
   end record;
   Unspecified_Internet_Address : constant Internet_Address
      := (C => (s_addr => POSIX.C.Netinet.INADDR_ANY));
   Loopback_Internet_Address    : constant Internet_Address
      := (C => (s_addr => POSIX.C.Netinet.INADDR_LOOPBACK));
   Broadcast_Internet_Address   : constant Internet_Address
      := (C => (s_addr => POSIX.C.Netinet.INADDR_BROADCAST));

   Unspecified_Network_Number : constant Network_Number := 0;
   Unspecified_Internet_Port : constant Internet_Port := 0;

end POSIX.XTI.Internet;

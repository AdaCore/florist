------------------------------------------------------------------------------
--                                                                          --
--   POSIX Ada95 Bindings for Protocol Independent Interfaces (P1003.5c)    --
--                                                                          --
--                P O S I X . S o c k e t s . I n t e r n e t               --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                                                                          --
--  Copyright (c) 1997 Lockheed Martin Corporation, All Rights Reserved.    --
--                                                                          --
--  This file is part of an implementation of an Ada95 API for the sockets  --
--  and network support services found in P1003.1g -- Protocol Independent  --
--  Interfaces.  It is integrated with the  FSU Implementation of POSIX.5b  --
--  (FLORIST), an Ada API for  POSIX OS services for use with the GNAT Ada  --
--  compiler and the FSU Gnu Ada Runtime Library (GNARL). The interface is  --
--  intended to be close to those specified in IEEE STD 1003.5: 1990, IEEE  --
--  STD 1003.5b: 1996, and IEEE Draft STD 1003.5c: 1997.                    --
--                                                                          --
--  This is free software;  you can redistribute it and/or modify it under  --
--  terms of the  GNU  General  Public  License as  published by the  Free  --
--  Software Foundation;  either version  2, or (at your option) any later  --
--  version.  This software is distributed  in  the hope  that  it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without  even the implied  warranty  --
--  of MERCHANTABILITY or FITNESS FOR A PARTICULAR  PURPOSE.  See  the GNU  --
--  General Public License for more details.  You  should have  received a  --
--  copy of the GNU General Public License  distributed  with  GNARL;  see  --
--  file  COPYING.  If not,  write to  the  Free  Software  Foundation, 59  --
--  Temple Place - Suite 330, Boston, MA 02111-1307, USA.                   --
--                                                                          --
--  As a special exception, if other files instantiate generics from  this  --
--  unit, or you link this unit with other files to produce an  executable, --
--  this  unit does not by itself cause the  resulting  executable  to  be  --
--  covered  by the  GNU  General  Public License. This exception does not  --
--  however invalidate any other  reasons why the executable file might be  --
--  covered by the GNU Public License.                                      --
--                                                                          --
------------------------------------------------------------------------------

with POSIX.C; use POSIX.C;
with POSIX.Implementation; use POSIX.Implementation;
with Unchecked_Conversion;
with System;
--   with Ada.Text_IO; use Ada.Text_IO;
--   with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
package body POSIX.Sockets.Internet is

   use POSIX.C.Sockets;
   use POSIX.C.NetDB;

   --  unchecked conversions

   function To_char_ptr is new Unchecked_Conversion
      (System.Address, char_ptr);

   function To_char_var_ptr is new Unchecked_Conversion
      (System.Address, char_var_ptr);

   type IP_Ancillary_Data_Access is access all IP_Ancillary_Data;

   function To_sockaddr_var_ptr is new Unchecked_Conversion
      (System.Address, sockaddr_var_ptr);

   function To_sockaddr_in_var_ptr is new Unchecked_Conversion
      (System.Address, sockaddr_in_var_ptr);

   function To_sockaddr_in_ptr is new Unchecked_Conversion
      (sockaddr_ptr, sockaddr_in_ptr);

   -----------------------------------------
   --  C interface routines
   -----------------------------------------

   function c_setsockopt
      (s : int;
       level : int;
       optname : int;
       optval : char_ptr;
       optlen : size_t)
     return int;
   pragma Import (C, c_setsockopt, setsockopt_LINKNAME);

   function c_getsockopt
      (s : int;
       level : int;
       optname : int;
       optval : char_var_ptr;
       optlen : size_t_var_ptr)
     return int;
   pragma Import (C, c_getsockopt, getsockopt_LINKNAME);

--     function c_htons (in_port : in_port_t) return in_port_t;
--     pragma Import (C, c_htons, "c_htons");

--     function c_ntohs (in_port : in_port_t) return in_port_t;
--     pragma Import (C, c_ntohs, "c_ntohs");

   ---------------------------------
   --  Internet Socket Addresses  --
   ---------------------------------

   function Get_Internet_Port (Name : Internet_Socket_Address)
      return Internet_Port is
   begin
      return Internet_Port (Name.C.sin_port);
   end Get_Internet_Port;

   procedure Set_Internet_Port
      (Name  : in out Internet_Socket_Address;
       Port  : in     Internet_Port) is
   begin
      Name.C.sin_port := in_port_t (Port);
   end Set_Internet_Port;

   function Get_Internet_Address (Name : Internet_Socket_Address)
      return Internet_Address is
   begin
      return (C => (s_addr => Name.C.sin_addr.s_addr));
   end Get_Internet_Address;

   procedure Set_Internet_Address
      (Name    : in out Internet_Socket_Address;
       Address : in     Internet_Address) is
   begin
      Name.C.sin_addr.s_addr := Address.C.s_addr;
   end Set_Internet_Address;

   --------------------------------------
   --  Internetwork Support Functions  --
   --------------------------------------

   --  Internet Address Manipulation
--     function c_htonl (in_addr : in_addr_t) return in_addr_t;
--     pragma Import (C, c_htonl, "c_htonl");

--   function Host_To_Network_Byte_Order
--      (Address : Internet_Address)
--     return Internet_Address is
--   begin
--      return (C => (s_addr => c_htonl (Address.C.s_addr)));
--   end Host_To_Network_Byte_Order;

--     function c_ntohl (in_addr : in_addr_t) return in_addr_t;
--     pragma Import (C, c_ntohl, "c_ntohl");

--   function Network_To_Host_Byte_Order (Address : Internet_Address)
--     return Internet_Address is
--   begin
--      return (C => (s_addr => c_ntohl (Address.C.s_addr)));
--   end Network_To_Host_Byte_Order;

   function c_inet_addr (str : char_ptr) return in_addr_t;
   pragma Import (C, c_inet_addr, POSIX.C.Netinet.inet_addr_LINKNAME);

   function String_To_Internet_Address (Address : POSIX.POSIX_String)
     return Internet_Address is
   begin
      return (C => (s_addr =>
         c_inet_addr (To_char_ptr (Address (Address'First)'Address))));
   end String_To_Internet_Address;

   function Is_Internet_Address (Address : POSIX.POSIX_String)
     return Boolean is
   begin
      if c_inet_addr
         (Address (Address'First)'Unchecked_Access) = INADDR_NONE then
         return False;
      else
         return True;
      end if;
   end Is_Internet_Address;

   function c_inet_ntoa (addr : System.Address) return char_ptr;
   pragma Import (C, c_inet_ntoa, "c_inet_ntoa");


   function strlen (str : in char_ptr) return size_t;
   pragma Import (C, strlen, "strlen");



   function Internet_Address_To_String (Address : Internet_Address)
     return POSIX.POSIX_String is
   begin
      return Form_POSIX_String
         (c_inet_ntoa (Address.C.s_addr'Address));
   end Internet_Address_To_String;

   --  Network Database Functions
   function Get_Name (Info_Item : Network_Info)
     return POSIX.POSIX_String is
   begin
      return Form_POSIX_String (Info_Item.C.n_name);
   end Get_Name;

   procedure For_Every_Network_Alias (Info_Item : Network_Info) is
      next_alias : char_ptr_ptr;
      Quit : Boolean := False;
   begin
      next_alias := Info_Item.C.n_aliases;
      if next_alias /= null then
         while next_alias.all /= null loop
            Action (Form_POSIX_String (next_alias.all), Quit);
            exit when Quit;
            Advance (next_alias);
         end loop;
      end if;
   end For_Every_Network_Alias;

   function Get_Family (Info_Item : Network_Info)
      return Protocol_Family is
   begin
      return Protocol_Family (Info_Item.C.n_addrtype);
   end Get_Family;

   function Get_Network_Number (Info_Item : Network_Info)
     return Network_Number is
   begin
      return Network_Number (Info_Item.C.n_net);
   end Get_Network_Number;

   function c_getnetbyaddr_r
      (net    : unsigned;
       typ    : int;
       result : netent_var_ptr;
       buffer : char_ptr;
       buflen : int)
     return netent_ptr;
   pragma Import (C, c_getnetbyaddr_r, getnetbyaddr_r_LINKNAME);

   function Get_Network_Info_By_Address
      (Number  : Network_Number;
       Family  : Protocol_Family;
       Storage : Database_Array_Pointer)
     return Network_Info is
      netent : aliased POSIX.C.NetDB.struct_netent;
      Result : POSIX.C.NetDB.netent_ptr;
      Temp   : Error_Code;
   begin
      --  Since the error is only caused under special conditions
      --  we have to make sure an old one is not left in the error
      --  number.
      if Fetch_Errno = ERANGE then
         Store_Errno (No_Error);
      end if;
      Result := c_getnetbyaddr_r
      (unsigned (Number), int (Family), netent'Unchecked_Access,
        To_char_ptr (Storage (Storage'First)'Address), Storage'Length);
      if Result /= null then
         return (C => netent);
      else
         Temp := Fetch_Errno;
         if Temp = ERANGE then
            raise Constraint_Error;
         else return (C => (n_name     => null,
                      n_aliases  => null,
                      n_addrtype => 0,
                      n_net      => 0));
         end if;
      end if;
   end Get_Network_Info_By_Address;

   function c_getnetbyname_r
      (name   : char_ptr;
       result : netent_var_ptr;
       buffer : char_ptr;
       buflen : int)
     return netent_ptr;
   pragma Import (C, c_getnetbyname_r, getnetbyname_r_LINKNAME);

   function Get_Network_Info_By_Name
      (Name    : POSIX.POSIX_String;
       Storage : Database_Array_Pointer)
     return Network_Info is
      netent   : aliased POSIX.C.NetDB.struct_netent;
      Result   : POSIX.C.NetDB.netent_ptr;
      Name_NUL : POSIX_String := Name & NUL;
      Temp     : POSIX.Error_Code;
   begin
      --  Since the error is only caused under special conditions
      --  we have to make sure an old one is not left in the error
      --  number.
      if Fetch_Errno = ERANGE then
         Store_Errno (No_Error);
      end if;
      Result := c_getnetbyname_r
      (Name_NUL (Name_NUL'First)'Unchecked_Access, netent'Unchecked_Access,
       To_char_ptr (Storage (Storage'First)'Address), Storage'Length);
      if Result /= null then
         return (C => netent);
      else
         Temp := Fetch_Errno;
         if Temp = ERANGE then
            raise Constraint_Error;
         else
            return (C => (n_name => null,
                          n_aliases => null,
                          n_addrtype => 0,
                          n_net => 0));
         end if;
      end if;
   end Get_Network_Info_By_Name;

   function c_setnetent (stayopen : int) return int;
   pragma Import (C, c_setnetent, setnetent_LINKNAME);

   procedure Open_Network_Database_Connection
      (Stay_Open : in Boolean) is
   begin
      if Stay_Open then
         Check (c_setnetent (1));
      else
         Check (c_setnetent (0));
      end if;
   end Open_Network_Database_Connection;

   function c_endnetent return int;
   pragma Import (C, c_endnetent, endnetent_LINKNAME);

   procedure Close_Network_Database_Connection is
   begin
      Check (c_endnetent);
   end Close_Network_Database_Connection;

   --  Protocol Database Functions
   function Get_Name (Info_Item : Protocol_Info)
     return POSIX.POSIX_String is
   begin
      return Form_POSIX_String (Info_Item.C.p_name);
   end Get_Name;

   procedure For_Every_Protocol_Alias (Info_Item : Protocol_Info) is
      next_alias : char_ptr_ptr;
      Quit : Boolean := False;
   begin
      next_alias := Info_Item.C.p_aliases;
      if next_alias /= null then
         while next_alias.all /= null loop
            Action (Form_POSIX_String (next_alias.all), Quit);
            exit when Quit;
            Advance (next_alias);
         end loop;
      end if;
   end For_Every_Protocol_Alias;

   function Get_Protocol_Number (Info_Item : Protocol_Info)
     return Protocol_Number is
   begin
      return Protocol_Number (Info_Item.C.p_proto);
   end Get_Protocol_Number;

   function c_getprotobynumber_r
      (proto : int;
       result : protoent_var_ptr;
       buffer : char_ptr;
       buflen : int)
     return protoent_ptr;
   pragma Import (C, c_getprotobynumber_r, getprotobynumber_r_LINKNAME);

   function Get_Protocol_Info_By_Number
      (Number  : Protocol_Number;
       Storage : Database_Array_Pointer)
     return Protocol_Info is
      protoent : aliased struct_protoent;
      Result   : protoent_ptr;
      Temp     : POSIX.Error_Code;
   begin
      --  Since the error is only caused under special conditions
      --  we have to make sure an old one is not left in the error
      --  number.
      if Fetch_Errno = ERANGE then
         Store_Errno (No_Error);
      end if;
      Result := c_getprotobynumber_r
      (int (Number), protoent'Unchecked_Access,
       To_char_ptr (Storage (Storage'First)'Address), Storage'Length);
      if Result /= null then
         return (C => protoent);
      else
         Temp := Fetch_Errno;
         if Temp = ERANGE then
            raise Constraint_Error;
         else
            return (C => (p_name => null,
                          p_aliases => null,
                          p_proto => 0));
         end if;
      end if;
   end Get_Protocol_Info_By_Number;

   function c_getprotobyname_r
      (name : char_ptr;
       result : protoent_var_ptr;
       buffer : char_ptr;
       buflen : int)
     return protoent_ptr;
   pragma Import (C, c_getprotobyname_r, getprotobyname_r_LINKNAME);

   function Get_Protocol_Info_By_Name
      (Name    : POSIX.POSIX_String;
       Storage : Database_Array_Pointer)
     return Protocol_Info is
      protoent : aliased struct_protoent;
      Result   : protoent_ptr;
      Name_NUL : POSIX.POSIX_String := Name & NUL;
      Temp     : POSIX.Error_Code;
   begin
      --  Since the error is only caused under special conditions
      --  we have to make sure an old one is not left in the error
      --  number.
      if Fetch_Errno = ERANGE then
         Store_Errno (No_Error);
      end if;
      Result := c_getprotobyname_r
      (Name_NUL (Name_NUL'First)'Unchecked_Access,
       protoent'Unchecked_Access,
       To_char_ptr (Storage (Storage'First)'Address), Storage'Length);
      if Result /= null then
         return (C => protoent);
      else
         Temp := Fetch_Errno;
         if Temp = ERANGE then
            raise Constraint_Error;
         else
            return (C => (p_name => null,
                          p_aliases => null,
                          p_proto => 0));
         end if;
      end if;
   end Get_Protocol_Info_By_Name;

   function c_setprotoent (stayopen : int) return int;
   pragma Import (C, c_setprotoent, setprotoent_LINKNAME);

   procedure Open_Protocol_Database_Connection
      (Stay_Open : in Boolean) is
   begin
      if Stay_Open then
         Check (c_setprotoent (1));
      else
         Check (c_setprotoent (0));
      end if;
   end Open_Protocol_Database_Connection;

   function c_endprotoent return int;
   pragma Import (C, c_endprotoent, endprotoent_LINKNAME);

   procedure Close_Protocol_Database_Connection is
   begin
      Check (c_endprotoent);
   end Close_Protocol_Database_Connection;

   -------------------------------
   --  Internet Socket Options  --
   -------------------------------

   --  TCP Keepalive Interval  --

   function Get_Keep_Alive_Interval
      (Socket : POSIX.IO.File_Descriptor)
      return Keep_Alive_Time is
      optval : aliased int;
      optlen : aliased size_t := optval'Size / char'Size;
   begin
      Check (c_getsockopt (int (Socket), int (IPPROTO_TCP),
        int (TCP_KEEPALIVE), To_char_var_ptr (optval'Address),
        optlen'Unchecked_Access));
      return Keep_Alive_Time (optval);
   end Get_Keep_Alive_Interval;

   procedure Set_Keep_Alive_Interval
      (Socket : in POSIX.IO.File_Descriptor;
       To     : in Keep_Alive_Time) is
      optval : aliased int;
      optlen : aliased size_t := optval'Size / char'Size;
   begin
      optval := int (To);
      Check (c_setsockopt (int (Socket), int (IPPROTO_TCP),
             int (TCP_KEEPALIVE), To_char_ptr (optval'Address), optlen));
   end Set_Keep_Alive_Interval;

   --  TCP No Delay  --

   function Get_No_Delay
      (Socket : POSIX.IO.File_Descriptor)
      return Socket_Option_Value is
      optval : aliased int;
      optlen : aliased size_t := optval'Size / char'Size;
   begin
      Check (c_getsockopt (int (Socket), int (IPPROTO_TCP), int (TCP_NODELAY),
             To_char_var_ptr (optval'Address), optlen'Unchecked_Access));
      if optval = 0 then
         return Disabled;
      else
         return Enabled;
      end if;
   end Get_No_Delay;

   procedure Set_No_Delay
      (Socket : in POSIX.IO.File_Descriptor;
       To     : in Socket_Option_Value) is
      optval : aliased int := 0;
      optlen : size_t := optval'Size / char'Size;
   begin
      if To = Enabled then optval := 1; end if;
      Check (c_setsockopt (int (Socket), int (IPPROTO_TCP), int (TCP_NODELAY),
             To_char_ptr (optval'Address), optlen));
   end Set_No_Delay;

   --  TCP Max Retransmit Time  --

   function Get_Retransmit_Time_Maximum
      (Socket : POSIX.IO.File_Descriptor)
      return Socket_Retransmit_Time is
      optval : aliased int;
      optlen : aliased size_t := optval'Size / char'Size;
   begin
      Check (c_getsockopt (int (Socket), int (IPPROTO_TCP), int (TCP_MAXRXT),
             To_char_var_ptr (optval'Address), optlen'Unchecked_Access));
      return Socket_Retransmit_Time (optval);
   end Get_Retransmit_Time_Maximum;

   procedure Set_Retransmit_Time_Maximum
      (Socket : in POSIX.IO.File_Descriptor;
       To     : in Socket_Retransmit_Time) is
      optval : aliased int;
      optlen : aliased size_t := optval'Size / char'Size;
   begin
      optval := int (To);
      Check (c_setsockopt (int (Socket), int (IPPROTO_TCP), int (TCP_MAXRXT),
             To_char_ptr (optval'Address), optlen));
   end Set_Retransmit_Time_Maximum;

   --  TCP Segment Size  --

   function Get_Segment_Size_Maximum
      (Socket : POSIX.IO.File_Descriptor)
      return Positive is
      optval : aliased int;
      optlen : aliased size_t := optval'Size / char'Size;
   begin
      Check (c_getsockopt (int (Socket), int (IPPROTO_TCP), int (TCP_MAXSEG),
             To_char_var_ptr (optval'Address), optlen'Unchecked_Access));
      return Positive (optval);
   end Get_Segment_Size_Maximum;

   --  TCP Standardized Handling of Urgent Data  --

   function Get_Standardized_Urgent_Data
      (Socket : POSIX.IO.File_Descriptor)
      return Socket_Option_Value is
      optval : aliased int;
      optlen : aliased size_t := optval'Size / char'Size;
   begin
      Check (c_getsockopt (int (Socket), int (IPPROTO_TCP), int (TCP_STDURG),
             To_char_var_ptr (optval'Address), optlen'Unchecked_Access));
      if optval = 0 then
         return Disabled;
      else
         return Enabled;
      end if;
   end Get_Standardized_Urgent_Data;

   procedure Set_Standardized_Urgent_Data
      (Socket : in POSIX.IO.File_Descriptor;
       To     : in Socket_Option_Value) is
      optval : aliased int := 0;
      optlen : size_t := optval'Size / char'Size;
   begin
      if To = Enabled then optval := 1; end if;
      Check (c_setsockopt (int (Socket), int (IPPROTO_TCP), int (TCP_STDURG),
             To_char_ptr (optval'Address), optlen));
   end Set_Standardized_Urgent_Data;

   --  IP Protocol Options in IP Header  --

   function IP_Header_Options_In_Use
      (Socket : POSIX.IO.File_Descriptor)
      return Boolean is
      optval : aliased struct_ip_opts;
      optlen : aliased size_t := optval'Size / char'Size;
   begin
      Check (c_getsockopt (int (Socket), int (IPPROTO_IP), int (IP_OPTIONS),
             To_char_var_ptr (optval'Address), optlen'Unchecked_Access));
      if optlen = 0 then
         return False;
      else
         return True;
      end if;
   end IP_Header_Options_In_Use;

   procedure Reset_IP_Header_Options              --  send null to reset
      (Socket : in POSIX.IO.File_Descriptor) is
   begin
      Check (c_setsockopt (int (Socket), int (IPPROTO_IP), int (IP_OPTIONS),
             To_char_ptr (System.Null_Address), 0));
   end Reset_IP_Header_Options;

   function Get_IP_Header_Options
      (Socket : POSIX.IO.File_Descriptor)
      return IP_Options_Buffer is
      optval : aliased struct_ip_opts;
      optlen : aliased size_t := optval'Size / char'Size;
   begin
      Check (c_getsockopt (int (Socket), int (IPPROTO_IP), int (IP_OPTIONS),
             To_char_var_ptr (optval'Address), optlen'Unchecked_Access));
      return IP_Options_Buffer'(C => optval);
   end Get_IP_Header_Options;

   procedure Set_IP_Header_Options
      (Socket  : in POSIX.IO.File_Descriptor;
       To      : in IP_Options_Buffer) is
      optlen : size_t := To'Size / char'Size;
   begin
      Check (c_setsockopt (int (Socket), int (IPPROTO_IP), int (IP_OPTIONS),
             To_char_ptr (To.C'Address), optlen));
   end Set_IP_Header_Options;

   function Get_First_Hop
      (Options : IP_Options_Buffer)
      return Internet_Address is
   begin
      return (C => Options.C.ip_dst);
   end Get_First_Hop;

   procedure Set_First_Hop
      (Options : in out IP_Options_Buffer;
       Address : in     Internet_Address) is
   begin
      Options.C.ip_dst := Address.C;
   end Set_First_Hop;

   function Get_IP_Options
      (Options : IP_Options_Buffer)
      return POSIX.Octet_Array is
   begin
      return Options.C.ip_opts;
   end Get_IP_Options;

   procedure Set_IP_Options
      (Options : in out IP_Options_Buffer;
       Buffer  : in     POSIX.Octet_Array) is
   begin
      Options.C.ip_opts := Buffer;
   end Set_IP_Options;

   --  IP Type of Service Value  --

   function Get_Type_Of_Service
      (Socket : POSIX.IO.File_Descriptor)
      return IP_Type_Of_Service is
      optval : aliased int;
      optlen : aliased size_t := optval'Size / char'Size;
   begin
      Check (c_getsockopt (int (Socket), int (IPPROTO_IP), int (IP_TOS),
        To_char_var_ptr (optval'Address), optlen'Unchecked_Access));
      case optval is
         when IPTOS_LOWDELAY =>    return Low_Delay;
         when IPTOS_THROUGHPUT =>  return High_Throughput;
         when IPTOS_RELIABILITY => return High_Reliability;
         when others =>            return Unspecified;
      end case;
   end Get_Type_Of_Service;

   procedure Set_Type_Of_Service
      (Socket : in POSIX.IO.File_Descriptor;
       To     : in IP_Type_Of_Service) is
      optval : aliased int := 0;
      optlen : size_t := optval'Size / char'Size;
   begin
      case To is
         when Low_Delay        => optval := IPTOS_LOWDELAY;
         when High_Throughput  => optval := IPTOS_THROUGHPUT;
         when High_Reliability => optval := IPTOS_RELIABILITY;
         when others           => optval := 0;
      end case;
      Check (c_setsockopt (int (Socket), int (IPPROTO_IP), int (IP_TOS),
             To_char_ptr (optval'Address), optlen));
   end Set_Type_Of_Service;

   --  IP Initial Time to Live Value  --

   function Get_Initial_Time_To_Live
      (Socket : POSIX.IO.File_Descriptor)
      return Time_To_Live is
      optval : aliased int;
      optlen : aliased size_t := optval'Size / char'Size;
   begin
      Check (c_getsockopt (int (Socket), int (IPPROTO_IP), int (IP_TTL),
        To_char_var_ptr (optval'Address), optlen'Unchecked_Access));
      return Time_To_Live (optval);
   end Get_Initial_Time_To_Live;

   procedure Set_Initial_Time_To_Live
      (Socket : in POSIX.IO.File_Descriptor;
       To     : in Time_To_Live) is
      optval : aliased int;
      optlen : aliased size_t := optval'Size / char'Size;
   begin
      optval := int (To);
      Check (c_setsockopt (int (Socket), int (IPPROTO_IP), int (IP_TTL),
             To_char_ptr (optval'Address), optlen));
   end Set_Initial_Time_To_Live;

   --  IP Request Destination Address of Incoming Packets  --

   function Get_Receive_Destination_Address
      (Socket : POSIX.IO.File_Descriptor)
      return Socket_Option_Value is
      optval : aliased int;
      optlen : aliased size_t := optval'Size / char'Size;
   begin
      Check (c_getsockopt (int (Socket), int (IPPROTO_IP),
        int (IP_RECVDSTADDR), To_char_var_ptr (optval'Address),
        optlen'Unchecked_Access));
      if optval = 0 then
         return Disabled;
      else
         return Enabled;
      end if;
   end Get_Receive_Destination_Address;

   procedure Set_Receive_Destination_Address
      (Socket : in POSIX.IO.File_Descriptor;
       To     : in Socket_Option_Value) is
      optval : aliased int := 0;
      optlen : size_t := optval'Size / char'Size;
   begin
      if To = Enabled then optval := 1; end if;
      Check (c_setsockopt (int (Socket), int (IPPROTO_IP),
        int (IP_RECVDSTADDR), To_char_ptr (optval'Address), optlen));
   end Set_Receive_Destination_Address;

   procedure Set_Ancillary_Data
      (Message : in out Socket_Message;
       Data    : in     IP_Ancillary_Data_Pointer) is
   begin
#     if BSD4_3 then
         null;
#     else
         Message.C.msg_control := To_char_ptr (Data.C1'Address);
         Message.C.msg_controllen := size_t (Data'Size / char'Size);
#     end if;
   end Set_Ancillary_Data;

   function Get_Destination_Address
      (Data : IP_Ancillary_Data)
      return Internet_Address is
# if BSD4_3 then
   begin
      return (C => (s_addr => 0));
# else
   begin
      return (C => Data.C2);
# end if;
   end Get_Destination_Address;

   --  IP Header Included with Outgoing Datagram  --

   function Get_Header_Included
      (Socket : POSIX.IO.File_Descriptor)
      return Socket_Option_Value is
      optval : aliased int;
      optlen : aliased size_t := optval'Size / char'Size;
   begin
      Check (c_getsockopt (int (Socket), int (IPPROTO_UDP), int (IP_HDRINCL),
        To_char_var_ptr (optval'Address), optlen'Unchecked_Access));
      if optval = 0 then
         return Disabled;
      else
         return Enabled;
      end if;
   end Get_Header_Included;

   procedure Set_Header_Included
      (Socket : in POSIX.IO.File_Descriptor;
       To     : in Socket_Option_Value) is
      optval : aliased int := 0;
      optlen : size_t := optval'Size / char'Size;
   begin
      if To = Enabled then optval := 1; end if;
      Check (c_setsockopt (int (Socket), int (IPPROTO_UDP), int (IP_HDRINCL),
             To_char_ptr (optval'Address), optlen));
   end Set_Header_Included;

   ---------------------
   --  Get_Peer_Name  --
   ---------------------

   function c_getpeername
      (s : int;
       socketaddress : sockaddr_var_ptr;
       addresslen : access size_t) return int;
   pragma Import (C, c_getpeername, getpeername_LINKNAME);

   function Get_Peer_Name (Socket : POSIX.IO.File_Descriptor)
     return Internet_Socket_Address is
      c_address : aliased struct_sockaddr_in;
      c_address_len : aliased size_t := c_address'Size / char'Size;
   begin
      Check (c_getpeername (int (Socket),
        To_sockaddr_var_ptr (c_address'Address),
        c_address_len'Access));
      if c_address_len /= struct_sockaddr_in'Size / char'Size then
         raise Constraint_Error;
      end if;
      return (C      => c_address,
              Length => c_address_len);
   end Get_Peer_Name;

   -----------------------
   --  Get_Socket_Name  --
   -----------------------

   function c_getsockname
      (s : int;
       socketaddress : sockaddr_in_var_ptr;
       addresslen : size_t_var_ptr) return int;
   pragma Import (C, c_getsockname, getsockname_LINKNAME);

   function Get_Socket_Name (Socket : POSIX.IO.File_Descriptor)
     return Internet_Socket_Address is
      c_address : aliased struct_sockaddr_in;
      c_address_len : aliased size_t := c_address'Size / char'Size;
   begin
      Check (c_getsockname (int (Socket),
        To_sockaddr_in_var_ptr (c_address'Address),
        c_address_len'Unchecked_Access));
      if c_address_len /= struct_sockaddr_in'Size / char'Size then
         raise Constraint_Error;
      end if;
      return (C      => c_address,
              Length => c_address_len);
   end Get_Socket_Name;

   -----------------------
   --  Get_Socket_Name  --
   -----------------------

   function Get_Socket_Name (Handle : Socket_Message)
     return Internet_Socket_Address is
   begin
      --  cast the generic address pointer to an internet socket
      --  address pointer and dereference it. Note that dot1g uses
      --  void* for these. Solaris uses typedef caddr_t which is char*.
      return (C => To_sockaddr_in_ptr (Handle.C.msg_name).all,
              Length => Handle.C.msg_namelen);
   end Get_Socket_Name;

   -------------------
   --  Get_Address  --
   -------------------

   function Get_Address (Info_Item : Socket_Address_Info)
     return Internet_Socket_Address is
   begin
      --  cast the generic socket address pointer to an internet socket
      --  address pointer and dereference it
      return (C => To_sockaddr_in_ptr (Info_Item.C.ai_addr).all,
              Length => Info_Item.C.ai_addrlen);
   end Get_Address;

   function To_Socket_Address is new Unchecked_Conversion
      (Internet_Socket_Address_Pointer, Socket_Address_Pointer);
   function "+" (Ptr : Internet_Socket_Address_Pointer)
     return Socket_Address_Pointer is
   begin
      return To_Socket_Address (Ptr);
   end "+";

   function To_Internet_Socket_Address is new Unchecked_Conversion
      (Socket_Address_Pointer, Internet_Socket_Address_Pointer);
   function "+" (Ptr : Socket_Address_Pointer)
     return Internet_Socket_Address_Pointer is
   begin
      if Ptr = null then
         return null;
      elsif (Ptr.C.sa_family = POSIX.C.Sockets.AF_INET) then
         return To_Internet_Socket_Address (Ptr);
      else
         --  Need to find out exact error ??
         return null;
         --  raise POSIX.Operation_Not_Implemented;
      end if;
   end "+";

   function Is_Internet_Socket_Address (Ptr : Socket_Address_Pointer)
     return Boolean is
   begin
      if Ptr = null then
         return false;
      elsif (Ptr.C.sa_family = POSIX.C.Sockets.AF_INET) then
         return true;
      else
         return false;
      end if;
   end Is_Internet_Socket_Address;

end POSIX.Sockets.Internet;

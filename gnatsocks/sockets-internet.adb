------------------------------------------------------------------------------
--                                                                          --
--                                 GNATSOCKS                                --
--                                                                          --
--                       S o c k e t s . I n t e r n e t                    --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                                                                          --
--  Copyright (c) 1997-1998             Florida  State  University  (FSU),  --
--  All Rights Reserved.                                                    --
--                                                                          --
--  This file is a component of GNATSOCKS, an Ada interfaces to socket I/O  --
--  services, for use with  the  GNAT  Ada  compiler.                       --
--                                                                          --
--  GNATSOCKS is free software;  you can  redistribute it and/or modify it  --
--  under terms of the  GNU  General  Public  License as  published by the  --
--  Free Software Foundation;  either version  2, or (at  your option) any  --
--  later version.  FLORIST is distributed  in  the hope  that  it will be  --
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
--  [$Revision$]

with Ada.Streams,
     POSIX,
     POSIX.C,
     POSIX.Implementation,
     System,
     Unchecked_Conversion;
package body Sockets.Internet is

   use POSIX,
       POSIX.C,
       POSIX.C.NetDB,
       POSIX.C.Netinet,
       POSIX.C.Sockets,
       POSIX.Implementation;

   Local_Hostname : String (1 .. 256) := (others => Character'Val (0));

   function inet_addr (cp : char_var_ptr) return unsigned_long;
      pragma Import (C, inet_addr, inet_addr_LINKNAME);
   function inet_network (cp : char_var_ptr) return unsigned_long;
      pragma Import (C, inet_network, inet_network_LINKNAME);
   function inet_makeaddr (net, lna : int) return struct_in_addr;
      pragma Import (C, inet_makeaddr, inet_makeaddr_LINKNAME);
   function inet_lnaof (addr : struct_in_addr) return int;
      pragma Import (C, inet_lnaof, inet_lnaof_LINKNAME);
   function inet_netof (addr : struct_in_addr) return int;
      pragma Import (C, inet_netof, inet_netof_LINKNAME);
   function inet_ntoa (addr : struct_in_addr) return char_ptr;
      pragma Import (C, inet_ntoa, inet_ntoa_LINKNAME);

   function gethostbyname_r
     (name : char_ptr;
      result : hostent_var_ptr;
      buffer : char_var_ptr;
      buflen : int;
      errnop : access int)
     return hostent_ptr;
      pragma Import (C, gethostbyname_r, gethostbyname_r_LINKNAME);

   function gethostbyaddr_r
     (addr : char_ptr;
      length : int;
      addr_type : int;
      result : hostent_var_ptr;
      buffer : char_var_ptr;
      buflen : int;
      errnop : access int)
     return hostent_ptr;
      pragma Import (C, gethostbyaddr_r, gethostbyaddr_r_LINKNAME);

   function htonl (hostlong  : unsigned_long) return unsigned_long;
      pragma Import (C, htonl, "c_htonl");
   function ntohl (netlong   : unsigned_long) return unsigned_long;
      pragma Import (C, ntohl, "c_ntohl");
   function htons (hostshort : unsigned_short) return unsigned_short;
      pragma Import (C, htons, "c_htons");
   function ntohs (netshort  : unsigned_short) return unsigned_short;
      pragma Import (C, ntohs, "c_ntohs");

   function gethostname
     (name : char_var_ptr; namelen : int)
     return int;
      pragma Import (C, gethostname, gethostname_LINKNAME);

   function read
     (fildes : int;
      buf : char_var_ptr;
      nbyte : size_t)
     return ssize_t;
      pragma Import (C, read, read_LINKNAME);

   function write
     (fildes : int;
      buf : char_ptr;
      nbyte : size_t)
     return ssize_t;
      pragma Import (C, write, write_LINKNAME);

   function open
     (path : char_ptr;
      oflag : int;
      mode  : mode_t := 0)
     return int;
      pragma Import (C, open, open_LINKNAME);

   function close (fd : int) return int;
      pragma Import (C, close, close_LINKNAME);
   --  close a file descriptor (or socket)

   function unlink (path : char_ptr) return int;
      pragma Import (C, unlink, unlink_LINKNAME);

   function link
     (existing : char_ptr;
      new_name : char_ptr)
     return int;
      pragma Import (C, link, link_LINKNAME);

   function fsync (fildes : int) return int;
      pragma Import (C, fsync, fsync_LINKNAME);

   function stat (path : char_ptr; buf : stat_ptr) return int;
      pragma Import (C, stat, stat_LINKNAME);

   function Get_AddressString (Addr : Internet_Address) return String is
   begin
      return Form_String (inet_ntoa (Addr.C));
   end Get_AddressString;

   function Hash_Code (Addr : Internet_Address) return Integer is
   begin
      return Integer (Addr.C.s_addr mod 2**(Integer'Size - 1));
   end Hash_Code;

   function "=" (Left, Right : Internet_Address) return Boolean is
   begin
      return Left.C.s_addr = Right.C.s_addr;
   end "=";

   function cptr_to_sia is new Unchecked_Conversion
     (char_ptr, in_addr_ptr);

   function Get_AddrByName (Host : String) return Internet_Address is
      name : constant POSIX_String := To_POSIX_String (Host) & NUL;
      hostent : aliased struct_hostent;
      buffer : POSIX_String (1 .. 1024);
      error_code : aliased int;
      p : hostent_ptr;
   begin
      p := gethostbyname_r
        (name =>   name (name'First)'Unchecked_Access,
         result => hostent'Unchecked_Access,
         buffer => buffer (buffer'First)'Unchecked_Access,
         buflen => buffer'Length,
         errnop => error_code'Unchecked_Access);
      if p = null then Raise_POSIX_Error; end if;
      return (C => cptr_to_sia (p.h_addr_list.all).all);
   end Get_AddrByName;

   function Get_HostByAddr (Addr : Internet_Address) return String is
      hostent : aliased struct_hostent;
      buffer : POSIX_String (1 .. 1024);
      error_code : aliased int;
      p : hostent_ptr;
      function "+" is new Unchecked_Conversion
        (in_addr_ptr, char_ptr);
   begin
      p := gethostbyaddr_r
        (addr =>   +Addr.C'Unchecked_Access,
         length => Addr.C'Size / char'Size,
         addr_type => AF_INET,
         result => hostent'Unchecked_Access,
         buffer => buffer (buffer'First)'Unchecked_Access,
         buflen => buffer'Length,
         errnop => error_code'Unchecked_Access);
      if p = null then Raise_POSIX_Error; end if;
      return Form_String (p.h_name);
   end Get_HostByAddr;

   function Local_Host return Internet_Address is
      function "+" is new Unchecked_Conversion
        (System.Address, char_var_ptr);
   begin
      if Local_Hostname (Local_Hostname'First) = Character'Val (0) then
         Check (gethostname (+Local_Hostname (Local_Hostname'First)'Address,
           Local_Hostname'Length));
      end if;
      return Get_AddrByName (Local_Hostname);
   end Local_Host;

   --------------------------------------

   function Get_AllByName (Host : String) return Internet_Address_Array is
      name : constant POSIX_String := To_POSIX_String (Host) & NUL;
      hostent : aliased struct_hostent;
      buffer : POSIX_String (1 .. 1024);
      error_code : aliased int;
      p : hostent_ptr;
      q : char_ptr_ptr;
      len : integer := 0;
   begin
      p := gethostbyname_r
        (name =>   name (name'First)'Unchecked_Access,
         result => hostent'Unchecked_Access,
         buffer => buffer (buffer'First)'Unchecked_Access,
         buflen => buffer'Length,
         errnop => error_code'Unchecked_Access);
      if p = null then Raise_POSIX_Error; end if;
      q := hostent.h_addr_list;
      while q.all /= null loop
         len := len + 1; Advance (q);
      end loop;
      declare
         result : Internet_Address_Array (1 .. len);
      begin
         q := hostent.h_addr_list;
         for I in result'Range loop
            result (I) := (C => cptr_to_sia (q.all).all);
            Advance (q);
         end loop;
         return result;
      end;
   end Get_AllByName;

   --------------------------------------

   function getsockname
     (socket_fd : int;
      name : struct_sockaddr_var_ptr;
      namelen : access int)
     return int;
      pragma Import (C, getsockname, getsockname_LINKNAME);
   --  returns the current name for a socket

   function Get_Address
     (Sock : in Socket'Class) return Internet_Socket_Address is
      addr : aliased struct_sockaddr_in;
      addrlen : aliased int := addr'Size / char'Size;
      function "+" is new Unchecked_Conversion
        (sockaddr_in_var_ptr, sockaddr_var_ptr);
   begin
      Check (getsockname (Sock.fd, +addr'Unchecked_Access,
        addrlen'Unchecked_Access));
      if addrlen /= addr'Size / char'Size then
         raise Constraint_Error;
      end if;
      return (Socket_Address with in_addr => addr);
   end Get_Address;

   function New_Address
     (Port : Port_Number;
      Addr : Internet_Address := Local_Host)
     return Internet_Socket_Address is
   begin
      return (Socket_Address with
        in_addr => (sin_family => AF_INET,
                    sin_port => in_port_t (htons (unsigned_short (Port))),
                    sin_addr => Addr.C,
                    sin_zero => (others => NUL)));
   end New_Address;

   function New_Address
     (Port : Port_Number;
      Host : String)
     return Internet_Socket_Address is
   begin
      return New_Address (Port, Get_AddrByName (Host));
   end New_Address;

   function Get_Internet_Address
     (Addr : Internet_Socket_Address) return Internet_Address is
   begin
      return (C => Addr.in_addr.sin_addr);
   end Get_Internet_Address;

   function Get_Port (Addr : Internet_Socket_Address) return Port_Number is
   begin
      return Port_Number (Addr.in_addr.sin_port);
   end Get_Port;

   ----------------------------------------

   function getpeername
     (socket_fd : int;
      name : access struct_sockaddr;
      namelen : access int)
     return int;
      pragma Import (C, getpeername, getpeername_LINKNAME);
   --  returns the name of the peer connected to a socket

   function Get_PeerAddress
     (Sock : Stream_Socket) return Internet_Socket_Address is
      addr : aliased struct_sockaddr_in;
      addrlen : aliased int := addr'Size / char'Size;
      function "+" is new Unchecked_Conversion
        (sockaddr_in_var_ptr, sockaddr_var_ptr);
   begin
      Check (getpeername
        (Sock.fd, +addr'Unchecked_Access, addrlen'Unchecked_Access));
      if addrlen /= addr'Size / char'Size then raise Constraint_Error; end if;
      return (Socket_Address with in_addr => addr);
   end Get_PeerAddress;

   ----------------------------------------

   function Address
     (Addr : Internet_Socket_Address) return sockaddr_ptr is
      function "+" is new Unchecked_Conversion
        (sockaddr_in_ptr, sockaddr_ptr);
   begin
      return +Addr.in_addr'Unchecked_Access;
   end Address;

   function Length (Addr : Internet_Socket_Address) return POSIX.C.int is
   begin
      return Addr.in_addr'Size / char'Size;
   end Length;

   function Valid (Addr : Internet_Socket_Address) return Boolean is
   --  This is only the most basic validity check.
   --  ???? Consider doing more?
   begin
      return Addr.in_addr.sin_family = AF_INET;
   end Valid;

   function Protocol_Family
     (Addr : Internet_Socket_Address) return POSIX.C.int is
   begin
      return PF_INET;
   end Protocol_Family;

end Sockets.Internet;




------------------------------------------------------------------------------
--                                                                          --
--                                 GNATSOCKS                                --
--                                                                          --
--                       S o c k e t s . I n t e r n e t                    --
--                                                                          --
--                                  S p e c                                 --
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

package Sockets.Internet is

   type Port_Number is new POSIX.C.unsigned_short;
   Any_Port : constant Port_Number;
   --  specifies to Open for a server socket
   --  that it may assign any free port number

   type Internet_Address is private;
   All_Local_Addresses : constant Internet_Address;
   --  may be used with Open on a server socket to
   --  allow accepting connections for any local Internet address

   function Get_AddressString (Addr : Internet_Address) return String;
   --  returns dot-separated numeric internet address as string,
   --  e.g. "128.186.121.10".

   function Hash_Code (Addr : Internet_Address) return Integer;
   --  returns value suitable for use in a hash-table lookup

   function "=" (Left, Right : Internet_Address) return Boolean;
   --  returns true iff both have the same numeric address,
   --  not necessarily if both designate the same host

   function Get_AddrByName (Host : String) return Internet_Address;
   --  returns the address of the named host

   function Get_HostByAddr (Addr : Internet_Address) return String;
   --  returns the name of the host with given address

   function Local_Host return Internet_Address;

   --------------------------------------

   type Internet_Address_Array is
     array (Positive range <>) of Internet_Address;

   function Get_AllByName (Host : String) return Internet_Address_Array;
   --  returns all the addresses of the named host

   --------------------------------------

   type Internet_Socket_Address is new Socket_Address with private;

   function Get_Address
     (Addr : in Socket'Class) return Internet_Socket_Address;

   function New_Address
     (Port : Port_Number;
      Addr : Internet_Address := Local_Host)
     return Internet_Socket_Address;
   --  specifies the host address and port number of the socket address

   function New_Address
     (Port : Port_Number;
      Host : String)
     return Internet_Socket_Address;
   --  specifies the host name and port number of the socket address

   function Get_Internet_Address
     (Addr : Internet_Socket_Address) return Internet_Address;

   function Get_Port (Addr : Internet_Socket_Address) return Port_Number;

   --------------------------------------------------------

   function Get_PeerAddress
     (Sock : Stream_Socket) return Internet_Socket_Address;
   --  return socket address of peer connected to open stream socket

private

   type Internet_Address is record
      C : aliased POSIX.C.Sockets.struct_in_addr;
   end record;

   Any_Port : constant Port_Number := 0;
   All_Local_Addresses : constant Internet_Address :=
      (C => (s_addr => POSIX.C.Netinet.INADDR_ANY));

   function Address
     (Addr : Internet_Socket_Address) return POSIX.C.Sockets.sockaddr_ptr;

   function Length (Addr : Internet_Socket_Address) return POSIX.C.int;

   function Valid (Addr : Internet_Socket_Address) return Boolean;

   function Protocol_Family
     (Addr : Internet_Socket_Address) return POSIX.C.int;

   -----------------------------------

   type Internet_Socket_Address is new Socket_Address with record
      in_addr : aliased POSIX.C.Sockets.struct_sockaddr_in;
   end record;

end Sockets.Internet;

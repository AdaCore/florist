------------------------------------------------------------------------------
--                                                                          --
--                                 GNATSOCKS                                --
--                                                                          --
--                          S o c k e t s . U n i x                         --
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

with POSIX.C;
package Sockets.Unix is

   type Unix_Socket_Address is new Socket_Address with private;

   function Get_Address
     (Sock : in Socket'Class) return Unix_Socket_Address;

   function New_Address (Path : String) return Unix_Socket_Address;
   --  specifies the pathname of the socket

   function Get_Path (Addr : Unix_Socket_Address) return String;

private

   type Unix_Socket_Address is new Socket_Address with record
      un_addr : aliased POSIX.C.Sockets.struct_sockaddr_un;
   end record;

   function Address
     (Addr : Unix_Socket_Address) return POSIX.C.Sockets.sockaddr_ptr;

   function Length (Addr : Unix_Socket_Address) return POSIX.C.int;

   function Valid (Addr : Unix_Socket_Address) return Boolean;

   function Protocol_Family
     (Addr : Unix_Socket_Address) return POSIX.C.int;

end Sockets.Unix;

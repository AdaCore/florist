-----------------------------------------------------------------------------
--                                                                          --
--                                 GNATSOCKS                                --
--                                                                          --
--                          S o c k e t s . U n i x                         --
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

with Ada.Finalization,
     Ada.Streams,
     POSIX.C,
     POSIX.Implementation,
     System,
     Unchecked_Conversion;
package body Sockets.Unix is

   use POSIX,
       POSIX.C,
       POSIX.C.Sockets,
       POSIX.Implementation;

   function getsockname
     (socket_fd : int;
      name : sockaddr_var_ptr;
      namelen : access int)
     return int;
      pragma Import (C, getsockname, getsockname_LINKNAME);
   --  returns the current name for a socket

   function "+" is new Unchecked_Conversion
     (sockaddr_un_ptr, sockaddr_var_ptr);
   function "+" is new Unchecked_Conversion
     (sockaddr_un_ptr, sockaddr_ptr);

   function Get_Address
     (Sock : in Socket'Class)
     return Unix_Socket_Address is
      addr : aliased struct_sockaddr_un;
      addrlen : aliased int := addr'Size / char'Size;
   begin
      Check (getsockname (Sock.fd, +addr'Unchecked_Access,
        addrlen'Unchecked_Access));
      if addrlen /= addr'Size / char'Size then raise Constraint_Error; end if;
      return (Socket_Address with un_addr => addr);
   end Get_Address;

   type String_Ptr is access all String;
   function New_Address (Path : String) return Unix_Socket_Address is
      addr : struct_sockaddr_un;
      function sptr_to_psptr is new Unchecked_Conversion
         (String_Ptr, POSIX_String_Ptr);
   begin
      if Path'Length > addr.sun_path'Length - 1 then
         raise Constraint_Error;
      end if;
      addr.sun_family := AF_UNIX;
      Nulterminate (addr.sun_path, Path);
      return (Socket_Address with un_addr => addr);
   end New_Address;

   function Get_Path (Addr : Unix_Socket_Address) return String is
   begin
      if Valid  (Addr) then raise Constraint_Error; end if;
      return Form_String (Addr.un_addr.sun_path (1)'Unchecked_Access);
   end Get_Path;

   ----------------------------------------

   function Address
     (Addr : Unix_Socket_Address)
     return sockaddr_ptr is
   begin
      return +Addr.un_addr'Unchecked_Access;
   end Address;

   function Length (Addr : Unix_Socket_Address) return POSIX.C.int is
   begin
      return Addr.un_addr'Size / char'Size;
   end Length;

   function Valid (Addr : Unix_Socket_Address) return Boolean is
   --  only the most basic validity check
   --  ... consider doing more?
   begin
      for I in Addr.un_addr.sun_path'Range loop
         if Addr.un_addr.sun_path (I) = NUL then
            return Addr.un_addr.sun_family = AF_UNIX;
         end if;
      end loop;
      return False;
   end Valid;

   function Protocol_Family
     (Addr : Unix_Socket_Address) return POSIX.C.int is
   begin
      return PF_UNIX;
   end Protocol_Family;

end Sockets.Unix;






------------------------------------------------------------------------------
--                                                                          --
--   POSIX Ada95 Bindings for Protocol Independent Interfaces (P1003.5c)    --
--                                                                          --
--                   P O S I X . S o c k e t s . L o c a l                  --
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
with GNAT.IO; use GNAT.IO;
with POSIX.Implementation; use POSIX.Implementation;
with Unchecked_Conversion;
with System;
package body POSIX.Sockets.Local is

   use POSIX.C.Sockets;
   use POSIX.C.NetDB;
   --  unchecked conversions

   function To_Ptr is
     new Unchecked_Conversion (sockaddr_ptr, sockaddr_un_ptr);
   function To_Ptr is
     new Unchecked_Conversion (char_ptr, sockaddr_un_ptr);
   function "+" is
     new Unchecked_Conversion (sockaddr_un_ptr, sockaddr_var_ptr);

   ----------------------------------
   --  Local IPC Socket Addresses  --
   ----------------------------------

   function Get_Socket_Path (Name : Local_Socket_Address)
      return POSIX.Pathname is
      Length : Integer := 0;
   begin
      for I in Name.C.sun_path'Range loop
         exit when Name.C.sun_path (I) = NUL;
         Length := Length + 1;
      end loop;
      return Name.C.sun_path (Name.C.sun_path'First .. Length);
   end Get_Socket_Path;

   procedure Set_Socket_Path
      (Name : in out Local_Socket_Address;
       Path : in     POSIX.Pathname) is
      Path_With_NUL : POSIX.Pathname := Path & NUL;
   begin
      Name.C.sun_path (Path_With_NUL'First .. Path_With_NUL'Last)
         := Path_With_NUL;
   end Set_Socket_Path;

   -------------------------------------------------------
   --  tagged operations for type Local_Socket_Address  --
   -------------------------------------------------------

   function Address (Name : Local_Socket_Address)
      return POSIX.C.Sockets.sockaddr_var_ptr is
   begin
      return +Name.C'Unchecked_Access;
   end Address;

   function Length (Name : Local_Socket_Address)
      return POSIX.C.size_t is
   begin
      for I in Name.C.sun_path'Range loop
         if Name.C.sun_path (I) = NUL then
            return size_t (I + (Name.C.sun_family'Size / System.Storage_Unit));
         end if;
      end loop;
      --  if no null was found return zero length
      return 0;
   end Length;

   ---------------------
   --  Get_Peer_Name  --
   ---------------------

   function c_getpeername
      (s : int; socketaddress : sockaddr_var_ptr;
       addresslen : size_t_var_ptr) return int;
   pragma Import (C, c_getpeername, getpeername_LINKNAME);

   function Get_Peer_Name (Socket : POSIX.IO.File_Descriptor)
      return Local_Socket_Address is
      c_address : aliased struct_sockaddr_un;
      c_address_len : aliased size_t := c_address'Size / char'Size;
   begin
      Check (c_getpeername (int (Socket), +c_address'Unchecked_Access,
             c_address_len'Unchecked_Access));
      if c_address_len /= struct_sockaddr_un'Size / char'Size then
         raise Constraint_Error;
      end if;
      return (Socket_Address with C => c_address);
   end Get_Peer_Name;

   -----------------------
   --  Get_Socket_Name  --
   -----------------------

   function c_getsockname
       (s : int; socketaddress : sockaddr_var_ptr;
        addresslen : size_t_var_ptr) return int;
   pragma Import (C, c_getsockname, getsockname_LINKNAME);

   function Get_Socket_Name (Socket : POSIX.IO.File_Descriptor)
      return Local_Socket_Address is
      c_address : aliased struct_sockaddr_un;
      c_address_len : aliased size_t := c_address'Size / char'Size;
   begin
      Check (c_getsockname (int (Socket), +c_address'Unchecked_Access,
             c_address_len'Unchecked_Access));
      if c_address_len /= struct_sockaddr_un'Size / char'Size then
         raise Constraint_Error;
      end if;
      return (Socket_Address with C => c_address);
   end Get_Socket_Name;

   -----------------------
   --  Get_Socket_Name  --
   -----------------------

   function Get_Socket_Name (Handle : Socket_Message)
      return Local_Socket_Address is
   begin
      --  cast the generic address pointer to a local socket
      --  address pointer and dereference it. Note that dot1g uses
      --  void* for these. Solaris uses typedef caddr_t which is char*.
      return (Socket_Address with C => To_Ptr (Handle.C.msg_name).all);
   end Get_Socket_Name;

   -------------------
   --  Get_Address  --
   -------------------

   function Get_Address (Info_Item : Socket_Address_Information)
      return Local_Socket_Address is
   begin
      --  cast the generic socket address pointer to a local socket
      --  address pointer and dereference it
      return (Socket_Address with C => To_Ptr (Info_Item.C.ai_addr).all);
   end Get_Address;

end POSIX.Sockets.Local;

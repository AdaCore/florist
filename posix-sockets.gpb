------------------------------------------------------------------------------
--                                                                          --
--   POSIX Ada95 Bindings for Protocol Independent Interfaces (P1003.5c)    --
--                                                                          --
--                         P O S I X . S o c k e t s                        --
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

with POSIX,
     POSIX.C,
     POSIX.Implementation,
     POSIX.IO,
     System,
     Unchecked_Conversion;

package body POSIX.Sockets is

   use POSIX.C;
   use POSIX.C.Sockets;
   use POSIX.C.NetDB;
   use POSIX.IO;
   use POSIX.Implementation;

   --  unchecked conversions for sockets system calls

   function To_int is new Unchecked_Conversion (Bits, int);
   function To_Bits is new Unchecked_Conversion (int, Bits);

   function To_char_ptr is new Unchecked_Conversion
     (System.Address, char_ptr);
   function To_char_ptr is new Unchecked_Conversion
     (sockaddr_var_ptr, char_ptr);

   function To_char_var_ptr is new Unchecked_Conversion
     (System.Address, char_var_ptr);

   function To_size_t_var_ptr is new Unchecked_Conversion
     (System.Address, size_t_var_ptr);

   function To_Address is new Unchecked_Conversion
     (char_ptr, System.Address);

   function To_addrinfo_ptr is new Unchecked_Conversion
     (System.Address, addrinfo_ptr);

   function To_iovec_ptr is new Unchecked_Conversion
      (System.Address, iovec_ptr);

   function To_sockaddr_ptr is new Unchecked_Conversion
     (System.Address, sockaddr_ptr);
   function To_sockaddr_var_ptr is new Unchecked_Conversion
     (System.Address, sockaddr_var_ptr);

   --------------------
   --  system calls  --
   --------------------

   function c_getsockopt
      (s : int;
       level : int;
       optname : int;
       optval : char_var_ptr;
       optlen : size_t_var_ptr)
     return int;
   pragma Import (C, c_getsockopt, getsockopt_LINKNAME);

   function c_setsockopt
      (s : int;
       level : int;
       optname : int;
       optval : char_ptr;
       optlen : size_t)
     return int;
   pragma Import (C, c_setsockopt, setsockopt_LINKNAME);

   type fdpair is array (1 .. 2) of int;

   --  getaddrinfo() support from <netdb.h>
   --  The following function prototypes are from a prototype getaddrinfo()
   --  by Richard Stevens.  They are not part of Solaris 2.5.
   type addrinfo_ptr_ptr is access all addrinfo_ptr;
   pragma Convention (C, addrinfo_ptr_ptr);

   --  select file descriptor list <sys/select.h>
   --  Note that on Solaris this is more complicated that shown here.
   --  The header builds a bitmap dynamically based on host wordsize
   --  and the FD_SETSIZE constant. The normal case is an FD_SETSIZE
   --  of 1024, so 32 32-bit longs are needed to bitmap that number
   --  of file descriptors. Not sure what this means for the
   --  configure program.
   type fd_mask is array (1 .. 32) of unsigned;
   type struct_fd_set is record
      fd_bits : fd_mask;
   end record;
   pragma Convention (C, struct_fd_set);
   type fd_set_ptr is access all struct_fd_set;
   pragma Convention (C, fd_set_ptr);
   type fd_set_const_ptr is access constant struct_fd_set;
   pragma Convention (C, fd_set_const_ptr);

   --  select macros to manipulate the fd_set bitmap
   procedure c_fd_set (fd : int; fdsetp : fd_set_ptr);
   pragma Import (C, c_fd_set, "c_fd_set");
   procedure c_fd_clr (fd : int; fdsetp : fd_set_ptr);
   pragma Import (C, c_fd_clr, "c_fd_clr");
   function c_fd_isset (fd : int; fdsetp : fd_set_const_ptr) return int;
   pragma Import (C, c_fd_isset, "c_fd_isset");
   procedure c_fd_zero (fdsetp : fd_set_ptr);
   pragma Import (C, c_fd_zero, "c_fd_zero");

   -----------------------
   --  Socket Messages  --
   -----------------------

   procedure Set_Socket_Name
      (Message : in out Socket_Message;
       Name    : in     Socket_Address_Pointer) is
   begin
      Message.C.msg_name := Name.C'Unchecked_Access;
      Message.C.msg_namelen := Name.Length;
   end Set_Socket_Name;

   procedure Set_IO_Vector_Array
      (Message : in out Socket_Message;
       Ptr     : in     IO_Vector_Array_Pointer) is
   begin
      Message.C.msg_iov := To_iovec_ptr (Ptr (Ptr'First)'Address);
      Message.C.msg_iovlen := size_t (Ptr.all'Length);
      Message.io_vector_array_ptr := Ptr;
   end Set_IO_Vector_Array;

   function Get_IO_Vector_Array (Message : Socket_Message)
      return IO_Vector_Array_Pointer is
   begin
      return Message.io_vector_array_ptr;
   end Get_IO_Vector_Array;

   procedure Set_Message_Options
      (Message : in out Socket_Message;
       Options : in     Message_Option_Set) is
   begin
#     if BSD4_3 then
         null;
#     else
         Message.C.msg_flags := To_int (Option_Set (Options).Option);
#     end if;
   end Set_Message_Options;

   function Get_Message_Status
      (Message : Socket_Message)
      return Message_Status_Set is
   begin
#     if BSD4_3 then
         return Message_Status_Set
            (Option_Set'(Option => To_Bits (0)));
#     else
         return Message_Status_Set
            (Option_Set'(Option => To_Bits (Message.C.msg_flags)));
#     end if;
   end Get_Message_Status;

   --------------------------
   --  Set_Ancillary_Data  --
   --------------------------

   procedure Set_Ancillary_Data
     (Message : in out Socket_Message;
      Data    : in     System.Address;
      Length  : in     POSIX.IO_Count) is
   begin
#     if BSD4_3 then
         null;
#     else
         Message.C.msg_control    := To_char_ptr (Data);
         Message.C.msg_controllen := size_t (Length);
#     end if;
   end Set_Ancillary_Data;

   --------------------------
   --  Get_Ancillary_Data  --
   --------------------------

   procedure Get_Ancillary_Data
     (Message : Socket_Message;
      Data    : out System.Address;
      Length  : out POSIX.IO_Count) is
   begin
#     if BSD4_3 then
         Data := System.Null_Address;
         Length := 0;
#     else
         Data   := To_Address (Message.C.msg_control);
         Length := POSIX.IO_Count (Message.C.msg_controllen);
#     end if;
   end Get_Ancillary_Data;

   -------------------------
   --  Accept_Connection  --
   -------------------------

   function c_accept
     (s : int;
      socketaddress : sockaddr_var_ptr;
      addresslen : size_t_var_ptr)
     return int;
   pragma Import (C, c_accept, accept_LINKNAME);

   procedure Accept_Connection
     (Socket            : in  POSIX.IO.File_Descriptor;
      Connection_Socket : out POSIX.IO.File_Descriptor;
      Name              : in  Socket_Address_Pointer)
   is
      Address_Length : aliased size_t;
   begin
      Address_Length := Name.Length;
      Connection_Socket := POSIX.IO.File_Descriptor (Check_NNeg
       (c_accept (int (Socket),
         To_sockaddr_var_ptr (Name.C'Address),
         Address_Length'Unchecked_Access)));
   end Accept_Connection;

   function Accept_Connection
      (Socket : POSIX.IO.File_Descriptor)
      return POSIX.IO.File_Descriptor is
      Result : int;
   begin
      Result :=
        c_accept (int (Socket), null, null);
      return POSIX.IO.File_Descriptor (Check_NNeg (Result));
   end Accept_Connection;

   ------------
   --  Bind  --
   ------------

   function c_bind
      (s : int;
       socketaddress : sockaddr_ptr;
       addresslen : size_t)
     return int;
   pragma Import (C, c_bind, bind_LINKNAME);

   procedure Bind
      (Socket : in POSIX.IO.File_Descriptor;
       Name   : in Socket_Address_Pointer) is
   begin
      Check (c_bind (int (Socket),
        To_sockaddr_ptr (Name.C'Address), Name.Length));
   end Bind;

   ---------------
   --  Connect  --
   ---------------

   function c_connect
      (s : int;
       socketaddress : sockaddr_ptr;
       addresslen : size_t)
     return int;
   pragma Import (C, c_connect, connect_LINKNAME);

   procedure Connect
      (Socket : in POSIX.IO.File_Descriptor;
       Peer   : in Socket_Address_Pointer) is
   begin
      Check (c_connect (int (Socket),
        To_sockaddr_ptr (Peer.C'Address), Peer.Length));
   end Connect;

   --------------------
   --  Specify_Peer  --
   --------------------

   procedure Specify_Peer
      (Socket : in POSIX.IO.File_Descriptor;
       Peer   : in Socket_Address_Pointer) is
   begin
      Check (c_connect (int (Socket),
        To_sockaddr_ptr (Peer.C'Address), Peer.Length));
   end Specify_Peer;

   ----------------------
   --  Unspecify_Peer  --
   ----------------------

   procedure Unspecify_Peer
      (Socket : in POSIX.IO.File_Descriptor) is
   begin
      Check (c_connect (int (Socket), null, 0));
   end Unspecify_Peer;

   --------------
   --  Create  --
   --------------

   function c_socket (protofamily : int; typ : int; protocol : int)
     return int;
   pragma Import (C, c_socket, socket_LINKNAME);

   function Create
      (Domain   : Protocol_Family;
       Of_Type  : Socket_Type;
       Protocol : Protocol_Number := Default_Protocol)
     return POSIX.IO.File_Descriptor is
      Result : int;
   begin
      Result := c_socket (int (Domain), int (Of_Type),
                          int (Protocol));
      return POSIX.IO.File_Descriptor (Check_NNeg (Result));
   end Create;

   -------------------
   --  Create_Pair  --
   -------------------

   function c_socketpair
      (protofamily : int; typ : int; protocol : int; sv : fdpair)
     return int;
   pragma Import (C, c_socketpair, socketpair_LINKNAME);

   procedure Create_Pair
      (Peer1    : out POSIX.IO.File_Descriptor;
       Peer2    : out POSIX.IO.File_Descriptor;
       Domain   : in  Protocol_Family;
       Of_Type  : in  Socket_Type;
       Protocol : in  Protocol_Number := Default_Protocol) is
      Result : fdpair := (0, 0);
   begin
      Check (c_socketpair (int (Domain), int (Of_Type),
             int (Protocol), Result));
      Peer1 := POSIX.IO.File_Descriptor (Result (1));
      Peer2 := POSIX.IO.File_Descriptor (Result (2));
   end Create_Pair;

   ---------------------
   --  Get_Peer_Name  --
   ---------------------

   --  this function is protocol specific. Look in the sockets child packages.

   ---------------------
   --  Make_Empty     --
   ---------------------

   procedure c_freeaddrinfo (ai : addrinfo_ptr);
   pragma Import (C, c_freeaddrinfo, freeaddrinfo_LINKNAME);

   procedure Make_Empty
      (Info_Item : in out Socket_Address_Info_List) is
   begin
      null;  -- remove later
      c_freeaddrinfo (Info_Item.C'Unchecked_Access);
   end Make_Empty;

   -------------------------------
   --  Get_Socket_Address_Info  --
   -------------------------------

   procedure Set_Flags
      (Info_Item : in out Socket_Address_Info;
       Flags     : in     Address_Flags) is
   begin
      Info_Item.C.ai_flags := To_int (Option_Set (Flags).Option);
   end Set_Flags;
   function Get_Flags (Info_Item : Socket_Address_Info)
      return Address_Flags is
   begin
      return Address_Flags
         (Option_Set'(Option => To_Bits (Info_Item.C.ai_flags)));
   end Get_Flags;

   procedure Set_Family
      (Info_Item : in out Socket_Address_Info;
       Family    : in     Protocol_Family) is
   begin
      Info_Item.C.ai_family := int (Family);
   end Set_Family;

   function Get_Family (Info_Item : Socket_Address_Info)
      return Protocol_Family is
   begin
      return Protocol_Family (Info_Item.C.ai_family);
   end Get_Family;

   procedure Set_Socket_Type
      (Info_Item : in out Socket_Address_Info;
       To        : in     Socket_Type) is
   begin
      Info_Item.C.ai_socktype := int (To);
   end Set_Socket_Type;

   function Get_Socket_Type (Info_Item : Socket_Address_Info)
      return Socket_Type is
   begin
      return Socket_Type (Info_Item.C.ai_socktype);
   end Get_Socket_Type;

   procedure Set_Protocol_Number
      (Info_Item : in out Socket_Address_Info;
       Protocol  : in     Protocol_Number) is
   begin
      Info_Item.C.ai_protocol := int (Protocol);
   end Set_Protocol_Number;
   function Get_Protocol_Number (Info_Item : Socket_Address_Info)
      return Protocol_Number is
   begin
      return Protocol_Number (Info_Item.C.ai_protocol);
   end Get_Protocol_Number;

   function Get_Canonical_Name (Info_Item : Socket_Address_Info)
      return POSIX.POSIX_String is
   begin
      return Form_POSIX_String (Info_Item.C.ai_canonname);
   end Get_Canonical_Name;

   function c_getaddrinfo
     (name : char_ptr; service : char_ptr; req : addrinfo_ptr;
       pai : addrinfo_ptr_ptr)
     return int;
   pragma Import (C, c_getaddrinfo, getaddrinfo_LINKNAME);

   procedure Get_Socket_Address_Info
      (Name    : in     POSIX.POSIX_String;
       Service : in     POSIX.POSIX_String;
       Request : in     Socket_Address_Info;
       Info    : in out Socket_Address_Info_List) is
      Pai : aliased addrinfo_ptr;
      Result : int;
      Name_NUL : POSIX.Pathname := Name & NUL;
      Service_NUL : POSIX.Pathname := Service & NUL;
      Host_Ptr : char_ptr := null;
      Service_Ptr : char_ptr := null;
      --  Not 100 percent why but you need this object to be local or
      --  else the function can cause problems.  It may be that its being
      --  passed in by registers (in parameter) and the pointers don't
      --  work.
      Tmp_Request : Socket_Address_Info := Request;
   begin
      if Name /= "" then
         Host_Ptr := Name_NUL (Name_NUL'First)'Unchecked_Access;
      end if;
      if Service /= "" then
         Service_Ptr := Service_NUL (Service_NUL'First)'Unchecked_Access;
      end if;
      Result := c_getaddrinfo (Host_Ptr,
                               Service_Ptr,
                               Tmp_Request.C'Unchecked_Access,
                               Pai'Unchecked_Access);
      if Result = 0 then
         Info.C := Pai.all;
      else
         --  bias the "EAI_" error codes by Addrinfo_Error_Code'First to
         --  make an errno (unless its EAI_SYSTEM, then errno is set already)
         if Result /= 11 then
            POSIX.Set_Error_Code (POSIX.Error_Code (Result) +
                                  Addrinfo_Error_Code'First);
         end if;
         Result := int (Get_Error_Code);
         Check_NZ (Result);
         Info.C := (ai_flags     => 0,    ai_family    => 0,
                    ai_socktype  => 0,    ai_protocol  => 0,
                    ai_addrlen   => 0,    ai_canonname => null,
                    ai_addr      => null, ai_next      => null);
      end if;
   end Get_Socket_Address_Info;

   procedure Get_Socket_Address_Info
      (Name    : POSIX.POSIX_String;
       Service : POSIX.POSIX_String;
       Info    : in out Socket_Address_Info_List) is
      Pai : aliased addrinfo_ptr;
      Result : int;
      Name_NUL : POSIX.POSIX_String := Name & NUL;
      Service_NUL : POSIX.POSIX_String := Service & NUL;
      Host_Ptr : char_ptr := null;
      Service_Ptr : char_ptr := null;
   begin
      if Name /= "" then
         Host_Ptr := Name_NUL (Name_NUL'First)'Unchecked_Access;
      end if;
      if Service /= "" then
         Service_Ptr := Service_NUL (Service_NUL'First)'Unchecked_Access;
      end if;
      Result := c_getaddrinfo (Host_Ptr,
                               Service_Ptr, null,
                               Pai'Unchecked_Access);
      if Result = 0 then
         Info.C := Pai.all;
      else
         --  bias the "EAI_" error codes by Addrinfo_Error_Code'First to
         --  make an errno (unless its EAI_SYSTEM, then errno is set already)
         if Result /= 11 then
            POSIX.Set_Error_Code (POSIX.Error_Code (Result) +
                                  Addrinfo_Error_Code'First);
         end if;
         Result := int (Get_Error_Code);
         Check_NZ (Result);
         Info.C := (ai_flags     => 0,    ai_family    => 0,
                    ai_socktype  => 0,    ai_protocol  => 0,
                    ai_addrlen   => 0,    ai_canonname => null,
                    ai_addr      => null, ai_next      => null);
      end if;
   end Get_Socket_Address_Info;

   procedure For_Every_Item (List : Socket_Address_Info_List) is
      next_item : addrinfo_ptr := To_addrinfo_ptr (List.C'Address);
      Quit      : Boolean      := False;
   begin
      if not Quit then
         next_item := List.C.ai_next;
         while next_item /= null loop
            Action (Info => (C => next_item.all), Quit => Quit);
            exit when Quit;
            next_item := next_item.all.ai_next;
         end loop;
      end if;
   end For_Every_Item;

   -----------------------
   --  Get_Socket_Name  --
   -----------------------

   --   this function is protocol specific. Look in the sockets child packages.

   -----------------------
   --  Get_Socket_Type  --
   -----------------------

   function Get_Socket_Type
      (Socket : POSIX.IO.File_Descriptor)
      return Socket_Type is
      optval : aliased int;
      optlen : aliased size_t := optval'Size / char'Size;
   begin
      Check (c_getsockopt (int (Socket), int (SOL_SOCKET), int (SO_TYPE),
             To_char_var_ptr (optval'Address), optlen'Unchecked_Access));
      return Socket_Type (optval);
   end Get_Socket_Type;

   -----------------------------------------------------------------------
   --  Get and Set Socket Options
   -----------------------------------------------------------------------

   --  Socket Broadcast --

   function Get_Socket_Broadcast
      (Socket : POSIX.IO.File_Descriptor)
      return Socket_Option_Value is
      optval : aliased int;
      optlen : aliased size_t := optval'Size / char'Size;
   begin
      Check (c_getsockopt (s       => int (Socket),
                           level   => int (SOL_SOCKET),
                           optname => int (SO_BROADCAST),
                           optval  => To_char_var_ptr (optval'Address),
                           optlen  => optlen'Unchecked_Access));
      if optval = 0 then
         return Disabled;
      else
         return Enabled;
      end if;
   end Get_Socket_Broadcast;

   procedure Set_Socket_Broadcast
      (Socket : in POSIX.IO.File_Descriptor;
       To     : in Socket_Option_Value) is
      optval : aliased int := 0;
      optlen : size_t := optval'Size / char'Size;
   begin
      if To = Enabled then optval := 1; end if;
      Check (c_setsockopt (s       => int (Socket),
                           level   => int (SOL_SOCKET),
                           optname => int (SO_BROADCAST),
                           optval  => To_char_ptr (optval'Address),
                           optlen  => optlen));
   end Set_Socket_Broadcast;

   --  Socket Debugging --

   function Get_Socket_Debugging
      (Socket : POSIX.IO.File_Descriptor)
      return Socket_Option_Value is
      optval : aliased int;
      optlen : aliased size_t := optval'Size / char'Size;
   begin
      Check (c_getsockopt (s       => int (Socket),
                           level   => int (SOL_SOCKET),
                           optname => int (SO_DEBUG),
                           optval  => To_char_var_ptr (optval'Address),
                           optlen  => optlen'Unchecked_Access));
      if optval = 0 then
         return Disabled;
      else
         return Enabled;
      end if;
   end Get_Socket_Debugging;

   procedure Set_Socket_Debugging
      (Socket : in POSIX.IO.File_Descriptor;
       To     : in Socket_Option_Value) is
      optval : aliased int := 0;
      optlen : size_t := optval'Size / char'Size;
   begin
      if To = Enabled then optval := 1; end if;
      Check (c_setsockopt (s       => int (Socket),
                           level   => int (SOL_SOCKET),
                           optname => int (SO_DEBUG),
                           optval  => To_char_ptr (optval'Address),
                           optlen  => optlen));
   end Set_Socket_Debugging;

   --  Socket Routing --

   function Get_Socket_Routing
      (Socket : POSIX.IO.File_Descriptor)
      return Socket_Option_Value is
      optval : aliased int;
      optlen : aliased size_t := optval'Size / char'Size;
   begin
      Check (c_getsockopt (s       => int (Socket),
                           level   => int (SOL_SOCKET),
                           optname => int (SO_DONTROUTE),
                           optval  => To_char_var_ptr (optval'Address),
                           optlen  => optlen'Unchecked_Access));
      if optval = 0 then
         return Enabled;
      else
         return Disabled;
      end if;
   end Get_Socket_Routing;

   procedure Set_Socket_Routing
      (Socket : in POSIX.IO.File_Descriptor;
       To     : in Socket_Option_Value) is
      optval : aliased int := 0;
      optlen : size_t := optval'Size / char'Size;
   begin
      if To = Disabled then optval := 1; end if;
      Check (c_setsockopt (s       => int (Socket),
                           level   => int (SOL_SOCKET),
                           optname => int (SO_DONTROUTE),
                           optval  => To_char_ptr (optval'Address),
                           optlen  => optlen));
   end Set_Socket_Routing;

   --  Socket Error Status --

   function Get_Socket_Error_Status
      (Socket : POSIX.IO.File_Descriptor)
      return POSIX.Error_Code is
      optval : aliased int;
      optlen : aliased size_t := optval'Size / char'Size;
   begin
      Check (c_getsockopt (s       => int (Socket),
                           level   => int (SOL_SOCKET),
                           optname => int (SO_ERROR),
                           optval  => To_char_var_ptr (optval'Address),
                           optlen  => optlen'Unchecked_Access));
      return POSIX.Error_Code (optval);
   end Get_Socket_Error_Status;

   --  Socket Keep Alive Interval --

   function Get_Socket_Keep_Alive
      (Socket : POSIX.IO.File_Descriptor)
      return Socket_Option_Value is
      optval : aliased int;
      optlen : aliased size_t := optval'Size / char'Size;
   begin
      Check (c_getsockopt (s       => int (Socket),
                           level   => int (SOL_SOCKET),
                           optname => int (SO_KEEPALIVE),
                           optval  => To_char_var_ptr (optval'Address),
                           optlen  => optlen'Unchecked_Access));
      if optval = 0 then
         return Disabled;
      else
         return Enabled;
      end if;
   end Get_Socket_Keep_Alive;

   procedure Set_Socket_Keep_Alive
      (Socket : in POSIX.IO.File_Descriptor;
       To     : in Socket_Option_Value) is
      optval : aliased int := 0;
      optlen : size_t := optval'Size / char'Size;
   begin
      if To = Enabled then optval := 1; end if;
      Check (c_setsockopt (s       => int (Socket),
                           level   => int (SOL_SOCKET),
                           optname => int (SO_KEEPALIVE),
                           optval  => To_char_ptr (optval'Address),
                           optlen  => optlen));
   end Set_Socket_Keep_Alive;

   --  Socket Linger Time --

   function Get_Socket_Linger_Time
      (Socket : POSIX.IO.File_Descriptor)
      return Linger_Time is
      optval : aliased struct_linger;
      optlen : aliased size_t := optval'Size / char'Size;
   begin
      Check (c_getsockopt (s       => int (Socket),
                           level   => int (SOL_SOCKET),
                           optname => int (SO_LINGER),
                           optval  => To_char_var_ptr (optval'Address),
                           optlen  => optlen'Unchecked_Access));
      return Linger_Time (optval.l_linger);
   end Get_Socket_Linger_Time;

   procedure Set_Socket_Linger_Time
      (Socket : in POSIX.IO.File_Descriptor;
       To     : in Linger_Time) is
      optval : aliased struct_linger;
      optlen : size_t := optval'Size / char'Size;
   begin
      if To = 0 then
         optval.l_onoff := 0;
         optval.l_linger := 0;
      else
         optval.l_onoff := 1;
         optval.l_linger := int (To);
      end if;
      Check (c_setsockopt (s       => int (Socket),
                           level   => int (SOL_SOCKET),
                           optname => int (SO_LINGER),
                           optval  => To_char_ptr (optval'Address),
                           optlen  => optlen));
   end Set_Socket_Linger_Time;

   --  Socket Leave Received Out_Of_Band Data Inline --

   function Get_Socket_OOB_Data_Inline
      (Socket : POSIX.IO.File_Descriptor)
      return Socket_Option_Value is
      optval : aliased int;
      optlen : aliased size_t := optval'Size / char'Size;
   begin
      Check (c_getsockopt (s       => int (Socket),
                           level   => int (SOL_SOCKET),
                           optname => int (SO_OOBINLINE),
                           optval  => To_char_var_ptr (optval'Address),
                           optlen  => optlen'Unchecked_Access));
      if optval = 0 then
         return Disabled;
      else
         return Enabled;
      end if;
   end Get_Socket_OOB_Data_Inline;

   procedure Set_Socket_OOB_Data_Inline
      (Socket : in POSIX.IO.File_Descriptor;
       To     : in Socket_Option_Value) is
      optval : aliased int := 0;
      optlen : size_t := optval'Size / char'Size;
   begin
      if To = Enabled then optval := 1; end if;
      Check (c_setsockopt (s       => int (Socket),
                           level   => int (SOL_SOCKET),
                           optname => int (SO_OOBINLINE),
                           optval  => To_char_ptr (optval'Address),
                           optlen  => optlen));
   end Set_Socket_OOB_Data_Inline;

   --  Socket Receive Buffer Size --

   function Get_Socket_Receive_Buffer_Size
      (Socket : POSIX.IO.File_Descriptor)
      return POSIX.IO_Count is
      optval : aliased int;
      optlen : aliased size_t := optval'Size / char'Size;
   begin
      Check (c_getsockopt (s       => int (Socket),
                           level   => int (SOL_SOCKET),
                           optname => int (SO_RCVBUF),
                           optval  => To_char_var_ptr (optval'Address),
                           optlen  => optlen'Unchecked_Access));
      return POSIX.IO_Count (optval);
   end Get_Socket_Receive_Buffer_Size;

   procedure Set_Socket_Receive_Buffer_Size
      (Socket : in POSIX.IO.File_Descriptor;
       To     : in POSIX.IO_Count) is
      optval : aliased int;
      optlen : aliased size_t := optval'Size / char'Size;
   begin
      optval := int (To);
      Check (c_setsockopt (s       => int (Socket),
                           level   => int (SOL_SOCKET),
                           optname => int (SO_RCVBUF),
                           optval  => To_char_ptr (optval'Address),
                           optlen  => optlen));
   end Set_Socket_Receive_Buffer_Size;

   --  Socket Receive Low-water Mark --

   function Get_Socket_Receive_Low_Water_Mark
      (Socket : POSIX.IO.File_Descriptor)
      return POSIX.IO_Count is
      optval : aliased int;
      optlen : aliased size_t := optval'Size / char'Size;
   begin
      Check (c_getsockopt (s       => int (Socket),
                           level   => int (SOL_SOCKET),
                           optname => int (SO_RCVLOWAT),
                           optval  => To_char_var_ptr (optval'Address),
                           optlen  => optlen'Unchecked_Access));
      return POSIX.IO_Count (optval);
   end Get_Socket_Receive_Low_Water_Mark;

   procedure Set_Socket_Receive_Low_Water_Mark
      (Socket : in POSIX.IO.File_Descriptor;
       To     : in POSIX.IO_Count) is
      optval : aliased int;
      optlen : aliased size_t := optval'Size / char'Size;
   begin
      optval := int (To);
      Check (c_setsockopt (s       => int (Socket),
                           level   => int (SOL_SOCKET),
                           optname => int (SO_RCVLOWAT),
                           optval  => To_char_ptr (optval'Address),
                           optlen  => optlen));
   end Set_Socket_Receive_Low_Water_Mark;

   --  Socket Receive Timeout --

   function Get_Socket_Receive_Timeout
      (Socket : POSIX.IO.File_Descriptor)
      return Duration is
      optval : aliased struct_timeval;
      optlen : aliased size_t := optval'Size / char'Size;
   begin
      Check (c_getsockopt (s       => int (Socket),
                           level   => int (SOL_SOCKET),
                           optname => int (SO_RCVTIMEO),
                           optval  => To_char_var_ptr (optval'Address),
                           optlen  => optlen'Unchecked_Access));
      return To_Duration (optval);
   end Get_Socket_Receive_Timeout;

   procedure Set_Socket_Receive_Timeout
      (Socket : in POSIX.IO.File_Descriptor;
       To     : in Duration) is
      optval : aliased struct_timeval;
      optlen : aliased size_t := optval'Size / char'Size;
   begin
      optval := POSIX.Implementation.To_Struct_Timeval (To);
      Check (c_setsockopt (s       => int (Socket),
                           level   => int (SOL_SOCKET),
                           optname => int (SO_RCVTIMEO),
                           optval  => To_char_ptr (optval'Address),
                           optlen  => optlen));
   end Set_Socket_Receive_Timeout;

   --  Socket Reuse Addresses --

   function Get_Socket_Reuse_Addresses
      (Socket : POSIX.IO.File_Descriptor)
      return Socket_Option_Value is
      optval : aliased int;
      optlen : aliased size_t := optval'Size / char'Size;
   begin
      Check (c_getsockopt (s       => int (Socket),
                           level   => int (SOL_SOCKET),
                           optname => int (SO_REUSEADDR),
                           optval  => To_char_var_ptr (optval'Address),
                           optlen  => optlen'Unchecked_Access));
      if optval = 0 then
         return Disabled;
      else
         return Enabled;
      end if;
   end Get_Socket_Reuse_Addresses;

   procedure Set_Socket_Reuse_Addresses
      (Socket : in POSIX.IO.File_Descriptor;
       To     : in Socket_Option_Value) is
      optval : aliased int := 0;
      optlen : size_t := optval'Size / char'Size;
   begin
      if To = Enabled then optval := 1; end if;
      Check (c_setsockopt (s       => int (Socket),
                           level   => int (SOL_SOCKET),
                           optname => int (SO_REUSEADDR),
                           optval  => To_char_ptr (optval'Address),
                           optlen  => optlen));
   end Set_Socket_Reuse_Addresses;

   --  Socket Send Buffer Size --

   function Get_Socket_Send_Buffer_Size
      (Socket : POSIX.IO.File_Descriptor)
      return POSIX.IO_Count is
      optval : aliased int;
      optlen : aliased size_t := optval'Size / char'Size;
   begin
      Check (c_getsockopt (s       => int (Socket),
                           level   => int (SOL_SOCKET),
                           optname => int (SO_SNDBUF),
                           optval  => To_char_var_ptr (optval'Address),
                           optlen  => optlen'Unchecked_Access));
      return POSIX.IO_Count (optval);
   end Get_Socket_Send_Buffer_Size;

   procedure Set_Socket_Send_Buffer_Size
      (Socket : in POSIX.IO.File_Descriptor;
       To     : in POSIX.IO_Count) is
      optval : aliased int;
      optlen : aliased size_t := optval'Size / char'Size;
   begin
      optval := int (To);
      Check (c_setsockopt (s       => int (Socket),
                           level   => int (SOL_SOCKET),
                           optname => int (SO_SNDBUF),
                           optval  => To_char_ptr (optval'Address),
                           optlen  => optlen));
   end Set_Socket_Send_Buffer_Size;

   --  Socket Send Low-water Mark --

   function Get_Socket_Send_Low_Water_Mark
      (Socket : POSIX.IO.File_Descriptor)
      return POSIX.IO_Count is
      optval : aliased int;
      optlen : aliased size_t := optval'Size / char'Size;
   begin
      Check (c_getsockopt (s       => int (Socket),
                           level   => int (SOL_SOCKET),
                           optname => int (SO_SNDLOWAT),
                           optval  => To_char_var_ptr (optval'Address),
                           optlen  => optlen'Unchecked_Access));
      return POSIX.IO_Count (optval);
   end Get_Socket_Send_Low_Water_Mark;

   procedure Set_Socket_Send_Low_Water_Mark
      (Socket : in POSIX.IO.File_Descriptor;
       To     : in POSIX.IO_Count) is
      optval : aliased int;
      optlen : aliased size_t := optval'Size / char'Size;
   begin
      optval := int (To);
      Check (c_setsockopt (s       => int (Socket),
                           level   => int (SOL_SOCKET),
                           optname => int (SO_SNDLOWAT),
                           optval  => To_char_ptr (optval'Address),
                           optlen  => optlen));
   end Set_Socket_Send_Low_Water_Mark;

   --  Socket Send Timeout --

   function Get_Socket_Send_Timeout
      (Socket : POSIX.IO.File_Descriptor)
      return Duration is
      optval : aliased struct_timeval;
      optlen : aliased size_t := optval'Size / char'Size;
   begin
      Check (c_getsockopt (s       => int (Socket),
                           level   => int (SOL_SOCKET),
                           optname => int (SO_SNDTIMEO),
                           optval  => To_char_var_ptr (optval'Address),
                           optlen  => optlen'Unchecked_Access));
      return To_Duration (optval);
   end Get_Socket_Send_Timeout;

   procedure Set_Socket_Send_Timeout
      (Socket : in POSIX.IO.File_Descriptor;
       To     : in Duration) is
      optval : aliased struct_timeval;
      optlen : aliased size_t := optval'Size / char'Size;
   begin
      optval := To_Struct_Timeval (To);
      Check (c_setsockopt (s       => int (Socket),
                           level   => int (SOL_SOCKET),
                           optname => int (SO_SNDTIMEO),
                           optval  => To_char_ptr (optval'Address),
                           optlen  => optlen));
   end Set_Socket_Send_Timeout;

   -------------------
   --  Is_A_Socket  --
   -------------------

   --  Note: Solaris does not have the isfdtype() function described in .1g.
   --  I think this needs to be done like the rest of the C stat() stuff in
   --  POSIX.File_Status, sort of like this:

--   function s_issock (mode : mode_t) return int;
--   pragma Import (C, s_issock, "s_issock");
--
--   function Is_Sock (File_Status : Status)
--      return Boolean is
--   begin
--      return s_issock (struct_stat(File_Status).st_mode) /= 0;
--   end Is_Sock;

   --  and in POSIX.Files, like this:

--   function Is_Sock (Pathname : POSIX.Pathname) return Boolean is
--      stat : POSIX.File_Status.status;
--   begin
--      stat := POSIX.File_Status.Get_File_Status (Pathname);
--      return (POSIX.File_Status.Is_Sock (stat));
--   exception
--      when POSIX_Error => return False;
--   end Is_Sock;

   --  Do we need to change the .5c binding to remove Is_A_Socket and instead
   --  make the appropriate updates to these 2 dot5 packages ???

   function Is_A_Socket (File : POSIX.IO.File_Descriptor)
      return Boolean is
   begin
      Raise_POSIX_Error (ENOSYS);
      return False;
   end Is_A_Socket;

   --------------
   --  Listen  --
   --------------

   function c_listen (s : int; backlog : int) return int;
   pragma Import (C, c_listen, listen_LINKNAME);

   procedure Listen
      (Socket  : in POSIX.IO.File_Descriptor;
       Backlog : in Connection_Queue_Length :=
                    Connection_Queue_Length'Last) is
   begin
      Check (c_listen (int (Socket), int (Backlog)));
   end Listen;

   ---------------
   --  Receive  --
   ---------------

   function c_recv
     (s : int;
      buf : System.Address;
      len : size_t;
      flags : int)
     return ssize_t;
   pragma Import (C, c_recv, recv_LINKNAME);

   procedure Receive
      (Socket           : in  POSIX.IO.File_Descriptor;
       Buffer           : in  System.Address;
       Octets_Requested : in  POSIX.IO_Count;
       Octets_Received  : out POSIX.IO_Count;
       Masked_Signals   : in  POSIX.Signal_Masking;
       Options          : in  Message_Option_Set
         := Message_Option_Set (POSIX.Empty_Set)) is
      Result   : ssize_t;
      Old_Mask : aliased Signal_Mask;
      use Ada_Streams;
   begin
      if Octets_Requested <= 0 then
         Octets_Received := 0;
         return;
      end if;
      Mask_Signals (Masked_Signals, Old_Mask'Unchecked_Access);
      Result := c_recv (s     => int (Socket),
                        buf   => Buffer,
                        len   => size_t (Octets_Requested),
                        flags => To_int (Option_Set (Options).Option));
      Restore_Signals (Masked_Signals, Old_Mask'Unchecked_Access);

      --  a positive result is the length of the data received
      if Result > 0 then
         Octets_Received := POSIX.IO_Count (Result);
      --  a zero result is a zero length record (possible for some
      --  protocols) or an eof or a closed connection
      elsif Result = 0 then
         Octets_Received := 0;
      --  anything else is an error condition
      else
         Raise_POSIX_Error;
      end if;
   end Receive;

   procedure Receive
      (Socket           : in  POSIX.IO.File_Descriptor;
       Buffer           : in  System.Address;
       Octets_Requested : in  POSIX.IO_Count;
       Octets_Received  : out POSIX.IO_Count;
       Options          : in  Message_Option_Set
         := Message_Option_Set (POSIX.Empty_Set)) is
      Result : ssize_t;
      use Ada_Streams;
   begin
      if Octets_Requested <= 0 then
         Octets_Received := 0;
         return;
      end if;
      Result := c_recv (s     => int (Socket),
                        buf   => Buffer,
                        len   => size_t (Octets_Requested),
                        flags => To_int (Option_Set (Options).Option));
      --  a positive result is the length of the data received
      if Result > 0 then
         Octets_Received := POSIX.IO_Count (Result);
      --  a zero result is a zero length record (possible for some
      --  protocols) or an eof or a closed connection
      elsif Result = 0 then
         Octets_Received := 0;
      --  anything else is an error condition
      else
         Raise_POSIX_Error;
      end if;
   end Receive;

   ----------------------
   --  Receive <From>  --
   ----------------------

   function c_recvfrom
     (s       : int;
      buf     : System.Address;
      len     : size_t;
      flags   : int;
      from    : sockaddr_var_ptr;
      fromlen : size_t_var_ptr) return ssize_t;
   pragma Import (C, c_recvfrom, recvfrom_LINKNAME);

   procedure Receive
      (Socket           : in  POSIX.IO.File_Descriptor;
       Buffer           : in  System.Address;
       Octets_Requested : in  POSIX.IO_Count;
       Octets_Received  : out POSIX.IO_Count;
       From             : in  Socket_Address_Pointer;
       Masked_Signals   : in  POSIX.Signal_Masking;
       Options          : in  Message_Option_Set
         := Message_Option_Set (POSIX.Empty_Set)) is
      Result : ssize_t;
      Old_Mask : aliased Signal_Mask;
      Address_Length : aliased size_t := 0;
      use Ada_Streams;
   begin
      if Octets_Requested <= 0 then
         Octets_Received := 0;
         return;
      end if;
      Mask_Signals (Masked_Signals, Old_Mask'Unchecked_Access);
      if From /= Null_Socket_Address then
         Address_Length := From.Length;
      end if;
      Result := c_recvfrom (s       => int (Socket),
                            buf     => Buffer,
                            len     => size_t (Octets_Requested),
                            flags   => To_int (Option_Set (Options).Option),
                            from    => To_sockaddr_var_ptr (From.C'Address),
                            fromlen => Address_Length'Unchecked_Access);
      Restore_Signals (Masked_Signals, Old_Mask'Unchecked_Access);
      --  a positive result is the length of the data received
      if Result > 0 then
         Octets_Received := POSIX.IO_Count (Result);
      --  a zero result is a zero length record (possible for some
      --  protocols) or an eof or a closed connection
      elsif Result = 0 then
         Octets_Received := 0;
      --  anything else is an error condition
      else
         Raise_POSIX_Error;
      end if;
   end Receive;

   procedure Receive
      (Socket           : in  POSIX.IO.File_Descriptor;
       Buffer           : in  System.Address;
       Octets_Requested : in  POSIX.IO_Count;
       Octets_Received  : out POSIX.IO_Count;
       From             : in  Socket_Address_Pointer;
       Options          : in  Message_Option_Set
         := Message_Option_Set (POSIX.Empty_Set)) is
      Result : ssize_t;
      Address_Length : aliased size_t;
      use Ada_Streams;
   begin
      if Octets_Requested <= 0 then
         Octets_Received := 0;
         return;
      end if;
      if From /= Null_Socket_Address then
         Address_Length := From.Length;
      end if;
      Result := c_recvfrom (s       => int (Socket),
        buf     => Buffer,
        len     => size_t (Octets_Requested),
        flags   => To_int (Option_Set (Options).Option),
        from    => To_sockaddr_var_ptr (From.C'Address),
        fromlen => Address_Length'Unchecked_Access);
      --  a positive result is the length of the data received
      if Result > 0 then
         Octets_Received := POSIX.IO_Count (Result);
      --  a zero result is a zero length record (possible for some
      --  protocols) or an eof or a closed connection
      elsif Result = 0 then
         Octets_Received := 0;
      --  anything else is an error condition
      else
         Raise_POSIX_Error;
      end if;
   end Receive;

   -----------------------
   --  Receive_Message  --
   -----------------------

   function c_recvmsg
     (s : int; msg : msghdr_ptr; flags : int) return ssize_t;
   pragma Import (C, c_recvmsg, recvmsg_LINKNAME);

   procedure Receive_Message
      (Socket          : in     POSIX.IO.File_Descriptor;
       Message         : in out Socket_Message;
       Octets_Received : out    POSIX.IO_Count;
       Masked_Signals  : in     POSIX.Signal_Masking;
       Options         : in     Message_Option_Set
         := Message_Option_Set (POSIX.Empty_Set)) is
      Result : ssize_t;
      Old_Mask : aliased Signal_Mask;
   begin
      Mask_Signals (Masked_Signals, Old_Mask'Unchecked_Access);
      Result := c_recvmsg
                   (s     => int (Socket),
                    msg   => Message.C'Unchecked_Access,
                    flags => To_int (Option_Set (Options).Option));
      Restore_Signals (Masked_Signals, Old_Mask'Unchecked_Access);
      if Result >= 0 then
         Octets_Received := POSIX.IO_Count (Result);
      else
         Raise_POSIX_Error;
      end if;
   end Receive_Message;

   procedure Receive_Message
      (Socket          : in     POSIX.IO.File_Descriptor;
       Message         : in out Socket_Message;
       Octets_Received : out    POSIX.IO_Count;
       Options         : in     Message_Option_Set
         := Message_Option_Set (POSIX.Empty_Set)) is
      Result : ssize_t;
   begin
      Result := c_recvmsg
                   (s     => int (Socket),
                    msg   => Message.C'Unchecked_Access,
                    flags => To_int (Option_Set (Options).Option));
      if Result >= 0 then
         Octets_Received := POSIX.IO_Count (Result);
      else
         Raise_POSIX_Error;
      end if;
   end Receive_Message;

   ------------
   --  Send  --
   ------------

   function c_send
     (s     : int;
      buf   : System.Address;
      len   : size_t;
      flags : int)
     return ssize_t;
   pragma Import (C, c_send, send_LINKNAME);

   procedure Send
      (Socket         : in  POSIX.IO.File_Descriptor;
       Buffer         : in  System.Address;
       Octets_To_Send : in  POSIX.IO_Count;
       Octets_Sent    : out POSIX.IO_Count;
       Masked_Signals : in  POSIX.Signal_Masking;
       Options        : in  Message_Option_Set
         := Message_Option_Set (POSIX.Empty_Set)) is
      Result : ssize_t;
      Old_Mask : aliased Signal_Mask;
      use Ada_Streams;
   begin
      if Octets_To_Send <= 0 then
         Octets_Sent := 0;
         return;
      end if;
      Mask_Signals (Masked_Signals, Old_Mask'Unchecked_Access);
      Result := c_send (s     => int (Socket),
                        buf   => Buffer,
                        len   => size_t (Octets_To_Send),
                        flags => To_int (Option_Set (Options).Option));
      Restore_Signals (Masked_Signals, Old_Mask'Unchecked_Access);
      if Result >= 0 then
         Octets_Sent := POSIX.IO_Count (Result);
      else Raise_POSIX_Error;
      end if;
   end Send;

   procedure Send
      (Socket         : in  POSIX.IO.File_Descriptor;
       Buffer         : in  System.Address;
       Octets_To_Send : in  POSIX.IO_Count;
       Octets_Sent    : out POSIX.IO_Count;
       Options        : in  Message_Option_Set
         := Message_Option_Set (POSIX.Empty_Set)) is
      Result : ssize_t;
      use Ada_Streams;
   begin
      if Octets_To_Send <= 0 then
         Octets_Sent := 0;
         return;
      end if;
      Result := c_send (s     => int (Socket),
                        buf   => Buffer,
                        len   => size_t (Octets_To_Send),
                        flags => To_int (Option_Set (Options).Option));
      if Result >= 0 then
         Octets_Sent := POSIX.IO_Count (Result);
      else Raise_POSIX_Error;
      end if;
   end Send;

   -----------------
   --  Send <To>  --
   -----------------

   function c_sendto
     (s     : int;
      buf   : System.Address;
      len   : size_t;
      flags : int;
      to    : sockaddr_ptr;
      tolen : size_t) return ssize_t;
   pragma Import (C, c_sendto, sendto_LINKNAME);

   procedure Send
      (Socket         : in  POSIX.IO.File_Descriptor;
       Buffer         : in  System.Address;
       Octets_To_Send : in  POSIX.IO_Count;
       Octets_Sent    : out POSIX.IO_Count;
       To             : in  Socket_Address_Pointer;
       Masked_Signals : in  POSIX.Signal_Masking;
       Options        : in  Message_Option_Set
         := Message_Option_Set (POSIX.Empty_Set)) is
      Result : ssize_t;
      Old_Mask : aliased Signal_Mask;
      use Ada_Streams;
   begin
      if Octets_To_Send <= 0 then
         Octets_Sent := 0;
         return;
      end if;
      Mask_Signals (Masked_Signals, Old_Mask'Unchecked_Access);
      Result := c_sendto (s     => int (Socket),
                          buf   => Buffer,
                          len   => size_t (Octets_To_Send),
                          flags => To_int (Option_Set (Options).Option),
                          to    => To_sockaddr_ptr (To.C'Address),
                          tolen => To.Length);
      Restore_Signals (Masked_Signals, Old_Mask'Unchecked_Access);
      if Result >= 0 then
         Octets_Sent := POSIX.IO_Count (Result);
      else
         Raise_POSIX_Error;
      end if;
   end Send;

   procedure Send
      (Socket         : in  POSIX.IO.File_Descriptor;
       Buffer         : in  System.Address;
       Octets_To_Send : in  POSIX.IO_Count;
       Octets_Sent    : out POSIX.IO_Count;
       To             : in  Socket_Address_Pointer;
       Options        : in  Message_Option_Set
         := Message_Option_Set (POSIX.Empty_Set)) is
      Result : ssize_t;
      use Ada_Streams;
   begin
      if Octets_To_Send <= 0 then
         Octets_Sent := 0;
         return;
      end if;
      Result := c_sendto (s     => int (Socket),
                          buf   => Buffer,
                          len   => size_t (Octets_To_Send),
                          flags => To_int (Option_Set (Options).Option),
                          to    => To_sockaddr_ptr (To.C'Address),
                          tolen => To.Length);
      if Result >= 0 then
         Octets_Sent := POSIX.IO_Count (Result);
      else
         Raise_POSIX_Error;
      end if;
   end Send;

   --------------------
   --  Send_Message  --
   --------------------

   function c_sendmsg
     (s : int; msg : msghdr_ptr; flags : int) return ssize_t;
   pragma Import (C, c_sendmsg, sendmsg_LINKNAME);

   procedure Send_Message
      (Socket         : in  POSIX.IO.File_Descriptor;
       Message        : in  Socket_Message;
       Octets_Sent    : out POSIX.IO_Count;
       Masked_Signals : in  POSIX.Signal_Masking;
       Options        : in  Message_Option_Set
         := Message_Option_Set (POSIX.Empty_Set)) is
      Result : ssize_t;
      Old_Mask : aliased Signal_Mask;
   begin
      Mask_Signals (Masked_Signals, Old_Mask'Unchecked_Access);
      Result := c_sendmsg
                   (s     => int (Socket),
                    msg   => Message.C'Unchecked_Access,
                    flags => To_int (Option_Set (Options).Option));
      Restore_Signals (Masked_Signals, Old_Mask'Unchecked_Access);
      if Result >= 0 then
         Octets_Sent := POSIX.IO_Count (Result);
      else
         Raise_POSIX_Error;
      end if;
   end Send_Message;

   procedure Send_Message
      (Socket      : in  POSIX.IO.File_Descriptor;
       Message     : in  Socket_Message;
       Octets_Sent : out POSIX.IO_Count;
       Options     : in  Message_Option_Set
         := Message_Option_Set (POSIX.Empty_Set)) is
      Result : ssize_t;
   begin
      Result := c_sendmsg
                   (s     => int (Socket),
                    msg   => Message.C'Unchecked_Access,
                    flags => To_int (Option_Set (Options).Option));
      if Result >= 0 then
         Octets_Sent := POSIX.IO_Count (Result);
      else
         Raise_POSIX_Error;
      end if;
   end Send_Message;

   -----------------------
   --  Shutdown_Socket  --
   -----------------------

   function c_shutdown (s : int; how : int) return int;
   pragma Import (C, c_shutdown, shutdown_LINKNAME);

   procedure Shutdown
      (Socket : in POSIX.IO.File_Descriptor;
       Mode   : in Shutdown_Mode) is
   begin
      case Mode is
         when Further_Receives_Disallowed =>
            Check (c_shutdown (int (Socket), SHUT_RD));
         when Further_Sends_Disallowed =>
            Check (c_shutdown (int (Socket), SHUT_WR));
         when Further_Sends_And_Receives_Disallowed =>
            Check (c_shutdown (int (Socket), SHUT_RDWR));
      end case;
   end Shutdown;

   -----------------------------
   --  Socket_Is_at_OOB_Mark  --
   -----------------------------

   function c_sockatmark (s : int) return int;
   pragma Import (C, c_sockatmark, sockatmark_LINKNAME);

   function Socket_Is_At_OOB_Mark (Socket : POSIX.IO.File_Descriptor)
      return Boolean is
      Result : int;
   begin
      Result := c_sockatmark (int (Socket));
      case Result is
         when 0 =>
            return False;
         when 1 =>
            return True;
         when others =>
            Raise_POSIX_Error;
            return False;
      end case;
   end Socket_Is_At_OOB_Mark;

end POSIX.Sockets;

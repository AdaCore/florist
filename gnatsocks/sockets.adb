------------------------------------------------------------------------------
--                                                                          --
--                                 GNATSOCKS                                --
--                                                                          --
--                               S o c k e t s                              --
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

with Ada.Text_IO,
     Ada.Streams,
     POSIX,
     POSIX.C,
     POSIX.Implementation,
     System,
     Unchecked_Conversion;
package body Sockets is

   use Ada.Text_IO,
       POSIX,
       POSIX.C,
       POSIX.C.Sockets,
       POSIX.Implementation;

   function close (fildes : int) return int;
   pragma Import (C, close, close_LINKNAME);

   procedure Close (Sock : in out Socket'Class) is
   begin
      if Sock.fd /= 0 then Check (close (Sock.fd)); end if;
      Sock.fd := 0;
   end Close;

   procedure Finalize (Sock : in out Socket) is
   begin
      Close (Sock);
   end Finalize;

   --------------------------------------

   function connect
     (socket_fd : int;
      name : sockaddr_ptr;
      namelen : int)
     return int;
      pragma Import (C, connect, connect_LINKNAME);
   --  attempts to make a connection to a named socket.

   function make_socket
     (domain : int;
      socket_type : int;
      protocol : int)
     return int;
      pragma Import (C, make_socket, socket_LINKNAME);

   procedure Open
     (Sock : in out Stream_Socket;
      Addr : Socket_Address'Class) is
   begin
      if not Valid (Addr) then raise Constraint_Error; end if;
      if Sock.fd /= 0 then raise Constraint_Error; end if;
      Sock.fd := Check (make_socket (PF_INET, SOCK_STREAM, 0));
      Check (connect (Sock.fd, Address (Addr), Length (Addr)));
      Sock.in_stream.sock := Sock'Unchecked_Access;
      Sock.in_ptr := Sock.in_stream'Unchecked_Access;
      Sock.out_stream.sock := Sock'Unchecked_Access;
      Sock.out_ptr := Sock.out_stream'Unchecked_Access;
      Sock.tag := Addr'Tag;
   end Open;

   function Get_Input_Stream (Sock : Stream_Socket) return Input_Stream_Ptr is
   begin
      if Sock.fd = 0 then raise Constraint_Error; end if;
      return Sock.in_ptr;
   end Get_Input_Stream;

   function Get_Output_Stream (Sock : Stream_Socket) return Output_Stream_Ptr is
   begin
      if Sock.fd = 0 then raise Constraint_Error; end if;
      return Sock.out_ptr;
   end Get_Output_Stream;

   -----------------------------------------

   function listen
     (socket_fd : int;
      backlog : int)
     return int;
      pragma Import (C, listen, listen_LINKNAME);
   --  specifies that we want to listen on the argument socket ID.
   --  backlog is the number of backlogged connections that are allowed.
   --  To accept connections, a socket is first crated with socked(),
   --  a backlog for incoming connections is specified with listen(),
   --  and then the connections are accepted with accept().

   function bind
     (socket_fd : int;
      name : sockaddr_ptr;
      namelen : int)
     return int;
      pragma Import (C, bind, bind_LINKNAME);
   --  assigns a name to an unnamed socket.
   --  A socket is first created with socket().
   --  Binding a socket in the UNIX (not Internet) domain
   --  causes creation of a socket in the file system, that must
   --  be deleted later when it is no longer needed, using unlink()

   procedure Open
     (Sock  : in out Server_Socket;
      Addr  : Socket_Address'Class;
      Count : Natural := 0) is
   begin
      if not Valid (Addr) then raise Constraint_Error; end if;
      if Sock.fd /= 0 then raise Constraint_Error; end if;
      Sock.fd := Check (make_socket (Protocol_Family (Addr), SOCK_STREAM, 0));
      Check (bind (Sock.fd, Address (Addr), Length (Addr)));
      Check (listen (Sock.fd, int (Count)));
      Sock.tag := Addr'Tag;
   end Open;

   function accept_connection
     (socket_fd : int;
      addr : sockaddr_ptr;
      addrlen : access int)
     return int;
      pragma Import (C, accept_connection, accept_LINKNAME);
   --  returns a socket ID for a new connection that has been
   --  requested via the socket whose ID is given as argument.
   --  The argument must be an existing socket that is bound to an address
   --  and is listeninf for connections.
   --  addr received the address of the connecting entity.

   procedure Accept_Connection
     (Server : Server_Socket;
      Stream : in out Stream_Socket'Class;
      Peer   : in out Socket_Address'Class) is
      addrlen : aliased int := Length (Peer);
      use type Ada.Tags.Tag;
   begin
      if Server.fd = 0 then raise Constraint_Error; end if;
      if Stream.fd /= 0 then raise Constraint_Error; end if;
      if Server.tag /= Peer'Tag then raise Constraint_Error; end if;
      --  require that the Peer and Server object be of the same type as the
      --  socket address used to create the Server
      Stream.fd := Check (accept_connection (Server.fd,
        Address (Peer), addrlen'Unchecked_Access));
      Stream.in_stream.sock := Stream'Unchecked_Access;
      Stream.in_ptr := Stream.in_stream'Unchecked_Access;
      Stream.out_stream.sock := Stream'Unchecked_Access;
      Stream.out_ptr := Stream.out_stream'Unchecked_Access;
      Stream.tag := Server.tag;
      if addrlen /= Length (Peer) then raise
         Constraint_Error;
      end if;
   end Accept_Connection;

   -----------------------------------------

   procedure Open
     (Sock  : in out Datagram_Socket;
      Addr  : Socket_Address'Class) is
   begin
      if not Valid (Addr) then raise Constraint_Error; end if;
      if Sock.fd /= 0 then raise Constraint_Error; end if;
      Sock.fd := Check (make_socket (Protocol_Family (Addr), SOCK_DGRAM, 0));
      Check (bind (Sock.fd, Address (Addr), Length (Addr)));
      Sock.tag := Addr'Tag;
   end Open;

   -----------------------------------------

   function read
     (fildes : int;
      buf : System.Address;
      nbyte : size_t)
     return ssize_t;
      pragma Import (C, read, read_LINKNAME);

   function write
     (fildes : int;
      buf : char_ptr;
      nbyte : size_t)
     return ssize_t;
      pragma Import (C, write, write_LINKNAME);

   procedure Read
      (Stream : in out Input_Stream;
       Item   : out Ada.Streams.Stream_Element_Array;
       Last   : out Ada.Streams.Stream_Element_Offset) is
      use type Ada.Streams.Stream_Element_Offset;
      Tmp : ssize_t;
      function "+" is new
        Unchecked_Conversion (System.Address, char_ptr);
   begin
      Tmp := read (Stream.sock.fd, Item'Address, size_t (Item'Length));
      Check (int (Tmp));
      Last := Item'First + Ada.Streams.Stream_Element_Offset (Tmp);
   end Read;

   procedure Write
      (Stream : in out Input_Stream;
       Item   : in Ada.Streams.Stream_Element_Array) is
   begin
      Raise_POSIX_Error (Operation_Not_Implemented);
   end Write;

   procedure Read
      (Stream : in out Output_Stream;
       Item   : out Ada.Streams.Stream_Element_Array;
       Last   : out Ada.Streams.Stream_Element_Offset) is
   begin
      Raise_POSIX_Error (Operation_Not_Implemented);
   end Read;

   procedure Write
      (Stream : in out Output_Stream;
       Item   : in Ada.Streams.Stream_Element_Array) is
      function "+" is new
        Unchecked_Conversion (System.Address, char_ptr);
   begin
      Check (int (write (Stream.sock.fd, +Item'Address,
        size_t (Item'Length))));
   end Write;

   ----------------------------------------------------

   function sendto
     (socket_fd : int;
      buf : char_ptr;
      len : int;
      flags : int;
      from : sockaddr_ptr;
      fromlen : int_ptr)
     return int;
      pragma Import (C, sendto, sendto_LINKNAME);

   procedure Send
     (Sock : in Datagram_Socket;
      Addr : in Socket_Address'Class;
      Data : in String) is
      Data_With_NUL : POSIX_String;
   begin
      if not Valid (Addr) then raise Constraint_Error; end if;
      if Sock.fd /= 0 then raise Constraint_Error; end if;
      if Sock.tag /= Addr'Tag then raise Constraint_Error; end if;
      Check (sendto (socket_fd => Sock.fd,
        buf => +Data (Data'First)'Address,
        len => Data'Length,
        flags => 0,
        from => Address (Addr),
        fromlen => Length (Addr)));
   end Send;

   function recvfrom
     (socket_fd : int;
      buf : char_ptr;
      len : int;
      flags : int;
      from : sockaddr_ptr; --  may be null
      fromlen : int_ptr)
     return int;
      pragma Import (C, recvfrom, recvfrom_LINKNAME);

   procedure Receive
     (Sock : in Datagram_Socket;
      Addr : out Socket_Address'Class;
      Buff : in out String;
      Last : out Natural) is
   begin
      if not Valid (Addr) then raise Constraint_Error; end if;
      if Sock.fd /= 0 then raise Constraint_Error; end if;
      if Sock.tag /= Addr'Tag then raise Constraint_Error; end if;
      Check (recvfrom (socket_fd => Sock.fd,
        buf => +Buff (Buff'First)'Address,
        len => Buff'Length,
        flags => 0,
        from => Address (Buff),
        fromlen => Length (Addr)));
   end Receive;

   ----------------------------------------------------


--  Stuff that may be bound-to in future extensions?

--     function getsockopt
--       (socket_fd : int;
--        level : int;
--        optname : int;
--        optval : char_ptr;
--        optlen : int_ptr)
--       return int;
--        pragma Import (C, getsockopt, getsockopt_LINKNAME);
--     --  get options associated with a socket

--     function setsockopt
--       (socket_fd : int;
--        level   : int;
--        optname : int;
--        optval  : char_ptr;
--        optlen  : int)
--       return int;
--        pragma Import (C, setsockopt, setsockopt_LINKNAME);
--     --  set options associated with a socket

--     function recv
--       (socket_fd : int;
--        buf : char_ptr;
--        len : int;
--        flags : int)
--       return int;
--        pragma Import (C, recv, recv_LINKNAME);
--     --  requires socket must be connected

--     function send
--       (socket_fd : int;
--        buf : char_ptr;
--        len : int;
--        flags : int)
--       return int;
--        pragma Import (C, send, send_LINKNAME);
--     --  requires socket must be connected

--     function sendmsg
--       (socket_fd : int;
--        msg : struct_msghdr_ptr;
--        flags : int)
--       return int;
--        pragma Import (C, recvmsg, recvmsg_LINKNAME);

--     function recvmsg
--       (socket_fd : int;
--        msg : struct_msghdr_ptr;
--        flags : int)
--       return int;
--        pragma Import (C, recvmsg, recvmsg_LINKNAME);

--  other functions we may need?
--    ioctl
--    fcntl
--    socketpair
--    shutdown

end Sockets;



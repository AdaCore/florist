------------------------------------------------------------------------------
--                                                                          --
--                                 GNATSOCKS                                --
--                                                                          --
--                               S o c k e t s                              --
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

--  This package is intended to provide convenient access to socket I/O.

--  It still needs design, documentation, and implementation work
--  to specify what happens if an error occurs,
--  for each of the operations.

with Ada.Finalization,
     Ada.Streams,
     Ada.Tags,
     POSIX.C,
     POSIX.C.Sockets;
package Sockets is

   --------------------------------------

   type Socket_Address is abstract tagged private;

   --------------------------------------

   type Socket is abstract tagged limited private;

   procedure Close (Obj : in out Socket'Class);
   --  closes the socket

   function Get_Address
     (Obj : in Socket'Class) return Socket_Address is abstract;
   --  gets address of socket

   --------------------------------------

   type Input_Stream is new Ada.Streams.Root_Stream_Type with private;
   type Input_Stream_Ptr is access all Input_Stream;
   type Output_Stream is new Ada.Streams.Root_Stream_Type with private;
   type Output_Stream_Ptr is access all Output_Stream;

   --------------------------------------

   type Stream_Socket is new Socket with private;

   procedure Open
     (Sock : in out Stream_Socket;
      Addr : Socket_Address'Class);
   --  creates a stream socket and connects it to the specified address

   function Get_Input_Stream (Obj : Stream_Socket) return Input_Stream_Ptr;

   function Get_Output_Stream (Obj : Stream_Socket) return Output_Stream_Ptr;

   --------------------------------------

   type Server_Socket is new Socket with private;

   procedure Open
     (Sock  : in out Server_Socket;
      Addr  : Socket_Address'Class;
      Count : Natural := 0);
   --  creates a server socket on the specified port
   --  with the specified backlog count
   
   procedure Accept_Connection
     (Server : Server_Socket;
      Stream : in out Stream_Socket'Class;
      Peer   : in out Socket_Address'Class);
   --  accept connection addressed to Server socket, and
   --  open Stream socket to handle the connection.

   --------------------------------------

   type Datagram_Socket is new Socket with private;

   procedure Open
     (Sock  : in out Datagram_Socket;
      Addr  : Socket_Address'Class);
   --  creates a datagram socket on the specified port
   --  with the specified local address

--  This part is "in progress".  We will add support for datagram sockets,
--  including send and receive operations.
--  Issues to address include whether to use type to distinguish connected
--  from non-connected datagram sockets (so as to tell whether to use send/recv
--  or sendto/recfrom), and whether to allow bind for sending.
--  Note that INADDR_ANY can be used for bind, to let system choose
--  the local IP address, for bind
   
private

   function Address
     (Obj : Socket_Address)
     return POSIX.C.Sockets.struct_sockaddr_ptr is abstract;

   function Length (Obj : Socket_Address) return Interfaces.C.int is abstract;

   function Valid (Obj : Socket_Address) return Boolean is abstract;

   function Protocol_Family
     (Obj : Socket_Address) return Interfaces.C.int is abstract;

   type Socket_Address is abstract tagged null record;

   -----------------------------------

   type Socket is new Ada.Finalization.Limited_Controlled with record
      fd  : Interfaces.C.int := 0;
      --  file descriptor of an open socket, of nonzero
      tag : Ada.Tags.Tag;
      --  tag of the socket address type used to open the socket, of fd /= 0
      --  This is used to check that other operations use only addresses
      --  that are in the same family.
   end record;

   procedure Finalize (Obj : in out Socket);

   -----------------------------------

   type Stream_Socket_Ptr is access all Stream_Socket;

   type Input_Stream is new Ada.Streams.Root_Stream_Type with record
       sock : Stream_Socket_Ptr;
   end record;

   type Output_Stream is new Ada.Streams.Root_Stream_Type with record
       sock : Stream_Socket_Ptr;
   end record;

   type Stream_Socket is new Socket with record
      in_stream  : aliased Input_Stream;
      out_stream : aliased Output_Stream;
      in_ptr : Input_Stream_Ptr;
      --  points to in_stream
      out_ptr : Output_Stream_Ptr;
      --  points to out_stream
      --  These pointers are needed to implement
      --  functions Get_InputStream and Get_OutputStream.
   end record;

   procedure Read
      (Stream : in out Input_Stream;
       Item   : out Ada.Streams.Stream_Element_Array;
       Last   : out Ada.Streams.Stream_Element_Offset);

   procedure Write
      (Stream : in out Input_Stream;
       Item   : in Ada.Streams.Stream_Element_Array);

   procedure Read
      (Stream : in out Output_Stream;
       Item   : out Ada.Streams.Stream_Element_Array;
       Last   : out Ada.Streams.Stream_Element_Offset);
   procedure Write
      (Stream : in out Output_Stream;
       Item   : in Ada.Streams.Stream_Element_Array);

   -----------------------------------

   type Server_Socket is new Socket with null record;

   -----------------------------------

   type Datagram_Socket is new Socket with null record;


end Sockets;

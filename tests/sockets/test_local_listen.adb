with POSIX; use POSIX;
with POSIX.IO; use POSIX.IO;
with POSIX.Files; use POSIX.Files;
with POSIX.Sockets; use POSIX.Sockets;
with POSIX.Sockets.Local; use POSIX.Sockets.Local;
with test_pkg; use test_pkg;
with Gnat.IO; use Gnat.IO;
with Ada.Streams; use Ada.Streams;

Procedure Test_Local_Listen is
   Listening_Socket: File_Descriptor;
   Accepting_Socket: File_Descriptor;
   Socket_Name:      Local_Socket_Address;
   Test_Name:        Local_Socket_Address;
   Socket_Path:      Pathname:="a_local_socket";
   Buffer:           string (1 .. 80);
   Last:             POSIX.IO_Count;
begin

   ---------------------------------------------------------------
   -- Listen on a local (UNIX) connection-mode (Stream) socket  --
   ---------------------------------------------------------------

   Test ("Listen on local socket");

   Comment ("Create file descriptor for a local stream socket");
   Listening_Socket := Create (Local_Protocol, Stream_Socket);

   Comment ("Put a pathname into a local socket address and read it back");
   Set_Socket_Path (Socket_Name, Socket_Path);
   Comment ("socket path:" & To_String(Get_Socket_Path (Socket_Name)));

   Comment ("Bind the address to the file descriptor");
   Bind (Listening_Socket, Socket_Name);

   Comment ("Listen for connections on the socket");
   Listen (Listening_Socket, 1);

   Comment ("Accept (wait) for connection...");
   Accepting_Socket := Accept_Connection (Listening_Socket);

   Comment ("Get accepting socket name");
   Test_Name := Get_Socket_Name (Accepting_Socket);
   Comment ("   listening socket path:" & To_String(Get_Socket_Path (Test_Name)));

   Comment ("Get talking (peer) socket name");
   Test_Name := Get_Peer_Name (Accepting_Socket);
   Comment ("   talking socket path:" & To_String(Get_Socket_Path (Test_Name)));

   Comment ("Receive data from the connection until it stops sending");
   loop
      Receive (Accepting_Socket, Buffer (Buffer'First)'Address, 
	       POSIX.IO_Count (Buffer'Length), Last);
      exit when Last = 0;
      Comment ("Received:" & Buffer(1..integer(Last)));
   end loop;

   Comment ("remove the socket from the file system");
   unlink (Socket_Path);

   Done;

   exception when E : others => Fail (E);

end Test_Local_Listen;

with POSIX; use POSIX;
with POSIX.IO; use POSIX.IO;
with POSIX.Files; use POSIX.Files;
with POSIX.Sockets; use POSIX.Sockets;
with POSIX.Sockets.Local; use POSIX.Sockets.Local;
with test_pkg; use test_pkg;
with Gnat.IO; use Gnat.IO;
with Ada.Streams; use Ada.Streams;
Procedure Test_Local_Talk is
   Talking_Socket:   File_Descriptor;
   Socket_Name:      Local_Socket_Address;
   Test_Name:        Local_Socket_Address;
   Socket_Path:      Pathname:="a_local_socket";
   Last:             POSIX.IO_Count;
   Message :         string (1 .. 11) := "Hello World";

begin

   -------------------------------------------------------------
   -- Talk on a local (UNIX) connection-mode (Stream) socket  --
   -------------------------------------------------------------

   Test ("Talk on local socket");

   Comment ("Create file descriptor for a local stream socket");
   Talking_Socket := Create (Local_Protocol, Stream_Socket);

   Comment ("Put a pathname into a local socket address and read it back");
   Set_Socket_Path (Socket_Name, Socket_Path);
   Comment ("socket path:" & To_String(Get_Socket_Path (Socket_Name)));

   Comment ("Connect to the socket (which should be listening)");
   Connect (Talking_Socket, Socket_Name);

   Comment ("Get connected socket name");
   Test_Name := Get_Socket_Name (Talking_Socket);
   Comment ("   listening socket path:" & To_String(Get_Socket_Path (Test_Name)));

   Comment ("Get listening (peer) socket name");
   Test_Name := Get_Peer_Name (Talking_Socket);
   Comment ("   talking socket path:" & To_String(Get_Socket_Path (Test_Name)));

   Comment ("Send some data to the connection");
   Send (Talking_Socket, Message'Address, Message'Length, Last);

   Comment ("Close the connection");
   close (Talking_Socket);

   Done;

   exception when E : others => Fail (E);

end Test_Local_Talk;

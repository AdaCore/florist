with POSIX; use POSIX;
with POSIX.IO; use POSIX.IO;
with POSIX.Files; use POSIX.Files;
with POSIX.Sockets; use POSIX.Sockets;
with POSIX.Sockets.Local; use POSIX.Sockets.Local;
with test_pkg; use test_pkg;
with Gnat.IO; use Gnat.IO;
with Ada.Streams; use Ada.Streams;

Procedure Test_Local_Ltime is
   Listening_Socket: File_Descriptor;
   Accepting_Socket: File_Descriptor;
   Socket_Name:      Local_Socket_Address;
   Socket_Path:      Pathname:="a_local_socket";
   Buffer:           string (1 .. 80);
   Last:             POSIX.IO_Count;
   Count:            Integer:=0;
begin

   ----------------------------------------------------------
   -- Time a local (UNIX) connection-mode (Stream) socket  --
   ----------------------------------------------------------

   Listening_Socket := Create (Local_Protocol, Stream_Socket);
   Set_Socket_Path (Socket_Name, Socket_Path);
   Bind (Listening_Socket, Socket_Name);
   Listen (Listening_Socket, 1);
   Put_Line ("Waiting for Data...");
   Accepting_Socket := Accept_Connection (Listening_Socket);
   loop
      Receive (Accepting_Socket, Buffer (Buffer'First)'Address, 
	       Buffer'Length, Last);
      Count := Count + Integer(Last);
      exit when Last = 0;
   end loop;
   unlink (Socket_Path);
   Put ("...Received ");
   Put (Count);
   Put_Line (" characters");
   Done;

   exception when E : others => Fail (E);

end Test_Local_Ltime;

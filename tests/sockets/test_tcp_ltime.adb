with POSIX; use POSIX;
with POSIX.IO; use POSIX.IO;
with POSIX.Files; use POSIX.Files;
with POSIX.Sockets; use POSIX.Sockets;
with POSIX.Sockets.Internet; use POSIX.Sockets.Internet;
with test_pkg; use test_pkg;
with Gnat.IO; use Gnat.IO;
with Ada.Streams; use Ada.Streams;

Procedure Test_TCP_Ltime is
   Listening_Socket: File_Descriptor;
   Accepting_Socket: File_Descriptor;
   Socket_Name:      Internet_Socket_Address;
   Buffer:           string (1 .. 80);
   Last:             POSIX.IO_Count;
   Count:            Integer:=0;
begin
   --------------------------
   -- Time a TCP/IP socket --
   --------------------------
   Listening_Socket := Create (Internet_Protocol, Stream_Socket);
   Set_Internet_Address (Socket_Name, Unspecified_Internet_Address);
   Set_Internet_Port (Socket_Name, 1234);
   Bind (Listening_Socket, Socket_Name);
   Listen (Listening_Socket, 1);
   Put_Line ("Waiting for Data on port 1234...");
   Accepting_Socket := Accept_Connection (Listening_Socket);
   loop
      Receive (Accepting_Socket, Buffer (Buffer'First)'Address,
	       Buffer'Length, Last);
      Count := Count + Integer(Last);
      exit when Last = 0;
      Comment ("Received:" & Buffer(1..integer(Last)));
   end loop;
   Put ("...Received ");
   Put (Count);
   Put_Line (" characters"); 
   Done;
   exception when E : others => Fail (E);
end Test_TCP_Ltime;

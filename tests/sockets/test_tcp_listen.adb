with POSIX; use POSIX;
with POSIX.IO; use POSIX.IO;
with POSIX.Files; use POSIX.Files;
with POSIX.Sockets; use POSIX.Sockets;
with POSIX.Sockets.Internet; use POSIX.Sockets.Internet;
with Test_Pkg; use Test_Pkg;
with GNAT.IO; use GNAT.IO;
with Ada.Streams; use Ada.Streams;

procedure Test_TCP_Listen is
   Listening_Socket : File_Descriptor;
   Accepting_Socket : File_Descriptor;
   Socket_Name      : Internet_Socket_Address;
   Test_Name        : Internet_Socket_Address;
   Test_Address     : Internet_Address;
   Buffer           : string (1 .. 80);
   Last             : POSIX.IO_Count;
begin

   --------------------------------
   -- Listen on a TCP/IP socket  --
   --------------------------------

   Test ("Listen on TCP/IP socket");

   Comment ("Create file descriptor for a TCP/IP socket");
   Listening_Socket := Create (Internet_Protocol, Stream_Socket);

   Comment ("Specify any address for the Internet address");
   Set_Internet_Address (Socket_Name, Unspecified_Internet_Address);

   Comment ("Specify any port (0) for the Internet address");
   Set_Internet_Port (Socket_Name, 16#FF00#);

   Comment ("Bind the address to the file descriptor");
   Bind (Listening_Socket, Socket_Name);

   Comment ("Get the Port Number bound to the address");
   Socket_Name := Get_Socket_Name (Listening_Socket);
   Put ("  ===>Test_TCP_Listen: Listening on port number (");
   Put (integer (Get_Internet_Port (Socket_Name)));
   Put_Line (")");

   Comment ("Listen for connections on the socket");
   Listen (Listening_Socket, 1);

   Comment ("Accept (wait) for connection...");
   Accepting_Socket := Accept_Connection (Listening_Socket);

   Comment ("Get accepting socket name");
   Test_Name := Get_Socket_Name (Accepting_Socket);
   Comment ("Extract the Internet address");
   Test_Address := Get_Internet_Address (Test_Name);
   Put ("  ===>Test_TCP_Listen: Accepting on (");
   declare
      Dot_Address : constant POSIX_String :=
         Internet_Address_To_String (Test_Address);
   begin
      Put (To_String (Dot_Address));
   end;
   Put (") port (");
   Put (integer (Get_Internet_Port (Test_Name)));
   Put_Line (")");

   Comment ("Get talking (peer) socket name");
   Test_Name := Get_Peer_Name (Accepting_Socket);
   Comment ("Extract the Internet address");
   Test_Address := Get_Internet_Address (Test_Name);
   Put ("  ===>Test_TCP_Listen: Accepted from (");
   declare
      Dot_Address : constant POSIX_String :=
         Internet_Address_To_String (Test_Address);
   begin
      Put (To_String (Dot_Address));
   end;
   Put (") port (");
   Put (integer (Get_Internet_Port (Test_Name)));
   Put_Line (")");

   Comment ("Receive data from the connection until it stops sending");
   loop
      Receive (Accepting_Socket, Buffer (Buffer'First)'Address, 
	       POSIX.IO_Count (Buffer'Length), Last);
      exit when Last = 0;
      Comment ("Received:" &
               Buffer (1 .. integer (Last)));
   end loop;

   Done;

   exception when E : others => Fail (E);

end Test_TCP_Listen;

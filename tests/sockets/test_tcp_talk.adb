with POSIX; use POSIX;
with POSIX.IO; use POSIX.IO;
with POSIX.Files; use POSIX.Files;
with POSIX.Sockets; use POSIX.Sockets;
with POSIX.Sockets.Internet; use POSIX.Sockets.Internet;
with test_pkg; use test_pkg;
with Gnat.IO; use Gnat.IO;
with Ada.Streams; use Ada.Streams;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO;
with Text_IO;

Procedure Test_TCP_Talk is
   Talking_Socket:   File_Descriptor;
   Socket_Name:      Internet_Socket_Address;
   Test_Name:        Internet_Socket_Address;
   Test_Address:     Internet_Address;
   Last:             POSIX.IO_Count;
   Arg_Host:         Positive;
   Arg_Port:         Positive;
   Arg_Last:         Positive;
   Port_Num:         Internet_Port;
   Message :         string (1 .. 11) := "Hello World";
   package Port_IO is new Ada.Text_IO.Modular_IO (Internet_Port);

begin

   ------------------------------
   -- Talk on a TCP/IP socket  --
   ------------------------------

   Test ("Talk on TCP/IP socket");

   Comment ("Create file descriptor for a TCP/IP socket");
   Talking_Socket := Create (Internet_Protocol, Stream_Socket);

   Comment ("Specify the address and port from command line");
   -- Arguments 1 and 2 (or 2 and 3 if "-v" was entered)
   if Verbose then
      Arg_Host := 2;
      Arg_Port := 3;
   else
      Arg_Host := 1;
      Arg_Port := 2;
   end if;

   if not Is_Internet_Address (To_POSIX_String (Argument (Arg_Host))) then
      Text_IO.Put_Line ("Bad Internet Address");
   end if;
      
   Test_Address := String_To_Internet_Address ( 
	    To_POSIX_String (Argument (Arg_Host)));

   Set_Internet_Address (
	 Name => Socket_Name,
	 Address_Value => Test_Address );
   Text_IO.Put ("  Remote Host: ");
   Text_IO.Put (Argument (Arg_Host));
   Port_IO.Get (Argument (Arg_Port), Port_Num, Arg_Last);
   Text_IO.Put ("  Port #");
   Port_IO.Put (Port_Num);
   Text_IO.Put (" IP ");
--   Text_IO.Put (To_String (Internet_Address_To_String (Test_Address)));
   Text_IO.Put (To_String (Internet_Address_To_String 
			   (Get_Internet_Address (Socket_Name))));
   Text_IO.New_Line;
   Set_Internet_Port (Socket_Name, Port_Num);

   Comment ("Connect to the socket (which should be listening)");
   Connect (Talking_Socket, Socket_Name);

   Comment ("Get connected socket name");
   Test_Name := Get_Socket_Name (Talking_Socket);
   Comment ("Extract the Internet address");
   Test_Address := Get_Internet_Address (Test_Name);
   Put ("  ===>Test_TCP_Talk: Connecting on (");
   declare
      Dot_Address: constant POSIX_String := 
         Internet_Address_To_String (Test_Address);
   begin
      Put (To_String (Dot_Address));
   end;
   Put (") port (");
   Put (integer(Get_Internet_Port (Test_Name)));
   Put_Line (")");

   Comment ("Get listening (peer) socket name");
   Test_Name := Get_Peer_Name (Talking_Socket);
   Comment ("Extract the Internet address");
   Test_Address := Get_Internet_Address (Test_Name);
   Put ("  ===>Test_TCP_Talk: Connecting from (");
   declare
      Dot_Address: constant POSIX_String := 
         Internet_Address_To_String (Test_Address);
   begin
      Put (To_String (Dot_Address));
   end;
   Put (") port (");
   Put (integer(Get_Internet_Port (Test_Name)));
   Put_Line (")");

   Comment ("Send some data to the connection");
   Send (Talking_Socket, Message'Address,
	 Message'Length, Last);
   Comment ("Close the connection");
   close (Talking_Socket);

   Done;

   exception when E : others => Fail (E);

end Test_TCP_Talk;

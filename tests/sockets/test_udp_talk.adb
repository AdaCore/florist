with POSIX; use POSIX;
with POSIX.IO; use POSIX.IO;
with POSIX.Files; use POSIX.Files;
with POSIX.Sockets; use POSIX.Sockets;
with POSIX.Sockets.Internet; use POSIX.Sockets.Internet;
with Test_Pkg; use Test_Pkg;
with GNAT.IO; use GNAT.IO;
with Ada.Streams; use Ada.Streams;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO;

procedure Test_UDP_Talk is
   Talking_Socket : File_Descriptor;
   Peer_Name      : Internet_Socket_Address;
   Last           : POSIX.IO_Count;
   Message        : Socket_Message;
   Buffer1        : Stream_Element_Array (1 .. 30);
   Buffer2        : Stream_Element_Array (1 .. 30);
   Buffer3        : Stream_Element_Array (1 .. 30);
   Buffer4        : Stream_Element_Array (1 .. 30);
   SM_Array      : Socket_Message_Array_Pointer :=
      new Socket_Message_Array (1 .. 3);
begin

   -------------------------------
   --  Talk on a UDP/IP socket  --
   -------------------------------

   Test ("Talk on UDP/IP socket");

   Set_Internet_Address (Peer_Name, Loopback_Internet_Address);
   Set_Internet_Port (Peer_Name, 2000);
   
   Buffer1 := POSIX.To_Stream_Element_Array
      ("This is a UDP/IP socket with  ");
   Buffer2 := POSIX.To_Stream_Element_Array
      ("the peer specified in the     ");
   Buffer3 := POSIX.To_Stream_Element_Array
      ("Connect()                     ");
   Buffer4 := POSIX.To_Stream_Element_Array
      ("@                             ");
   --  Test case 1: Prespecify the peer
   Talking_Socket := Create (Internet_Protocol, Datagram_Socket);
   Connect (Talking_Socket, Peer_Name);
   Send (Talking_Socket, Buffer1 (Buffer1'First)'Address,
			 Buffer1'Length, Last);
   Send (Talking_Socket, Buffer2 (Buffer2'First)'Address,
			 Buffer2'Length, Last);
   Send (Talking_Socket, Buffer3 (Buffer3'First)'Address,
			 Buffer3'Length, Last);
   Send (Talking_Socket, Buffer4 (Buffer4'First)'Address,
			 Buffer4'Length, Last);
   Close (Talking_Socket);

   Buffer1 := POSIX.To_Stream_Element_Array
      ("This is a UDP/IP socket with  ");
   Buffer2 := POSIX.To_Stream_Element_Array
      ("the peer in the Send()        ");
   Buffer3 := POSIX.To_Stream_Element_Array
      ("@                             ");
   --  Test case 2: Specify the peer via Send with the <to> parameter
   Talking_Socket := Create (Internet_Protocol, Datagram_Socket);
   Send (Talking_Socket, Buffer1 (Buffer1'First)'Address,
			 Buffer1'Length, Last, Peer_Name);
   Send (Talking_Socket, Buffer2 (Buffer2'First)'Address,
			 Buffer2'Length, Last, Peer_Name);
   Send (Talking_Socket, Buffer3 (Buffer3'First)'Address,
			 Buffer3'Length, Last, Peer_Name);

   --  Test case 3: Specify the peer via a Socket_Message
   Buffer1 := POSIX.To_Stream_Element_Array
      ("This is a UDP/IP socket with t");
   Buffer2 := POSIX.To_Stream_Element_Array
      ("he peer & buffers specified in");
   Buffer3 := POSIX.To_Stream_Element_Array
      (" a Socket_Message object      ");
   Set_Segment (SM_Array (1), Buffer1 (Buffer1'First)'Address, Buffer1'Length);
   Set_Segment (SM_Array (2), Buffer2 (Buffer2'First)'Address, Buffer2'Length);
   Set_Segment (SM_Array (3), Buffer3 (Buffer3'First)'Address, Buffer3'Length);
   Set_Socket_Name (Message, Peer_Name);
   Set_Socket_Message_Array (Message, SM_Array);
   Send_Message (Talking_Socket, Message, Last);
   Close (Talking_Socket);

   Done;

   exception when E : others => Fail (E);

end Test_UDP_Talk;

with POSIX; use POSIX;
with POSIX.IO; use POSIX.IO;
with POSIX.Files; use POSIX.Files;
with POSIX.Sockets; use POSIX.Sockets;
with POSIX.Sockets.Internet; use POSIX.Sockets.Internet;
with Test_Pkg; use Test_Pkg;
with GNAT.IO; use GNAT.IO;
with Ada.Streams; use Ada.Streams;

procedure Test_UDP_Listen is
   Receiving_Socket : File_Descriptor;
   Socket_Name      : Internet_Socket_Address;
   Callers_Name     : Internet_Socket_Address;
   Buffer           : Stream_Element_Array (1 .. 80);
   Last             : POSIX.IO_Count;
   Message          : Socket_Message;
   Buffer1          : Stream_Element_Array (1 .. 20);
   Buffer2          : Stream_Element_Array (1 .. 20);
   Buffer3          : Stream_Element_Array (1 .. 20);
   Buffer4          : Stream_Element_Array (1 .. 20);
   Buffer5          : Stream_Element_Array (1 .. 20);
   SM_Array         : Socket_Message_Array_Pointer :=
		       new Socket_Message_Array (1 .. 5);
begin

   ----------------------------------
   --  Receive on a UDP/IP socket  --
   ----------------------------------

   Test ("Receive on UDP/IP socket");

   Set_Internet_Address (Socket_Name, Unspecified_Internet_Address);
   Set_Internet_Port (Socket_Name, 2000);

   Receiving_Socket := Create (Internet_Protocol, Datagram_Socket);
   Bind (Receiving_Socket, Socket_Name);

   Comment ("Receive datagrams from bound address until '@' ...");
   loop
      Receive (Receiving_Socket, Buffer(Buffer'First)'Address,
               Buffer'Length, Last);
      exit when Buffer (1) = 64;
      Comment ("Received:" & POSIX.To_String
               (To_POSIX_String (Buffer (1 .. 
			Ada.Streams.Stream_Element_Offset (Last)))));
   end loop;

   Comment ("Receive datagrams (and the caller's address) until '@' ...");
   loop
      Receive (Receiving_Socket, Buffer (Buffer'First)'Address,
	       Buffer'Length, Last, Callers_Name);
      exit when Buffer (1) = 64;
      Comment ("Received:" & POSIX.To_String
               (To_POSIX_String (Buffer (1 .. 
			Ada.Streams.Stream_Element_Offset (Last)))));
      Comment ("  (from):" & POSIX.To_String
              (Internet_Address_To_String (Get_Internet_Address
              (Callers_Name))));
   end loop;

   Comment ("Receive and gather datagrams as a message ...");
   Set_Segment (SM_Array(1), Buffer1'Address, Buffer1'Length);
   Set_Segment (SM_Array(2), Buffer2'Address, Buffer2'Length);
   Set_Segment (SM_Array(3), Buffer3'Address, Buffer3'Length);
   Set_Segment (SM_Array(4), Buffer4'Address, Buffer4'Length);
   Set_Segment (SM_Array(5), Buffer5'Address, Buffer5'Length);
   Set_Socket_Name (Message, Callers_Name);
   Set_Socket_Message_Array (Message, SM_Array);
   Receive_Message (Receiving_Socket, Message, Last);
   Comment ("Received:" & POSIX.To_String (To_POSIX_String (Buffer1)));
   Comment ("        :" & POSIX.To_String (To_POSIX_String (Buffer2)));
   Comment ("        :" & POSIX.To_String (To_POSIX_String (Buffer3)));
   Comment ("        :" & POSIX.To_String (To_POSIX_String (Buffer4)));
   Comment ("        :" & POSIX.To_String (To_POSIX_String (Buffer5)));

   Done;

   exception when E : others => Fail (E);

end Test_UDP_Listen;

with POSIX; use POSIX;
with POSIX.IO; use POSIX.IO;
with POSIX.Files; use POSIX.Files;
with POSIX.Event_Management; use POSIX.Event_Management;
with POSIX.Sockets; use POSIX.Sockets;
with POSIX.Sockets.Internet; use POSIX.Sockets.Internet;
with Test_Pkg; use Test_Pkg;
with GNAT.IO; use GNAT.IO;
with Ada.Streams; use Ada.Streams;

procedure Test_Poll_Listen is
   Listening_Socket1 : File_Descriptor;
   Listening_Socket2 : File_Descriptor;
   Listening_Socket3 : File_Descriptor;
   Accepting_Socket  : File_Descriptor;
   Socket_Name1      : Internet_Socket_Address;
   Socket_Name2      : Internet_Socket_Address;
   Socket_Name3      : Internet_Socket_Address;
   Events            : Poll_Events := Empty_Set;
   Returned_Events   : Poll_Events := Empty_Set;
   Files             : Poll_File_Descriptor_Set (1..3);
   Response_Count    : Natural := 0;
   Buffer            : string (1 .. 80);
   Last              : POSIX.IO_Count;
begin
   ---------------------------------------------
   --  Poll several listening TCP/IP sockets  --
   ---------------------------------------------
   --
   --  Create 3 sockets
   Listening_Socket1 := Create (Internet_Protocol, Stream_Socket);
   Listening_Socket2 := Create (Internet_Protocol, Stream_Socket);
   Listening_Socket3 := Create (Internet_Protocol, Stream_Socket);

   --  Bind them to 3 different ports and set them listening
   Set_Internet_Address (Socket_Name1, Unspecified_Internet_Address);
   Set_Internet_Address (Socket_Name2, Unspecified_Internet_Address);
   Set_Internet_Address (Socket_Name3, Unspecified_Internet_Address);
   Set_Internet_Port (Socket_Name1, 2000);
   Set_Internet_Port (Socket_Name2, 2001);
   Set_Internet_Port (Socket_Name3, 2002);
   Bind (Listening_Socket1, Socket_Name1);
   Bind (Listening_Socket2, Socket_Name2);
   Bind (Listening_Socket3, Socket_Name3);
   Listen (Listening_Socket1, 3);
   Listen (Listening_Socket2, 3);
   Listen (Listening_Socket3, 3);

   --  Put the descriptors in a set suitable for Poll
   Set_File (Files(1), Listening_Socket1);
   Set_File (Files(2), Listening_Socket2);
   Set_File (Files(3), Listening_Socket3);

   --  Set Poll events to check for incoming connections
   --  and/or normal data
   Set_Events (Files(1), Read_Normal+Read_Not_High);
   Set_Events (Files(2), Read_Normal+Read_Not_High);
   Set_Events (Files(3), Read_Normal+Read_Not_High);

   --  Poll should indicate all listening sockets with incoming
   --  connections as readable (i.e., Accept_Connection will not block)

   --  Poll for files with incoming connections (i.e., ready for reading)
   Comment ("...Poll Files (block)...");
   Poll (Files, Response_Count);

   --  Process all the incoming connections
   Comment ("...Something's ready...Accept all Connections...");
   for i in Files'range loop
      if (Get_Returned_Events (Files(i)) > Read_Normal) or
         (Get_Returned_Events (Files(i)) > Read_Not_High) then
         Comment ("...Accept connection...");
         Accepting_Socket := Accept_Connection (Get_File (Files(i)));
         Comment ("...Receive something on accepted connection...");
         Receive (Accepting_Socket, Buffer (Buffer'First)'Address,
		  Buffer'Length, Last);
         if (Last > 0) then
            Comment ("...Received:" & Buffer (1 .. integer (Last)));
         end if;
      end if;
   end loop;

   exception when E : others => Fail (E);
end Test_Poll_Listen;

with POSIX; use POSIX;
with POSIX.IO; use POSIX.IO;
with POSIX.Files; use POSIX.Files;
with POSIX.Event_Management; use POSIX.Event_Management;
with POSIX.Sockets; use POSIX.Sockets;
with POSIX.Sockets.Internet; use POSIX.Sockets.Internet;
with Test_Pkg; use Test_Pkg;
with GNAT.IO; use GNAT.IO;
with Ada.Streams; use Ada.Streams;

procedure Test_Select_Listen is
   Listening_Socket1 : Select_File_Descriptor;
   Listening_Socket2 : Select_File_Descriptor;
   Listening_Socket3 : Select_File_Descriptor;
   Accepting_Socket  : File_Descriptor;
   Socket_Name1      : Internet_Socket_Address;
   Socket_Name2      : Internet_Socket_Address;
   Socket_Name3      : Internet_Socket_Address;
   Read_Set          : File_Descriptor_Set := Empty_File_Descriptor_Set;
   Write_Set         : File_Descriptor_Set := Empty_File_Descriptor_Set;
   Ex_Set            : File_Descriptor_Set := Empty_File_Descriptor_Set;
   Files_Selected    : Natural := 0;
   Buffer            : string (1 .. 80);
   Last              : POSIX.IO_Count;
begin
   ----------------------------------------------------
   --  Select from several listening TCP/IP sockets  --
   ----------------------------------------------------
   --
   --  Create 3 sockets
   Listening_Socket1 := Select_File_Descriptor
      (Create (Internet_Protocol, Stream_Socket));
   Listening_Socket2 := Select_File_Descriptor
      (Create (Internet_Protocol, Stream_Socket));
   Listening_Socket3 := Select_File_Descriptor
      (Create (Internet_Protocol, Stream_Socket));

   --  Bind them to 3 different ports and set them listening
   Set_Internet_Address (Socket_Name1, Unspecified_Internet_Address);
   Set_Internet_Address (Socket_Name2, Unspecified_Internet_Address);
   Set_Internet_Address (Socket_Name3, Unspecified_Internet_Address);
   Set_Internet_Port (Socket_Name1, 2000);
   Set_Internet_Port (Socket_Name2, 2001);
   Set_Internet_Port (Socket_Name3, 2002);
   Bind (File_Descriptor(Listening_Socket1), Socket_Name1);
   Bind (File_Descriptor(Listening_Socket2), Socket_Name2);
   Bind (File_Descriptor(Listening_Socket3), Socket_Name3);
   Listen (File_Descriptor(Listening_Socket1), 3);
   Listen (File_Descriptor(Listening_Socket2), 3);
   Listen (File_Descriptor(Listening_Socket3), 3);

   --  Put the descriptors in a set suitable for Select_File
   Make_Empty (Read_Set);
   Add_File_Descriptor_To_Set (Read_Set, Listening_Socket1);
   Add_File_Descriptor_To_Set (Read_Set, Listening_Socket2);
   Add_File_Descriptor_To_Set (Read_Set, Listening_Socket3);

   --  Select_File should indicate all listening sockets with incoming
   --  connections as readable (i.e., Accept_Connection will not block)
   declare
      procedure Accept_One
        (File : in     Select_File_Descriptor;
         Quit : in out Boolean) is
      begin
         if (File = Listening_Socket1) then
            Comment ("...Accept connection on port 2000...");
         elsif (File = Listening_Socket2) then
            Comment ("...Accept connection on port 2001...");
         elsif (File = Listening_Socket3) then
            Comment ("...Accept connection on port 2002...");
         else
            Comment ("...unknown fd in set?");
         end if;
         Accepting_Socket := Accept_Connection (File_Descriptor(File));
         Comment ("...Receive something on accepted connection...");
         Receive (Accepting_Socket, Buffer (Buffer'First)'Address,
		  Buffer'Length, Last);
         if (Last > 0) then
            Comment ("...Received:" & Buffer (1 .. integer (Last)));
         end if;
      end Accept_One;
      procedure Accept_All_Connections is new For_Every_File_In (Accept_One);
   begin
      --  Select all files with incoming connections (i.e., ready for reading)
      Comment ("...Select Files (block)...");
      Select_File (Read_Set, Write_Set, Ex_Set, Files_Selected);
      --  Process all the incoming connections
      Comment ("...Something's ready...Accept all Connections...");
      Accept_All_Connections (Read_Set);
      Done;
   end;
   exception when E : others => Fail (E);
end Test_Select_Listen;

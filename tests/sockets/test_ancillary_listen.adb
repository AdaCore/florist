with POSIX; use POSIX;
with POSIX.IO; use POSIX.IO;
with POSIX.Files; use POSIX.Files;
with POSIX.Sockets; use POSIX.Sockets;
with POSIX.Sockets.Local; use POSIX.Sockets.Local;
with Test_Pkg; use Test_Pkg;
with GNAT.IO; use GNAT.IO;
with Ada.Streams; use Ada.Streams;
procedure Test_Ancillary_Listen is
   Listening_Socket : File_Descriptor;
   Accepting_Socket : File_Descriptor;
   Socket_Name      : Local_Socket_Address;
   Socket_Path      : Pathname := "a_local_socket";
   Last             : POSIX.IO_Count;
   SGV_Array        : Socket_Message_Array_Pointer
      := new Socket_Message_Array (1 .. 1);       -- note this is born null
   Files            : Fd_Array (1 .. 3);
   Message          : Socket_Message;
begin

   -----------------------------------------------
   -- Receive ancillary data on a local socket  --
   -----------------------------------------------

   Test ("Receive ancillary data on local socket");

   Listening_Socket := Create (Local_Protocol, Stream_Socket);
   Set_Socket_Path (Socket_Name, Socket_Path);
   Bind (Listening_Socket, Socket_Name);
   Listen (Listening_Socket, 1);
   Accepting_Socket := Accept_Connection (Listening_Socket);

   Comment ("Receive message with ancillary data only...");
   Set_Ancillary_Data (Message, Files'Address);
   Set_Socket_Message_Array (Message, SGV_Array);
   Receive_Message (Accepting_Socket, Message, Last);
   -- Files := Get_Ancillary_Data (Message);
   if Verbose then
      Put ("...Ancillary data received:");
      Put (Integer(Files.all(1))); Put (", ");
      Put (Integer(Files.all(2))); Put (", ");
      Put (Integer(Files.all(3))); New_Line;
   end if;
   Unlink (Socket_Path);

   Done;

   exception when E : others => Fail (E);

end Test_Ancillary_Listen;

with POSIX; use POSIX;
with POSIX.IO; use POSIX.IO;
with POSIX.Files; use POSIX.Files;
with POSIX.Sockets; use POSIX.Sockets;
with POSIX.Sockets.Local; use POSIX.Sockets.Local;
with Test_Pkg; use Test_Pkg;
with GNAT.IO; use GNAT.IO;
with Ada.Streams; use Ada.Streams;
procedure Test_Ancillary_Talk is
   Talking_Socket : File_Descriptor;
   Socket_Name    : Local_Socket_Address;
   Socket_Path    : Pathname := "a_local_socket";
   Last           : POSIX.IO_Count;
   SGV_Array      : Socket_Message_Array_Pointer
      := new Socket_Message_Array (1 .. 1);       -- note this is born null
   Files          : Fd_Array_Access := new Fd_Array (1 .. 3);
   Message        : Socket_Message;
begin

   --------------------------------------------
   -- Send ancillary data on a local socket  --
   --------------------------------------------

   Test ("Send ancillary data on local socket");

   Talking_Socket := Create (Local_Protocol, Stream_Socket);
   Set_Socket_Path (Socket_Name, Socket_Path);
   Connect (Talking_Socket, Socket_Name);

   Comment ("Send message with ancillary data only...");
   Files.all (1) := Talking_Socket;
   Files.all (2) := 222;
   Files.all (3) := 333;
   Set_Ancillary_Data (Message, Files);
   Set_Socket_Message_Array (Message, SGV_Array);
   Send_Message (Talking_Socket, Message, Last);

   Close (Talking_Socket);

   Done;

   exception when E : others => Fail (E);

end Test_Ancillary_Talk;

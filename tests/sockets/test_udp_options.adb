with POSIX; use POSIX;
with POSIX.IO; use POSIX.IO;
with POSIX.Files; use POSIX.Files;
with POSIX.Sockets; use POSIX.Sockets;
with POSIX.Sockets.Internet; use POSIX.Sockets.Internet;
with Test_Pkg; use Test_Pkg;
with GNAT.IO; use GNAT.IO;
with Ada.Streams; use Ada.Streams;

Procedure Test_UDP_Options is

   UDP_Socket:  File_Descriptor;
   Socket_Name: Internet_Socket_Address;
   Err:         Error_Code := No_Error;
   
   procedure Option_Status (on_off:Socket_Option) is
   begin
      if on_off = Enabled then
         if Verbose then Put_Line ("Enabled"); end if;
      else
         if Verbose then Put_Line ("Disabled"); end if;
      end if;
   end;
   
   procedure Option_Status (op_val:Natural) is
   begin
      if Verbose then Put (op_val); New_Line; end if;
   end;

   procedure Display_Socket_Options (Socket: in File_Descriptor) is
   begin

      -- All the Enabled/Disabled options at IP level

      begin
         if Verbose then Put ("  ...(IP) Receive_Destination_Address: "); end if;
         Option_Status (Get_Receive_Destination_Address (Socket));
         exception when POSIX_Error => Put_Line (Image(Get_Error_Code));
      end;
      begin
         if Verbose then Put ("  ...(IP) Header_Included: "); end if;
         Option_Status (Get_Header_Included (Socket));
         exception when POSIX_Error => Put_Line (Image(Get_Error_Code));
      end;
         
   end;
   
begin

   ---------------------------
   -- UDP/IP socket options --
   ---------------------------

   Test ("Test UDP/IP socket options");

   Comment ("Create & Bind file descriptor for a UDP/IP socket");
   UDP_Socket := Create (Internet_Protocol, Datagram_Socket);
   Set_Internet_Address (Socket_Name, Unspecified_Internet_Address);
   Set_Internet_Port (Socket_Name, 2000);
   Bind (UDP_Socket, Socket_Name);
   Display_Socket_Options (UDP_Socket);
   
   Comment ("Test Set Options...");
   Set_Receive_Destination_Address (UDP_Socket, Enabled);
   Set_Header_Included (UDP_Socket, Enabled);
   Display_Socket_Options (UDP_Socket);

   Comment ("Test Reset Options...");
   Set_Receive_Destination_Address (UDP_Socket, Disabled);
   Set_Header_Included (UDP_Socket, Disabled);
   Display_Socket_Options (UDP_Socket);

   Done;

end Test_UDP_Options;

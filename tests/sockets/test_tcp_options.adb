with POSIX; use POSIX;
with POSIX.IO; use POSIX.IO;
with POSIX.Files; use POSIX.Files;
with POSIX.Sockets; use POSIX.Sockets;
with POSIX.Sockets.Internet; use POSIX.Sockets.Internet;
with test_pkg; use test_pkg;
with Gnat.IO; use Gnat.IO;
with Ada.Streams; use Ada.Streams;

Procedure Test_TCP_Options is

   TCP_Socket:  File_Descriptor;
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

      -- First, all the Enabled/Disabled options at socket level

      begin
         if Verbose then Put ("  ...Broadcast: "); end if;
         Option_Status (Get_Socket_Broadcast (Socket));
         exception when POSIX_Error => Put_Line (Image(Get_Error_Code));
      end;
      begin
         if Verbose then Put ("  ...Debugging: "); end if;
         Option_Status (Get_Socket_Debugging (Socket));
         exception when POSIX_Error => Put_Line (Image(Get_Error_Code));
      end;
      begin
         if Verbose then Put ("  ...No_Routing: "); end if;
         Option_Status (Get_Socket_Routing (Socket));
         exception when POSIX_Error => Put_Line (Image(Get_Error_Code));
      end;
      begin
         if Verbose then Put ("  ...Keepalive: "); end if;
         Option_Status (Get_Socket_Keep_Alive (Socket));
         exception when POSIX_Error => Put_Line (Image(Get_Error_Code));
      end;
      begin
         if Verbose then Put ("  ...OOB_Data_Inline: "); end if;
         Option_Status (Get_Socket_OOB_Data_Inline (Socket));
         exception when POSIX_Error => Put_Line (Image(Get_Error_Code));
      end;
      begin
         if Verbose then Put ("  ...Reuse_Addresses: "); end if;
         Option_Status (Get_Socket_Reuse_Addresses (Socket));
         exception when POSIX_Error => Put_Line (Image(Get_Error_Code));
      end;
      
      -- Then, the ones with other data types
      begin
         if Verbose then Put ("  ...Linger_Time: "); end if;
         Option_Status (Get_Socket_Linger_Time (Socket));
         exception when POSIX_Error => Put_Line (Image(Get_Error_Code));
      end;
     
      -- Then, all the Enabled/Disabled options at TCP level

      begin
         if Verbose then Put ("  ...(TCP) No_Delay: "); end if;
         Option_Status (Get_No_Delay (Socket));
         exception when POSIX_Error => Put_Line (Image(Get_Error_Code));
      end;
      begin
         if Verbose then Put ("  ...(TCP) Standardized_Urgent_Data: "); end if;
         Option_Status (Get_Standardized_Urgent_Data (Socket));
         exception when POSIX_Error => Put_Line (Image(Get_Error_Code));
      end;

      -- Then, all the Enabled/Disabled options at IP level

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
   -- TCP/IP socket options --
   ---------------------------

   Test ("Test TCP/IP socket options");

   Comment ("Create & Bind file descriptor for a TCP/IP socket");
   TCP_Socket := Create (Internet_Protocol, Stream_Socket);
   Set_Internet_Address (Socket_Name, Unspecified_Internet_Address);
   Set_Internet_Port (Socket_Name, 0);
   Bind (TCP_Socket, Socket_Name);
   Display_Socket_Options (TCP_Socket);
   
   Comment ("Test Set Options...");
   Set_Socket_Broadcast (TCP_Socket, Enabled);
   Set_Socket_Routing (TCP_Socket, Enabled);
   Set_Socket_OOB_Data_InLine (TCP_Socket, Enabled);
   Set_Socket_Linger_Time (TCP_Socket, 123);
   Display_Socket_Options (TCP_Socket);

   Comment ("Test Reset Options...");
   Set_Socket_Broadcast (TCP_Socket, Disabled);
   Set_Socket_Routing (TCP_Socket, Disabled);
   Set_Socket_OOB_Data_InLine (TCP_Socket, Disabled);
   Set_Socket_Linger_Time (TCP_Socket, 0);
   Display_Socket_Options (TCP_Socket);

   Done;

end Test_TCP_Options;

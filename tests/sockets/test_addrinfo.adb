with POSIX; use POSIX;
with POSIX.IO; use POSIX.IO;
with POSIX.Files; use POSIX.Files;
with POSIX.Sockets; use POSIX.Sockets;
with POSIX.Sockets.Internet; use POSIX.Sockets.Internet;
with test_pkg; use test_pkg;
with Gnat.IO; use Gnat.IO;
with Ada.Streams; use Ada.Streams;
with Ada.Text_IO;

with Text_IO;

procedure Test_Addrinfo is

   procedure Print_Item 
     (Info: in     Socket_Address_Information;
      Quit: in out Boolean) is
      Name:  constant POSIX_String    := Get_Canonical_Name (Info);
      Flags: constant Address_Flags   := Get_Flags (Info);
   begin
      if Verbose then
         Put_Line ("...Canonical name: " & To_String(Name));
         case Get_Family (Info) is
            when Internet_Protocol =>
               Put_Line ("...Protocol Family: Internet_Domain");
               declare
                  Addr: constant Internet_Socket_Address := Get_Address (Info);
                  In_Addr: constant Internet_Address :=
				 Get_Internet_Address (Addr);
                  Dot_Address: constant POSIX_String := 
                     Internet_Address_To_String (In_Addr);
                  begin
                     Put_Line ("      addr=" & To_String (Dot_Address));
                     Put ("      port=");
                     Put (integer(Get_Internet_Port (Addr))); New_line;
                  end;
            when Others =>
               Put_Line ("...Address Family: Unknown_Domain");
         end case;
         case Get_Socket_Type (Info) is
            when Stream_Socket =>
               Put_Line ("...Socket type: Stream Socket");
            when Datagram_Socket =>
               Put_Line ("...Socket type: Datagram Socket");
            when Raw_Socket =>
               Put_Line ("...Socket type: Raw Socket");
            when Sequenced_Packet_Socket =>
               Put_Line ("...Socket type: Sequenced Packet Socket");
            when Others =>
               Put_Line ("...Socket type: Unknown");
         end case;         
         Put ("...Protocol Number: ");
         Put (integer(Get_Protocol_Number (Info))); New_line;
      end if;
   end Print_Item;
   procedure Print_Every_Item is new POSIX.Sockets.For_Every_Item (Print_Item);
      
   Addr_Info: Socket_Address_Information;
   Request_Info: Socket_Address_Information;

begin

   Test ("Get_Socket_Address_Information");

   Comment ("Get Socket Address Information by Name");
   Addr_Info := Get_Socket_Address_Information (
      Name => "129.190.223.103",
      Service => "");
   Print_Every_Item (Addr_Info);
   
   Comment ("Again, with some hints in the request parameter");
   Set_Flags (Request_Info, Canonical_Name+Use_For_Binding);
   Set_Family (Request_Info, Internet_Protocol);
   Set_Socket_Type (Request_Info, Stream_Socket);
   Set_Protocol_Number (Request_Info, Default_Protocol);
   Addr_Info := Get_Socket_Address_Information (
      Name => "129.190.223.103",
      Service => "",
      Request => Request_Info);
   Comment ("Completed Get_Socket_Address_Information");
   Print_Every_Item (Addr_Info);
   
   Comment ("Get Socket Address Information by Service");
   Addr_Info := Get_Socket_Address_Information (
      Name => "",
      Service => "telnet");
   Print_Every_Item (Addr_Info);
   
   Done;
   
   -- exception when POSIX_Error => Put_Line (Image(Get_Error_Code));
   exception when E : others => Fail (E);

end Test_Addrinfo;

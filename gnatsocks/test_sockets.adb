------------------------------------------------------------------------
--  file : test_sockets.adb  [$Revision$]
------------------------------------------------------------------------

--  The bare bones of a test for packages Sockets and Sockets.Internet.

with Ada.Characters.Latin_1,
     POSIX_Report,
     Sockets,
     Sockets.Internet;
procedure Test_Sockets is

   use POSIX_Report,
       Sockets,
       Sockets.Internet;

   Stream_Sock : Sockets.Stream_Socket;
   Server_Sock : Sockets.Server_Socket;
   In_Stream   : Input_Stream_Ptr;
   Out_Strem   : Output_Stream_Ptr;

   Lf : constant Character := Ada.Characters.Latin_1.Lf;
   Cr : constant Character := Ada.Characters.Latin_1.Cr;

   Localhost : Internet_Address;
   Telnet_Port : constant Port_Number := 23;
   Sockaddr : Internet_Socket_Address;

begin
   Header ("Test_Sockets");
   Test ("package Sockets");

   declare
      Addr : Internet_Address;
   begin
      Test ("Local_Host");
      Localhost := Local_Host;
      Test ("Get_Hostname");
      declare
         Address : constant String := Get_AddressString (Localhost);
      begin
         Comment ("address = " & Address);
         declare
            Hostname : constant String := Get_HostByAddr (Localhost);
         begin
            Comment ("hostname = " & Hostname);
            Test ("Get_AddrByName");
            Addr := Get_AddrByName (Hostname);
            Assert (Localhost = Addr, "A000: Get_AddrByName, Get_HostName");
         end;
      end;
      Comment ("name of All_Local_Addresses = "
        & Get_HostByAddr (All_Local_Addresses));
   exception when E : others => Fail (E, "A001");
   end;

   declare
      Ch : Character;
   begin
      Test ("open stream socket to telnet port of local host");
      Sockets.Open (Stream_Sock,
        Sockets.Internet.New_Address (Telnet_Port, Localhost));
      In_Stream := Sockets.Get_Input_Stream (Stream_Sock);
      Test ("close");
      Close (Stream_Sock);
   exception when E : others => Fail (E, "A002");
   end;

   begin
      null;
   exception when E : others => Fail (E, "A003");
   end;

   Done;
exception when E : others => Fatal_Exception (E, "A004");
end Test_Sockets;

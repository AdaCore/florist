with POSIX;
with POSIX.Files;
with POSIX.IO;
with POSIX.XTI;                use POSIX.XTI;
with POSIX.XTI.Internet;
with Text_IO;
with Ada_Streams;              use Ada_Streams;

procedure Test_TCP_Listen is

   type Test_Buffer_Type is record
      Item1 : integer;
      Item2 : float;
      Item3 : POSIX.Octet_Array (1 .. 4000);
   end record;
   Test_Buffer : aliased Test_Buffer_Type;

   function To_Buffer_Pointer is new POSIX.XTI.To_Buffer_Pointer 
	  (Test_Buffer_Type);
   function To_Buffer_Pointer is new POSIX.XTI.To_Buffer_Pointer
          (POSIX.Octet);

   Comm_Provider_Info  : aliased POSIX.XTI.Communications_Provider_Information;
   Internet_Addr       : aliased POSIX.XTI.Internet.Internet_XTI_Address;
   Response_Addr       : aliased POSIX.XTI.Internet.Internet_XTI_Address;
   Connection_Addr     : aliased POSIX.XTI.Internet.Internet_XTI_Address;

   Endpoint            : POSIX.IO.File_Descriptor;
   Connected_Endpoint  : POSIX.IO.File_Descriptor;
   Options             : POSIX.IO.Open_Option_Set;

   Response_Qlen       : integer;
   Response_INET_Addr  : POSIX.XTI.Internet.Internet_Address;

   Connect_Info        : aliased POSIX.XTI.Connection_Information;

   Buffer              : POSIX.Octet_Array (1 .. 4000);
   Bytes               : POSIX.IO_Count;
   Flags               : POSIX.XTI.XTI_Flags;
   Total               : POSIX.IO_Count;

   Option              : POSIX.XTI.Protocol_Option;
   Option_List         : aliased POSIX.XTI.Protocol_Option_List;
   Option_Buffer       : POSIX.XTI.Octet_Buffer_Pointer :=
                    new POSIX.Octet_Array (1 .. 256);
   Option_Result       : POSIX.XTI.Option_Status;

   package Int_IO is new Text_IO.Integer_IO (integer);
   use Int_IO;

begin

   if (POSIX.Files.Is_Character_Special_File ("/dev/inet_cots")) then
      POSIX.XTI.Open (Endpoint,
                      "/dev/inet_cots",
                      POSIX.IO.Read_Write,
                      Options);
   elsif (POSIX.Files.Is_Character_Special_File ("/dev/ticots")) then
      POSIX.XTI.Open (Endpoint,
                      "/dev/ticots",
                      POSIX.IO.Read_Write,
                      Options);
   end if;

   Text_IO.Put_Line ("Opened endpoint");

   --
   --  Let's get some Info about the Communications Provider
   --
   POSIX.XTI.Get_Information (Endpoint, Comm_Provider_Info'access);

   Text_IO.Put_Line ("Some Info on the Communications Provider:");
   Text_IO.Put ("   Protocol Addresses are ");
   if (POSIX.XTI.Protocol_Addresses_Are_Valid (Comm_Provider_Info)
       = True) then
      Text_IO.Put ("Valid, Max Size is ");
      Int_IO.Put (POSIX.XTI.Get_Max_Size_Protocol_Address
                      (Comm_Provider_Info));
      Text_IO.New_Line;
   else
      Text_IO.Put_Line ("NOT Valid");
   end if;

   Text_IO.Put ("   Protocol Options are ");
   if (POSIX.XTI.Protocol_Options_Are_Valid (Comm_Provider_Info) = True) then
      Text_IO.Put ("Valid, Max Size is ");
      Int_IO.Put (POSIX.XTI.Get_Max_Size_Protocol_Options
                            (Comm_Provider_Info));
      Text_IO.New_Line;
   else
      Text_IO.Put_Line ("NOT Valid");
   end if;


   Text_IO.Put ("   SDU is ");
   if (POSIX.XTI.SDU_Is_Supported (Comm_Provider_Info) = True) then
      Text_IO.Put ("Supported,");
      if (POSIX.XTI.SDU_Is_Infinite (Comm_Provider_Info) = True) then
         Text_IO.Put (" Infinite,");
      else
         Text_IO.Put (" NOT Infinite,");
      end if;
      if (POSIX.XTI.SDU_Is_Valid (Comm_Provider_Info) = True) then
         Text_IO.Put (" Valid,");
      else
         Text_IO.Put (" NOT Valid,");
      end if;
      Text_IO.Put (" Max Size is ");
      Int_IO.Put (POSIX.XTI.Get_Max_Size_SDU (Comm_Provider_Info));
      Text_IO.New_Line;
   else
      Text_IO.Put_Line ("NOT Supported");
   end if;
   
   Text_IO.Put ("   SEDU is ");
   if (POSIX.XTI.SEDU_Is_Supported (Comm_Provider_Info) = True) then
      Text_IO.Put ("Supported,");
      if (POSIX.XTI.SEDU_Is_Infinite (Comm_Provider_Info) = True) then
         Text_IO.Put (" Infinite,");
      else
         Text_IO.Put (" NOT Infinite,");
      end if;
      if (POSIX.XTI.SEDU_Is_Valid (Comm_Provider_Info) = True) then
         Text_IO.Put (" Valid,");
      else
         Text_IO.Put (" NOT Valid,");
      end if;
      Text_IO.Put (" Max Size is ");
      Int_IO.Put (POSIX.XTI.Get_Max_Size_SEDU (Comm_Provider_Info));
      Text_IO.New_Line;
   else
      Text_IO.Put_Line ("NOT Supported");
   end if;
   
   Text_IO.Put ("   Connect Data is ");
   if (POSIX.XTI.Connect_Data_Is_Valid (Comm_Provider_Info) = True) then
      Text_IO.Put_Line ("Valid, Max Size is ");
      Int_IO.Put (POSIX.XTI.Get_Max_Size_Connect_Data (Comm_Provider_Info));
   else
      Text_IO.Put_Line ("NOT Valid");
   end if;

   Text_IO.Put ("   Disconnect Data is ");
   if (POSIX.XTI.Disconnect_Data_Is_Valid (Comm_Provider_Info) = True) then
      Text_IO.Put_Line ("Valid, Max Size is ");
      Int_IO.Put (POSIX.XTI.Get_Max_Size_Disconnect_Data (Comm_Provider_Info));
   else
      Text_IO.Put_Line ("NOT Valid");
   end if;

   Text_IO.Put ("   Service Type is ");
   if (POSIX.XTI.Get_Service_Type (Comm_Provider_Info) =
       POSIX.XTI.Connection_Mode) then
      Text_IO.Put_Line ("Connection Mode");
   elsif (POSIX.XTI.Get_Service_Type (Comm_Provider_Info) =
           POSIX.XTI.Connection_Mode_With_Orderly_Release) then
      Text_IO.Put_Line ("Connection Mode With Orderly Release");
   elsif (POSIX.XTI.Get_Service_Type (Comm_Provider_Info) =
       POSIX.XTI.Connectionless_Mode) then
      Text_IO.Put_Line ("Connectionless Mode");
   end if;

   --
   --  Set up the Protocol Address
   --
   POSIX.XTI.Internet.Set_Internet_Port (Internet_Addr, 16#FF00#);
   POSIX.XTI.Internet.Set_Internet_Address (Internet_Addr,
                      POSIX.XTI.Internet.Unspecified_Internet_Address);

   Text_IO.Put_Line ("Set Port and Internet Address in Internet_XTI_Address");

   --
   --  Need to do this to set the netbuf parameters in the implementation
   --  Need to fix implemenation.
   --
   POSIX.XTI.Internet.Set_Internet_Port (Response_Addr, 16#00#);

   POSIX.XTI.Bind (Endpoint,
                   Internet_Addr,
                   5,
                   Response_Addr'access,
                   Response_Qlen);
   Text_IO.Put_Line ("Completed Bind");

   Text_IO.Put ("   Port: ");
   Int_IO.Put (integer (POSIX.XTI.Internet.Get_Internet_Port (Internet_Addr)));
   Text_IO.New_Line;
   Text_IO.Put ("   Queue Length: ");
   Int_IO.Put (Response_Qlen);
   Text_IO.New_Line;

   Response_INET_Addr := POSIX.XTI.Internet.Get_Internet_Address
                                   (Response_Addr);

   Text_IO.Put ("   Address: ");
   Text_IO.Put (POSIX.To_String (
   (POSIX.XTI.Internet.Internet_Address_To_String (Response_INET_Addr))));
   Text_IO.New_Line;

   POSIX.XTI.Set_Address (Connect_Info, Internet_Addr);

   POSIX.XTI.Listen (Endpoint, Connect_Info'access);

   Text_IO.Put ("Got a Connection Request from ");
   Response_INET_Addr := POSIX.XTI.Internet.Get_Internet_Address
                                        (Internet_Addr);

   Text_IO.Put (POSIX.To_String (
      (POSIX.XTI.Internet.Internet_Address_To_String (Response_INET_Addr))));
   Text_IO.Put_Line (", REJECTING");

   --
   --  Reject the first connect just to test Send_Disconnect_Request with
   --  Sequence_Number
   --

   POSIX.XTI.Send_Disconnect_Request (Endpoint,
                     POSIX.XTI.Get_Sequence_Number (Connect_Info));

   Text_IO.Put_Line ("Doing some more Listening...");
   --
   --  Do some more Listening
   --
   POSIX.XTI.Listen (Endpoint, Connect_Info'access);

   Text_IO.Put ("Got a Connection Request from ");
   Response_INET_Addr := POSIX.XTI.Internet.Get_Internet_Address
                                        (Internet_Addr);

   Text_IO.Put (POSIX.To_String (
      (POSIX.XTI.Internet.Internet_Address_To_String (Response_INET_Addr))));
   Text_IO.New_Line;


   POSIX.XTI.Open (Connected_Endpoint,
                  "/dev/inet_cots",
                  POSIX.IO.Read_Write,
                  Options);
   Text_IO.Put_Line ("Opened Connection endpoint");
   POSIX.XTI.Bind (Connected_Endpoint, Response_Addr'access);

   Text_IO.Put_Line ("Completed Bind on Connection endpoint");

   POSIX.XTI.Internet.Set_Internet_Port (Connection_Addr, 16#FF00#);
   POSIX.XTI.Internet.Set_Internet_Address (Connection_Addr,
                  POSIX.XTI.Internet.Unspecified_Internet_Address);

   POSIX.XTI.Set_Address (Connect_Info, Connection_Addr);

   POSIX.XTI.Accept_Connection (Endpoint,
                                Connected_Endpoint,
                                Connect_Info);

   Text_IO.Put_Line ("Accepted Connection");

   --
   --  Set options on Connected_Endpoint
   --

   POSIX.XTI.Set_Buffer (Option_List, Option_Buffer);
--   POSIX.XTI.Set_Option_Buffer_Maximum_Length (Option_Buffer'Last,
--                                           Option_List);

   --
   --  Set RCV Buffer Size
   --
--   POSIX.XTI.Set_Level (Option, POSIX.XTI.XTI_Protocol_Level);
--   POSIX.XTI.Set_Name (Option, POSIX.XTI.Receive_Buffer_Size);
--   POSIX.XTI.Set_Value (Option, (255 * 1024));

   POSIX.XTI.Set_Option (Option,
			 POSIX.XTI.XTI_Protocol_Level,
                         POSIX.XTI.Receive_Buffer_Size,
                         (255 * 1024));

   POSIX.XTI.Append (Option_List, Option);

   --
   --  Set SND Buffer Size
   --
--   POSIX.XTI.Set_Level (Option, POSIX.XTI.XTI_Protocol_Level);
--   POSIX.XTI.Set_Name (Option, POSIX.XTI.Send_Buffer_Size);
--   POSIX.XTI.Set_Value (Option, (255 * 1024));

   POSIX.XTI.Set_Option (Option,
			 POSIX.XTI.XTI_Protocol_Level,
                         POSIX.XTI.Send_Buffer_Size,
                         (255 * 1024));

   POSIX.XTI.Append (Option_List, Option);

   Text_IO.Put_Line ("Added Options to Option_List, Calling Manage_Options");

   POSIX.XTI.Manage_Options (Connected_Endpoint,
                             Option_List,
                             POSIX.XTI.Negotiate_Options,
                             Option_List'access,
                             Option_Result);
   Text_IO.Put ("The Result of Manage Options (Set SND & RCV buf) was ");
   if (Option_Result = Success) then
      Text_IO.Put_Line ("Success");
   elsif (Option_Result = Partial_Success) then
      Text_IO.Put_Line ("Partial Success");
   elsif (Option_Result = Failure) then
      Text_IO.Put_Line ("Failure");
   elsif (Option_Result = Read_Only) then
      Text_IO.Put_Line ("Read Only");
   elsif (Option_Result = Not_Supported) then
      Text_IO.Put_Line ("Not Supported");
   end if;

   --
   --  Test wrap message first
   --

   POSIX.XTI.Receive 
	     (Connected_Endpoint,
              To_Buffer_Pointer (Test_Buffer'Access),
              POSIX.IO_Count (Test_Buffer'Size / POSIX.Octet'Size),
              Bytes,
              Flags);
 
   --  Send it back
   POSIX.XTI.Send (Connected_Endpoint,
                   To_Buffer_Pointer (Test_Buffer'Access),
                   Bytes,
                   Flags,
		   Bytes);

   Text_IO.Put_Line ("Wrapped Message, Doing Receive...");
   Total := 0;
   loop
      POSIX.XTI.Receive (Connected_Endpoint,
                         To_Buffer_Pointer (Buffer (Buffer'First)'Access),
                         POSIX.IO_Count (Buffer'Last),
                         Bytes,
                         Flags);
      Total := POSIX.IO_Count (Integer (Total) + Integer (Bytes));
   end loop;

   Text_IO.Put ("Received ");
   Int_IO.Put (integer (Total));
   Text_IO.Put (" bytes");
   Text_IO.New_Line;

   delay 2.0;
   POSIX.XTI.Close (Connected_Endpoint);
   POSIX.XTI.Close (Endpoint);
   Text_IO.Put_Line ("Performed Close");

exception

   when POSIX.POSIX_Error =>
      if (POSIX.XTI.Look (Connected_Endpoint) =
         POSIX.XTI.Disconnect_Request_Received) then
         Text_IO.Put_Line ("Endpoint Closed by Remote Node");
      elsif (POSIX.XTI.Look (Connected_Endpoint) =
         POSIX.XTI.Connect_Request_Received) then
         Text_IO.Put_Line ("Connect Request Received");
      elsif (POSIX.XTI.Look (Connected_Endpoint) =
         POSIX.XTI.Connect_Response_Received) then
         Text_IO.Put_Line ("Connect Response Received");
      elsif (POSIX.XTI.Look (Connected_Endpoint) =
         POSIX.XTI.Error_In_Previously_Sent_Datagram) then
         Text_IO.Put_Line ("Error in Previously Sent Datagram");
      elsif (POSIX.XTI.Look (Connected_Endpoint) =
         POSIX.XTI.Expedited_Data_Received) then
         Text_IO.Put_Line ("Expedited Data Received");
      elsif (POSIX.XTI.Look (Connected_Endpoint) =
         POSIX.XTI.Normal_Data_Received) then
         Text_IO.Put_Line ("Normal Data Received");
      elsif (POSIX.XTI.Look (Connected_Endpoint) =
         POSIX.XTI.Orderly_Release_Request_Received) then
         Text_IO.Put_Line ("Orderly Release Request Received");
      else
         Text_IO.Put_Line ("Some other event");
      end if;

      Text_IO.Put ("Received ");
      Int_IO.Put (integer (Total));
      Text_IO.Put (" bytes");
      Text_IO.New_Line;

      POSIX.XTI.Close (Connected_Endpoint);
      POSIX.XTI.Close (Endpoint);


end Test_TCP_Listen;

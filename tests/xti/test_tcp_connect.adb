with POSIX;
with POSIX.Files;
with POSIX.IO;
with POSIX.XTI;                    use POSIX.XTI;
with POSIX.XTI.Internet;           use POSIX.XTI.Internet;
with Text_IO;
with Ada.Calendar;                 use Ada.Calendar;
with POSIX.Process_Environment;

procedure Test_TCP_Connect is

   type Test_Buffer_Type is record
      int : integer;
      flt : float;
      arr : POSIX.Octet_Array (1 .. 4000);
   end record;
   Test_Buffer        : aliased Test_Buffer_Type;
   Test_Buffer_Return : aliased Test_Buffer_Type;

   function To_Buffer_Pointer is new POSIX.XTI.To_Buffer_Pointer 
          (Test_Buffer_Type);
   function To_Buffer_Pointer is new POSIX.XTI.To_Buffer_Pointer 
          (POSIX.Octet);

   Internet_Addr        : POSIX.XTI.Internet.Internet_XTI_Address;
   Response_Addr        : aliased POSIX.XTI.Internet.Internet_XTI_Address;

   Endpoint             : POSIX.IO.File_Descriptor;
   Options              : POSIX.IO.Open_Option_Set;

   Response_INET_Addr   : POSIX.XTI.Internet.Internet_Address;

   Connect_Send_Info    : POSIX.XTI.Connection_Information;
   Connect_Receive_Info : POSIX.XTI.Connection_Information;

   Buffer               : aliased POSIX.Octet_Array (1 .. 4000);
   Bytes                : POSIX.IO_Count;
   Flags                : POSIX.XTI.XTI_Flags;
   Total                : POSIX.IO_Count;

   Start_Time           : Ada.Calendar.Day_Duration;
   End_Time             : Ada.Calendar.Day_Duration;

   Option               : POSIX.XTI.Protocol_Option;
   Option_List          : aliased POSIX.XTI.Protocol_Option_List;
   Option_Buffer        : POSIX.XTI.Octet_Buffer_Pointer :=
               new POSIX.Octet_Array (1 .. 256);
   Option_Result        : POSIX.XTI.Option_Status;

   Linger               : POSIX.XTI.Linger_Information;

   Remote_INET_Addr     : POSIX.XTI.Internet.Internet_Address :=
               POSIX.XTI.Internet.Loopback_Internet_Address; --  by default

   Arg_Number           : integer := 1;

   procedure Parse (Item  : in POSIX.POSIX_String;
                    Quit  : in out Boolean) is
   begin
      if (Arg_Number = 2) then

         Remote_INET_Addr :=
                  POSIX.XTI.Internet.String_To_Internet_Address (Item);
         Text_IO.Put ("Will connect to: ");
         Text_IO.Put_Line (POSIX.To_String (
        (POSIX.XTI.Internet.Internet_Address_To_String (Remote_INET_Addr))));
      end if;
      Arg_Number := 2;
   exception
      when others =>
         Text_IO.Put_Line
                  ("POSIX.XTI.Internet.String_To_Internet_Address FAILED");
   end Parse;

   procedure For_Every_Item is new POSIX.For_Every_Item (Parse);

   package Int_IO is new Text_IO.Integer_IO (integer);
   use Int_IO;

   package Float_IO is new Text_IO.Float_IO (Float);
   use Float_IO;

   procedure Print_Option (Info : in     Protocol_Option;
                            Quit : in out Boolean) is
      Name      : POSIX.XTI.Option_Name;
      Level     : POSIX.XTI.Option_Level;
      Value     : POSIX.XTI.Option_Value;
      No_Delay  : POSIX.XTI.Internet.XTI_Option;
      Linger    : POSIX.XTI.Linger_Information;
      Keep_Alive : POSIX.XTI.Internet.Keep_Alive_Information;
      IP_Ops    : POSIX.XTI.Internet.IP_Option_List (0 .. 1023);
      Count     : Natural;
   begin
      Text_IO.Put ("Option ");
      Name  := Get_Name (Info);
      Level := Get_Level (Info);

      if Level = XTI_Protocol_Level then

         case Name is
            when POSIX.XTI.Enable_Debugging =>
               Text_IO.Put ("Enable_Debugging,       Value is ");
               Value := POSIX.XTI.Get_Value (Info);
               Int_IO.Put (integer (Value));
               Text_IO.New_Line;

            when POSIX.XTI.Linger_On_Close_If_Data_Present =>
               Text_IO.Put (
                     "Linger_On_Close_If_Data_Present, Linger on/off is ");
               Linger := POSIX.XTI.Get_Value (Info);
               if (POSIX.XTI.Get_Status (Linger) = POSIX.XTI.Linger_Off) then
                  Text_IO.Put_Line ("OFF");
               else
                  Text_IO.Put ("ON, Period is ");
                  if (POSIX.XTI.Period_Is_Unspecified (Linger)) then
                     Text_IO.Put_Line ("UNSPECIFIED");
                  elsif (POSIX.XTI.Period_Is_Infinite (Linger)) then
                     Text_IO.Put_Line ("INFINITE");
                  else
                     Int_IO.Put (integer (POSIX.XTI.Get_Period (Linger)));
                     Text_IO.Put_Line (" Seconds");
                  end if;
               end if;

            when POSIX.XTI.Receive_Buffer_Size =>
               Text_IO.Put ("Receive_Buffer_Size,    Value is ");
               Value := POSIX.XTI.Get_Value (Info);
               Int_IO.Put (integer (Value));
               Text_IO.Put_Line (" Bytes");

            when POSIX.XTI.Receive_Low_Water_Mark =>
               Text_IO.Put ("Receive_Low_Water_Mark, Value is ");
               Value := POSIX.XTI.Get_Value (Info);
               Int_IO.Put (integer (Value));
               Text_IO.Put_Line (" Bytes");

            when POSIX.XTI.Send_Buffer_Size =>
               Text_IO.Put ("Send_Buffer_Size,       Value is ");
               Value := POSIX.XTI.Get_Value (Info);
               Int_IO.Put (integer (Value));
               Text_IO.Put_Line (" Bytes");

            when POSIX.XTI.Send_Low_Water_Mark =>
               Text_IO.Put ("Send_Low_Water_Mark,    Value is ");
               Value := POSIX.XTI.Get_Value (Info);
               Int_IO.Put (integer (Value));
               Text_IO.Put_Line (" Bytes");
           
            when others =>
               Text_IO.Put_Line ("UNKNOWN XTI Level Option");
         end case;

      elsif Level = TCP_Level then
         case Name is
            when TCP_Keep_Alive_Interval =>
               Text_IO.Put ("TCP_Keep_Alive_Interval,  Status is ");
               Keep_Alive := POSIX.XTI.Internet.Get_Value (Info);
               if POSIX.XTI.Internet.Get_Status (Keep_Alive) =
                  POSIX.XTI.Internet.Keep_Alive_On then
                  Text_IO.Put ("ON, Timeout ");
                  Int_IO.Put (integer (
                  POSIX.XTI.Internet.Get_Keep_Alive_Timeout (Keep_Alive)));
               elsif POSIX.XTI.Internet.Get_Status (Keep_Alive) =
                  POSIX.XTI.Internet.Keep_Alive_Off then
                  Text_IO.Put ("Off");
               end if;
               Text_IO.New_Line;
            when TCP_Segment_Size_Maximum =>
               Text_IO.Put ("TCP_Segment_Size_Maximum, Value is ");
               Value := POSIX.XTI.Get_Value (Info);
               Int_IO.Put (integer (Value));
               Text_IO.Put (" Bytes");
               Text_IO.New_Line;
            when TCP_No_Delay =>
               Text_IO.Put ("TCP_No_Delay,             Option is ");
               No_Delay := POSIX.XTI.Internet.Get_Value (Info);
               if (No_Delay = POSIX.XTI.Internet.Enabled) then
                  Text_IO.Put ("         ENABLED");
               else
                  Text_IO.Put ("         DISABLED");
               end if;
               Text_IO.New_Line;
            when others =>
               Text_IO.Put_Line ("UNKNOWN TCP Level Option");
         end case;

      elsif Level = IP_Level then
         case Name is
            when IP_Options =>
               Text_IO.Put ("IP_Options,          Value is ");
               POSIX.XTI.Internet.Get_Value (Info, IP_Ops, Count);
               if Count > 0 then
                  for i in IP_Ops'First .. 
                                      (integer (IP_Ops'First) + Count)
                  loop
                     Int_IO.Put (integer (IP_Ops (i)));
                  end loop;
               else
                  Text_IO.Put ("NO IP OPTIONS");
               end if;
               Text_IO.New_Line;
            when IP_Type_Of_Service =>
               Text_IO.Put ("IP_Type_Of_Service,  Value is ");
               Value := POSIX.XTI.Get_Value (Info);
               Int_IO.Put (integer (Value));
               Text_IO.New_Line;
            when IP_Time_To_Live =>
               Text_IO.Put ("IP_Time_To_Live,     Value is ");
               Value := POSIX.XTI.Get_Value (Info);
               Int_IO.Put (integer (Value), Base => 16);
               Text_IO.New_Line;
            when IP_Reuse_Address =>
               Text_IO.Put ("IP_Reuse_Address,    Value is ");
               Value := POSIX.XTI.Get_Value (Info);
               Int_IO.Put (integer (Value));
               Text_IO.New_Line;
            when IP_Dont_Route =>
               Text_IO.Put ("IP_Dont_Route,       Value is ");
               Value := POSIX.XTI.Get_Value (Info);
               Int_IO.Put (integer (Value));
               Text_IO.New_Line;
            when IP_Permit_Broadcast =>
               Text_IO.Put ("IP_Permit_Broadcast, Value is ");
               Value := POSIX.XTI.Get_Value (Info);
               Int_IO.Put (integer (Value));
               Text_IO.New_Line;
            when others =>
               Text_IO.Put_Line ("UNKNOWN IP Level Option");
         end case;
      end if;
   end Print_Option;
   procedure For_Every_Item is new POSIX.XTI.For_Every_Item (Print_Option);

   Quit      : boolean;

begin

   For_Every_Item (POSIX.Process_Environment.Argument_List);

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

   ------------------------------------------------------------------------
   --
   --  Need to do this to set the netbuf parameters in the implementation
   --  Need to fix implemenation BUG.
   --
   POSIX.XTI.Internet.Set_Internet_Port (Response_Addr, 16#00#);
   ------------------------------------------------------------------------

   POSIX.XTI.Bind (Endpoint,
                   Response_Addr'access);
   Text_IO.Put_Line ("Completed Bind");

   Text_IO.Put ("   Port: ");
   Int_IO.Put (integer (POSIX.XTI.Internet.Get_Internet_Port (Response_Addr)));
   Text_IO.New_Line;

   Response_INET_Addr := POSIX.XTI.Internet.Get_Internet_Address
                                   (Response_Addr);

   Text_IO.Put ("Response Address is: ");
   Text_IO.Put (POSIX.To_String (
   (POSIX.XTI.Internet.Internet_Address_To_String (Response_INET_Addr))));
   Text_IO.New_Line;

   --
   --  Set the Port and Internet Address.  Note that the Remote_INET_Addr
   --  was set above by either using the default Loopback or Resolving
   --  the name in the second parameter of the Argument_List.
   --
   POSIX.XTI.Internet.Set_Internet_Port (Internet_Addr, 16#FF00#);
   POSIX.XTI.Internet.Set_Internet_Address (Internet_Addr, Remote_INET_Addr);

   POSIX.XTI.Set_Address (Connect_Send_Info, Internet_Addr);
   POSIX.XTI.Set_Address (Connect_Receive_Info, Response_Addr);

   Text_IO.Put_Line ("Calling Connect, expect to be REJECTED");

   begin
      POSIX.XTI.Connect (Endpoint, Connect_Send_Info);
      Text_IO.Put_Line ("Connected Clean, should have been REJECTED!!!");
   exception
      when POSIX.POSIX_Error =>
         if (POSIX.XTI.Look (Endpoint) =
              POSIX.XTI.Disconnect_Request_Received) then
            Text_IO.Put_Line ("Connect Rejected by Remote Node");
         else
            Text_IO.Put_Line ("Some other event");
         end if;
   end;

   delay 1.0;
   Text_IO.Put_Line ("Calling Connect Again");
   POSIX.XTI.Close (Endpoint);
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
   POSIX.XTI.Bind (Endpoint);
   POSIX.XTI.Internet.Set_Internet_Port (Internet_Addr, 16#FF00#);
   POSIX.XTI.Internet.Set_Internet_Address (Internet_Addr, Remote_INET_Addr);
   POSIX.XTI.Set_Address (Connect_Send_Info, Internet_Addr);
   POSIX.XTI.Connect (Endpoint, Connect_Send_Info);
   Text_IO.Put_Line ("Connected");

--   POSIX.XTI.Confirm_Connection (Endpoint, Connect_Receive_Info);
--   Text_IO.Put_Line ("Confirmed Connection");

   POSIX.XTI.Set_Buffer (Option_List, Option_Buffer);
--   POSIX.XTI.Set_Option_Buffer_Maximum_Length (Option_Buffer'Last,
--                                          Option_List);

   --
   --  Set the TCP Level options
   --

   --
   --  Set TCP No_Delay
   --
--   POSIX.XTI.Set_Level (Option, POSIX.XTI.Internet.TCP_Level);
--   POSIX.XTI.Set_Name (Option, POSIX.XTI.Internet.TCP_No_Delay);
--   POSIX.XTI.Internet.Set_Value (Option, POSIX.XTI.Internet.Enabled);

   POSIX.XTI.Internet.Set_Option (Option, 
                                  POSIX.XTI.Internet.TCP_Level,
                                  POSIX.XTI.Internet.TCP_No_Delay,
                                  POSIX.XTI.Internet.Enabled);

   POSIX.XTI.Append (Option_List, Option);

   POSIX.XTI.Manage_Options (Endpoint,
                             Option_List,
                             POSIX.XTI.Negotiate_Options,
                             Option_List'access,
                             Option_Result);
   Text_IO.Put ("The Result of Manage Options (Set TCP NODELAY) was ");
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
   --  Set the XTI Level Options
   --

   POSIX.XTI.Make_Empty (Option_List);

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

   --
   --  Set Linger Time
   --
--   POSIX.XTI.Set_Level (Option, POSIX.XTI.XTI_Protocol_Level);
--   POSIX.XTI.Set_Name (Option, POSIX.XTI.Linger_On_Close_If_Data_Present);
--   POSIX.XTI.Set_Status (Linger, POSIX.XTI.Linger_On);
--   POSIX.XTI.Set_Period (Linger, 10);
--   POSIX.XTI.Set_Value (Option, Linger);
   POSIX.XTI.Set_Option (Option, 
                         POSIX.XTI.XTI_Protocol_Level,
                         POSIX.XTI.Linger_On_Close_If_Data_Present,
                         Linger);

   POSIX.XTI.Append (Option_List, Option);

   Text_IO.Put_Line ("Added Options to Option_List, Calling Manage_Options");

   POSIX.XTI.Manage_Options (Endpoint,
                             Option_List,
                             POSIX.XTI.Negotiate_Options,
                             Option_List'access,
                             Option_Result);
   Text_IO.Put (
    "The Result of Manage Options (Set RCV & SND BUF and LINGER) was ");
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
   --  Let's Test the Get_Current_Options portion of the Manage_Options
   --  and print that out using the iterator.
   --

   POSIX.XTI.Make_Empty (Option_List);

   --  Get All Options
--   POSIX.XTI.Set_Level (Option, POSIX.XTI.XTI_Protocol_Level);
--   POSIX.XTI.Set_Name (Option, POSIX.XTI.All_Options);
--   POSIX.XTI.Set_Value (Option, Unspecified);
   POSIX.XTI.Set_Option (Option, 
                         POSIX.XTI.XTI_Protocol_Level,
                         POSIX.XTI.All_Options,
                         Unspecified);
   POSIX.XTI.Append (Option_List, Option);

   begin
      POSIX.XTI.Manage_Options (Endpoint,
                                Option_List,
                                POSIX.XTI.Get_Current_Options,
                                Option_List'access,
                                Option_Result);
      Text_IO.Put ("The Result of Manage Options (Get_Current_Options) was ");
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
      --  Let's get the number of options
      --
      Text_IO.Put ("There are ");
      Int_IO.Put (POSIX.XTI.Number_Of_Options (Option_List), 2);
      Text_IO.Put_Line (" XTI Level Options, They are:");
    
      --
      --  Iterate the Option List
      --
      For_Every_Item (Option_List);

      --
      --  Check out Get_Option
      --
      Text_IO.Put_Line ("Here are the Options again using Get_Option:");
      for i in 1 .. POSIX.XTI.Number_Of_Options (Option_List) loop
         POSIX.XTI.Get_Option (Option_List, i, Option);
         Quit := False;
         Print_Option (Option, Quit);
      end loop;

   exception
      when others =>
         Text_IO.Put_Line ("Got an error after Manage_Options");
   end;

   --
   --  Let's Test the Get_Current_Options for IP Level Options
   --

   POSIX.XTI.Make_Empty (Option_List);

   --  Get All IP Options
--   POSIX.XTI.Set_Level (Option, POSIX.XTI.Internet.IP_Level);
--   POSIX.XTI.Set_Name (Option, POSIX.XTI.All_Options);
--   POSIX.XTI.Set_Value (Option, Unspecified);
   POSIX.XTI.Set_Option (Option, 
                         POSIX.XTI.Internet.IP_Level,
                         POSIX.XTI.All_Options,
                         Unspecified);
   POSIX.XTI.Append (Option_List, Option);

   begin
      POSIX.XTI.Manage_Options (Endpoint,
                                Option_List,
                                POSIX.XTI.Get_Current_Options,
                                Option_List'access,
                                Option_Result);
      Text_IO.Put ("The Result of Manage Options (Get_Current_Options) was ");
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
      --  Let's get the number of options
      --
      Text_IO.Put ("There are ");
      Int_IO.Put (POSIX.XTI.Number_Of_Options (Option_List), 2);
      Text_IO.Put_Line (" IP Level Options, They are:");
    
      --
      --  Iterate the Option List
      --
      For_Every_Item (Option_List);

   exception
      when others =>
         raise;
--        Text_IO.Put_Line ("Got an error after Manage_Options");
   end;

   --
   --  Let's Test the Get_Current_Options for TCP Level Options
   --

   POSIX.XTI.Make_Empty (Option_List);

   --  Get All IP Options
--   POSIX.XTI.Set_Level (Option, POSIX.XTI.Internet.TCP_Level);
--   POSIX.XTI.Set_Name (Option, POSIX.XTI.All_Options);
--   POSIX.XTI.Set_Value (Option, Unspecified);
   POSIX.XTI.Set_Option (Option, 
                         POSIX.XTI.Internet.TCP_Level,
                         POSIX.XTI.All_Options,
                         Unspecified);
   POSIX.XTI.Append (Option_List, Option);

   begin
      POSIX.XTI.Manage_Options (Endpoint,
                                Option_List,
                                POSIX.XTI.Get_Current_Options,
                                Option_List'access,
                                Option_Result);
      Text_IO.Put ("The Result of Manage Options (Get_Current_Options) was ");
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
      --  Let's get the number of options
      --
      Text_IO.Put ("There are ");
      Int_IO.Put (POSIX.XTI.Number_Of_Options (Option_List), 2);
      Text_IO.Put_Line (" TCP Level Options, They are:");
    
      --
      --  Iterate the Option List
      --
      For_Every_Item (Option_List);

   exception
      when others =>
         Text_IO.Put_Line ("Got an error after Manage_Options");
   end;

   --
   --
   --  Set the data to a known pattern
   --
   Test_Buffer.int := 1234;
   Test_Buffer.flt := 1234.1234;
   for i in Test_Buffer.arr'First .. Test_Buffer.arr'Last loop
      Test_Buffer.arr (i) := POSIX.Octet (i);
   end loop;

   --
   --  Send data and wait for wrap back
   --
   POSIX.XTI.Send (Endpoint,
                   To_Buffer_Pointer (Test_Buffer'Access),
                   POSIX.IO_Count (Test_Buffer'Size / POSIX.Octet'Size),
                   Flags,
		   Bytes);

   if (Integer (Bytes) = (Test_Buffer'Size / POSIX.Octet'Size)) then
      --
      --  Wait for data to come back
      --
      POSIX.XTI.Receive 
            (Endpoint,
               To_Buffer_Pointer (Test_Buffer_Return'Access),
               POSIX.IO_Count (Test_Buffer_Return'Size / POSIX.Octet'Size),
               Bytes,
               Flags);
   else
      Text_IO.Put_Line ("Bad Send, didn't send all data");
   end if;

   if (Integer (Bytes) = (Test_Buffer'Size / POSIX.Octet'Size)) then
      --
      --  Check and make sure data is correct
      --
      if not (Test_Buffer.int = Test_Buffer_Return.int) then
       Text_IO.Put_Line ("Data Integer is not the same as sent!!!");
      end if;
      if not (Test_Buffer.flt = Test_Buffer_Return.flt) then
       Text_IO.Put_Line ("Data Float is not the same as sent!!!");
      end if;
      for i in Test_Buffer_Return.arr'First .. Test_Buffer_Return.arr'Last
      loop
         if not (Integer (Test_Buffer.arr (i)) = 
             Integer (Test_Buffer_Return.arr (i))) then
            Text_IO.Put ("Data is not the same as sent, was ");
            Int_IO.Put (Integer (Test_Buffer_Return.arr (i)));
            Text_IO.Put (", suppose to be ");
            Int_IO.Put (Integer (Test_Buffer.arr (i)));
            Text_IO.New_Line;
            exit;
         end if;
         if (i = Test_Buffer_Return.arr'Last) then
            Text_IO.Put_Line ("Data is good!!!!");
         end if;
      end loop;
   end if;

   delay 2.0;

   Text_IO.Put_Line ("Starting Send Timing...");
   Total := 0;
   
   Start_Time := Seconds (Clock);
   for i in 1 .. 100000 loop
      POSIX.XTI.Send (Endpoint,
                      To_Buffer_Pointer (Buffer (Buffer'First)'Access),
                      POSIX.IO_Count (Buffer'Last),
                      Flags,
		      Bytes);
      Total := POSIX.IO_Count (Integer (Total) + Integer (Bytes));
   end loop;
   End_Time := Seconds (Clock);

   Text_IO.Put ("Sent ");
   Int_IO.Put (integer (Total));
   Text_IO.Put (" bytes in ");
   Float_IO.Put (float (End_Time - Start_Time));
   Text_IO.Put (" seconds");
   Text_IO.New_Line;

   delay 2.0;
   Text_IO.Put_Line ("Sending Disconnect");
   POSIX.XTI.Send_Disconnect_Request (Endpoint);

   POSIX.XTI.Close (Endpoint);
   Text_IO.Put_Line ("Performed Close");

exception

   when POSIX.POSIX_Error =>
      if (POSIX.XTI.Look (Endpoint) =
          POSIX.XTI.Disconnect_Request_Received) then
         Text_IO.Put_Line ("Endpoint Closed by Remote Node");
      elsif (POSIX.XTI.Look (Endpoint) =
          POSIX.XTI.Connect_Request_Received) then
         Text_IO.Put_Line ("Connect Request Received");
      elsif (POSIX.XTI.Look (Endpoint) =
          POSIX.XTI.Connect_Response_Received) then
         Text_IO.Put_Line ("Connect Response Received");
      elsif (POSIX.XTI.Look (Endpoint) =
          POSIX.XTI.Error_In_Previously_Sent_Datagram) then
         Text_IO.Put_Line ("Error in Previously Sent Datagram");
      elsif (POSIX.XTI.Look (Endpoint) =
          POSIX.XTI.Expedited_Data_Received) then
         Text_IO.Put_Line ("Expedited Data Received");
      elsif (POSIX.XTI.Look (Endpoint) =
          POSIX.XTI.Normal_Data_Received) then
         Text_IO.Put_Line ("Normal Data Received");
      elsif (POSIX.XTI.Look (Endpoint) =
          POSIX.XTI.Orderly_Release_Request_Received) then
         Text_IO.Put_Line ("Orderly Release Request Received");
      else
         Text_IO.Put_Line ("Some other event");
      end if;

end Test_TCP_Connect;

------------------------------------------------------------------------------
--                                                                          --
--   POSIX Ada95 Bindings for Protocol Independent Interfaces (P1003.5c)    --
--                                                                          --
--                        P O S I X . X T I  . Internet                     --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Streams,
     POSIX.C,
     POSIX.Implementation,
     System,
     Text_IO,
     Unchecked_Conversion,
     System.Address_Image;

package body POSIX.XTI.Internet is

   use Ada.Streams,
       POSIX.C,
       POSIX.Implementation,
       POSIX.C.Sockets,
       POSIX.C.NetDB;

   type char_array is array (size_t range <>) of aliased char;

   function c_inet_addr (str : char_ptr) return POSIX.C.Sockets.in_addr_t;
   pragma Import (C, c_inet_addr, POSIX.C.Netinet.inet_addr_LINKNAME);
   function c_inet_ntoa (addr : System.Address)
              return char_ptr;
   pragma Import (C, c_inet_ntoa, "c_inet_ntoa");
--   pragma Import (C, c_inet_ntoa, POSIX.C.Netinet.inet_ntoa_LINKNAME);

   function To_char_ptr is new Unchecked_Conversion (System.Address, char_ptr);
   function To_Address is new Unchecked_Conversion (char_ptr, System.Address);
   function To_sockaddr_in is new Unchecked_Conversion
                               (char_ptr, POSIX.C.Sockets.sockaddr_in_ptr);
   function To_ptr is new Unchecked_Conversion
                               (System.Address, POSIX.C.XTI.t_bind_ptr);

   function To_in_addr_ptr is new Unchecked_Conversion
                               (System.Address, POSIX.C.Sockets.in_addr_ptr);

   function c_t_getprotaddr (fd           : int;
                             boundaddr    : POSIX.C.XTI.t_bind_ptr;
                             peeraddr     : POSIX.C.XTI.t_bind_ptr) return int;
   pragma Import (C, c_t_getprotaddr, POSIX.C.XTI.t_getprotaddr_LINKNAME);

   --  Internet support from <netdb.h>
   --  Note : using the thread safe versions of the following rather
   --         than the MT unsafe versions spec'd in dot1g
   function c_getnetbyname_r
      (name : char_ptr; result : netent_ptr; buffer : char_ptr;
       buflen : int)
     return netent_ptr;
   pragma Import (C, c_getnetbyname_r, POSIX.C.NetDB.getnetbyname_r_LINKNAME);
   function c_getnetbyaddr_r
      (net : unsigned; typ : int; result : netent_ptr;
       buffer : char_ptr; buflen : int)
     return netent_ptr;
   pragma Import (C, c_getnetbyaddr_r, POSIX.C.NetDB.getnetbyaddr_r_LINKNAME);
   function c_setnetent (stayopen : int) return int;
   pragma Import (C, c_setnetent, POSIX.C.NetDB.setnetent_LINKNAME);
   function c_endnetent return int;
   pragma Import (C, c_endnetent, POSIX.C.NetDB.endnetent_LINKNAME);
   function c_getprotobyname_r
      (name : char_ptr; result : protoent_ptr; buffer : char_ptr;
       buflen : int)
     return protoent_ptr;
   pragma Import (C, c_getprotobyname_r,
                  POSIX.C.NetDB.getprotobyname_r_LINKNAME);
   function c_getprotobynumber_r
      (proto : int; result : protoent_ptr; buffer : char_ptr; buflen : int)
     return protoent_ptr;
   pragma Import (C, c_getprotobynumber_r,
                 POSIX.C.NetDB.getprotobynumber_r_LINKNAME);
   function c_setprotoent (stayopen : int) return int;
   pragma Import (C, c_setprotoent, POSIX.C.NetDB.setprotoent_LINKNAME);
   function c_endprotoent return int;
   pragma Import (C, c_endprotoent, POSIX.C.NetDB.endprotoent_LINKNAME);

   package Integer_IO is new Text_IO.Integer_IO (integer);
   use Integer_IO;

   function Get_Value (Option_Item : Protocol_Option)
      return XTI_Option is

      type opthdr_and_data is record
         header : POSIX.C.XTI.struct_t_opthdr;
         data   : integer;
      end record;
      pragma Pack (opthdr_and_data);
      type opthdr_and_data_ptr is access opthdr_and_data;
      function To_opthdr_and_data_ptr is new Unchecked_Conversion
              (System.Address, opthdr_and_data_ptr);

   begin
      if (To_opthdr_and_data_ptr (Option_Item.C'Address).data
         = integer (POSIX.C.XTI.T_YES)) then
         return Enabled;
      elsif (To_opthdr_and_data_ptr (Option_Item.C'Address).data
         = integer (POSIX.C.XTI.T_NO)) then
         return Disabled;
      else
         return Disabled;
      end if;
   end Get_Value;

   procedure Set_Option
      (Option_Item : in out Protocol_Option;
       Level       : in     Option_Level;
       Name        : in     Option_Name;
       To          : in     XTI_Option) is

      type opthdr_and_data is record
         header : POSIX.C.XTI.struct_t_opthdr;
         data   : POSIX.XTI.Option_Value;
      end record;
      pragma Pack (opthdr_and_data);
      type opthdr_and_data_ptr is access opthdr_and_data;
      function To_opthdr_and_data_ptr is new Unchecked_Conversion
              (System.Address, opthdr_and_data_ptr);

   begin
      if (To = Enabled) then
         To_opthdr_and_data_ptr (Option_Item.C'Address).data
                                 := POSIX.XTI.Option_Value (POSIX.C.XTI.T_YES);
      else
         To_opthdr_and_data_ptr (Option_Item.C'Address).data
                                := POSIX.XTI.Option_Value (POSIX.C.XTI.T_NO);
      end if;
      Option_Item.C.len := (POSIX.C.XTI.struct_t_opthdr'Size / char'Size) +
                          (POSIX.XTI.Option_Value'Size / char'Size);
      Option_Item.C.level := unsigned_long (Level);
      Option_Item.C.name  := unsigned_long (Name);
   end Set_Option;

   function Get_Internet_Port (Name : Internet_XTI_Address)
      return Internet_Port is
   begin
      return Internet_Port (Name.sockaddr_in.sin_port);
   end Get_Internet_Port;

   procedure Set_Internet_Port
      (Name       : in out Internet_XTI_Address;
       Port_Value : in     Internet_Port) is
   begin
      Name.sockaddr_in.sin_port := POSIX.C.Sockets.in_port_t (Port_Value);
      Name.netbuf.maxlen := Name.sockaddr_in'Size / char'Size;
      Name.netbuf.len    := Name.sockaddr_in'Size / char'Size;
      Name.netbuf.buf    := To_char_ptr (Name.sockaddr_in'Address);

   end Set_Internet_Port;

   function Get_Internet_Address (Name : Internet_XTI_Address)
      return Internet_Address is
   begin
      return (C => (s_addr => Name.sockaddr_in.sin_addr.s_addr));
   end Get_Internet_Address;

   procedure Set_Internet_Address
      (Name          : in out Internet_XTI_Address;
       Address_Value : in     Internet_Address) is
   begin
      Name.sockaddr_in.sin_addr.s_addr := Address_Value.C.s_addr;
      Name.netbuf.maxlen := Name.sockaddr_in'Size / char'Size;
      Name.netbuf.len    := Name.sockaddr_in'Size / char'Size;
      Name.netbuf.buf    := To_char_ptr (Name.sockaddr_in'Address);
   end Set_Internet_Address;

   --------------------------------------
   --  Internetwork Support Functions  --
   --------------------------------------

   --  Internet Address Manipulation
   function c_htons (x : Internet_Port) return Internet_Port;
   pragma Import (C, c_htons, "c_htons");
--   function Host_To_Network_Byte_Order (Port : Internet_Port)
--     return Internet_Port is
--   begin
--      return c_htons (Port);
--   end Host_To_Network_Byte_Order;

   function c_ntohs (x : Internet_Port) return Internet_Port;
   pragma Import (C, c_ntohs, "c_ntohs");
--   function Network_To_Host_Byte_Order (Port : Internet_Port)
--     return Internet_Port is
--   begin
--      return c_ntohs (Port);
--   end Network_To_Host_Byte_Order;

   function c_htonl (x : POSIX.C.Sockets.in_addr_t)
                return POSIX.C.Sockets.in_addr_t;
   pragma Import (C, c_htonl, "c_htonl");
--   function Host_To_Network_Byte_Order (Address : Internet_Address)
--     return Internet_Address is
--   begin
--      return (C => (s_addr => c_htonl (Address.C.s_addr)));
--   end Host_To_Network_Byte_Order;

   function c_ntohl (x : POSIX.C.Sockets.in_addr_t)
                return POSIX.C.Sockets.in_addr_t;
   pragma Import (C, c_ntohl, "c_ntohl");
--   function Network_To_Host_Byte_Order (Address : Internet_Address)
--     return Internet_Address is
--   begin
--      return (C => (s_addr => c_ntohl (Address.C.s_addr)));
--   end Network_To_Host_Byte_Order;

   function String_To_Internet_Address (Address : POSIX.POSIX_String)
     return Internet_Address is
   begin
      return (C => (s_addr =>
          c_inet_addr (To_char_ptr (Address (Address'First)'Address))));
   end String_To_Internet_Address;

   function Is_Internet_Address (Address : POSIX.POSIX_String)
     return Boolean is
   begin
      if c_inet_addr
       (Address (Address'First)'Unchecked_Access) =
                 POSIX.C.Netinet.INADDR_NONE then
         return False;
      else
         return True;
      end if;
   end Is_Internet_Address;

   function Internet_Address_To_String (Address : Internet_Address)
     return POSIX.POSIX_String is
      temp_var : POSIX.C.Sockets.struct_in_addr;
   begin
      temp_var.s_addr := Address.C.s_addr;
      return POSIX.C.Form_POSIX_String
                     (c_inet_ntoa (Address.C.s_addr'Address));
   end Internet_Address_To_String;

   --  Network Database Functions
   function Get_Name (Info_Item : Network_Info)
     return POSIX.POSIX_String is
   begin
      return Form_POSIX_String (Info_Item.C.n_name);
   end Get_Name;

   procedure For_Every_Network_Alias (Info_Item : Network_Info) is
      next_alias : char_ptr_ptr;
      Quit : Boolean := False;
   begin
      next_alias := Info_Item.C.n_aliases;
      if next_alias /= null then
         while next_alias.all /= null loop
            Action (Form_POSIX_String (next_alias.all), Quit);
            exit when Quit;
            Advance (next_alias);
         end loop;
      end if;
   end For_Every_Network_Alias;

--   function Get_Address_Family (Info_Item : Network_Info)
--     return Address_Family is
--   begin
--      return Address_Family (Info_Item.C.n_addrtype);
--   end Get_Address_Family;

   function Get_Network_Number (Info_Item : Network_Info)
     return Network_Number is
   begin
      return Network_Number (Info_Item.C.n_net);
   end Get_Network_Number;

   function Get_Network_Info_By_Address
      (Number  : Network_Number;
       Family  : Protocol_Family;
       Storage : Database_Array_Pointer)
     return Network_Info is
      netent : aliased POSIX.C.NetDB.struct_netent;
      Result : POSIX.C.NetDB.netent_ptr;
   begin
      Result := c_getnetbyaddr_r
         (unsigned (Number), int (Family), netent'Unchecked_Access,
         To_char_ptr (Storage (Storage'First)'Address), Storage'Length);
      if Result /= null then
         return (C => netent);
      else
         return (C => (n_name => null,
                       n_aliases => null,
                       n_addrtype => 0,
                       n_net => 0));
      end if;
   end Get_Network_Info_By_Address;

   function Get_Network_Info_By_Name
      (Name : POSIX.POSIX_String;
       Storage : Database_Array_Pointer)
     return Network_Info is
      netent : aliased POSIX.C.NetDB.struct_netent;
      Result : POSIX.C.NetDB.netent_ptr;
   begin
      Result := c_getnetbyname_r
          (Name (Name'First)'Unchecked_Access, netent'Unchecked_Access,
           To_char_ptr (Storage (Storage'First)'Address), Storage'Length);
      if Result /= null then
         return (C => netent);
      else
         return (C => (n_name => null,
                       n_aliases => null,
                       n_addrtype => 0,
                       n_net => 0));
      end if;
   end Get_Network_Info_By_Name;

   procedure Open_Network_Database_Connection
      (Stay_Open : in Boolean) is
   begin
      if Stay_Open then
         Check (c_setnetent (1));
      else
         Check (c_setnetent (0));
      end if;
   end Open_Network_Database_Connection;

   procedure Close_Network_Database_Connection is
   begin
      Check (c_endnetent);
   end Close_Network_Database_Connection;

   --  Network Protocol Database Functions
   function Get_Name (Info_Item : Protocol_Info)
     return POSIX.POSIX_String is
   begin
      return Form_POSIX_String (Info_Item.C.p_name);
   end Get_Name;

   procedure For_Every_Protocol_Alias (Info_Item : Protocol_Info) is
      next_alias : char_ptr_ptr;
      Quit : Boolean := False;
   begin
      next_alias := Info_Item.C.p_aliases;
      if next_alias /= null then
         while next_alias.all /= null loop
            Action (Form_POSIX_String (next_alias.all), Quit);
            exit when Quit;
            Advance (next_alias);
         end loop;
      end if;
   end For_Every_Protocol_Alias;

   function Get_Family (Info_Item : Network_Info)
      return Protocol_Family is
   begin
      return Protocol_Family (Info_Item.C.n_addrtype);
   end Get_Family;

   function Get_Protocol_Number (Info_Item : Protocol_Info)
     return Protocol_Number is
   begin
      return Protocol_Number (Info_Item.C.p_proto);
   end Get_Protocol_Number;

   function Get_Protocol_Info_By_Number (Number : Protocol_Number)
     return Protocol_Info is
      protoent : aliased POSIX.C.NetDB.struct_protoent;
      buffer : char_array (0 .. 1023);
      Result : POSIX.C.NetDB.protoent_ptr;
   begin
      Result := c_getprotobynumber_r
         (int (Number), protoent'Unchecked_Access,
          buffer (buffer'First)'Unchecked_Access, buffer'Length);
      if Result /= null then
         return (C => protoent);
      else
         return (C => (p_name => null,
                     p_aliases => null,
                     p_proto => 0));
      end if;
   end Get_Protocol_Info_By_Number;

   function Get_Protocol_Info_By_Name (Name : POSIX.POSIX_String)
     return Protocol_Info is
      protoent : aliased POSIX.C.NetDB.struct_protoent;
      buffer : char_array (0 .. 1023);
      Result : POSIX.C.NetDB.protoent_ptr;
   begin
      Result := c_getprotobyname_r
           (Name (Name'First)'Unchecked_Access, protoent'Unchecked_Access,
             buffer (buffer'First)'Unchecked_Access, buffer'Length);
      if Result /= null then
         return (C => protoent);
      else
         return (C => (p_name => null,
                       p_aliases => null,
                       p_proto => 0));
      end if;
   end Get_Protocol_Info_By_Name;

   procedure Open_Protocol_Database_Connection
      (Stay_Open : in Boolean) is
   begin
      if Stay_Open then
         Check (c_setprotoent (1));
      else
         Check (c_setprotoent (0));
      end if;
   end Open_Protocol_Database_Connection;

   procedure Close_Protocol_Database_Connection is
   begin
      Check (c_endprotoent);
   end Close_Protocol_Database_Connection;

   function Get_Status (Info_Item : Keep_Alive_Info)
      return Keep_Alive_Status is
   begin
      if Info_Item.C.kp_onoff = POSIX.C.XTI.T_YES then
         return Keep_Alive_On;
      elsif Info_Item.C.kp_onoff = POSIX.C.XTI.T_NO then
         return Keep_Alive_Off;
      elsif Info_Item.C.kp_onoff = POSIX.C.XTI.T_GARBAGE then
         return Send_Garbage;
      else
         raise Constraint_Error;
      end if;
   end Get_Status;

   procedure Set_Status
      (Info_Item : in out Keep_Alive_Info;
       To        : in     Keep_Alive_Status) is
   begin
      if To = Keep_Alive_On then
         Info_Item.C.kp_onoff := POSIX.C.XTI.T_YES;
      elsif To = Keep_Alive_Off then
         Info_Item.C.kp_onoff := POSIX.C.XTI.T_NO;
      else
         Info_Item.C.kp_onoff := POSIX.C.XTI.T_GARBAGE;
      end if;
   end Set_Status;

   procedure Set_Keep_Alive_Interval_Default
      (Info_Item : in out Keep_Alive_Info) is
   begin
      Info_Item.C.kp_timeout := long (POSIX.C.XTI.T_UNSPEC);
   end Set_Keep_Alive_Interval_Default;

   procedure Set_Keep_Alive_Timeout
      (Info_Item : in out Keep_Alive_Info;
       Minutes   : in     Positive) is
   begin
      Info_Item.C.kp_timeout := long (Minutes);
   end Set_Keep_Alive_Timeout;

   function Get_Keep_Alive_Timeout
        (Info_Item : Keep_Alive_Info)
      return Positive is
   begin
      return Positive (Info_Item.C.kp_timeout);
   end Get_Keep_Alive_Timeout;

   function Get_Value (Option_Item : Protocol_Option)
      return Keep_Alive_Info is
      type opthdr_and_kpalive is record
         header : POSIX.C.XTI.struct_t_opthdr;
         data   : POSIX.C.XTI.struct_t_kpalive;
      end record;
      pragma Pack (opthdr_and_kpalive);
      type opthdr_and_kpalive_ptr is access opthdr_and_kpalive;
      function To_opthdr_and_kpalive_ptr is new Unchecked_Conversion
              (System.Address, opthdr_and_kpalive_ptr);
      function To_Option_Name is new Unchecked_Conversion
               (unsigned_long, Option_Name);
   begin

      if (To_Option_Name (Option_Item.C.name) =
        POSIX.XTI.Internet.TCP_Keep_Alive_Interval) then
         return (C => (To_opthdr_and_kpalive_ptr
                          (Option_Item.C'Address).data));
      else
         --  Not a linger Option, raise Operation_Not_Permitted
         Raise_POSIX_Error (Operation_Not_Permitted);
         --  Fake return so we don't get compiler warning
         return (C => (To_opthdr_and_kpalive_ptr
                           (Option_Item.C'Address).data));
      end if;
   end Get_Value;

   procedure Set_Option
      (Option_Item : in out  Protocol_Option;
       Level       : in      Option_Level;
       Name        : in      Option_Name;
       Value       : in      Keep_Alive_Info) is
      type opthdr_and_kpalive is record
         header : POSIX.C.XTI.struct_t_opthdr;
         data   : POSIX.C.XTI.struct_t_kpalive;
      end record;
      pragma Pack (opthdr_and_kpalive);
      type opthdr_and_kpalive_ptr is access opthdr_and_kpalive;
      function To_opthdr_and_kpalive_ptr is new Unchecked_Conversion
              (System.Address, opthdr_and_kpalive_ptr);
   begin
      To_opthdr_and_kpalive_ptr (Option_Item.C'Address).data
           := Value.C;
      Option_Item.C.len := (POSIX.C.XTI.struct_t_opthdr'Size / char'Size) +
              (POSIX.C.XTI.struct_t_kpalive'Size / char'Size);
      Option_Item.C.level := unsigned_long (Level);
      Option_Item.C.name  := unsigned_long (Name);
   end Set_Option;

   procedure Get_Value (Option_Item : in  Protocol_Option;
                        IP_Option   : out IP_Option_List;
                        Count       : out Natural) is
      list_size : Natural :=
         Natural (Option_Item.C.len -
                   (POSIX.C.XTI.struct_t_opthdr'Size / char'Size));
      type opthdr_and_data is record
         header : POSIX.C.XTI.struct_t_opthdr;
         data   : IP_Option_List (0 .. list_size);
      end record;
      pragma Pack (opthdr_and_data);
      type opthdr_and_data_ptr is access opthdr_and_data;
      function To_opthdr_and_data_ptr is new Unchecked_Conversion
              (System.Address, opthdr_and_data_ptr);
   begin
      list_size :=
         Natural (Option_Item.C.len -
                     (POSIX.C.XTI.struct_t_opthdr'Size / char'Size));
      if list_size > 0 then
         IP_Option (IP_Option'First .. (IP_Option'First + (list_size - 1)))
          := To_opthdr_and_data_ptr (Option_Item.C'Address).data;
      end if;
      Count := list_size;
   end Get_Value;

   procedure Set_Option
      (Option_Item : in out Protocol_Option;
       Level       : in     Option_Level;
       Name        : in     Option_Name;
       To          : in     IP_Option_List) is
      type opthdr_and_data is record
         header : POSIX.C.XTI.struct_t_opthdr;
         data   : IP_Option_List (To'First .. To'Last);
      end record;
      pragma Pack (opthdr_and_data);
      type opthdr_and_data_ptr is access opthdr_and_data;
      function To_opthdr_and_data_ptr is new Unchecked_Conversion
              (System.Address, opthdr_and_data_ptr);
   begin
      To_opthdr_and_data_ptr (Option_Item.C'Address).data := To;
      Option_Item.C.len := (POSIX.C.XTI.struct_t_opthdr'Size / char'Size) +
              (To'Size / char'Size);
      Option_Item.C.level := unsigned_long (Level);
      Option_Item.C.name  := unsigned_long (Name);
   end Set_Option;

   function Get_Value (Option_Item : Protocol_Option)
      return IP_Service_Type is
   begin
      Raise_POSIX_Error (Operation_Not_Implemented);
      return Normal;
   end Get_Value;

   function Get_Value (Option_Item : Protocol_Option)
      return IP_Precedence_Level is
   begin
      Raise_POSIX_Error (Operation_Not_Implemented);
      return Routine;
   end Get_Value;

   procedure Set_Option
      (Option_Item : in out Protocol_Option;
       Level       : in     Option_Level;
       Name        : in     Option_Name;
       Service     : in     IP_Service_Type;
       Precedence  : in     IP_Precedence_Level) is
      type opthdr_and_data is record
         header : POSIX.C.XTI.struct_t_opthdr;
         data   : integer;
      end record;
      pragma Pack (opthdr_and_data);
      type opthdr_and_data_ptr is access opthdr_and_data;
      function To_opthdr_and_data_ptr is new Unchecked_Conversion
              (System.Address, opthdr_and_data_ptr);
   begin
      Raise_POSIX_Error (Operation_Not_Implemented);
      Option_Item.C.level := unsigned_long (Level);
      Option_Item.C.name  := unsigned_long (Name);
   end Set_Option;

   --
   --  POSIX.XTI protocol specific functions
   --

   --  This will not work because the dummy.netbuf.buf will overwrite
   --  the application's ????.netbuf.buf which, when the next call to
   --  the POSIX.XTI interface, will cause a Memory Error.  This would
   --  only work if the XTI_Address were an object of some sort with
   --  a life and the Internet_XTI_Address was mearly a pointer to that
   --  object. Suggested change or delete:
   --   procedure Get_Address (Info_Item : Binding_Info;
   --                          Address   : in out Internet_XTI_Address) is

--   function Get_Address (Info_Item : Binding_Info)
--       return Internet_XTI_Address is

--      dummy : Internet_XTI_Address;
--      addr_ptr : POSIX.C.Sockets.sockaddr_in_ptr;
--   begin

--      addr_ptr := To_sockaddr_in (Info_Item.C.addr.buf);
--      dummy.sockaddr_in.sin_port := addr_ptr.sin_port;
--      dummy.sockaddr_in.sin_addr := addr_ptr.sin_addr;
--      dummy.sockaddr_in.sin_family := addr_ptr.sin_family;
--      dummy.sockaddr_in.sin_zero   := addr_ptr.sin_zero;
--
--      dummy.netbuf.len             := Info_Item.C.addr.len;
--      dummy.netbuf.maxlen          := Info_Item.C.addr.maxlen;
      --  .buf is set in the routines that use the XTI_Address

--      return dummy;
--   end Get_Address;

   --  Bussiere Suggested changes

--   procedure Get_Address (Info_Item : Unit_Data;
--                          Address   : in out Internet_XTI_Address) is
--      addr_ptr : POSIX.C.Sockets.sockaddr_in_ptr;
--   begin

--      addr_ptr := To_sockaddr_in (Info_Item.C.addr.buf);
--      Address.sockaddr_in.sin_port := addr_ptr.sin_port;
--      Address.sockaddr_in.sin_addr := addr_ptr.sin_addr;
--      Address.sockaddr_in.sin_family := addr_ptr.sin_family;
--      Address.sockaddr_in.sin_zero   := addr_ptr.sin_zero;

--      Address.netbuf.len             := Info_Item.C.addr.len;
--      Address.netbuf.maxlen          := Info_Item.C.addr.maxlen;
      --  .buf is set in the routines that use the XTI_Address

--   end Get_Address;

--   procedure Get_Address (Info_Item : Unit_Data_Error;
--                          Address   : in out Internet_XTI_Address) is
--      addr_ptr : POSIX.C.Sockets.sockaddr_in_ptr;
--   begin

--      addr_ptr := To_sockaddr_in (Info_Item.C.addr.buf);
--      Address.sockaddr_in.sin_port := addr_ptr.sin_port;
--      Address.sockaddr_in.sin_addr := addr_ptr.sin_addr;
--      Address.sockaddr_in.sin_family := addr_ptr.sin_family;
--      Address.sockaddr_in.sin_zero   := addr_ptr.sin_zero;

--      Address.netbuf.len             := Info_Item.C.addr.len;
--      Address.netbuf.maxlen          := Info_Item.C.addr.maxlen;
      --  .buf is set in the routines that use the XTI_Address

--   end Get_Address;

   procedure Get_Address (Info_Item : Connection_Info;
                          Address   : in out Internet_XTI_Address) is
      addr_ptr : POSIX.C.Sockets.sockaddr_in_ptr;
   begin

      addr_ptr := To_sockaddr_in (Info_Item.C.addr.buf);
      Address.sockaddr_in.sin_port := addr_ptr.sin_port;
      Address.sockaddr_in.sin_addr := addr_ptr.sin_addr;
      Address.sockaddr_in.sin_family := addr_ptr.sin_family;
      Address.sockaddr_in.sin_zero   := addr_ptr.sin_zero;

      Address.netbuf.len             := Info_Item.C.addr.len;
      Address.netbuf.maxlen          := Info_Item.C.addr.maxlen;
      --  .buf is set in the routines that use the XTI_Address

   end Get_Address;

   function To_XTI_Address is new Unchecked_Conversion
      (Internet_XTI_Address_Pointer, XTI_Address_Pointer);
   function "+" (Ptr : Internet_XTI_Address_Pointer)
     return XTI_Address_Pointer is
   begin
      Ptr.netbuf.maxlen := Ptr.sockaddr_in'Size / char'Size;
      Ptr.netbuf.len    := Ptr.sockaddr_in'Size / char'Size;
      Ptr.netbuf.buf    := To_char_ptr (Ptr.sockaddr_in'Address);
      return To_XTI_Address (Ptr);
   end "+";

   function To_Internet_XTI_Address is new Unchecked_Conversion
      (XTI_Address_Pointer, Internet_XTI_Address_Pointer);
   function "+" (Ptr : XTI_Address_Pointer)
     return Internet_XTI_Address_Pointer is
      in_XTI_Addr_ptr : Internet_XTI_Address_Pointer;
   begin
      in_XTI_Addr_ptr := To_Internet_XTI_Address (Ptr);
      if in_XTI_Addr_ptr.sockaddr_in.sin_family = POSIX.C.Sockets.AF_INET then
         return in_XTI_Addr_ptr;
      else
         --  Need to find out exact error ??
         return null;
         --  raise POSIX.Operation_Not_Implemented;
      end if;
   end "+";

   function Is_Internet_XTI_Address (Ptr : XTI_Address_Pointer)
     return Boolean is
      in_XTI_Addr_ptr : Internet_XTI_Address_Pointer;
   begin
      if in_XTI_Addr_ptr.sockaddr_in.sin_family = POSIX.C.Sockets.AF_INET then
         return true;
      else
         return false;
      end if;
   end Is_Internet_XTI_Address;

end POSIX.XTI.Internet;

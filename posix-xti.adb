------------------------------------------------------------------------------
--                                                                          --
--   POSIX Ada95 Bindings for Protocol Independent Interfaces (P1003.5c)    --
--                                                                          --
--                             P O S I X . X T I                            --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                                                                          --
--  This file is part of an implementation of an Ada95 API for the sockets  --
--  and network support services found in P1003.1g -- Protocol Independent  --
--  Interfaces.  It is integrated with the  FSU Implementation of POSIX.5b  --
--  (FLORIST), an Ada API for  POSIX OS services for use with the GNAT Ada  --
--  compiler and the FSU Gnu Ada Runtime Library (GNARL).                   --
--                                                                          --
--  This package specification contains some text extracted from  IEEE STD  --
--  1003.5: 1990, Information Technology --  POSIX Ada Language Interfaces  --
--  Part 1:  Binding for System Application Program Interface,  as amended  --
--  by IEEE STD 1003.5b: 1996,  Amendment 1: Realtime  Extensions,  and as  --
--  further amended by IEEE Draft STD 1003.5c: 1997, Amendment 2: Protocol  --
--  Independent Interfaces,  copyright 1997 by the Institute of Electrical  --
--  and Electronics Engineers, Inc.                                         --
--                                                                          --
--  The package specifications in the IEEE standards cited above represent  --
--  only a  portion  of  the  documents  and  are  not  to  be interpreted  --
--  outside the context  of  the documents.  The standards must be used in  --
--  conjunction  with  the  package   specifications  in  order  to  claim  --
--  conformance.   The IEEE takes no responsibility for and will assume no  --
--  liability for damages resulting from the reader's misinterpretation of  --
--  said  information resulting from its out-of-context nature.   To order  --
--  copies of the IEEE standards,  please contact the  IEEE Service Center  --
--  at 445 Hoes Lane, PO Box 1331, Piscataway, NJ 08855-1331; via phone at  --
--  1-800-678-IEEE, 908-981-1393; or via fax at 908-981-9667.               --
--                                                                          --
--  These  package  specifications are  distributed in  the hope that they  --
--  will  be useful, but  WITHOUT  ANY  WARRANTY; without even the implied  --
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.        --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Streams,
     Ada.IO_Exceptions,
     POSIX,
     POSIX.C,
     POSIX.Implementation,
     POSIX.IO,
     Text_IO,
     System,
     System.Address_Image,
     System.Storage_Elements,
     Unchecked_Conversion;
package body POSIX.XTI is

   use Ada.Streams;
   use POSIX.C;
   use POSIX.C.XTI;
   use POSIX.Implementation;
   use System.Storage_Elements;

   package Integer_IO is new Text_IO.Integer_IO (integer);
   use Integer_IO;

   --  unchecked conversions

   function To_ptr is new
     Unchecked_Conversion (System.Address, POSIX.C.XTI.t_call_ptr);
   function To_ptr is new
     Unchecked_Conversion (System.Address, POSIX.C.XTI.t_discon_ptr);
   function To_ptr is new
     Unchecked_Conversion (System.Address, POSIX.C.XTI.t_bind_ptr);
   function To_ptr is new
     Unchecked_Conversion (System.Address, POSIX.C.XTI.t_unitdata_ptr);
   function To_iovec_ptr is new
     Unchecked_Conversion (System.Address, POSIX.C.XTI.t_iovec_ptr);
   function To_ptr is new
     Unchecked_Conversion (System.Address, POSIX.C.XTI.t_info_ptr);
   function To_ptr is new
     Unchecked_Conversion (System.Address, POSIX.C.XTI.t_optmgmt_ptr);
   function To_ptr is new
     Unchecked_Conversion (System.Address, POSIX.C.XTI.t_uderr_ptr);
   function To_int_ptr is new Unchecked_Conversion (System.Address, int_ptr);

   function To_int is new Unchecked_Conversion (XTI_Flags, int);
   function To_int is new Unchecked_Conversion (POSIX.IO.Open_Option_Set, int);
   function To_long is new Unchecked_Conversion (XTI_Flags, long);
   function To_XTI_Flags is new Unchecked_Conversion (long, XTI_Flags);
   function To_XTI_Events is new Unchecked_Conversion (int, XTI_Events);
   function To_CP_Flags is new Unchecked_Conversion (long, CP_Flags);
   function To_Option_Name is new
     Unchecked_Conversion (unsigned_long, Option_Name);

   function To_Address is new Unchecked_Conversion (char_ptr, System.Address);
   function To_Address is new
     Unchecked_Conversion (POSIX.C.XTI.t_opthdr_ptr, System.Address);
   function To_char_ptr is new Unchecked_Conversion (System.Address, char_ptr);
   function To_opthdr_ptr is new
     Unchecked_Conversion (char_ptr, POSIX.C.XTI.t_opthdr_ptr);

   --------------------------------
   --  The C Interface routines  --
   --------------------------------

   function Fetch_T_Errno return Error_Code;
   pragma Import (C, Fetch_T_Errno, "fetch_t_errno");

   function c_t_accept
     (fd     : int;
      resfd  : int;
      t_call : POSIX.C.XTI.t_call_ptr) return int;
   pragma Import (C, c_t_accept, POSIX.C.XTI.t_accept_LINKNAME);

   function c_t_alloc
     (fd          : int;
      struct_type : int;
      fields      : int) return int;
   pragma Import (C, c_t_alloc, POSIX.C.XTI.t_alloc_LINKNAME);

   function c_t_bind
     (fd  : int;
      req : POSIX.C.XTI.t_bind_ptr;
      ret : POSIX.C.XTI.t_bind_ptr) return int;
   pragma Import (C, c_t_bind, POSIX.C.XTI.t_bind_LINKNAME);

   function c_t_close (fd : int) return int;
   pragma Import (C, c_t_close, POSIX.C.XTI.t_close_LINKNAME);

   function c_t_connect
     (fd      : int;
      sndcall : POSIX.C.XTI.t_call_ptr;
      rcvcall : POSIX.C.XTI.t_call_ptr) return int;
   pragma Import (C, c_t_connect, POSIX.C.XTI.t_connect_LINKNAME);

   function c_t_error (errmsg : char_ptr) return int;
   pragma Import (C, c_t_error, POSIX.C.XTI.t_error_LINKNAME);

   function c_t_free
     (ptr         : char_ptr;
      struct_type : int) return int;
   pragma Import (C, c_t_free, POSIX.C.XTI.t_free_LINKNAME);

   function c_t_getinfo
     (fd   : int;
      info : POSIX.C.XTI.t_info_ptr) return int;
   pragma Import (C, c_t_getinfo, POSIX.C.XTI.t_getinfo_LINKNAME);

   function c_t_getprotaddr
     (fd        : int;
      boundaddr : POSIX.C.XTI.t_bind_ptr;
      peeraddr  : POSIX.C.XTI.t_bind_ptr) return int;
   pragma Import (C, c_t_getprotaddr, POSIX.C.XTI.t_getprotaddr_LINKNAME);

   function c_t_getstate (fd : int) return int;
   pragma Import (C, c_t_getstate, POSIX.C.XTI.t_getstate_LINKNAME);

   function c_t_listen
     (fd   : int;
      call : POSIX.C.XTI.t_call_ptr) return int;
   pragma Import (C, c_t_listen, POSIX.C.XTI.t_listen_LINKNAME);

   function c_t_look (fd : int) return int;
   pragma Import (C, c_t_look, POSIX.C.XTI.t_look_LINKNAME);

   function c_t_open
     (name  : char_ptr;
      oflag : int;
      info  : POSIX.C.XTI.t_info_ptr) return int;
   pragma Import (C, c_t_open, POSIX.C.XTI.t_open_LINKNAME);

   function c_t_optmgmt
     (fd  : int;
      req : POSIX.C.XTI.t_optmgmt_ptr;
      ret : POSIX.C.XTI.t_optmgmt_ptr) return int;
   pragma Import (C, c_t_optmgmt, POSIX.C.XTI.t_optmgmt_LINKNAME);

   function c_t_rcv
     (fd     : int;
      buf    : char_ptr;
      nbytes : unsigned;
      flags  : int_ptr) return int;
   pragma Import (C, c_t_rcv, POSIX.C.XTI.t_rcv_LINKNAME);

   function c_t_rcvconnect
     (fd   : int;
      call : POSIX.C.XTI.t_call_ptr) return int;
   pragma Import (C, c_t_rcvconnect, POSIX.C.XTI.t_rcvconnect_LINKNAME);

   function c_t_rcvdis
     (fd     : int;
      discon : POSIX.C.XTI.t_discon_ptr) return int;
   pragma Import (C, c_t_rcvdis, POSIX.C.XTI.t_rcvdis_LINKNAME);

   function c_t_rcvrel (fd : int) return int;
   pragma Import (C, c_t_rcvrel, POSIX.C.XTI.t_rcvrel_LINKNAME);

   function c_t_rcvreldata
     (fd     : int;
      discon : POSIX.C.XTI.t_discon_ptr)
      return int;
   pragma Import (C, c_t_rcvreldata, POSIX.C.XTI.t_rcvreldata_LINKNAME);

   function c_t_rcvudata
     (fd       : int;
      unitdata : POSIX.C.XTI.t_unitdata_ptr;
      flags    : int_ptr) return int;
   pragma Import (C, c_t_rcvudata, POSIX.C.XTI.t_rcvudata_LINKNAME);

   function c_t_rcvuderr
     (fd    : int;
      uderr : POSIX.C.XTI.t_uderr_ptr)
      return int;
   pragma Import (C, c_t_rcvuderr, POSIX.C.XTI.t_rcvuderr_LINKNAME);

   function c_t_rcvv
     (fd       : int;
      iov      : POSIX.C.XTI.t_iovec_ptr;
      iovcount : unsigned;
      flags    : int_ptr) return int;
   pragma Import (C, c_t_rcvv, POSIX.C.XTI.t_rcvv_LINKNAME);

   function c_t_rcvvudata
     (fd       : int;
      unitdata : POSIX.C.XTI.t_unitdata_ptr;
      iov      : POSIX.C.XTI.t_iovec_ptr;
      iovcount : unsigned;
      flags    : int_ptr) return int;
   pragma Import (C, c_t_rcvvudata, POSIX.C.XTI.t_rcvvudata_LINKNAME);

   function c_t_snd
     (fd     : int;
      buf    : char_ptr;
      nbytes : unsigned;
      flags  : int) return int;
   pragma Import (C, c_t_snd, POSIX.C.XTI.t_snd_LINKNAME);

   function c_t_snddis
     (fd   : int;
      call : POSIX.C.XTI.t_call_ptr) return int;
   pragma Import (C, c_t_snddis, POSIX.C.XTI.t_snddis_LINKNAME);

   function c_t_sndrel (fd : int) return int;
   pragma Import (C, c_t_sndrel, POSIX.C.XTI.t_sndrel_LINKNAME);

   function c_t_sndreldata
     (fd     : int;
      discon : POSIX.C.XTI.t_discon_ptr) return int;
   pragma Import (C, c_t_sndreldata, POSIX.C.XTI.t_sndreldata_LINKNAME);

   function c_t_sndudata
     (fd       : int;
      unitdata : POSIX.C.XTI.t_unitdata_ptr) return int;
   pragma Import (C, c_t_sndudata, POSIX.C.XTI.t_sndudata_LINKNAME);

   function c_t_sndv
     (fd       : int;
      iov      : POSIX.C.XTI.t_iovec_ptr;
      iovcount : unsigned;
      flags    : int) return int;
   pragma Import (C, c_t_sndv, POSIX.C.XTI.t_sndv_LINKNAME);

   function c_t_sndvudata
     (fd       : int;
      unitdata : POSIX.C.XTI.t_unitdata_ptr;
      iov      : POSIX.C.XTI.t_iovec_ptr;
      iovcount : int) return int;
   pragma Import (C, c_t_sndvudata, POSIX.C.XTI.t_sndvudata_LINKNAME);

   function c_t_strerror (errnum : int) return char_ptr;
   pragma Import (C, c_t_strerror, POSIX.C.XTI.t_strerror_LINKNAME);

   function c_t_sync (fd : int) return int;
   pragma Import (C, c_t_sync, POSIX.C.XTI.t_sync_LINKNAME);

   function c_t_unbind (fd : int) return int;
   pragma Import (C, c_t_unbind, POSIX.C.XTI.t_unbind_LINKNAME);

   function c_OPT_NEXTHDR
     (pbuf    :   char_ptr;
      buflen  :   unsigned_int;
      poption :   POSIX.C.XTI.t_opthdr_ptr)
      return POSIX.C.XTI.t_opthdr_ptr;
   pragma Import (C, c_OPT_NEXTHDR, "c_OPT_NEXTHDR");

   ------------------------
   --  XTI Ada routines  --
   ------------------------

   function Protocol_Addresses_Are_Valid
     (Info_Item : Communications_Provider_Info) return Boolean is
   begin
      if (Info_Item.C.addr > 0) then
         return True;
      else
         return False;
      end if;
   end Protocol_Addresses_Are_Valid;

   function Get_Max_Size_Protocol_Address
     (Info_Item : Communications_Provider_Info) return Positive is
   begin
      return Positive (Info_Item.C.addr);
   end Get_Max_Size_Protocol_Address;

   function Protocol_Options_Are_Valid
     (Info_Item : Communications_Provider_Info) return Boolean is
   begin
      if (Info_Item.C.options > 0) then
         return True;
      else
         return False;
      end if;
   end Protocol_Options_Are_Valid;

   function Get_Max_Size_Protocol_Options
     (Info_Item : Communications_Provider_Info) return Positive is
   begin
      return Positive (Info_Item.C.options);
   end Get_Max_Size_Protocol_Options;

   function SDU_Is_Supported
     (Info_Item : Communications_Provider_Info) return Boolean is
   begin
      if (Info_Item.C.tsdu = POSIX.C.XTI.T_INVALID) then
         return False;
      elsif (Info_Item.C.tsdu = POSIX.C.XTI.T_NULL) then
         return False;
      elsif (Info_Item.C.tsdu > 0) then
         return True;
      else
         return False;
      end if;
   end SDU_Is_Supported;

   function SDU_Is_Infinite (Info_Item : Communications_Provider_Info)
     return Boolean is
   begin
      if (Info_Item.C.tsdu = POSIX.C.XTI.T_INFINITE) then
         return True;
      else
         return False;
      end if;
   end SDU_Is_Infinite;

   function SDU_Is_Valid (Info_Item : Communications_Provider_Info)
     return Boolean is
   begin
      if (Info_Item.C.tsdu = POSIX.C.XTI.T_INVALID) then
         return False;
      else
         return True;
      end if;
   end SDU_Is_Valid;

   function Get_Max_Size_SDU (Info_Item : Communications_Provider_Info)
     return Positive is
   begin
      return Positive (Info_Item.C.tsdu);
   end Get_Max_Size_SDU;

   function SEDU_Is_Supported (Info_Item : Communications_Provider_Info)
     return Boolean is
   begin
      if (Info_Item.C.etsdu = POSIX.C.XTI.T_INVALID) then
         return False;
      elsif (Info_Item.C.etsdu = POSIX.C.XTI.T_NULL) then
         return False;
      elsif (Info_Item.C.etsdu > 0) then
         return True;
      else
         return False;
      end if;
   end SEDU_Is_Supported;

   function SEDU_Is_Infinite (Info_Item : Communications_Provider_Info)
     return Boolean is
   begin
      if (Info_Item.C.etsdu = POSIX.C.XTI.T_INFINITE) then
         return True;
      else
         return False;
      end if;
   end SEDU_Is_Infinite;

   function SEDU_Is_Valid (Info_Item : Communications_Provider_Info)
     return Boolean is
   begin
      if (Info_Item.C.etsdu = POSIX.C.XTI.T_INVALID) then
         return False;
      else
         return True;
      end if;
   end SEDU_Is_Valid;

   function Get_Max_Size_SEDU (Info_Item : Communications_Provider_Info)
     return Positive is
   begin
      return Positive (Info_Item.C.etsdu);
   end Get_Max_Size_SEDU;

   function Connect_Data_Is_Valid (Info_Item : Communications_Provider_Info)
     return Boolean is
   begin
      if (Info_Item.C.connect < 0) then
         return False;
      else
         return True;
      end if;
   end Connect_Data_Is_Valid;

   function Get_Max_Size_Connect_Data
     (Info_Item : Communications_Provider_Info) return Positive is
   begin
      return Positive (Info_Item.C.connect);
   end Get_Max_Size_Connect_Data;

   function Disconnect_Data_Is_Valid (Info_Item : Communications_Provider_Info)
     return Boolean is
   begin
      if (Info_Item.C.discon < 0) then
         return False;
      else
         return True;
      end if;
   end Disconnect_Data_Is_Valid;

   function Get_Max_Size_Disconnect_Data
     (Info_Item : Communications_Provider_Info) return Positive is
   begin
      return Positive (Info_Item.C.discon);
   end Get_Max_Size_Disconnect_Data;

   function Get_CP_Flags (Info_Item : Communications_Provider_Info)
     return CP_Flags is
   begin
--  ??? Used to be the other way around, but this didn't compile on Solaris
--  (flags not defined).
#  if TLI then
      return To_CP_Flags (Info_Item.C.flags);
#  else
      return CP_Flags (POSIX.Empty_Set);
#  end if;
   end Get_CP_Flags;

   function Get_Service_Type (Info_Item : Communications_Provider_Info)
     return Service_Type is
   begin
      return Service_Type (Info_Item.C.servtype);
   end Get_Service_Type;

   function Get_Status (Item : Linger_Info) return Linger_Option is
   begin
      if (Item.C.l_onoff = POSIX.C.XTI.T_NO) then
         return Linger_Off;
      else
         return Linger_On;
      end if;
   end Get_Status;

   procedure Set_Status
     (Item   : in out Linger_Info;
      Linger : in     Linger_Option) is
   begin
      if (Linger = Linger_Off) then
         Item.C.l_onoff := POSIX.C.XTI.T_NO;
      else
         Item.C.l_onoff := POSIX.C.XTI.T_YES;
      end if;
   end Set_Status;

   function Period_Is_Infinite (Item : Linger_Info) return Boolean is
   begin
      if (Item.C.l_linger = POSIX.C.XTI.T_INFINITE) then
         return True;
      else
         return False;
      end if;
   end Period_Is_Infinite;

   function Period_Is_Unspecified (Item : Linger_Info) return Boolean is
   begin
      if (Item.C.l_linger = POSIX.C.XTI.T_UNSPEC) then
         return True;
      else
         return False;
      end if;
   end Period_Is_Unspecified;

   function Get_Period (Item : Linger_Info) return Linger_Time is
   begin
      return Linger_Time (Item.C.l_linger);
   end Get_Period;

   procedure Set_Period_Infinite (Item : in out Linger_Info) is
   begin
      Item.C.l_linger := POSIX.C.XTI.T_INFINITE;
   end Set_Period_Infinite;

   procedure Set_Period_Unspecified (Item : in out Linger_Info) is
   begin
      Item.C.l_linger := POSIX.C.XTI.T_UNSPEC;
   end Set_Period_Unspecified;

   procedure Set_Period
     (Item : in out Linger_Info;
      Time : in     Linger_Time) is
   begin
      Item.C.l_linger := long (Time);
   end Set_Period;

   function Get_Level (Option_Item : Protocol_Option) return Option_Level is
   begin
      return Option_Level (Option_Item.C.level);
   end Get_Level;

--   procedure Set_Level
--      (Option_Item : in out  Protocol_Option;
--       Level       : in      Option_Level) is
--   begin
--      Option_Item.C.level := unsigned_long (Level);
--   end Set_Level;

   function Get_Name (Option_Item : Protocol_Option) return Option_Name is
   begin
      return Option_Name (Option_Item.C.name);
   end Get_Name;

--   procedure Set_Name
--      (Option_Item : in out  Protocol_Option;
--       Name        : in      Option_Name) is
--   begin
--      Option_Item.C.name := unsigned_long (Name);
--   end Set_Name;

   procedure Set_Option
     (Option_Item : in out  Protocol_Option;
      Level       : in      Option_Level;
      Name        : in      Option_Name) is
   begin
      Option_Item.C.level := unsigned_long (Level);
      Option_Item.C.name := unsigned_long (Name);
   end Set_Option;

   function Get_Status (Option_Item : Protocol_Option) return Option_Status is
   begin
      if ((Option_Item.C.status and unsigned_long (POSIX.C.XTI.T_SUCCESS))
           = unsigned_long (POSIX.C.XTI.T_SUCCESS))
      then
         return Success;
      elsif ((Option_Item.C.status
        and unsigned_long (POSIX.C.XTI.T_PARTSUCCESS))
          = unsigned_long (POSIX.C.XTI.T_PARTSUCCESS))
      then
         return Partial_Success;
      elsif ((Option_Item.C.status
        and unsigned_long (POSIX.C.XTI.T_FAILURE))
          = unsigned_long (POSIX.C.XTI.T_FAILURE))
      then
         return Failure;
      elsif ((Option_Item.C.status
        and unsigned_long (POSIX.C.XTI.T_READONLY))
          = unsigned_long (POSIX.C.XTI.T_READONLY))
      then
         return Read_Only;
      elsif ((Option_Item.C.status
        and unsigned_long (POSIX.C.XTI.T_NOTSUPPORT))
          = unsigned_long (POSIX.C.XTI.T_NOTSUPPORT))
      then
         return Not_Supported;
      else
         raise Program_Error;

         --  fake return to avoid compiler warning message

         return Failure;
      end if;
   end Get_Status;

   function Get_Value (Option_Item : Protocol_Option) return Option_Value is
      type opthdr_and_data is record
         header : POSIX.C.XTI.struct_t_opthdr;
         data   : POSIX.XTI.Option_Value;
      end record;
      pragma Pack (opthdr_and_data);

      type opthdr_and_data_ptr is access opthdr_and_data;
      function To_opthdr_and_data_ptr is new
        Unchecked_Conversion (System.Address, opthdr_and_data_ptr);

   begin
      return To_opthdr_and_data_ptr (Option_Item.C'Address).data;
   end Get_Value;

   procedure Set_Option
     (Option_Item : in out  Protocol_Option;
      Level       : in      Option_Level;
      Name        : in      Option_Name;
      Value       : in      Option_Value)
   is
      type opthdr_and_data is record
         header : POSIX.C.XTI.struct_t_opthdr;
         data   : POSIX.XTI.Option_Value;
      end record;
      pragma Pack (opthdr_and_data);

      type opthdr_and_data_ptr is access opthdr_and_data;
      function To_opthdr_and_data_ptr is new
        Unchecked_Conversion (System.Address, opthdr_and_data_ptr);

   begin
      To_opthdr_and_data_ptr (Option_Item.C'Address).data := Value;
      Option_Item.C.len :=
        (POSIX.C.XTI.struct_t_opthdr'Size / char'Size) +
        (Value'Size / char'Size);
      Option_Item.C.level := unsigned_long (Level);
      Option_Item.C.name := unsigned_long (Name);
   end Set_Option;

   function Get_Value (Option_Item : Protocol_Option)
     return Option_Value_Array
   is
      type opthdr_and_data is record
         header : POSIX.C.XTI.struct_t_opthdr;
         data   : POSIX.XTI.Option_Value_Array
                   (1 .. ((positive (Option_Item.C.len) -
                    positive ((POSIX.C.XTI.struct_t_opthdr'Size / char'Size)))
                  / positive ((POSIX.XTI.Option_Value'Size / char'Size))));
      end record;
      pragma Pack (opthdr_and_data);

      type opthdr_and_data_ptr is access opthdr_and_data;
      function To_opthdr_and_data_ptr is new
        Unchecked_Conversion (System.Address, opthdr_and_data_ptr);

   begin
      return To_opthdr_and_data_ptr (Option_Item.C'Address).data;
   end Get_Value;

   procedure Set_Option
     (Option_Item : in out  Protocol_Option;
      Level       : in      Option_Level;
      Name        : in      Option_Name;
      Value       : in      Option_Value_Array)
   is
      type opthdr_and_data is record
         header : POSIX.C.XTI.struct_t_opthdr;
         data   : POSIX.XTI.Option_Value_Array (Value'First .. Value'Last);
      end record;
      pragma Pack (opthdr_and_data);

      type opthdr_and_data_ptr is access opthdr_and_data;
      function To_opthdr_and_data_ptr is new
        Unchecked_Conversion (System.Address, opthdr_and_data_ptr);

   begin
      To_opthdr_and_data_ptr (Option_Item.C'Address).data
                        (Value'First .. Value'Last) :=
          Value (Value'First .. Value'Last);
      Option_Item.C.len := (POSIX.C.XTI.struct_t_opthdr'Size / char'Size) +
                           (Value'Size / char'Size);
      Option_Item.C.level := unsigned_long (Level);
      Option_Item.C.name := unsigned_long (Name);
   end Set_Option;

   function Get_Value (Option_Item : Protocol_Option) return Linger_Info is
      type opthdr_and_data is record
         header : POSIX.C.XTI.struct_t_opthdr;
         data   : POSIX.C.XTI.struct_t_linger;
      end record;
      pragma Pack (opthdr_and_data);

      type opthdr_and_data_ptr is access opthdr_and_data;
      function To_opthdr_and_data_ptr is new
        Unchecked_Conversion (System.Address, opthdr_and_data_ptr);

   begin
      if (To_Option_Name (Option_Item.C.name) =
          POSIX.XTI.Linger_On_Close_If_Data_Present)
      then
         return (C => (To_opthdr_and_data_ptr (Option_Item.C'Address).data));
      else

         --  Not a linger Option, raise Operation_Not_Permitted

         Raise_POSIX_Error (Operation_Not_Permitted);

         --  Fake return so we don't get compiler warning

         return (C => (To_opthdr_and_data_ptr (Option_Item.C'Address).data));
      end if;
   end Get_Value;

   procedure Set_Option
     (Option_Item : in out  Protocol_Option;
      Level       : in      Option_Level;
      Name        : in      Option_Name;
      Value       : in      Linger_Info)
   is
      type opthdr_and_data is record
         header : POSIX.C.XTI.struct_t_opthdr;
         data   : POSIX.C.XTI.struct_t_linger;
      end record;
      pragma Pack (opthdr_and_data);

      type opthdr_and_data_ptr is access opthdr_and_data;
      function To_opthdr_and_data_ptr is new
        Unchecked_Conversion (System.Address, opthdr_and_data_ptr);

   begin
      To_opthdr_and_data_ptr (Option_Item.C'Address).data := Value.C;
      Option_Item.C.len := (POSIX.C.XTI.struct_t_opthdr'Size / char'Size) +
                     (POSIX.C.XTI.struct_t_linger'Size / char'Size);
      Option_Item.C.level := unsigned_long (Level);
      Option_Item.C.name := unsigned_long (Name);
   end Set_Option;

--   function Get_Address (Info_Item : Binding_Info)
--     return XTI_Address_Pointer is
--  this functon is protocol specific. Look in the xti child packages.

--   procedure Set_Address
--     (Info_Item : in out Binding_Info;
--      Address   : in     XTI_Address_Pointer) is
--   begin
--      Info_Item.C.addr.maxlen := Address.netbuf.maxlen;
--      Info_Item.C.addr.len    := Address.netbuf.len;
--      Info_Item.C.addr.buf    := Address.netbuf.buf;
--   end Set_Address;

--   function Get_Endpoint_Queue_Length (Info_Item : Binding_Info)
--     return Integer is
--   begin
--      return Integer (Info_Item.C.qlen);
--   end Get_Endpoint_Queue_Length;

--   procedure Set_Endpoint_Queue_Length
--     (Info_Item : in out Binding_Info;
--      Max_Conn  : in     Integer) is
--   begin
--      Info_Item.C.qlen := unsigned (Max_Conn);
--   end Set_Endpoint_Queue_Length;

   ---------------------------
   --  Option Buffer stuff  --
   ---------------------------

   procedure Set_Buffer
     (Info_Item      : in out Protocol_Option_List;
      Options_Buffer : in     Octet_Buffer_Pointer) is
   begin
      Info_Item.C.buf :=
        To_char_ptr (Options_Buffer (Options_Buffer'First)'Address);
      Info_Item.C.maxlen :=
        unsigned_int ((Options_Buffer'Last - Options_Buffer'First) + 1);
      Info_Item.buf_ptr := Options_Buffer;
   end Set_Buffer;

   procedure Make_Empty (Info_Item : in out Protocol_Option_List) is
   begin
      --  Set the length back to zero

      Info_Item.C.len := 0;

      --  Clear the Buffer

      if (Info_Item.C.maxlen > 0) then
         Info_Item.buf_ptr (1 .. integer (Info_Item.C.maxlen)) :=
           (others => 0);
      end if;
   end Make_Empty;

   procedure Append
     (Info_Item : in out Protocol_Option_List;
      Option    : in     Protocol_Option)
   is
      type local_t_opthdr_ptr is access POSIX.C.XTI.struct_t_opthdr;
      function To_local_t_opthdr_ptr is new
        Unchecked_Conversion (POSIX.C.XTI.t_opthdr_ptr, local_t_opthdr_ptr);

      current_option_ptr : POSIX.C.XTI.t_opthdr_ptr :=
                            To_opthdr_ptr (Info_Item.C.buf);
      option_ptr         : POSIX.C.XTI.t_opthdr_ptr := current_option_ptr;
      option_data_size   : Integer;
      option_offset      : Storage_Offset;

   begin
      while (current_option_ptr /= null) loop
         current_option_ptr :=
           c_OPT_NEXTHDR
             (Info_Item.C.buf, Info_Item.C.maxlen, current_option_ptr);

         if (option_ptr = current_option_ptr) then
            --  Not getting anywhere, exit.

            exit;
         elsif (current_option_ptr = null) then
            exit;
         end if;

         option_ptr := current_option_ptr;
      end loop;

      To_local_t_opthdr_ptr (option_ptr).len := Option.C.len;
      To_local_t_opthdr_ptr (option_ptr).level := Option.C.level;
      To_local_t_opthdr_ptr (option_ptr).name  := Option.C.name;
      To_local_t_opthdr_ptr (option_ptr).status := Option.C.status;

      option_offset :=
        (To_Address (option_ptr) - To_Address (Info_Item.C.buf)) +
         Storage_Offset (POSIX.C.XTI.struct_t_opthdr'Size / char'Size);

      option_data_size :=
        (Integer (Option.C.len) -
        (POSIX.C.XTI.struct_t_opthdr'Size / char'Size));

      if option_data_size > 0 then
         --  Add data after header

         Info_Item.buf_ptr
           ((Integer (option_offset) + 1) ..
            (Integer (option_offset) + option_data_size))
             := Option.Data (1 .. option_data_size);
      end if;

      Info_Item.C.len :=
        Info_Item.C.len + (POSIX.C.XTI.struct_t_opthdr'Size / char'Size) +
        unsigned_int (option_data_size);
   end Append;

   procedure For_Every_Item (Info_Item : Protocol_Option_List) is
      Quit               : boolean := False;
      current_option_ptr : POSIX.C.XTI.t_opthdr_ptr :=
                            To_opthdr_ptr (Info_Item.C.buf);
      option_ptr         : POSIX.C.XTI.t_opthdr_ptr := current_option_ptr;
      option             : POSIX.XTI.Protocol_Option;
      option_data_size   : Integer;
      option_offset      : Storage_Offset;
   begin
      while (current_option_ptr /= null) loop
         option_ptr := current_option_ptr;

         --  Exit if there is no option

         if current_option_ptr.len = 0 then
            exit;
         end if;

         option.C.len    := current_option_ptr.len;
         option.C.level  := current_option_ptr.level;
         option.C.name   := current_option_ptr.name;
         option.C.status := current_option_ptr.status;
         option_data_size :=
           Integer (option.C.len) -
             (POSIX.C.XTI.struct_t_opthdr'Size / char'Size);
         option_offset :=
           (To_Address (current_option_ptr) - To_Address (Info_Item.C.buf)) +
             Storage_Offset (POSIX.C.XTI.struct_t_opthdr'Size / char'Size);
         option.Data (1 .. option_data_size) :=
           Info_Item.buf_ptr
             ((Integer (option_offset) + 1) ..
              (Integer (option_offset) + option_data_size));

         Action (option, Quit);

         exit when Quit;

         current_option_ptr :=
           c_OPT_NEXTHDR
             (Info_Item.C.buf, Info_Item.C.maxlen, current_option_ptr);

         if option_ptr = current_option_ptr then
            --  Not getting anywhere, exit.

            exit;
         elsif current_option_ptr = null then
            exit;
         end if;
      end loop;
   end For_Every_Item;

   function Number_Of_Options (Info_Item : Protocol_Option_List)
     return Natural
   is
      current_option_ptr : POSIX.C.XTI.t_opthdr_ptr :=
                            To_opthdr_ptr (Info_Item.C.buf);
      option_ptr         : POSIX.C.XTI.t_opthdr_ptr := current_option_ptr;
      count              : Integer := 0;

   begin
      while current_option_ptr /= null loop
         option_ptr := current_option_ptr;

         --  Exit if there is no option

         if current_option_ptr.len = 0 then
            exit;
         end if;

         current_option_ptr :=
           c_OPT_NEXTHDR
             (Info_Item.C.buf, Info_Item.C.maxlen, current_option_ptr);

         if option_ptr = current_option_ptr then
            --  Not getting anywhere, exit.

            exit;
         elsif current_option_ptr = null then
            exit;
         end if;

         count := count + 1;
      end loop;
      return count;
   end Number_Of_Options;

   procedure Get_Option
     (Info_Item     : in  Protocol_Option_List;
      Option_Number : in  Positive;
      Option        : out Protocol_Option)
   is
      current_option_ptr : POSIX.C.XTI.t_opthdr_ptr :=
                            To_opthdr_ptr (Info_Item.C.buf);
      option_ptr         : POSIX.C.XTI.t_opthdr_ptr := current_option_ptr;
      option_data_size   : Integer;
      option_offset      : Storage_Offset;
      count              : integer := 1;

   begin
      while current_option_ptr /= null loop
         option_ptr := current_option_ptr;

         --  Exit if there is no option

         if current_option_ptr.len = 0 then
            exit;
         end if;

         --  We are at the option to pass back

         if count = Option_Number then
            Option.C.len    := current_option_ptr.len;
            Option.C.level  := current_option_ptr.level;
            Option.C.name   := current_option_ptr.name;
            Option.C.status := current_option_ptr.status;
            option_data_size :=
              Integer (Option.C.len) -
                (POSIX.C.XTI.struct_t_opthdr'Size / char'Size);
            option_offset :=
              (To_Address (current_option_ptr) - To_Address (Info_Item.C.buf)) +
                Storage_Offset (POSIX.C.XTI.struct_t_opthdr'Size / char'Size);

            Option.Data (1 .. option_data_size) :=
              Info_Item.buf_ptr
                ((Integer (option_offset) + 1) ..
                 (Integer (option_offset) + option_data_size));
            return;
         end if;

         current_option_ptr :=
           c_OPT_NEXTHDR
             (Info_Item.C.buf, Info_Item.C.maxlen, current_option_ptr);

         if option_ptr = current_option_ptr then
            --  Not getting anywhere, exit.

            exit;
         elsif current_option_ptr = null then
            exit;
         end if;

         count := count + 1;
      end loop;

      --  Need to Raise Some Exception

      Raise_POSIX_Error (Invalid_Argument);
   end Get_Option;

   procedure Set_Address
     (Info_Item : in out Connection_Info;
      Address   : in     XTI_Address_Pointer) is
   begin
      Info_Item.C.addr.maxlen := Address.netbuf.maxlen;
      Info_Item.C.addr.len    := Address.netbuf.len;
      Info_Item.C.addr.buf    := Address.netbuf.buf;
   end Set_Address;

   function Get_Options (Info_Item : Connection_Info)
     return Protocol_Option_List is
   begin
      return (C => (maxlen => Info_Item.C.opt.maxlen,
                    len    => Info_Item.C.opt.len,
                    buf    => Info_Item.C.opt.buf),
              buf_ptr => Info_Item.opt_buf_ptr);
   end Get_Options;

   procedure Set_Options
     (Info_Item : in out Connection_Info;
      Options   : in     Protocol_Option_List_Pointer) is
   begin
      Info_Item.C.opt.maxlen := Options.C.maxlen;
      Info_Item.C.opt.len    := Options.C.len;
      Info_Item.C.opt.buf    := Options.C.buf;
   end Set_Options;

   procedure Set_User_Data
     (Info_Item  : in out Connection_Info;
      User_Data  : in     System.Address;
      Max_Length : in     POSIX.IO_Count) is
   begin
      Info_Item.C.udata.buf    := To_char_ptr (User_Data);
      Info_Item.C.udata.maxlen := unsigned_int (Max_Length);
      Info_Item.C.udata.len    := 0;
   end Set_User_Data;

   procedure Set_User_Data_Length
     (Info_Item  : in out Connection_Info;
      Length     : in     POSIX.IO_Count) is
   begin
      Info_Item.C.udata.len := unsigned_int (Length);
   end Set_User_Data_Length;

   function Get_User_Data_Length (Info_Item : Connection_Info)
     return POSIX.IO_Count is
   begin
      return POSIX.IO_Count (Info_Item.C.udata.len);
   end Get_User_Data_Length;

   function Get_Sequence_Number (Info_Item : Connection_Info)
     return Natural is
   begin
      return Natural (Info_Item.C.sequence);
   end Get_Sequence_Number;

   procedure Set_Sequence_Number
     (Info_Item : in out Connection_Info;
      Number    : in     Natural) is
   begin
      Info_Item.C.sequence := int (Number);
   end Set_Sequence_Number;

   procedure Accept_Connection
     (Listening_Endpoint  : in POSIX.IO.File_Descriptor;
      Responding_Endpoint : in POSIX.IO.File_Descriptor;
      Call                : in Connection_Info) is
   begin
      if c_t_accept (int (Listening_Endpoint),
                     int (Responding_Endpoint),
                     To_ptr (Call'Address)) < 0
      then
         --  Error
         Raise_XTI_Error;
      end if;
   end Accept_Connection;

   procedure Accept_Connection
     (Listening_Endpoint  : in     POSIX.IO.File_Descriptor;
      Responding_Endpoint : in     POSIX.IO.File_Descriptor)
   is
      Call : POSIX.C.XTI.struct_t_call;
   begin
      Call.addr.len  := 0;
      Call.opt.len   := 0;
      Call.udata.len := 0;

      if c_t_accept (int (Listening_Endpoint),
                     int (Responding_Endpoint),
                     To_ptr (Call'Address)) < 0
      then
         --  Error
         Raise_XTI_Error;
      end if;
   end Accept_Connection;

   procedure Acknowledge_Orderly_Release
     (Endpoint : in POSIX.IO.File_Descriptor) is
   begin
      if c_t_rcvrel (int (Endpoint)) < 0 then
         --  Error
         Raise_XTI_Error;
      end if;
   end Acknowledge_Orderly_Release;

--   procedure Acknowledge_Orderly_Release_With_Data
--     (Endpoint : in     POSIX.IO.File_Descriptor;
--      Info     : in out Disconnect_Info) is
--   begin
--      if (c_t_rcvreldata (int (Endpoint),
--                          To_ptr (Info'Address)) < 0) then
         --  Error
--         Raise_XTI_Error;
--      end if;
--   end Acknowledge_Orderly_Release_With_Data;

   procedure Acknowledge_Orderly_Release
     (Endpoint  : in     POSIX.IO.File_Descriptor;
      Reason    : out    Reason_Code)
   is
      Info : POSIX.C.XTI.struct_t_discon;
   begin
      Info.udata.len    := 0;
      Info.udata.maxlen := 0;
      Info.udata.buf    := null;

      if c_t_rcvreldata (int (Endpoint), To_ptr (Info'Address)) < 0 then
         --  Error
         Raise_XTI_Error;
      end if;

      Reason := Reason_Code (Info.reason);
   end Acknowledge_Orderly_Release;

   procedure Acknowledge_Orderly_Release_With_Data
     (Endpoint         : in  POSIX.IO.File_Descriptor;
      Reason           : out Reason_Code;
      User_Data        : in  System.Address;
      Octets_Requested : in  POSIX.IO_Count;
      Octets_Received  : out POSIX.IO_Count)
   is
      Info : POSIX.C.XTI.struct_t_discon;
   begin
      Info.udata.len    := unsigned_int (Octets_Requested);
      Info.udata.maxlen := unsigned_int (Octets_Requested);
      Info.udata.buf    := To_char_ptr (User_Data);

      if c_t_rcvreldata (int (Endpoint), To_ptr (Info'Address)) < 0 then
         --  Error
         Raise_XTI_Error;
      end if;

      Reason          := Reason_Code (Info.reason);
      Octets_Received := POSIX.IO_Count (Info.udata.len);
   end Acknowledge_Orderly_Release_With_Data;

--   procedure Bind
--     (Endpoint : in     POSIX.IO.File_Descriptor;
--      Request  : in     Binding_Info;
--      Response : in out Binding_Info) is
--   begin
--      if (c_t_bind (int (Endpoint),
--                    To_ptr (Request'Address),
--                    To_ptr (Response'Address)) < 0) then
         --  Error
--         Raise_XTI_Error;
--      end if;
--   end Bind;

   procedure Close (Endpoint : in POSIX.IO.File_Descriptor) is
   begin
      if (c_t_close (int (Endpoint)) < 0) then
         --  Error
         Raise_XTI_Error;
      end if;
   end Close;

   procedure Confirm_Connection
     (Endpoint : in POSIX.IO.File_Descriptor;
      Call     : in Connection_Info_Pointer) is
   begin
      if c_t_rcvconnect (int (Endpoint), To_ptr (Call.C'Address)) < 0 then
         --  Error
         Raise_XTI_Error;
      end if;
   end Confirm_Connection;

   procedure Confirm_Connection (Endpoint : in POSIX.IO.File_Descriptor) is
      Call : POSIX.C.XTI.struct_t_call;
   begin
      Call.addr.len  := 0;
      Call.opt.len   := 0;
      Call.udata.len := 0;

      if c_t_rcvconnect (int (Endpoint), To_ptr (Call'Address)) < 0 then
         --  Error
         Raise_XTI_Error;
      end if;
   end Confirm_Connection;

   procedure Connect
     (Endpoint : in POSIX.IO.File_Descriptor;
      Send     : in Connection_Info;
      Receive  : in Connection_Info_Pointer) is
   begin
      if c_t_connect (int (Endpoint),
                      To_ptr (Send.C'Address),
                      To_ptr (Receive.C'Address)) < 0
      then
         --  Error
         Raise_XTI_Error;
      end if;
   end Connect;

   procedure Connect
     (Endpoint : in     POSIX.IO.File_Descriptor;
      Send     : in     Connection_Info) is
   begin
      if c_t_connect (int (Endpoint),
                      To_ptr (Send'Address),
                      null) < 0
      then
         --  Error
         Raise_XTI_Error;
      end if;
   end Connect;

   procedure Gather_And_Send_Data
     (Endpoint        : in  POSIX.IO.File_Descriptor;
      Vector          : in  IO_Vector_Array;
      Flags           : in  XTI_Flags;
      Octets_Sent     : out POSIX.IO_Count)
   is
      c_bytes : int;
   begin
      c_bytes :=
        c_t_sndv
          (int (Endpoint), To_iovec_ptr (Vector (Vector'First)'Address),
           unsigned (Vector'Length), To_int (Flags));

      if c_bytes < 0 then
         --  Error
         Raise_XTI_Error;
      else
         Octets_Sent := POSIX.IO_Count (c_bytes);
      end if;
   end Gather_And_Send_Data;

   procedure Gather_And_Send_Data_Unit
     (Endpoint        : in POSIX.IO.File_Descriptor;
      Address         : in XTI_Address_Pointer;
      Vector          : in IO_Vector_Array;
      Options         : in Protocol_Option_List)
   is
      c_bytes : int;
      data    : POSIX.C.XTI.struct_t_unitdata;

   begin
      data.addr.len    := Address.netbuf.len;
      data.addr.maxlen := Address.netbuf.maxlen;
      data.addr.buf    := Address.netbuf.buf;
      data.opt.len     := Options.C.len;
      data.opt.maxlen  := Options.C.maxlen;
      data.opt.buf     := Options.C.buf;
      data.udata.len   := 0;

      c_bytes := c_t_sndvudata (int (Endpoint),
                                To_ptr (data'Address),
                                To_iovec_ptr (Vector (Vector'First)'Address),
                                int (Vector'Length));
      if c_bytes < 0 then
         --  Error
         Raise_XTI_Error;
      end if;
   end Gather_And_Send_Data_Unit;

   procedure Gather_And_Send_Data_Unit
     (Endpoint        : in POSIX.IO.File_Descriptor;
      Address         : in XTI_Address_Pointer;
      Vector          : in IO_Vector_Array)
   is
      c_bytes : int;
      data    : POSIX.C.XTI.struct_t_unitdata;

   begin
      data.addr.len    := Address.netbuf.len;
      data.addr.maxlen := Address.netbuf.maxlen;
      data.addr.buf    := Address.netbuf.buf;
      data.opt.len     := 0;
      data.udata.len   := 0;
      c_bytes := c_t_sndvudata (int (Endpoint),
                                To_ptr (data'Address),
                                To_iovec_ptr (Vector (Vector'First)'Address),
                                int (Vector'Length));
      if c_bytes < 0 then
         --  Error
         Raise_XTI_Error;
      end if;
   end Gather_And_Send_Data_Unit;

   function Get_Current_State (Endpoint : POSIX.IO.File_Descriptor)
     return Interface_State
   is
      S : constant int := c_t_getstate (int (Endpoint));
   begin
      --  Use if-statement instead of case-statement or table,
      --  to allow for sparse or (if not supported) zero values
      --  of C-language constants.
      --  ????? change .5c?
      --  Make Interface_State into a private type with deferred
      --  constants.

      if S = POSIX.C.XTI.T_UNINIT then
         return Uninitialized;
      end if;

      if S = POSIX.C.XTI.T_UNBND then
         return Unbound;
      end if;

      if S = POSIX.C.XTI.T_IDLE then
         return Idle;
      end if;

      if S = POSIX.C.XTI.T_OUTCON then
         return Outgoing_Connect;
      end if;

      if S = POSIX.C.XTI.T_INCON then
         return Incoming_Connect;
      end if;

      if S = POSIX.C.XTI.T_DATAXFER then
         return Data_Transfer;
      end if;

      if S = POSIX.C.XTI.T_OUTREL then
         return Outgoing_Release;
      end if;

      if S = POSIX.C.XTI.T_INREL then
         return Incoming_Release;
      end if;

      if S = -1 then
         Raise_XTI_Error;
      end if;

      raise Program_Error;
      return Uninitialized;
   end Get_Current_State;

   procedure Get_Info
     (Endpoint : in POSIX.IO.File_Descriptor;
      Info     : in Communications_Provider_Info_Pointer) is
   begin
      if c_t_getinfo (int (Endpoint), To_ptr (Info.C'Address)) < 0 then
         --  Error
         Raise_XTI_Error;
      end if;
   end Get_Info;

   procedure Get_Protocol_Address
     (Endpoint      : in POSIX.IO.File_Descriptor;
      Bound_Address : in XTI_Address_Pointer;
      Peer_Address  : in XTI_Address_Pointer)
   is
      bound_addr : POSIX.C.XTI.struct_t_bind;
      peer_addr  : POSIX.C.XTI.struct_t_bind;

   begin
      bound_addr.addr.len    := Bound_Address.netbuf.len;
      bound_addr.addr.maxlen := Bound_Address.netbuf.maxlen;
      bound_addr.addr.buf    := Bound_Address.netbuf.buf;
      peer_addr.addr.len     := Peer_Address.netbuf.len;
      peer_addr.addr.maxlen  := Peer_Address.netbuf.maxlen;
      peer_addr.addr.buf     := Peer_Address.netbuf.buf;

      if c_t_getprotaddr (int (Endpoint),
                          To_ptr (bound_addr'Address),
                          To_ptr (peer_addr'Address)) < 0
      then
         --  Error
         Raise_XTI_Error;
      end if;
   end Get_Protocol_Address;

   procedure Initiate_Orderly_Release
     (Endpoint : in POSIX.IO.File_Descriptor) is
   begin
      if c_t_sndrel (int (Endpoint)) < 0 then
         --  Error
         Raise_XTI_Error;
      end if;
   end Initiate_Orderly_Release;

--   procedure Initiate_Orderly_Release_With_Data
--     (Endpoint : in POSIX.IO.File_Descriptor;
--      Info     : in Disconnect_Info) is
--   begin
--      if (c_t_sndreldata (int (Endpoint),
--                         To_ptr (Info'Address)) < 0) then
         --  Error
--         Raise_XTI_Error;
--      end if;
--   end Initiate_Orderly_Release_With_Data;

   procedure Initiate_Orderly_Release
     (Endpoint  : in POSIX.IO.File_Descriptor;
      Reason    : in Reason_Code)
   is
      Info : POSIX.C.XTI.struct_t_discon;
   begin
      Info.udata.len    := 0;
      Info.udata.maxlen := 0;
      Info.udata.buf    := null;
      Info.reason       := int (Reason);

      if c_t_sndreldata (int (Endpoint), To_ptr (Info'Address)) < 0 then
         --  Error
         Raise_XTI_Error;
      end if;
   end Initiate_Orderly_Release;

   procedure Initiate_Orderly_Release_With_Data
     (Endpoint       : in POSIX.IO.File_Descriptor;
      Reason         : in Reason_Code;
      User_Data      : in System.Address;
      Octets_To_Send : in POSIX.IO_Count)
   is
      Info : POSIX.C.XTI.struct_t_discon;
   begin
      Info.udata.len    := unsigned_int (Octets_To_Send);
      Info.udata.maxlen := unsigned_int (Octets_To_Send);
      Info.udata.buf    := To_char_ptr (User_Data);
      Info.reason       := int (Reason);

      if c_t_sndreldata (int (Endpoint), To_ptr (Info'Address)) < 0 then
         --  Error
         Raise_XTI_Error;
      end if;
   end Initiate_Orderly_Release_With_Data;

   procedure Listen
     (Endpoint : in POSIX.IO.File_Descriptor;
      Call     : in Connection_Info_Pointer) is
   begin
      if c_t_listen (int (Endpoint),
                     To_ptr (Call.C'Address)) < 0 then
         Raise_XTI_Error;
      end if;
   end Listen;

   function Look (Endpoint : POSIX.IO.File_Descriptor) return XTI_Events is
      c_events : int;
   begin
      c_events := c_t_look (int (Endpoint));

      if c_events < 0 then
         --  Error
         Raise_XTI_Error;
         return XTI_Events (POSIX.Empty_Set);
      else
         return To_XTI_Events (c_events);
      end if;
   end Look;

   procedure Manage_Options
     (Endpoint       : in  POSIX.IO.File_Descriptor;
      Request        : in  Protocol_Option_List;
      Request_Flag   : in  Options_Flags;
      Response       : in  Protocol_Option_List_Pointer;
      Response_Flags : out Option_Status)
   is
      c_req :  POSIX.C.XTI.struct_t_optmgmt;
      c_ret :  POSIX.C.XTI.struct_t_optmgmt;

   begin
      c_req.opt.maxlen := Request.C.maxlen;
      c_req.opt.len    := Request.C.len;
      c_req.opt.buf    := Request.C.buf;
      c_req.flags      := long (Request_Flag);
      c_ret.opt.maxlen := Response.C.maxlen;
      c_ret.opt.len    := Response.C.len;
      c_ret.opt.buf    := Response.C.buf;

      if c_t_optmgmt (int (Endpoint),
                      To_ptr (c_req'Address),
                      To_ptr (c_ret'Address)) < 0
      then
         --  Error
         Raise_XTI_Error;
      end if;

      --  Set the Response_Flags

      if (unsigned (c_ret.flags) and unsigned (POSIX.C.XTI.T_SUCCESS))
           = unsigned (POSIX.C.XTI.T_SUCCESS)
      then
         Response_Flags := Success;
      elsif (unsigned (c_ret.flags) and unsigned (POSIX.C.XTI.T_PARTSUCCESS))
           = unsigned (POSIX.C.XTI.T_PARTSUCCESS)
      then
         Response_Flags := Partial_Success;
      elsif (unsigned (c_ret.flags) and unsigned (POSIX.C.XTI.T_FAILURE))
           = unsigned (POSIX.C.XTI.T_FAILURE)
      then
         Response_Flags := Failure;
      elsif (unsigned (c_ret.flags) and unsigned (POSIX.C.XTI.T_READONLY))
           = unsigned (POSIX.C.XTI.T_READONLY)
      then
         Response_Flags := Read_Only;
      elsif (unsigned (c_ret.flags) and unsigned (POSIX.C.XTI.T_NOTSUPPORT))
           = unsigned (POSIX.C.XTI.T_NOTSUPPORT)
      then
         Response_Flags := Not_Supported;
      end if;
   end Manage_Options;

   procedure Open
     (Endpoint : out POSIX.IO.File_Descriptor;
      Name     : in  POSIX.POSIX_String;
      Mode     : in  POSIX.IO.File_Mode;
      Options  : in  POSIX.IO.Open_Option_Set;
      Info     : in  Communications_Provider_Info_Pointer)
   is
      c_endpoint : int;
      c_oflags   : int;

   begin
      c_oflags := To_int (Options);

      case Mode is
         when POSIX.IO.Read_Only =>
            c_oflags := c_oflags + POSIX.C.O_RDONLY;
         when POSIX.IO.Write_Only =>
            c_oflags := c_oflags + POSIX.C.O_WRONLY;
         when POSIX.IO.Read_Write =>
            c_oflags := c_oflags + POSIX.C.O_RDWR;
      end case;

      c_endpoint :=
        c_t_open
          (To_char_ptr (Name'Address), c_oflags, To_ptr (Info.C'Address));

      if c_endpoint < 0 then
         --  Error
         Raise_XTI_Error;
      else
         Endpoint := POSIX.IO.File_Descriptor (c_endpoint);
      end if;
   end Open;

   procedure Open
     (Endpoint : out POSIX.IO.File_Descriptor;
      Name     : in  POSIX.POSIX_String;
      Mode     : in  POSIX.IO.File_Mode;
      Options  : in  POSIX.IO.Open_Option_Set)
   is
      c_endpoint : int;
      c_oflags   : int;

   begin
      c_oflags := To_int (Options);

      case Mode is
         when POSIX.IO.Read_Only =>
            c_oflags := c_oflags + POSIX.C.O_RDONLY;
         when POSIX.IO.Write_Only =>
            c_oflags := c_oflags + POSIX.C.O_WRONLY;
         when POSIX.IO.Read_Write =>
            c_oflags := c_oflags + POSIX.C.O_RDWR;
      end case;

      c_endpoint := c_t_open (To_char_ptr (Name'Address), c_oflags, null);

      if c_endpoint < 0 then
         --  Error
         Raise_XTI_Error;
      else
         Endpoint := POSIX.IO.File_Descriptor (c_endpoint);
      end if;
   end Open;

   procedure Receive
     (Endpoint         : in  POSIX.IO.File_Descriptor;
      Buffer           : in  System.Address;
      Octets_Requested : in  POSIX.IO_Count;
      Octets_Received  : out POSIX.IO_Count;
      Flags            : out XTI_Flags)
   is
      c_bytes : int;
   begin
      c_bytes := c_t_rcv (int (Endpoint),
                          To_char_ptr (Buffer),
                          unsigned (Octets_Requested),
                          To_int_ptr (Flags'Address));

      if c_bytes < 0 then
         --  Error
         Raise_XTI_Error;
      else
         Octets_Received := POSIX.IO_Count (c_bytes);
      end if;
   end Receive;

   procedure Receive_And_Scatter_Data
     (Endpoint        : in  POSIX.IO.File_Descriptor;
      Vector          : in  IO_Vector_Array;
      Octets_Received : out POSIX.IO_Count;
      Flags           : out XTI_Flags)
   is
      c_bytes : int;
   begin
      c_bytes := c_t_rcvv (int (Endpoint),
                           To_iovec_ptr (Vector (Vector'First)'Address),
                           unsigned (Vector'Length),
                           To_int_ptr (Flags'Address));

      if c_bytes < 0 then
         --  Error
         Raise_XTI_Error;
      else
         Octets_Received := POSIX.IO_Count (c_bytes);
      end if;
   end Receive_And_Scatter_Data;

   procedure Receive_And_Scatter_Data_Unit
     (Endpoint          : in  POSIX.IO.File_Descriptor;
      Address           : in  XTI_Address_Pointer;
      Options           : in  Protocol_Option_List_Pointer;
      Vector            : in  IO_Vector_Array;
      Octets_Received   : out POSIX.IO_Count;
      Flags             : out XTI_Flags)
   is
      c_bytes : int;
      Data    : POSIX.C.XTI.struct_t_unitdata;

   begin
      Data.udata.len   := 0;
      Data.addr.len    := Address.netbuf.len;
      Data.addr.maxlen := Address.netbuf.maxlen;
      Data.addr.buf    := Address.netbuf.buf;
      Data.opt.len     := Options.C.len;
      Data.opt.maxlen  := Options.C.maxlen;
      Data.opt.buf     := Options.C.buf;
      c_bytes := c_t_rcvvudata (int (Endpoint),
                                To_ptr (Data'Address),
                                To_iovec_ptr (Vector (Vector'First)'Address),
                                unsigned (Vector'Length),
                                To_int_ptr (Flags'Address));
      if c_bytes < 0 then
         --  Error
         Raise_XTI_Error;
      else
         Octets_Received := POSIX.IO_Count (c_bytes);
      end if;
   end Receive_And_Scatter_Data_Unit;

   procedure Receive_Data_Unit
     (Endpoint         : in  POSIX.IO.File_Descriptor;
      User_Data        : in  System.Address;
      Octets_Requested : in  POSIX.IO_Count;
      Address          : in  XTI_Address_Pointer;
      Options          : in  Protocol_Option_List_Pointer;
      Flags            : out XTI_Flags)
   is
      Data    : POSIX.C.XTI.struct_t_unitdata;
   begin
      Data.udata.len    := unsigned_int (Octets_Requested);
      Data.udata.maxlen := unsigned_int (Octets_Requested);
      Data.udata.buf    := To_char_ptr (User_Data);
      Data.addr.len     := Address.netbuf.len;
      Data.addr.maxlen  := Address.netbuf.maxlen;
      Data.addr.buf     := Address.netbuf.buf;
      Data.opt.len      := Options.C.len;
      Data.opt.maxlen   := Options.C.maxlen;
      Data.opt.buf      := Options.C.buf;

      if c_t_rcvudata (int (Endpoint),
                       To_ptr (Data'Address),
                       To_int_ptr (Flags'Address)) < 0
      then
         --  Error
         Raise_XTI_Error;
      end if;
   end Receive_Data_Unit;

   procedure Receive_Data_Unit
     (Endpoint         : in  POSIX.IO.File_Descriptor;
      User_Data        : in  System.Address;
      Octets_Requested : in  POSIX.IO_Count;
      Address          : in  XTI_Address_Pointer;
      Flags            : out XTI_Flags)
   is
      Data    : POSIX.C.XTI.struct_t_unitdata;
   begin
      Data.udata.len    := unsigned_int (Octets_Requested);
      Data.udata.maxlen := unsigned_int (Octets_Requested);
      Data.udata.buf    := To_char_ptr (User_Data);
      Data.addr.len     := Address.netbuf.len;
      Data.addr.maxlen  := Address.netbuf.maxlen;
      Data.addr.buf     := Address.netbuf.buf;
      Data.opt.len      := 0;
      Data.opt.maxlen   := 0;
      Data.opt.buf      := null;

      if c_t_rcvudata (int (Endpoint),
                       To_ptr (Data'Address),
                       To_int_ptr (Flags'Address)) < 0 then
         --  Error
         Raise_XTI_Error;
      end if;
   end Receive_Data_Unit;

--   function Get_Address (Error_Item : Unit_Data_Error)
--     return XTI_Address_Pointer is
--  this function is protocol specific. Look in the xti child packages.

--   function Get_Options (Error_Item : Unit_Data_Error)
--     return Protocol_Option_List is
--   begin
--      return (C => (maxlen => Error_Item.C.opt.maxlen,
--                    len    => Error_Item.C.opt.len,
--                    buf    => Error_Item.C.opt.buf),
--               buf_ptr => Error_Item.opt_buf_ptr);
--   end Get_Options;

--   function Get_Error_Code (Error_Item : Unit_Data_Error)
--     return Integer is
--   begin
--      return Integer (Error_Item.C.error);
--   end Get_Error_Code;

--   procedure Receive_Data_Unit_Error
--        (Endpoint : in  POSIX.IO.File_Descriptor;
--         Error    :    in out Unit_Data_Error) is

--   begin
--      if (c_t_rcvuderr (int (Endpoint),
--                       To_ptr (Error'Address)) < 0) then
         --  Error
--         Raise_XTI_Error;
--      end if;
--   end Receive_Data_Unit_Error;

   procedure Retrieve_Data_Unit_Error
       (Endpoint : in  POSIX.IO.File_Descriptor;
        Address  : in  XTI_Address_Pointer;
        Options  : in  Protocol_Option_List_Pointer;
        Error    : out Unit_Data_Error_Code)
   is
      Error_Data : POSIX.C.XTI.struct_t_uderr;
   begin
      Error_Data.addr.len := Address.netbuf.len;
      Error_Data.addr.maxlen := Address.netbuf.maxlen;
      Error_Data.addr.buf := Address.netbuf.buf;
      Error_Data.opt.len := Options.C.len;
      Error_Data.opt.maxlen := Options.C.maxlen;
      Error_Data.opt.buf := Options.C.buf;

      if c_t_rcvuderr (int (Endpoint), To_ptr (Error_Data'Address)) < 0 then
         --  Error
         Raise_XTI_Error;
      end if;

      Error := Unit_Data_Error_Code (Error_Data.error);
   end Retrieve_Data_Unit_Error;

   procedure Retrieve_Data_Unit_Error
       (Endpoint : in  POSIX.IO.File_Descriptor;
        Address  : in  XTI_Address_Pointer;
        Error    : out Unit_Data_Error_Code)
   is
      Error_Data : POSIX.C.XTI.struct_t_uderr;
   begin
      Error_Data.addr.len := Address.netbuf.len;
      Error_Data.addr.maxlen := Address.netbuf.maxlen;
      Error_Data.addr.buf := Address.netbuf.buf;
      Error_Data.opt.len := 0;
      Error_Data.opt.maxlen := 0;
      Error_Data.opt.buf := null;

      if c_t_rcvuderr (int (Endpoint), To_ptr (Error_Data'Address)) < 0 then
         --  Error
         Raise_XTI_Error;
      end if;

      Error := Unit_Data_Error_Code (Error_Data.error);
   end Retrieve_Data_Unit_Error;

   procedure Retrieve_Data_Unit_Error
     (Endpoint : in  POSIX.IO.File_Descriptor;
      Options  : in  Protocol_Option_List_Pointer;
      Error    : out Unit_Data_Error_Code)
   is
      Error_Data : POSIX.C.XTI.struct_t_uderr;
   begin
      Error_Data.addr.len := 0;
      Error_Data.addr.maxlen := 0;
      Error_Data.addr.buf := null;
      Error_Data.opt.len := Options.C.len;
      Error_Data.opt.maxlen := Options.C.maxlen;
      Error_Data.opt.buf := Options.C.buf;

      if c_t_rcvuderr (int (Endpoint), To_ptr (Error_Data'Address)) < 0 then
         --  Error
         Raise_XTI_Error;
      end if;

      Error := Unit_Data_Error_Code (Error_Data.error);
   end Retrieve_Data_Unit_Error;

   procedure Retrieve_Data_Unit_Error
     (Endpoint : in     POSIX.IO.File_Descriptor;
      Error    : out    Unit_Data_Error_Code)
   is
      Error_Data : POSIX.C.XTI.struct_t_uderr;
   begin
      Error_Data.addr.len := 0;
      Error_Data.addr.maxlen := 0;
      Error_Data.addr.buf := null;
      Error_Data.opt.len := 0;
      Error_Data.opt.maxlen := 0;
      Error_Data.opt.buf := null;

      if c_t_rcvuderr (int (Endpoint), To_ptr (Error_Data'Address)) < 0 then
         --  Error
         Raise_XTI_Error;
      end if;
      Error := Unit_Data_Error_Code (Error_Data.error);
   end Retrieve_Data_Unit_Error;

--   procedure Retrieve_Disconnect_Info
--     (Endpoint : in     POSIX.IO.File_Descriptor;
--      Info     : in out Disconnect_Info) is
--   begin
--      if (c_t_rcvdis (int (Endpoint),
--                     To_ptr (Info'Address)) < 0) then
         --  Error
--         Raise_XTI_Error;
--      end if;
--   end Retrieve_Disconnect_Info;

   procedure Retrieve_Disconnect_Info
     (Endpoint         : in  POSIX.IO.File_Descriptor;
      User_Data        : in  System.Address;
      Octets_Requested : in  POSIX.IO_Count;
      Reason           : out Reason_Code;
      Sequence_Number  : out Natural)
   is
      Info : POSIX.C.XTI.struct_t_discon;
   begin
      Info.udata.len    := 0;
      Info.udata.maxlen := unsigned_int (Octets_Requested);
      Info.udata.buf    := To_char_ptr (User_Data);

      if c_t_rcvdis (int (Endpoint), To_ptr (Info'Address)) < 0 then
         --  Error
         Raise_XTI_Error;
      end if;

      Reason := Reason_Code (Info.reason);
      Sequence_Number := Integer (Info.sequence);
   end Retrieve_Disconnect_Info;

   procedure Clear_Disconnect_Info (Endpoint : in POSIX.IO.File_Descriptor) is
   begin
      if c_t_rcvdis (int (Endpoint), null) < 0 then
         --  Error
         Raise_XTI_Error;
      end if;
   end Clear_Disconnect_Info;

   procedure Send
     (Endpoint       : in  POSIX.IO.File_Descriptor;
      Buffer         : in  System.Address;
      Octets_To_Send : in  POSIX.IO_Count;
      Flags          : in  XTI_Flags;
      Octets_Sent    : out POSIX.IO_Count)
   is
      c_count : int;
   begin
      c_count := c_t_snd (int (Endpoint),
                          To_char_ptr (Buffer),
                          unsigned (Octets_To_Send),
                          To_int (Flags));

      if c_count < 0 then
         --  Error
         Raise_XTI_Error;
      else
         Octets_Sent := POSIX.IO_Count (c_count);
      end if;
   end Send;

   procedure Send_Data_Unit
     (Endpoint       : in POSIX.IO.File_Descriptor;
      User_Data      : in System.Address;
      Octets_To_Send : in POSIX.IO_Count;
      Address        : in XTI_Address_Pointer;
      Options        : in Protocol_Option_List)
   is
      c_count : int;
      Data    : POSIX.C.XTI.struct_t_unitdata;

   begin
      Data.udata.len    := unsigned_int (Octets_To_Send);
      Data.udata.maxlen := unsigned_int (Octets_To_Send);
      Data.udata.buf    := To_char_ptr (User_Data);
      Data.addr.len     := Address.netbuf.len;
      Data.addr.maxlen  := Address.netbuf.maxlen;
      Data.addr.buf     := Address.netbuf.buf;
      Data.opt.len      := Options.C.len;
      Data.opt.maxlen   := Options.C.maxlen;
      Data.opt.buf      := Options.C.buf;
      c_count := c_t_sndudata (int (Endpoint), To_ptr (Data'Address));

      if c_count < 0 then
         --  Error
         Raise_XTI_Error;
      end if;
   end Send_Data_Unit;

   procedure Send_Data_Unit
     (Endpoint       : in POSIX.IO.File_Descriptor;
      User_Data      : in System.Address;
      Octets_To_Send : in POSIX.IO_Count;
      Address        : in XTI_Address_Pointer)
   is
      c_count : int;
      Data    : POSIX.C.XTI.struct_t_unitdata;

   begin
      Data.udata.len    := unsigned_int (Octets_To_Send);
      Data.udata.maxlen := unsigned_int (Octets_To_Send);
      Data.udata.buf    := To_char_ptr (User_Data);
      Data.addr.len     := Address.netbuf.len;
      Data.addr.maxlen  := Address.netbuf.maxlen;
      Data.addr.buf     := Address.netbuf.buf;
      Data.opt.len      := 0;
      Data.opt.maxlen   := 0;
      Data.opt.buf      := null;
      c_count := c_t_sndudata (int (Endpoint), To_ptr (User_Data));

      if c_count < 0 then
         --  Error
         Raise_XTI_Error;
      end if;
   end Send_Data_Unit;

   procedure Send_Disconnect_Request
     (Endpoint : in POSIX.IO.File_Descriptor;
      Call     : in Connection_Info) is
   begin
      if c_t_snddis (int (Endpoint), To_ptr (Call.C'Address)) < 0 then
         --  Error
         Raise_XTI_Error;
      end if;
   end Send_Disconnect_Request;

   procedure Send_Disconnect_Request
     (Endpoint : in POSIX.IO.File_Descriptor) is
   begin
      if c_t_snddis (int (Endpoint), null) < 0 then
         --  Error
         Raise_XTI_Error;
      end if;
   end Send_Disconnect_Request;

   function Synchronize_Endpoint (Endpoint : in POSIX.IO.File_Descriptor)
     return Interface_State
   is
      S : constant int := c_t_sync (int (Endpoint));
   begin
      return Interface_State (S);
   end Synchronize_Endpoint;

   procedure Unbind (Endpoint : in POSIX.IO.File_Descriptor) is
   begin
      if (c_t_unbind (int (Endpoint)) < 0) then
         --  Error
         Raise_XTI_Error;
      end if;
   end Unbind;

   procedure Raise_XTI_Error is
      c_t_errno : Error_Code;
   begin
      c_t_errno := Fetch_T_Errno;
      Raise_POSIX_Error (c_t_errno + POSIX.XTI_Error_Code'First);
   end Raise_XTI_Error;

   procedure Bind
     (Endpoint              : in  POSIX.IO.File_Descriptor;
      Request_Address       : in  XTI_Address_Pointer;
      Request_Queue_Length  : in  Natural;
      Response_Address      : in  XTI_Address_Pointer;
      Response_Queue_Length : out Natural)
   is
      request  : POSIX.C.XTI.struct_t_bind;
      response : POSIX.C.XTI.struct_t_bind;

   begin
      request.qlen         := unsigned (Request_Queue_Length);
      request.addr.len     := Request_Address.netbuf.len;
      request.addr.maxlen  := Request_Address.netbuf.maxlen;
      request.addr.buf     := Request_Address.netbuf.buf;
      response.addr.len    := Response_Address.netbuf.len;
      response.addr.maxlen := Response_Address.netbuf.maxlen;
      response.addr.buf    := Response_Address.netbuf.buf;

      if c_t_bind (int (Endpoint),
                   To_ptr (request'Address),
                   To_ptr (response'Address)) < 0
      then
         --  Error
         Raise_XTI_Error;
      end if;

      Response_Queue_Length := Integer (response.qlen);
   end Bind;

   procedure Bind
     (Endpoint              : in  POSIX.IO.File_Descriptor;
      Request_Queue_Length  : in  Natural;
      Response_Address      : in  XTI_Address_Pointer;
      Response_Queue_Length : out Natural)
   is
      request  : POSIX.C.XTI.struct_t_bind;
      response : POSIX.C.XTI.struct_t_bind;

   begin
      request.qlen         := unsigned (Request_Queue_Length);
      request.addr.len     := 0;
      request.addr.maxlen  := 0;
      request.addr.buf     := null;
      response.addr.len    := Response_Address.netbuf.len;
      response.addr.maxlen := Response_Address.netbuf.maxlen;
      response.addr.buf    := Response_Address.netbuf.buf;

      if c_t_bind (int (Endpoint),
                   To_ptr (request'Address),
                   To_ptr (response'Address)) < 0
      then
         --  Error
         Raise_XTI_Error;
      end if;

      Response_Queue_Length := Integer (response.qlen);
   end Bind;

   procedure Bind
     (Endpoint              : in     POSIX.IO.File_Descriptor;
      Request_Address       : in     XTI_Address_Pointer;
      Request_Queue_Length  : in     Natural)
   is
      request  : POSIX.C.XTI.struct_t_bind;
   begin
      request.qlen         := unsigned (Request_Queue_Length);
      request.addr.len     := Request_Address.netbuf.len;
      request.addr.maxlen  := Request_Address.netbuf.maxlen;
      request.addr.buf     := Request_Address.netbuf.buf;

      if c_t_bind (int (Endpoint),
                   To_ptr (request'Address),
                   null) < 0
      then
         --  Error
         Raise_XTI_Error;
      end if;
   end Bind;

   procedure Bind
     (Endpoint         : in  POSIX.IO.File_Descriptor;
      Response_Address : in  XTI_Address_Pointer)
   is
      response : POSIX.C.XTI.struct_t_bind;
   begin
      response.addr.len    := Response_Address.netbuf.len;
      response.addr.maxlen := Response_Address.netbuf.maxlen;
      response.addr.buf    := Response_Address.netbuf.buf;

      if c_t_bind (int (Endpoint), null, To_ptr (response'Address)) < 0 then
         --  Error
         Raise_XTI_Error;
      end if;
   end Bind;

   procedure Bind (Endpoint : in POSIX.IO.File_Descriptor) is
   begin
      if c_t_bind (int (Endpoint), null, null) < 0 then
         --  Error
         Raise_XTI_Error;
      end if;
   end Bind;

   procedure Send_Disconnect_Request
     (Endpoint        : in POSIX.IO.File_Descriptor;
      Sequence_Number : in Natural)
   is
      call : POSIX.C.XTI.struct_t_call;
   begin
      call.sequence := int (Sequence_Number);
      call.udata.len := 0;
      call.addr.len := 0;
      call.opt.len := 0;

      if c_t_snddis (int (Endpoint), To_ptr (call'Address)) < 0 then
         --  Error
         Raise_XTI_Error;
      end if;
   end Send_Disconnect_Request;

   procedure Send_Disconnect_Request
     (Endpoint       : in POSIX.IO.File_Descriptor;
      User_Data      : in System.Address;
      Octets_To_Send : in POSIX.IO_Count)
   is
      call : POSIX.C.XTI.struct_t_call;
   begin
      --  sequence, addr, and opt are not used

      call.sequence := 0;
      call.addr.len := 0;
      call.addr.maxlen := 0;
      call.addr.buf := null;
      call.opt.len := 0;
      call.opt.maxlen := 0;
      call.opt.buf := null;
      call.udata.len := unsigned_int (Octets_To_Send);
      call.udata.maxlen := unsigned_int (Octets_To_Send);
      call.udata.buf := To_char_ptr (User_Data);

      if c_t_snddis (int (Endpoint), To_ptr (call'Address)) < 0 then
         --  Error
         Raise_XTI_Error;
      end if;
   end Send_Disconnect_Request;

end POSIX.XTI;

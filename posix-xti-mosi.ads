------------------------------------------------------------------------------
--                                                                          --
--   POSIX Ada95 Bindings for Protocol Independent Interfaces (P1003.5c)    --
--                                                                          --
--                        P O S I X . X T I . m O S I                       --
--                                                                          --
--                                  S p e c                                 --
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
--  only a  portion  of  the  documents  and  are  not to be interpreteted  --
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

with POSIX.C;
package POSIX.XTI.mOSI is

   type mOSI_XTI_Address is new XTI_Address with private;
   mOSI_Address_Length_Maximum : constant Natural := 0;
   type AP_Invocation_Id is new POSIX.Octet_Array;
   type AE_Invocation_Id is new POSIX.Octet_Array;
   type AP_Title is new POSIX.Octet_Array;
   type AE_Qualifier is new POSIX.Octet_Array;
   type Presentation_Address is new POSIX.Octet_Array;
   type mOSI_Address_Flags is new POSIX.Option_Set;
   AP_Invocation_Id_Valid : constant mOSI_Address_Flags := Empty_Set;
   AE_Invocation_Id_Valid : constant mOSI_Address_Flags := Empty_Set;
   function Get_Flags
      (Address : mOSI_XTI_Address)
     return mOSI_Address_Flags;
   procedure Set_Flags
      (Address : in out mOSI_XTI_Address;
       To      : in     mOSI_Address_Flags);
   function Get_AP_Invocation_Id
      (Address : mOSI_XTI_Address)
     return AP_Invocation_Id;
   procedure Set_AP_Invocation_Id
      (Address : in out mOSI_XTI_Address;
       To      : in     AP_Invocation_Id);
   function Get_AE_Invocation_Id
      (Address : mOSI_XTI_Address)
     return AE_Invocation_Id;
   procedure Set_AE_Invocation_Id
      (Address : in out mOSI_XTI_Address;
       To      : in     AE_Invocation_Id);
   function Get_AP_Title
      (Address : mOSI_XTI_Address)
     return AP_Title;
   function Get_AE_Qualifier
      (Address : mOSI_XTI_Address)
     return AE_Qualifier;
   function Get_Presentation_Address
      (Address : mOSI_XTI_Address)
     return Presentation_Address;
   procedure Set_OSI_Address
      (Address : in out mOSI_XTI_Address;
       AP      : in     AP_Title;
       AE      : in     AE_Qualifier;
       PA      : in     Presentation_Address);
   --  Dispatching operations for mOSI_XTI_Address
   procedure Get_Address
      (Info_Item : in     Connection_Information;
       Address   : in out mOSI_XTI_Address);

   mOSI_Connection_Mode     : constant Option_Level := 0;
   mOSI_Connectionless_Mode : constant Option_Level := 0;
   Application_Context      : constant Option_Name := 0;
   Presentation_Context     : constant Option_Name := 0;
   type Object_Identifier is new POSIX.Octet_Array;
   type Application_Context_Name is new Object_Identifier;
   type Presentation_Context_List is private;
   function Get_Value (Option_Item : Protocol_Option)
     return Application_Context_Name;
   procedure Set_Option
      (Option_Item : in out Protocol_Option;
       Level       : in     Option_Level;
       Name        : in     Option_Name;
       Value       : in     Application_Context_Name);
   function Get_Value (Option_Item : Protocol_Option)
     return Presentation_Context_List;
   procedure Set_Option
      (Option_Item : in out Protocol_Option;
       Level       : in     Option_Level;
       Name        : in     Option_Name;
       Value       : in     Presentation_Context_List);
   --  Presentation Context Definition and Result List
   type Presentation_Context_Item is private;
   type Presentation_Item_Id is new Integer;
   procedure Set_Presentation_Id
      (List : in out Presentation_Context_Item;
       Item : in     Presentation_Item_Id);
   function Get_Presentation_Id
      (Item : Presentation_Context_Item)
     return Presentation_Item_Id;
   type Negotiation_Result is
      (Presentation_Context_Accepted, Presentation_Context_Rejected,
       Rejected_No_Reason_Specified, Abstract_Syntax_Not_Supported,
       Transfer_Syntax_Not_Supported, Local_DCS_Limit_Exceeded);
   procedure Set_Negotiation_Result
      (List : in out Presentation_Context_Item;
       Item : in     Negotiation_Result);
   function Get_Negotiation_Result
      (Item : Presentation_Context_Item)
     return Negotiation_Result;
   type Syntax_Object_List is private;
   procedure Set_Syntax_Object
      (List : in out Presentation_Context_Item;
       Item : in     Syntax_Object_List);
   function Get_Syntax_Object
      (Item : Presentation_Context_Item)
     return Syntax_Object_List;
   Empty_Presentation_Context_List : constant Presentation_Context_List;
   procedure Make_Empty (List : in out Presentation_Context_List);
   procedure Add_Item_To_List
      (List : in out Presentation_Context_List;
       Item : in     Presentation_Context_Item);
   generic
       with procedure Action
          (Id   : in     Presentation_Context_Item;
           Quit : in out Boolean);
   procedure For_Every_Presentation_Context_Item
      (List : in Presentation_Context_List);
   Empty_Syntax_Object_List : constant Syntax_Object_List;
   procedure Make_Empty (List : in out Syntax_Object_List);
   procedure Add_Item_To_List
      (List : in out Syntax_Object_List;
       Item : in     Object_Identifier);
   generic
       with procedure Action
          (Object : in     Object_Identifier;
           Quit   : in out Boolean);
   procedure For_Every_Object_Identifier
      (List : in Syntax_Object_List);

   Rejected_By_Peer          : constant Reason_Code := 0;
   AC_Name_Not_Supported     : constant Reason_Code := 0;
   Unrecognized_AP_Title     : constant Reason_Code := 0;
   Unrecognized_AE_Qualifier : constant Reason_Code := 0;
   Authentication_Required   : constant Reason_Code := 0;
   Aborted_By_Peer           : constant Reason_Code := 0;
   No_Common_Version         : constant Reason_Code := 0;

private
   
   type mOSI_XTI_Address is new XTI_Address with null record;
   type Presentation_Context_List is new Integer;
   type Presentation_Context_Item is new Integer;
   type Syntax_Object_List is new Integer;
   Empty_Presentation_Context_List : constant Presentation_Context_List := 0;
   Empty_Syntax_Object_List : constant Syntax_Object_List := 0;
end POSIX.XTI.mOSI;

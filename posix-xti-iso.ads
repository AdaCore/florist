--  This code extracted from file dot5c.tex by Latex.

------------------------------------------------------------------------------
--                                                                          --
--   POSIX Ada95 Bindings for Protocol Independent Interfaces (P1003.5c)    --
--                                                                          --
--                         P O S I X . X T I . I S O                        --
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
package POSIX.XTI.ISO is

   ISO_TP_Level :  constant Option_Level := 0;
   type ISO_XTI_Address is new XTI_Address with private;
   type ISO_Option is (Enabled, Disabled);
   function Get_Value (Option_Item : Protocol_Option)
     return ISO_Option;
   procedure Set_Option
      (Option_Item : in out Protocol_Option;
       Level       : in     Option_Level;
       Name        : in     Option_Name;
       To          : in     ISO_Option);
   Residual_Error_Rate : constant Option_Name := 0;
   Priority            : constant Option_Name := 0;
   Protection          : constant Option_Name := 0;
   type Rate is private;
   type Priority_Level is (Top, High, Medium, Low, Default);
   type Protection_Level is new POSIX.Option_Set;
   No_Protection        : constant Protection_Level := Empty_Set;
   Passive_Protection   : constant Protection_Level := Empty_Set;
   Active_Protection    : constant Protection_Level := Empty_Set;
   Absolute_Requirement : constant Protection_Level := Empty_Set;
   function Get_Target_Rate (Item : Rate)
     return Duration;
   procedure Set_Target_Rate
      (Item : in out Rate;
       To   : in     Duration);
   function Get_Minimum_Acceptable_Rate (Item : Rate)
     return Duration;
   procedure Set_Minimum_Acceptable_Rate
      (Item : in out Rate;
       To   : in     Duration);
   function Get_Value (Option_Item : Protocol_Option)
     return Rate;
   procedure Set_Option
      (Option_Item : in out Protocol_Option;
       Level       : in     Option_Level;
       Name        : in     Option_Name;
       To          : in     Rate);
   function Get_Value (Option_Item : Protocol_Option)
     return Priority_Level;
   procedure Set_Option
      (Option_Item : in out Protocol_Option;
       Level       : in     Option_Level;
       Name        : in     Option_Name;
       To          : in     Priority_Level);
   function Get_Value (Option_Item : Protocol_Option)
     return Protection_Level;
   procedure Set_Option
      (Option_Item : in out Protocol_Option;
       Level       : in     Option_Level;
       Name        : in     Option_Name;
       To          : in     Protection_Level);
   --  Dispatching operations for ISO_XTI_Address
   procedure Get_Address
      (Info_Item : in     Connection_Information;
       Address   : in out ISO_XTI_Address);

   Throughput                   : constant Option_Name := 0;
   Connection_Transit_Delay     : constant Option_Name := 0;
   Transfer_Fail_Probability    : constant Option_Name := 0;
   Establishment_Fail_Probability
                                : constant Option_Name := 0;
   Release_Fail_Probability     : constant Option_Name := 0;
   Establishment_Delay          : constant Option_Name := 0;
   Release_Delay                : constant Option_Name := 0;
   Connection_Resilience        : constant Option_Name := 0;
   Expedited_Data               : constant Option_Name := 0;
   type Requested_Rate is private;
   type Throughput_Rate is private;
   type Transit_Delay_Rate is private;
   function Get_Called_Rate (Item : Requested_Rate)
     return Rate;
   procedure Set_Called_Rate
      (Item : in out Requested_Rate;
       To   : in     Rate);
   function Get_Calling_Rate (Item : Requested_Rate)
     return Rate;
   procedure Set_Calling_Rate
      (Item : in out Requested_Rate;
       To   : in     Rate);
   function Get_Throughput_Maximum (Item : Throughput_Rate)
     return Requested_Rate;
   procedure Set_Throughput_Maximum
      (Item : in out Throughput_Rate;
       To   : in     Requested_Rate);
   function Get_Throughput_Average (Item : Throughput_Rate)
     return Requested_Rate;
   procedure Set_Throughput_Average
      (Item : in out Throughput_Rate;
       To   : in     Requested_Rate);
   function Get_Transit_Delay_Maximum (Item : Transit_Delay_Rate)
     return Requested_Rate;
   procedure Set_Transit_Delay_Maximum
      (Item : in out Transit_Delay_Rate;
       To   : in     Requested_Rate);
   function Get_Transit_Delay_Average (Item : Transit_Delay_Rate)
     return Requested_Rate;
   procedure Set_Transit_Delay_Average
      (Item : in out Transit_Delay_Rate;
       To   : in     Requested_Rate);
   function Get_Value (Option_Item : Protocol_Option)
     return Throughput_Rate;
   procedure Set_Option
      (Option_Item : in out Protocol_Option;
       Level       : in     Option_Level;
       Name        : in     Option_Name;
       To          : in     Throughput_Rate);
   function Get_Value (Option_Item : Protocol_Option)
     return Transit_Delay_Rate;
   procedure Set_Option
      (Option_Item : in out Protocol_Option;
       Level       : in     Option_Level;
       Name        : in     Option_Name;
       To          : in     Transit_Delay_Rate);
   TPDU_Length_Maximum          : constant Option_Name := 0;
   Acknowledge_Time             : constant Option_Name := 0;
   Reassignment_Time            : constant Option_Name := 0;
   Preferred_Class              : constant Option_Name := 0;
   Alternative_Class_1          : constant Option_Name := 0;
   Alternative_Class_2          : constant Option_Name := 0;
   Alternative_Class_3          : constant Option_Name := 0;
   Alternative_Class_4          : constant Option_Name := 0;
   Extended_Format              : constant Option_Name := 0;
   Flow_Control                 : constant Option_Name := 0;
   Connection_Checksum          : constant Option_Name := 0;
   Network_Expedited_Data       : constant Option_Name := 0;
   Network_Receipt_Confirmation : constant Option_Name := 0;
   type ISO_COTS_Option is (Enabled, Disabled, Unspecified);
   type Transport_Class is
      (Class_0, Class_1, Class_2, Class_3, Class_4, Unspecified);
   function Get_Value (Option_Item : Protocol_Option)
     return ISO_COTS_Option;
   procedure Set_Option
      (Option_Item : in out Protocol_Option;
       Level       : in     Option_Level;
       Name        : in     Option_Name;
       To          : in     ISO_COTS_Option);
   function Get_Value (Option_Item : Protocol_Option)
     return Transport_Class;
   procedure Set_Option
      (Option_Item : in out Protocol_Option;
       Level       : in     Option_Level;
       Name        : in     Option_Name;
       To          : in     Transport_Class);
   function Get_Value (Option_Item : Protocol_Option)
     return Duration;
   procedure Set_Option
      (Option_Item : in out Protocol_Option;
       Level       : in     Option_Level;
       Name        : in     Option_Name;
       To          : in     Duration);

   Connectionless_Transit_Delay : constant Option_Name := 0;
   Connectionless_Checksum      : constant Option_Name := 0;

private
   
   type ISO_XTI_Address is new XTI_Address with null record;
   type Rate is new Integer;
   type Requested_Rate is new Integer;
   type Throughput_Rate is new Integer;
   type Transit_Delay_Rate is new Integer;
end POSIX.XTI.ISO;

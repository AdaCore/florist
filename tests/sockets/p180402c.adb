------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5c VALIDATION TEST SUITE                      --
--                                                                          --
--                            P 1 8 0 4 0 2 C                               --
--                                                                          --
--                                B o d y                                   --
--                                                                          --
--                                                                          --
--  Copyright (c) 1995-1999 Florida  State  University  (FSU).  All Rights  --
--  Reserved.                                                               --
--                                                                          --
--  This is free software;  you can redistribute it and/or modify it under  --
--  terms of the  GNU  General  Public  License  as published by the  Free  --
--  Software Foundation;  either version 2, or (at your option) any  later  --
--  version.  This  software  is distributed  in the hope that it  will be  --
--  useful, but WITHOUT ANY WARRANTY; without even the implied warranty of  --
--  MERCHANTABILITY   or  FITNESS FOR A PARTICULAR PURPOSE.   See the  GNU  --
--  General Public License for more details.  You  should have  received a  --
--  copy of the GNU General Public License  distributed  with  GNARL;  see  --
--  file  COPYING.  If not,  write to  the  Free  Software  Foundation, 59  --
--  Temple Place - Suite 330, Boston, MA 02111-1307, USA.                   --
--                                                                          --
--  Under contract  GS-35F-4506G, the U. S. Government obtained  unlimited  --
--  rights in the software and documentation contained herein.   Unlimited  --
--  rights are defined in DFAR 252,227-7013(a)(19).  By making this public  --
--  release,   the  Government  intends  to  confer  upon  all  recipients  --
--  unlimited  rights equal to those held by the Government.  These rights  --
--  include rights to use,  duplicate,  release  or  disclose the released  --
--  data an computer software  in whole or in part,  in any manner and for  --
--  any purpose whatsoever, and to have or permit others to do so.          --
--                                                                          --
--  DISCLAIMER   --   ALL MATERIALS OR INFORMATION HEREIN RELEASED,   MADE  --
--  AVAILABLE OR DISCLOSED ARE AS IS.   THE GOVERNMENT MAKES NO EXPRESS OR  --
--  IMPLIED WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS  --
--  OF THE SOFTWARE,  DOCUMENTATION  OR  OTHER INFORMATION RELEASED,  MADE  --
--  AVAILABLE OR DISCLOSED,  OR THE OWNERSHIP,  MERCHANTABILITY, OR FITNESS  --
--  FOR A PARTICULAR PURPOSE OF SAID MATERIAL.                              --
--                                                                          --
------------------------------------------------------------------------------

--  Multiprocess integrated test for package POSIX_Sockets
--  in IEEE Std 1003.5c Section 18.4 with package
--  POSIX_Sockets_Interent.

--  This test covers only features that depend only on
--  the packages (POSIX_Sockets/_Interent) and features from
--  other packages that are required to be supported.

with POSIX,
     POSIX_Sockets,
     POSIX_Sockets_Internet,
     POSIX_Report,
     POSIX_Process_Environment,
     System,
     POSIX_IO;

procedure p180402c is

   use POSIX,
       POSIX_Sockets,
       POSIX_Sockets_Internet,
       POSIX_IO,
       POSIX_Process_Environment,
       POSIX_Report;

   --  These are declared in Posix.C but not intended to be used from there
   PF_MAX    : constant := 25;
   PF_UNSPEC : constant := 0;
   PF_LOCAL  : constant := 1;
   PF_UNIX   : constant := 1;
   PF_INET   : constant := 2;
   PF_OSI    : constant := 19;
   --  *** MISSING: PF_ISO ***  --
   PF_ISO    : constant := 0;

   IPPROTO_IP   : constant := 0;
   IPPROTO_ICMP : constant := 1;
   IPPROTO_TCP  : constant := 6;
   IPPROTO_UDP  : constant := 17;
   IPPROTO_RAW  : constant := 255;

   procedure Send3_Tests (
      Socket   : in POSIX_IO.File_Descriptor;
      Buffer   : in System.Address;
      Options  : in Message_Option_Set := Empty_Set;
      To       : in Socket_Address_Pointer
               := Null_Socket_Address;
      Mask     : POSIX.Signal_Masking := All_Signals;
      Expected : in Error_Code := No_Error;
      Er1, Er2, Er3 : in String := "");

   procedure Send3_Tests (
      Socket   : in POSIX_IO.File_Descriptor;
      Buffer   : in System.Address;
      Options  : in Message_Option_Set := Empty_Set;
      To       : in Socket_Address_Pointer
               := Null_Socket_Address;
      Mask     : POSIX.Signal_Masking := All_Signals;
      Expected : in Error_Code := No_Error;
      Er1, Er2, Er3 : in String := "")
   is
      Sent    : POSIX.IO_Count;
   begin
      Send (Socket, Buffer, 11, Sent, To, Mask);
      if Er1 /= "" then
         Expect_Exception (Er1);
      else
         Comment ("Sent " & Integer'Image (Integer (Sent)) & " octets");
      end if;
   exception
   when E1 : POSIX_Error =>
      if Get_Error_Code /= Expected then
         Unexpected_Exception (E1, Er2);
      end if;
   when E2 : others => Unexpected_Exception (E2, Er3);
   end Send3_Tests;

--------------------------------------------------------------------------
--  Begin Tests

begin
--   Header ("p180402c");
--   Test ("package POSIX.Sockets");

   -----------------------------------------------------------------------
   --  Send on a socket

   Test ("Send 3 [18.4.13] (->)");
   declare
      Socket  : POSIX_IO.File_Descriptor := 0;
      Int_Add : aliased Internet_Socket_Address;
      Name    : Internet_Socket_Address_Pointer :=
        Int_Add'Unchecked_Access;
      Port    : Internet_Port;
      --  P180402 expects the following message
      Buffer  : POSIX_String (1 .. 11) := "Send 3 test";
   begin
      Socket := Create (PF_INET, Datagram_Socket, IPPROTO_UDP);
      Port := Internet_Port (Integer'Value
        (To_String (Value (Argument_List, 3))));
      Set_Internet_Port (Int_Add, Port);
      Set_Internet_Address (Int_Add,
        String_To_Internet_Address ("127.0.0.1"));
      Send3_Tests (Socket, Buffer'Address, To => +Name);
      Close (Socket);
   exception
      when E : others => Unexpected_Exception (E, "Ac01");
   end;

   -----------------------------------------------------------------------
   --  Trying to send on a closed file descriptor generates
   --  the Bad_File_Desriptor error code.

   Test ("-> Bad_File_Descriptor [18.4.13.3]");
   declare
      Socket : POSIX_IO.File_Descriptor := 0;
      Buffer : POSIX_String := "test";
      Int_Add : aliased Internet_Socket_Address;
      Name    : Internet_Socket_Address_Pointer :=
        Int_Add'Unchecked_Access;
   begin
      Set_Internet_Address (Int_Add,
        String_To_Internet_Address ("127.0.0.1"));
      Close (Socket);
      Send3_Tests (Socket, Buffer'Address, To => +Name,
                   Expected => Bad_File_Descriptor, Er1 => "Ac02",
                   Er2 => "Ac04", Er3 => "Ac05");
   exception
      when E : others => Unexpected_Exception (E, "Ac06");
   end;

   -----------------------------------------------------------------------
   --  Trying to send on a socket that is not connected generates
   --  the Not_Connected error code.

   Test ("-> Not_Connected [18.4.13.3]");
   declare
      Socket : POSIX_IO.File_Descriptor;
      Buffer : POSIX_String := "test";
      Int_Add : aliased Internet_Socket_Address;
      Name    : Internet_Socket_Address_Pointer :=
        Int_Add'Unchecked_Access;
   begin
      Set_Internet_Address (Int_Add,
         String_To_Internet_Address ("127.0.0.1"));
      Socket := Create (PF_INET, Stream_Socket, IPPROTO_TCP);
      Send3_Tests (Socket, Buffer'Address, To => +Name,
                   Expected => Not_Connected, Er1 => "Ac07",
                   Er2 => "Ac08", Er3 => "Ac09");
   exception
      when E : others => Unexpected_Exception (E, "Ac10");
   end;

   -----------------------------------------------------------------------
   --  Trying to send on a file descriptor that is not a socket generates
   --  the Not_A_Socket error code.

   Test ("-> Not_A_Socket [18.4.13.3]");
   declare
      Socket : POSIX_IO.File_Descriptor := 1;
      Buffer : POSIX_String := "test";
      Int_Add : aliased Internet_Socket_Address;
      Name    : Internet_Socket_Address_Pointer :=
        Int_Add'Unchecked_Access;
   begin
      Set_Internet_Address (Int_Add,
         String_To_Internet_Address ("127.0.0.1"));
      Send3_Tests (Socket, Buffer'Address, To => +Name,
                   Expected => Not_A_Socket, Er1 => "Ac11",
                   Er2 => "Ac12", Er3 => "Ac13");
   exception
      when E : others => Unexpected_Exception (E, "Ac14");
   end;

   -----------------------------------------------------------------------
   --  Trying to send on a file descriptor that is not a socket generates
   --  the Not_A_Socket error code.

--   Test ("-> Is_Already_Connected");
--   declare
--      Socket  : POSIX_IO.File_Descriptor := 0;
--      Int_Add : aliased Internet_Socket_Address;
--     Name    : Internet_Socket_Address_Pointer :=
--        Int_Add'Unchecked_Access;
--     Port    : Internet_Port;
--   begin
--      Socket := Create (PF_INET, Stream_Socket, IPPROTO_TCP);
--      Port := Internet_Port (Integer'Value
--        (To_String (Value (Argument_List, 3))));
--      Set_Internet_Port (Int_Add, Port);
--      Set_Internet_Address (Int_Add,
--        String_To_Internet_Address ("127.0.0.1"));
--      Connect (Socket, +Name);
--      Connect (Socket, +Name);
--      Close (Socket);
--   exception
--   when E1 : POSIX_Error =>
--      if Get_Error_Code /= Is_Already_Connected then
--        Unexpected_Exception (E1, "Ac15");
--      end if;
--   when E2 : others => Unexpected_Exception (E2, "Ac16");
--   end;

   -----------------------------------------------------------------------
   --  ??? what causes this

--   Test ("-> Incorrect_Address_Type [18.4.13]");
--   declare
--      Socket  : POSIX_IO.File_Descriptor := 0;
--      Int_Add : aliased Internet_Socket_Address;
--      Name    : Internet_Socket_Address_Pointer :=
--        Int_Add'Unchecked_Access;
--      Port    : Internet_Port;
--      --  P180402 expects the following message
--      Buffer  : POSIX_String (1 .. 11) := "Send 3 test";
--   begin
--      Socket := Create (PF_INET, Datagram_Socket, IPPROTO_UDP);
--      Port := Internet_Port (Integer'Value
--        (To_String (Value (Argument_List, 3))));
--      Set_Internet_Port (Int_Add, Port);
--      Set_Internet_Address (Int_Add,
--        String_To_Internet_Address ("127.0.0.1.7"));
--      Send3_Tests (Socket, Buffer'Address, To => +Name,
--                   Expected => Invalid_Argument, Er1 => "Ac17",
--                   Er2 => "Ac18", Er3 => "Ac19");
--      Close (Socket);
--   exception
--      when E : others => Unexpected_Exception (E, "Ac20");
--   end;


   Done;

exception when E : others => Fatal_Exception (E, "Ac21");
end p180402c;

------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5c VALIDATION TEST SUITE                      --
--                                                                          --
--                            P 1 8 0 4 0 2 D                               --
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
     POSIX_IO;

procedure p180402d is

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

   procedure Send_Message1_Tests (
      Socket   : in POSIX_IO.File_Descriptor;
      Message  : Socket_Message;
      Options  : in Message_Option_Set := Empty_Set;
      Mask     : POSIX.Signal_Masking := All_Signals;
      Expected : in Error_Code := No_Error;
      Er1, Er2, Er3 : in String := "");

   procedure Send_Message1_Tests (
      Socket   : in POSIX_IO.File_Descriptor;
      Message  : Socket_Message;
      Options  : in Message_Option_Set := Empty_Set;
      Mask     : POSIX.Signal_Masking := All_Signals;
      Expected : in Error_Code := No_Error;
      Er1, Er2, Er3 : in String := "")
   is
      Sent    : POSIX.IO_Count;
   begin
      Send_Message (Socket, Message, Sent, Mask);
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
   end Send_Message1_Tests;

--------------------------------------------------------------------------
--  Begin Tests

begin
--   Header ("p180402d");
--   Test ("package POSIX.Sockets");

   -----------------------------------------------------------------------
   --  Send a message on a connection-less socket

   Test ("Send Message 1 [18.4.13] (->)");
   declare
      Socket  : POSIX_IO.File_Descriptor := 0;
      Message  : Socket_Message;
      Int_Add : aliased Internet_Socket_Address;
      Name    : Internet_Socket_Address_Pointer :=
        Int_Add'Unchecked_Access;
      Port    : Internet_Port;

      Buffer1        : POSIX_String := "Test message. ";
      Buffer2        : POSIX_String := "This is a UDP/IP message. ";
      Buffer3        : POSIX_String := "It was sent using Send_Message1. ";
      IOV_Array      : IO_Vector_Array_Pointer :=
      new IO_Vector_Array (1 .. 3);

   begin
      Set_Buffer (IOV_Array (1), Buffer1 (Buffer1'First)'Address,
                  Buffer1'Length);
      Set_Buffer (IOV_Array (2), Buffer2 (Buffer2'First)'Address,
                  Buffer2'Length);
      Set_Buffer (IOV_Array (3), Buffer3 (Buffer3'First)'Address,
                  Buffer3'Length);

      Socket := Create (PF_INET, Datagram_Socket, IPPROTO_UDP);
      Port := Internet_Port (Integer'Value
                (To_String (Value (Argument_List, 3))));
      Set_Internet_Port (Int_Add, Port);
      Set_Internet_Address (Int_Add,
                String_To_Internet_Address ("127.0.0.1"));
      Set_Socket_Name (Message, +Name);
      Set_IO_Vector_Array (Message, IOV_Array);
      Send_Message1_Tests (Socket, Message, Er2 => "Ad01", Er3 => "Ad02");
      Close (Socket);
   exception
      when E : others => Unexpected_Exception (E, "Ad03");
   end;

   -----------------------------------------------------------------------
   --  Trying to send on a closed file descriptor generates
   --  the Bad_File_Desriptor error code.

   Test ("-> Bad_File_Descriptor [18.4.13.3]");
   declare
      Socket : POSIX_IO.File_Descriptor := 0;
      Message  : Socket_Message;
   begin
      Close (Socket);
      Send_Message1_Tests (Socket, Message,
                   Expected => Bad_File_Descriptor, Er1 => "Ad04",
                   Er2 => "Ad05", Er3 => "Ad06");
   exception
      when E : others => Unexpected_Exception (E, "Ad07");
   end;

   -----------------------------------------------------------------------
   --  Trying to send on a socket that is not connected generates
   --  the Not_Connected error code.

   Test ("-> Not_Connected [18.4.13.3]");
   declare
      Socket : POSIX_IO.File_Descriptor;
      Message  : Socket_Message;
   begin
      Socket := Create (PF_INET, Stream_Socket, IPPROTO_TCP);
      Send_Message1_Tests (Socket, Message,
                   Expected => Not_Connected, Er1 => "Ad08",
                   Er2 => "Ad09", Er3 => "Ad10");
   exception
      when E : others => Unexpected_Exception (E, "Ad11");
   end;


   -----------------------------------------------------------------------
   --  Trying to send on a file descriptor that is not a socket generates
   --  the Not_A_Socket error code.

   Test ("-> Not_A_Socket [18.4.13.3]");
   declare
      Socket : POSIX_IO.File_Descriptor := 1;
      Message  : Socket_Message;
   begin
      Send_Message1_Tests (Socket, Message,
                   Expected => Not_A_Socket, Er1 => "Ad12",
                   Er2 => "Ad13", Er3 => "Ad14");
   exception
      when E : others => Unexpected_Exception (E, "Ad15");
   end;

   Done;
exception when E : others => Fatal_Exception (E, "Ad16");
end p180402d;

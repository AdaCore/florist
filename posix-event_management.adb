------------------------------------------------------------------------------
--                                                                          --
--   POSIX Ada95 Bindings for Protocol Independent Interfaces (P1003.5c)    --
--                                                                          --
--               P O S I X . E v e n t _ M a n a g e m e n t                --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                                                                          --
--  Copyright (c) 1997 Lockheed Martin Corporation, All Rights Reserved.    --
--                                                                          --
--  This file is part of an implementation of an Ada95 API for the sockets  --
--  and network support services found in P1003.1g -- Protocol Independent  --
--  Interfaces.  It is integrated with the  FSU Implementation of POSIX.5b  --
--  (FLORIST), an Ada API for  POSIX OS services for use with the GNAT Ada  --
--  compiler and the FSU Gnu Ada Runtime Library (GNARL). The interface is  --
--  intended to be close to those specified in IEEE STD 1003.5: 1990, IEEE  --
--  STD 1003.5b: 1996, and IEEE Draft STD 1003.5c: 1997.                    --
--                                                                          --
--  This is free software;  you can redistribute it and/or modify it under  --
--  terms of the  GNU  General  Public  License as  published by the  Free  --
--  Software Foundation;  either version  2, or (at your option) any later  --
--  version.  This software is distributed  in  the hope  that  it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without  even the implied  warranty  --
--  of MERCHANTABILITY or FITNESS FOR A PARTICULAR  PURPOSE.  See  the GNU  --
--  General Public License for more details.  You  should have  received a  --
--  copy of the GNU General Public License  distributed  with  GNARL;  see  --
--  file  COPYING.  If not,  write to  the  Free  Software  Foundation, 59  --
--  Temple Place - Suite 330, Boston, MA 02111-1307, USA.                   --
--                                                                          --
--  As a special exception, if other files instantiate generics from  this  --
--  unit, or you link this unit with other files to produce an  executable, --
--  this  unit does not by itself cause the  resulting  executable  to  be  --
--  covered  by the  GNU  General  Public License. This exception does not  --
--  however invalidate any other  reasons why the executable file might be  --
--  covered by the GNU Public License.                                      --
--                                                                          --
------------------------------------------------------------------------------

with POSIX,
     POSIX.IO,
     POSIX.Signals;
with POSIX.C,
     POSIX.Implementation,
     System,
     Unchecked_Conversion;
package body POSIX.Event_Management is

   use POSIX.C,
       POSIX.C.Sockets,
       POSIX.Implementation;

   --  unchecked conversions for poll/select system calls

   function To_Int is new Unchecked_Conversion (Bits, int);
   function To_Bits is new Unchecked_Conversion (int, Bits);
   function To_ptr is new Unchecked_Conversion (System.Address, timeval_ptr);

   --  poll file descriptors from <sys/poll.h>
   function c_poll (fds : pollfd_ptr; nfds : unsigned; timeout : int)
            return int;
   pragma Import (C, c_poll, poll_LINKNAME);

   --  select file descriptors from <sys/select.h>
   function c_select (nfds : int; readfds : fd_set_ptr; writefds : fd_set_ptr;
                    exceptfds : fd_set_ptr; timeout : timeval_ptr)
           return int;
   pragma Import (C, c_select, select_LINKNAME);
   --  select macros to manipulate the fd_set bitmap
   procedure c_fd_set (fd : int; fdsetp : fd_set_ptr);
   pragma Import (C, c_fd_set, "c_fd_set");
   procedure c_fd_clr (fd : int; fdsetp : fd_set_ptr);
   pragma Import (C, c_fd_clr, "c_fd_clr");
   function c_fd_isset (fd : int; fdsetp : fd_set_ptr) return int;
   pragma Import (C, c_fd_isset, "c_fd_isset");
   procedure c_fd_zero (fdsetp : fd_set_ptr);
   pragma Import (C, c_fd_zero, "c_fd_zero");

   ------------
   --  Poll  --
   ------------

   function Get_File (Poll_Item : Poll_File_Descriptor)
      return POSIX.IO.File_Descriptor is
   begin
      return POSIX.IO.File_Descriptor (Poll_Item.C.fd);
   end Get_File;

   procedure Set_File
      (Poll_Item : in out Poll_File_Descriptor;
       File      : in     POSIX.IO.File_Descriptor) is
   begin
      Poll_Item.C.fd := int (File);
   end Set_File;
   
   function Get_Events (Poll_Item : Poll_File_Descriptor)
      return Poll_Events is
   begin
      return Poll_Events (Option_Set'
       (Option => To_Bits (int (Poll_Item.C.events))));
   end Get_Events;

   procedure Set_Events
      (Poll_Item : in out Poll_File_Descriptor;
       Events    : in     Poll_Events) is
   begin
      Poll_Item.C.events := short (To_Int (Option_Set (Events).Option));
   end Set_Events;

   function Get_Returned_Events (Poll_Item : Poll_File_Descriptor)
      return Poll_Events is
   begin
      return Poll_Events (Option_Set'
         (Option => To_Bits (int (Poll_Item.C.revents))));
   end Get_Returned_Events;

   procedure Set_Returned_Events
      (Poll_Item : in out Poll_File_Descriptor;
       Events    : in     Poll_Events) is
   begin
      Poll_Item.C.revents := short (To_Int (Option_Set (Events).Option));
   end Set_Returned_Events;

   procedure Poll
      (Files          : in out Poll_File_Descriptor_Set;
       Response_Count : out    Natural;
       Timeout        : in     Duration) is
   begin
      Response_Count := Natural (Check_NNeg (c_poll (
         fds     => Files (Files'First).C'Unchecked_Access,
         nfds    => unsigned (Files'Length),
         timeout => int (Long_Long_Integer (Timeout * 1000)))));
   end Poll;

   procedure Poll
      (Files          : in out Poll_File_Descriptor_Set;
       Response_Count : out    Natural) is
   begin
      Response_Count := Natural (Check_NNeg (c_poll (
         fds     => Files (Files'First).C'Unchecked_Access,
         nfds    => unsigned (Files'Length),
         timeout => INFTIM)));
   end Poll;
       
   -------------------
   --  Select_File  --
   -------------------

   procedure Make_Empty (Set : in out File_Descriptor_Set) is
   begin
      c_fd_zero (Set.C'Unchecked_Access);
   end Make_Empty;

   procedure Add_File_Descriptor_To_Set
      (Set  : in out File_Descriptor_Set;
       File : in     Select_File_Descriptor) is
   begin
      c_fd_set (int (File), Set.C'Unchecked_Access);
   end Add_File_Descriptor_To_Set;

   procedure Remove_File_Descriptor_From_Set
      (Set  : in out File_Descriptor_Set;
       File : in     Select_File_Descriptor) is
   begin
      c_fd_clr (int (File), Set.C'Unchecked_Access);
   end Remove_File_Descriptor_From_Set;

   function In_File_Descriptor_Set
      (Set  : File_Descriptor_Set;
       File : Select_File_Descriptor)
      return Boolean is
   begin
      if (c_fd_isset (int (File), Set.C'Unchecked_Access) = 0) then
         return False;
      else
         return True;
      end if;
   end In_File_Descriptor_Set;

   procedure Select_File
      (Read_Files     : in out File_Descriptor_Set;
       Write_Files    : in out File_Descriptor_Set;
       Except_Files   : in out File_Descriptor_Set;
       Files_Selected :    out Natural) is
   begin
      Files_Selected := Natural (Check_NNeg (c_select (
         nfds      => int (FD_SETSIZE),
         readfds   => Read_Files.C'Unchecked_Access,
         writefds  => Write_Files.C'Unchecked_Access,
         exceptfds => Except_Files.C'Unchecked_Access,
         timeout   => To_ptr (System.Null_Address))));
   end Select_File;

   procedure Select_File
      (Read_Files     : in out File_Descriptor_Set;
       Write_Files    : in out File_Descriptor_Set;
       Except_Files   : in out File_Descriptor_Set;
       Files_Selected :    out Natural;
       Timeout        : in     Duration) is
      Timeval : aliased struct_timeval;
   begin
      Timeval := To_Struct_Timeval (Timeout);
      Files_Selected := Natural (Check_NNeg (c_select (
         nfds      => int (FD_SETSIZE),
         readfds   => Read_Files.C'Unchecked_Access,
         writefds  => Write_Files.C'Unchecked_Access,
         exceptfds => Except_Files.C'Unchecked_Access,
         timeout   => Timeval'Unchecked_Access)));
   end Select_File;

   procedure Select_File
      (Read_Files     : in out File_Descriptor_Set;
       Write_Files    : in out File_Descriptor_Set;
       Except_Files   : in out File_Descriptor_Set;
       Files_Selected :    out Natural;
       Signals        : in     POSIX.Signals.Signal_Set) is
      Old_Mask : POSIX.Signals.Signal_Set;
   begin
      POSIX.Signals.Set_Blocked_Signals (Signals, Old_Mask);
      Files_Selected := Natural (Check_NNeg (c_select (
         nfds      => int (FD_SETSIZE),
         readfds   => Read_Files.C'Unchecked_Access,
         writefds  => Write_Files.C'Unchecked_Access,
         exceptfds => Except_Files.C'Unchecked_Access,
         timeout   => To_ptr (System.Null_Address))));
      POSIX.Signals.Set_Blocked_Signals (Old_Mask, Old_Mask);
   end Select_File;

   procedure Select_File
      (Read_Files     : in out File_Descriptor_Set;
       Write_Files    : in out File_Descriptor_Set;
       Except_Files   : in out File_Descriptor_Set;
       Files_Selected :    out Natural;
       Signals        : in     POSIX.Signals.Signal_Set;
       Timeout        : in     Duration) is
      Timeval  : aliased struct_timeval;
      Old_Mask : POSIX.Signals.Signal_Set;
   begin
      Timeval := To_Struct_Timeval (Timeout);
      POSIX.Signals.Set_Blocked_Signals (Signals, Old_Mask);
      Files_Selected := Natural (Check_NNeg (c_select (
         nfds      => int (FD_SETSIZE),
         readfds   => Read_Files.C'Unchecked_Access,
         writefds  => Write_Files.C'Unchecked_Access,
         exceptfds => Except_Files.C'Unchecked_Access,
         timeout   => Timeval'Unchecked_Access)));
      POSIX.Signals.Set_Blocked_Signals (Old_Mask, Old_Mask);
   end Select_File;

   procedure For_Every_File_In (Set : File_Descriptor_Set) is
      Quit : Boolean := False;
   begin
      for I in 0 .. FD_SETSIZE - 1 loop
         if (c_fd_isset (int (I), Set.C'Unchecked_Access) /= 0) then
            Action (Select_File_Descriptor (I), Quit);
            exit when Quit;
         end if;
      end loop;
   end For_Every_File_In;

end POSIX.Event_Management;

------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                            P 9 9 0 0 0 0 b                               --
--                                                                          --
--                                B o d y                                   --
--                                                                          --
--                                                                          --
--  Copyright (c) 1998-1999 Florida  State  University  (FSU).  All Rights  --
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
--  AVAILABLE OR DISCLOSED,  OR THE OWNERSHIP,  MECHANTABILITY, OR FITNESS  --
--  FOR A PARTICULAR PURPOSE OF SAID MATERIAL.                              --
--                                                                          --
------------------------------------------------------------------------------
--  [$Revision$]

--  See P9900doc.ads for more detailed explanation.


with POSIX,
     POSIX_Configurable_System_Limits,
     POSIX_Files,
     POSIX_IO,
     POSIX_Memory_Mapping,
     POSIX_Page_Alignment,
     POSIX_Permissions,
     POSIX_Report,
     P9900doc,
     P990000,
     System,
     System_Storage_Elements,
     Unchecked_Conversion;

package body P990003b is

   use POSIX,
       POSIX_IO,
       POSIX_Memory_Mapping,
       POSIX_Permissions,
       POSIX_Report,
       P990000,
       System_Storage_Elements;

   function Address_For_Data_Area return System.Address;

   Page_Size : constant Storage_Offset :=
     Storage_Offset (POSIX_Configurable_System_Limits.Page_Size);

   Shared_Data_Filename : constant POSIX_String := "P990003b_data";

   FD : File_Descriptor;

   Length : constant Integer :=
     Shared_Data_Area'Size / POSIX_Character'Size;

   function Address_For_Data_Area return System.Address is
      Result : System.Address;
      Dummy_Data : Shared_Data_Area;
      subtype Buf is IO_Buffer (1 .. Length);
      type Buf_Ptr is access Buf;
      function To_Buf_Ptr is
        new Unchecked_Conversion (System.Address, Buf_Ptr);
      Last : IO_Count := 0;
   begin

      --  Open or create file to hold shared data.

      begin
         FD := Open_Or_Create
           (Name => Shared_Data_Filename,
            Mode => Write_Only,
            Permissions => (Owner_Write => True, Others => False),
            Options => Exclusive);
         --  We are the first to create the file, so we can safely
         --  initialize it while others do not have read permission.
         Dummy_Data.Check := 9999;
         Write (FD, To_Buf_Ptr (Dummy_Data'Address).all, Last);
         --  The above will set the size, and initialize the file.
         Assert (Last = IO_Count (Length), "A001: P99003b");
         Change_Permissions (FD, Owner_Permission_Set);
         Close (FD);
      exception
      when E : POSIX_Error =>
         if Get_Error_Code = File_Exists then null;
            --  We will just open the file, below.
         else Fatal_Exception (E, "A002: P990003b");
         end if;
      end;

      --  The file already exists.
      loop
         begin
            FD := Open
              (Name => Shared_Data_Filename,
               Mode => Read_Write);
            exit;
         exception
         when POSIX_Error =>
            Check_Error_Code (Permission_Denied, "A003: P99003b");
            delay 1.0;
            Comment ("retrying open");
         end;
      end loop;

      --  Map the file into shared memory.

      Result := Map_Memory
        (Length => Storage_Offset (Length),
         Protection => Allow_Read + Allow_Write,
         Mapping => Map_Shared,
         File => FD,
         Offset => 0);

      return Result;

   exception
   when E : others =>
      Unexpected_Exception (E, "A004: P990003b");
      return System.Null_Address;
   end Address_For_Data_Area;

   Data_Address : constant System.Address := Address_For_Data_Area;

   Data  : aliased P990000.Shared_Data_Area;

   for Data'Address use Data_Address;

   pragma Import (Ada, Data);
   --  The reason we have the Import pragma here is that
   --  we don't want the compiler to try to initialize the shared
   --  object Data, since it may already contain data values written
   --  there by another process.

   function Shared_Data return P990000.Shared_Data_Ptr is
   begin
      return Data'Access;
   end Shared_Data;

   function To_Ptr is new Unchecked_Conversion
     (System.Address, P990000.Shared_Data_Ptr);
   Data_Ptr : P990000.Shared_Data_Ptr := To_Ptr (Data_Address);

   procedure Finalize is
   begin
      Unmap_Memory (Data_Address, Storage_Offset (Length));
      Close (FD);
      POSIX_Files.Unlink (Shared_Data_Filename);
   end Finalize;

begin
   Optional (Memory_Mapped_Files_Option, "A005: P990003b");
   Assert (Data.Check = 9999, "A006: P99003b");

exception when E : others => Fatal_Exception (E, "A007: P990003b");
end P990003b;

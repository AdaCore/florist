------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                             P 1 2 0 3 0 0                                --
--                                                                          --
--                                B o d y                                   --
--                                                                          --
--                                                                          --
--  Copyright (c) 1995-1998 Florida  State  University  (FSU).  All Rights  --
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
--  AVAILABLE OR DISCLOSED,  OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS  --
--  FOR A PARTICULAR PURPOSE OF SAID MATERIAL.                              --
--                                                                          --
------------------------------------------------------------------------------
--  [$Revision$]


--  Test for Memory_Mapping package

--  This is a pretty minimal test.  It needs expansion.

--  The test should check the protection, by trying some legal and
--  illegal accesses, and catching the possible exceptions
--  (Storage_Error, Program_Error, Constraint_Error).

--  The test should also check for error cases, such as overlapping
--  regions.

with POSIX,
     POSIX_Files,
     POSIX_IO,
     POSIX_Memory_Mapping,
     POSIX_Page_Alignment,
     POSIX_Permissions,
     POSIX_Report,
     System,
     System_Storage_Elements;

procedure p120300 is

   use POSIX,
       POSIX_IO,
       POSIX_Memory_Mapping,
       POSIX_Page_Alignment,
       POSIX_Permissions,
       POSIX_Report,
       System,
       System_Storage_Elements;

   Read_Write_Perms : constant Permission_Set :=
     (Owner_Read | Owner_Write => True,
      Group_Read | Group_Write => True,
      Others_Read | Others_Write => True,
      others => False);

begin

   Header ("p120300");

   -----------------------------------------------------------------------

   Test ("Map_Memory, Unmap_Memory (default address) [12.3.1]");
   declare
      Test_fd    : POSIX_IO.File_Descriptor;
      Last       : POSIX.IO_Count;
      Start_Addr : System.Address;
      Len        : constant System_Storage_Elements.Storage_Offset := 5;
      --  We are going to deal with strings of length 5.

      Offset     : constant IO_Count := Truncate_To_Page (1000);
      New_Offset : IO_Offset;

   begin

      Test_fd := Open_Or_Create ("test_file", Read_Write, Read_Write_Perms);

      --  set the file size
      --  Move the file pointer to the beginning of the nearest
      --  page in the file.
      Seek (Test_fd, IO_Offset (Offset), New_Offset, From_Beginning);

      --  Map the memory which is allocated by the OS.
      Start_Addr :=
        Map_Memory (Len, Allow_Read, Map_Shared, Test_fd, Offset);

      Assert (Start_Addr /= To_Address (-1), "A001");
      --  should not have failed, since this was the first one;

      declare
         C_Addr : constant System.Address := Start_Addr;
         S : POSIX_String (1 .. Integer (Len));
         for S'Address use C_Addr;
      begin
         --  Move the file pointer to the beginning of the nearest
         --  page in the file.
         Seek (Test_fd, IO_Offset (Offset), New_Offset, From_Beginning);
         --  Write something to the file
         Write (Test_fd, "hello", Last);

         --  Is the previous write reflected to S?
         Assert (S = "hello", "A002: mapped string does not match");
      end;

      Unmap_Memory (Start_Addr, Len);

      --  Map_Memory with Allow_Write option
      Start_Addr := Map_Memory
        (Len, Allow_Write, Map_Shared, Test_fd, Offset);

      declare
         C_Addr : constant System.Address := Start_Addr;
         S : POSIX_String (1 .. Integer (Len));
         for S'Address use C_Addr;
         Out_Str : POSIX_String (1 .. Integer (Len));
      begin
         S := "Good!";
         --  Move the file pointer to the beginning of the nearest
         --  page in the file.
         Seek (Test_fd, IO_Offset (Offset), New_Offset, From_Beginning);

         Read (Test_fd, Out_Str, Last);

         --  Is the previous memory update reflected to the file?
         Assert (Out_Str = "Good!", "A003: mapped string does not match");
      end;

      Unmap_Memory (Start_Addr, Len);

      --  Map_Memory with Allow_Read + Allow_Write option
      Start_Addr := Map_Memory
        (Len, Allow_Read + Allow_Write, Map_Shared, Test_fd, Offset);

      declare
         C_Addr : constant System.Address := Start_Addr;
         S : POSIX_String (1 .. Integer (Len));
         for S'Address use C_Addr;
         Out_Str : POSIX_String (1 .. Integer (Len));
      begin

         S := "First";  --  Wrote this

         --  Move the file pointer to the beginning of the nearest
         --  page in the file.
         Seek (Test_fd, IO_Offset (Offset), New_Offset, From_Beginning);
         Read (Test_fd, Out_Str, Last);

         --  File should have "First"
         Assert (Out_Str = "First", "A004: mapped string does not match");

         --  Move the file pointer to the beginning of the nearest
         --  page in the file.
         Seek (Test_fd, IO_Offset (Offset), New_Offset, From_Beginning);
         Write (Test_fd, "Good!", Last);

         --  Memory should have "Good!"
         Assert (S = "Good!", "A005: mapped string does not match");
      end;

      Unmap_Memory (Start_Addr, Len);

      --  Map_Memory with Empty_Set option
      Start_Addr := Map_Memory (Len, Empty_Set, Map_Shared, Test_fd, Offset);

      declare
         C_Addr : constant System.Address := Start_Addr;
         S : POSIX_String (1 .. Integer (Len));
         for S'Address use C_Addr;
      begin
         if Is_Supported (Memory_Protection_Option) then
            S := "First";  --  Wrote this
            --  If Memory_Protection_Option is supported this should raise an
            --  exception
            Assert (False, "A006: Expected Exception");
         end if;
      exception
         when E1 : others =>
            null;
      end;

      Unmap_Memory (Start_Addr, Len);

      --  Map_Memory with Allow_Execute option
      --  This test need to be further elaborated. Right now we just
      --  check if Map/Unmap_Memory return with no error ?????
      Start_Addr := Map_Memory
        (Len, Allow_Execute, Map_Shared, Test_fd, Offset);

      declare
         C_Addr : constant System.Address := Start_Addr;
         S : POSIX_String (1 .. Integer (Len));
         for S'Address use C_Addr;
      begin
         null;
      end;

      Unmap_Memory (Start_Addr, Len);

   exception
      when E1 : POSIX.POSIX_Error =>
         Optional (Memory_Mapped_Files_Option, Operation_Not_Implemented, E1,
                   "A007");
   when E2 : others => Unexpected_Exception (E2, "A008");
   end;

   -----------------------------------------------------------------------

   Test ("Map_Memory, Unmap_Memory (Nearby_Address) [12.3.2]");

   declare
      Test_fd    : POSIX_IO.File_Descriptor;
      Last       : POSIX.IO_Count;
      Test_Str   : POSIX_String := "hello";
      Start_Addr : System.Address;
      First      : System.Address := Truncate_To_Page (Test_Str'Address);
      Len        : constant System_Storage_Elements.Storage_Offset := 5;
      --  We are going to deal with strings of length 5.

      Offset     : constant IO_Count := Truncate_To_Page (1000);
      New_Offset : IO_Offset;

   begin

      Test_fd := Open_Or_Create ("test_file", Read_Write, Read_Write_Perms);

      --  set the file size
      --  Move the file pointer to the beginning of the nearest
      --  page in the file.
      Seek (Test_fd, IO_Offset (Offset), New_Offset, From_Beginning);

      --  Map the memory which is allocated by the OS to the nearest
      --  address specified.

      begin
         Start_Addr := Map_Memory (First, Len, Allow_Read, Map_Shared,
           Nearby_Address, Test_fd, Offset);
         --  On some systems the above call will fail,
         --  because the OS does not let us to specify Addr (First).
         --  In such case, let's use a system allocated page
         --  by providing Null_Address for First.
      exception
         when others =>
            First := System.Null_Address;
            Start_Addr := Map_Memory (First, Len, Allow_Read, Map_Shared,
              Nearby_Address, Test_fd, Offset);
      end;

      declare
         C_Addr : constant System.Address := Start_Addr;
         S : POSIX_String (1 .. Integer (Len));
         for S'Address use C_Addr;
      begin
         --  Move the file pointer to the beginning of the nearest
         --  page in the file.
         Seek (Test_fd, IO_Offset (Offset), New_Offset, From_Beginning);
         --  Write something to the file
         Write (Test_fd, "hello", Last);

         --  Is the previous write reflected to S?
         Assert (S = "hello", "A009: mapped string does not match");
      end;

      Unmap_Memory (Start_Addr, Len);

      --  Map_Memory with Allow_Write option
      Start_Addr := Map_Memory (First, Len, Allow_Write, Map_Shared,
        Nearby_Address, Test_fd, Offset);

      declare
         C_Addr : constant System.Address := Start_Addr;
         S : POSIX_String (1 .. Integer (Len));
         for S'Address use C_Addr;
         Out_Str : POSIX_String (1 .. Integer (Len));
      begin
         S := "Good!";
         --  Move the file pointer to the beginning of the nearest
         --  page in the file.
         Seek (Test_fd, IO_Offset (Offset), New_Offset, From_Beginning);

         Read (Test_fd, Out_Str, Last);

         --  Is the previous memory update reflected to the file?
         Assert (Out_Str = "Good!", "A010: mapped string does not match");
      end;

      Unmap_Memory (Start_Addr, Len);

      --  Map_Memory with Allow_Read + Allow_Write option
      Start_Addr :=
        Map_Memory (First, Len, Allow_Read + Allow_Write, Map_Shared,
          Nearby_Address, Test_fd, Offset);

      declare
         C_Addr : constant System.Address := Start_Addr;
         S : POSIX_String (1 .. Integer (Len));
         for S'Address use C_Addr;
         Out_Str : POSIX_String (1 .. Integer (Len));
      begin
         S := "First";  --  Wrote this

         --  Move the file pointer to the beginning of the nearest
         --  page in the file.
         Seek (Test_fd, IO_Offset (Offset), New_Offset, From_Beginning);

         Read (Test_fd, Out_Str, Last);

         --  File should have "First"
         Assert (Out_Str = "First", "A011: mapped string does not match");

         --  Move the file pointer to the beginning of the nearest
         --  page in the file.
         Seek (Test_fd, IO_Offset (Offset), New_Offset, From_Beginning);
         Write (Test_fd, "Good!", Last);

         --  Memory should have "Good!"
         Assert (S = "Good!", "A012: mapped string does not match");
      end;

      Unmap_Memory (Start_Addr, Len);

      --  Map_Memory with Empty_Set option
      Comment ("Map_Memory");
      Start_Addr := Map_Memory (First, Len, Empty_Set, Map_Shared,
        Nearby_Address, Test_fd, Offset);

      declare
         C_Addr : constant System.Address := Start_Addr;
         S : POSIX_String (1 .. Integer (Len));
         for S'Address use C_Addr;
      begin
         if Is_Supported (Memory_Protection_Option) then
            Comment ("Assignment to variable in read-only memory");
            S := "First";  --  Wrote this
            --  If Memory_Protection_Option is supported the
            --  above should raise an exception.
            Assert (False, "A013: No exception raised");
         end if;
      exception when E : others => null;
      end;

      Comment ("Unmap memory");
      Unmap_Memory (Start_Addr, Len);

      --  Map_Memory with Allow_Execute option
      --  This test need to be further elaborated. Right now we just
      --  check if Map/Unmap_Memory return with no error ?????
      Start_Addr := Map_Memory (First, Len, Allow_Execute, Map_Shared,
      Nearby_Address, Test_fd, Offset);
      --  should not fail, since this is the first one,

      declare
         C_Addr : constant System.Address := Start_Addr;
         S : POSIX_String (1 .. Integer (Len));
         for S'Address use C_Addr;
      begin
         null;
      end;

      Unmap_Memory (Start_Addr, Len);

   exception
      when E1 : POSIX.POSIX_Error =>
         Optional (Memory_Mapped_Files_Option, Operation_Not_Implemented, E1,
                   "A014");
   when E2 : others => Unexpected_Exception (E2, "A015");
   end;

   -----------------------------------------------------------------------

   Test ("Change_Protection [12.3.3]");

   declare

      Test_fd    : POSIX_IO.File_Descriptor;
      Last       : POSIX.IO_Count;
      Start_Addr : System.Address;
      Len        : constant System_Storage_Elements.Storage_Offset := 5;
      --  We are going to deal with strings of length 5.

      Offset     : constant IO_Count := Truncate_To_Page (1000);
      New_Offset : IO_Offset;
   begin

      Test_fd := Open_Or_Create ("test_file", Read_Write, Read_Write_Perms);

      --  set the file size
      --  Move the file pointer to the beginning of the nearest
      --  page in the file.
      Seek (Test_fd, IO_Offset (Offset), New_Offset, From_Beginning);

      --  Map the memory which is allocated by the OS.
      Start_Addr :=
        Map_Memory (Len, Allow_Read, Map_Shared, Test_fd, Offset);

      declare
         C_Addr : constant System.Address := Start_Addr;
         S : POSIX_String (1 .. Integer (Len));
         for S'Address use C_Addr;
      begin
         --  Move the file pointer to the beginning of the nearest
         --  page in the file.
         Seek (Test_fd, IO_Offset (Offset), New_Offset, From_Beginning);

         --  Write something to the file
         Write (Test_fd, "hello", Last);

         --  Is the previous write reflected to S?
         Assert (S = "hello", "A016: mapped string does not match");

         S := "Good!";   --  This should raise an exception.
         Assert (False, "A017: exception not raised");
      exception
         when E1 : others =>
            null;
      end;

      --  We change the memory protection to Read/Write.
      Change_Protection (Start_Addr, Len, Allow_Read + Allow_Write);

      declare
         C_Addr : constant System.Address := Start_Addr;
         S : POSIX_String (1 .. Integer (Len));
         for S'Address use C_Addr;
         Out_Str : POSIX_String (1 .. Integer (Len));
      begin
         --  Move the file pointer to the beginning of the nearest
         --  page in the file.
         Seek (Test_fd, IO_Offset (Offset), New_Offset, From_Beginning);

         --  Write something to the file
         Write (Test_fd, "hello", Last);

         --  Is the previous write reflected to S?
         Assert (S = "hello", "A018: mapped string does not match");

         S := "Good!";

         Seek (Test_fd, IO_Offset (Offset), New_Offset, From_Beginning);

         Read (Test_fd, Out_Str, Last);

         --  Is the previous memory update reflected to the file?
         Assert (Out_Str = "Good!", "A019: mapped string does not match");
      end;

      Unmap_Memory (Start_Addr, Len);

   exception
      when E1 : POSIX.POSIX_Error =>
         Optional (Memory_Mapped_Files_Option, Operation_Not_Implemented, E1,
                   "A020");
      when E2 : others => Unexpected_Exception (E2, "A021");
   end;

   -----------------------------------------------------------------------

   Test ("Synchronize_Memory");
   --  Test this only if Memory_Mapped_Files_Option and
   --  Synchronized_IO_Option is supported.

   declare

      Test_fd    : POSIX_IO.File_Descriptor;
      Last       : POSIX.IO_Count;
      Start_Addr : System.Address;
      Len        : constant System_Storage_Elements.Storage_Offset := 5;
      --  We are going to deal with strings of length 5.

      Offset     : constant IO_Count := Truncate_To_Page (1000);
      New_Offset : IO_Offset;
   begin

      Test_fd := Open_Or_Create ("test_file", Read_Write, Read_Write_Perms);

      --  set the file size
      --  Move the file pointer to the beginning of the nearest
      --  page in the file.
      Seek (Test_fd, IO_Offset (Offset), New_Offset, From_Beginning);

      --  Map the memory which is allocated by the OS.
      Start_Addr := Map_Memory
        (Len, Allow_Read + Allow_Write, Map_Shared, Test_fd, Offset);

      declare
         C_Addr : constant System.Address := Start_Addr;
         S : POSIX_String (1 .. Integer (Len));
         for S'Address use C_Addr;
         Out_Str : POSIX_String (1 .. Integer (Len));
      begin

         S := "hello";

         Synchronize_Memory (Start_Addr, Len, Wait_For_Completion);

         --  Move the file pointer to the beginning of the nearest
         --  page in the file.
         Seek (Test_fd, IO_Offset (Offset), New_Offset, From_Beginning);

         Read (Test_fd, Out_Str, Last);

         --  Is the previous memory update reflected to the file?
         Assert (Out_Str = "hello", "A022: mapped string does not match");

         S := "Good!";

         Synchronize_Memory (Start_Addr, Len, Invalidate_Cached_Data);

         Seek (Test_fd, IO_Offset (Offset), New_Offset, From_Beginning);
         Read (Test_fd, Out_Str, Last);

         --  Is the previous memory update reflected to the file?
         Assert (Out_Str = "Good!", "A023: mapped string does not match");
      end;

      Unmap_Memory (Start_Addr, Len);

   exception
      when E1 : POSIX.POSIX_Error =>
         Optional (Memory_Mapped_Files_Option, Synchronized_IO_Option,
           Operation_Not_Implemented, E1, "A024");
      when E2 : others => Unexpected_Exception (E2, "A025");
   end;

   -----------------------------------------------------------------------

   --  remove the file created for this test.
   POSIX_Files.Unlink ("test_file");

   Done;

exception when E : others => Fatal_Exception (E, "A026");
end p120300;

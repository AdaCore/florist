-----------------------------------------------------------------
--  file: bug.adb  [$Revision$]
-----------------------------------------------------------------

--  Demonstrates incorrect implementation of record assignment.
--  The bug appears at least in GNAT 3.10p,
--  and the version of 3.11w we are using here (FSU).
--  This problem seems to depend on:
--     the alignment clause
--     the representation clause
--     the aliased component

--  On Solaris 2.6 SuperSPARC (Sparcstation 20 HS14) the test
--  fails, and prints:

--  dad% ERROR: record assignment does not copy C.s_addr correctly.
--  Addr.C.s_addr= 1
--  Tmp_Addr.in_addr.sin_addr.s_addr= 0
--  Assignment works OK at leaf component level.
--  Tmp_Addr.in_addr.sin_addr.s_addr= 1

--  This bug came up in the Florist POSIX sockets interface.

--  --Ted Baker (baker@cs.fsu.edu)

with Ada.Text_IO;
procedure bug is

   ALIGNMENT : constant := 8;
   type int16 is range -2**15 .. 2**15 - 1;

   type in_addr_t is mod 2**32;

   type struct_in_addr is record
      s_addr : in_addr_t;
   end record;
   for struct_in_addr'Alignment use ALIGNMENT;

   type Internet_Address is record
      C : struct_in_addr;
   end record;

   type struct_sockaddr_in is record
      sin_family : int16;
      sin_port :   int16;
      sin_addr : struct_in_addr;
      sin_zero : String (1 .. 8);
   end record;
   for struct_sockaddr_in use record
      sin_family at 0 range 0 .. 15;
      sin_port at 2 range 0 .. 15;
      sin_addr at 4 range 0 .. 31;
      sin_zero at 8 range 0 .. 63;
   end record;

   type Internet_Socket_Address is record
      in_addr : aliased struct_sockaddr_in;
   end record;

   Addr : Internet_Address;
   Tmp_Addr : Internet_Socket_Address;

begin

   Addr.C.s_addr := 1;
   Tmp_Addr.in_addr.sin_addr := Addr.C;

   if Tmp_Addr.in_addr.sin_addr.s_addr /= Addr.C.s_addr then
      Ada.Text_IO.Put_Line
        ("ERROR: record assignment does not copy C.s_addr correctly.");
      Ada.Text_IO.Put_Line ("Addr.C.s_addr=" &
        in_addr_t'Image (Addr.C.s_addr));
      Ada.Text_IO.Put_Line
        ("Tmp_Addr.in_addr.sin_addr.s_addr=" &
         in_addr_t'Image (Tmp_Addr.in_addr.sin_addr.s_addr));
      Tmp_Addr.in_addr.sin_addr.s_addr := Addr.C.s_addr;
      if Tmp_Addr.in_addr.sin_addr.s_addr = Addr.C.s_addr then
         Ada.Text_IO.Put_Line
           ("Assignment works OK at leaf component level.");
         Ada.Text_IO.Put_Line
         ("Tmp_Addr.in_addr.sin_addr.s_addr=" &
          in_addr_t'Image (Tmp_Addr.in_addr.sin_addr.s_addr));
      end if;
   else
      Ada.Text_IO.Put_Line ("Assignment works OK.");
   end if;

end bug;

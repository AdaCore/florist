------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                             P 0 2 1 0 0 0                                --
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

with POSIX,
     POSIX_Limits,
     POSIX_Page_Alignment,
     POSIX_Report,
     POSIX_Configurable_System_Limits,
     System,
     System.Storage_Elements;

procedure p021000 is

   use POSIX,
       POSIX_Page_Alignment,
       POSIX_Report,
       POSIX_Configurable_System_Limits,
       System,
       System.Storage_Elements;

   PageSize : POSIX_Limits.Page_Size_Range;

begin

   Header ("p021000");
   Test ("package POSIX_Page_Alignment");

   -----------------------------------------------------------------------

   begin
      Test ("Determining Page Size");
      PageSize := Page_Size;
      Comment ("Page_Size =" &
        POSIX_Limits.Page_Size_Range'Image (PageSize));
      --   check zero page size
      Assert (PageSize /= 0, "A001");
   exception when E : others => Fatal_Exception (E, "A002");
   end;

   -----------------------------------------------------------------------

   declare
      Start_Addr, Original_Addr : System.Address;
      X : POSIX.POSIX_String (1 .. 5000);
   begin
      Test ("Truncate_To_Page (Address) [2.10]");
      Original_Addr := X'Address;
      Start_Addr := Truncate_To_Page (Original_Addr);
      Assert ((Start_Addr <= Original_Addr) and
         (Start_Addr mod Storage_Offset (PageSize) = 0), "A003");
   exception when E : others => Unexpected_Exception (E, "A004");
   end;

   -----------------------------------------------------------------------

   declare
      Start_Offset : POSIX.IO_Count;
      Original_Offset : POSIX.IO_Count := 45667;
   begin
      Test ("Truncate_To_Page (Offset) [2.10]");
      Start_Offset := Truncate_To_Page (Original_Offset);
      Assert ((Start_Offset <= Original_Offset)
         and (Integer (Start_Offset) mod PageSize = 0), "A005");
   exception when E : others => Unexpected_Exception (E, "A006");
   end;

   -----------------------------------------------------------------------

   declare
      Obj_Length : Storage_Offset;
      X : POSIX.POSIX_String (1 .. 5000);
   begin
      Test ("Adjust_Length (Address, Length) [2.10]");
      Obj_Length := Adjust_Length (X'Address, X'Size / System.Storage_Unit);
      Assert ((Integer (Obj_Length) mod PageSize = 0)
         and (Obj_Length >= (X'Size / System.Storage_Unit + (X'Address -
             Truncate_To_Page (X'Address)))), "A007");
   exception when E : others => Unexpected_Exception (E, "A008");
   end;

   -----------------------------------------------------------------------

   declare
      Obj_Length : Storage_Offset;
      Tmp, A : POSIX.IO_Count := 45667;
      L : Storage_Offset := 100;
   begin
      Test ("Adjust_Length (Offset, Length) [2.10]");
      Obj_Length := Adjust_Length (A, L);
      Tmp := A - Truncate_To_Page (A);
      Assert ((Integer (Obj_Length) mod PageSize = 0)
         and (Obj_Length >= L + Storage_Offset (Tmp)), "A009");
   exception when E : others => Unexpected_Exception (E, "A010");
   end;

   -----------------------------------------------------------------------

   declare
      Size : Natural := 5345;
      Result : Storage_Offset;
   begin
      Test ("Length (Size) [2.10]");
      Result := Length (Size);
      Assert (Length (Size) >= Storage_Offset (Size / System.Storage_Unit),
              "A011");
   exception when E : others => Unexpected_Exception (E, "A012");
   end;

   -----------------------------------------------------------------------

   Done;

exception when E : others =>  Fatal_Exception (E, "A013");
end p021000;

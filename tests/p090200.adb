------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                             P 0 9 0 2 0 0                                --
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
--  AVAILABLE OR DISCLOSED,  OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS  --
--  FOR A PARTICULAR PURPOSE OF SAID MATERIAL.                              --
--                                                                          --
------------------------------------------------------------------------------
--  [$Revision$]

with POSIX,
     POSIX_Group_Database,
     POSIX_Process_Identification,
     POSIX_Report,
     Test_Parameters,
     Text_IO,
     Unchecked_Deallocation;

procedure p090200 is

   use POSIX,
       POSIX_Group_Database,
       POSIX_Process_Identification,
       POSIX_Report,
       Test_Parameters;

begin

   Header ("p090200");
   Test ("package POSIX_Group_Database [9.2]");

   declare

      function Int_to_Gid (X : Integer) return Group_ID;
      function Int_to_Gid (X : Integer) return Group_ID is
      begin
         return Value (Integer'Image (X));
      end Int_to_Gid;

      Dummy_GID : Group_ID;
      Dummy_Name : POSIX_String (1 .. 2);
      Gd_gid : Group_Database_Item;
      Gd_name : Group_Database_Item;
      Dummy_Gd : Group_Database_Item;
      Uninitialized_Gd : Group_Database_Item;

      procedure Print_ID (ID : in POSIX.POSIX_String; Quit : in out Boolean);
      procedure Print_ID (ID : in POSIX.POSIX_String; Quit : in out Boolean)
      is
      begin
         Text_IO.Put (POSIX.To_String (ID) & " ");
      end Print_ID;

      function "=" (L, R : Group_ID_List) return Boolean;

      --  Predefined "=" may fail, because the type is private
      --  and no special "=" is specfied by the standard.
      --  Therefore, we have to define a proper equality test
      --  here.
      --  .... Fix POSIX 1003.5?
      --  It would be easier to implement a proper "=" in the
      --  package body, and friendlier.  Perhaps the standard should
      --  be revised to require that.

      type POSIX_String_Ptr is access POSIX_String;
      procedure Free is new Unchecked_Deallocation
        (POSIX_String, POSIX_String_Ptr);
      function "=" (L, R : Group_ID_List) return Boolean is
         List : array (Integer'(1) .. Length (L)) of POSIX_String_Ptr;
         Next : Integer := 1;
         Equal : Boolean := True;
         procedure Copy_One (S : POSIX_String; Quit : in out Boolean);
         procedure Copy_One (S : POSIX_String; Quit : in out Boolean) is
         begin
            List (Next) := new POSIX_String'(S);  Next := Next + 1;
         end Copy_One;
         procedure Copy_List is new For_Every_Member (Copy_One);
         procedure Compare_One (S : POSIX_String; Quit : in out Boolean);
         procedure Compare_One (S : POSIX_String; Quit : in out Boolean) is
         begin
            if List (Next).all /= S then Equal := False;
            end if;
            Free (List (Next));
            Next := Next + 1;
         end Compare_One;
         procedure Compare_Lists is new For_Every_Member (Compare_One);
      begin
         if Length (L) /= Length (R) then return False;
         end if;
         Copy_List (L);
         Next := 0;
         Compare_Lists (R);
         return Equal;
      end "=";

   begin
      Comment ("Get_Group_Database_Item");
      Gd_gid := Get_Group_Database_Item (Get_Real_Group_ID);
      Gd_name := Get_Group_Database_Item (Group_Name_Of (Gd_gid));
      Comment ("Group_Name_Of");
      Assert (Group_Name_Of (Gd_gid) = Group_Name_Of (Gd_name), "A001");
      Comment ("Group_ID_Of");
      Assert (Group_ID_Of (Gd_gid) = Group_ID_Of (Gd_name), "A002");
      Comment ("Length");
      Assert (Length (Group_ID_List_Of (Gd_gid)) =
        Length (Group_ID_List_Of (Gd_name)), "A003");
      Comment ("equality");
      --  Check equality on Group_ID_List
      Assert (Group_ID_List_Of (Gd_gid) = Group_ID_List_Of (Gd_name), "A004");
      Comment ("find unused group ID value");
      --  try to find a group id that is not used in this system
      for I in 1 .. Integer'Last loop
         begin
            Dummy_Gd := Get_Group_Database_Item (Int_to_Gid (I));
         exception
         when POSIX_Error =>
            Check_Error_Code (Invalid_Argument, "A005");
            exit;
         when E : others => Unexpected_Exception (E, "A006");
            exit;
         end;
         Assert (I /= Integer'Last, "A007: no free group ID");
      end loop;

      Comment ("Group_ID_Of non-member");
      begin
         --  Uninitialized_Gd should be non-valid since it not aquired using
         --  Get_Group_Database_Item. Test will raise exception.
         Dummy_GID := Group_ID_Of (Uninitialized_Gd);
         Expect_Exception ("A008");
      exception
      when POSIX_Error =>  Check_Error_Code (Invalid_Argument, "A009");
      end;
      Comment ("Get_Group_Database_Item of invalid group name");
      declare
         Gd : Group_Database_Item;
      begin
         --  try to find a group name that will not be used in most system.
         Gd := Get_Group_Database_Item (Unused_Group_Name);
         Expect_Exception ("A010");
      exception
      when POSIX_Error => Check_Error_Code (Invalid_Argument, "A011");
      end;
      Comment ("Group_Name_Of invalid item");
      begin
         --  Uninitialized_Gd should be a non-valid one.
         --   Test will raise exception.
         Dummy_Name := Group_Name_Of (Uninitialized_Gd);
         Expect_Exception ("A012");
      exception
      when POSIX_Error => Check_Error_Code (Invalid_Argument, "A013");
      end;
   exception
   when E : others => Unexpected_Exception (E, "A014");
   end;

   ------------------------------------------------------------------

   Done;

exception when E : others => Fatal_Exception (E, "A015");
end p090200;

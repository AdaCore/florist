------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                             P 0 4 0 1 0 0                                --
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

--  .... A legacy test, not in real-time area; could be improved.

with POSIX,
     POSIX_Process_Identification,
     POSIX_Report;

procedure p040100 is

   use POSIX,
       POSIX_Process_Identification,
       POSIX_Report;

begin

   Header ("p040100");
   Test ("package POSIX_Process_Identification");

   -----------------------------------------------------------

   declare
      Uid : User_ID;
      Gid : Group_ID;
   begin

      Test ("User Identification [4.1.3]");
      Comment ("Get_Real_User_ID");
      Uid := Get_Real_User_ID;

      Comment ("Get_Effective_User_ID");
      Assert (Uid = Get_Effective_User_ID, "A001");

      Comment ("Image and Value of User_ID");
      declare
         I : String := Image (Uid);
      begin
         Assert (I'First = 1, "A002");
         Assert (Value (I) = Uid, "A003");
      exception when E : others => Unexpected_Exception (E, "A004");
      end;

      --  Validity Of user And Group Ids Are Further tested As Part Of The
      --  test Of Get_File_Status.

      Comment ("Set_User_ID");
      Set_User_ID (Uid);

      Comment ("Set_User_ID to root");
      begin
         Set_User_ID (Value ("0"));
         Expect_Exception ("A005");
      exception
      when E1 : POSIX_Error =>
         Check_Error_Code (Operation_Not_Permitted, E1, "A006");
      when E2 : others => Unexpected_Exception (E2, "A007");
      end;

   -----------------------------------------------------------

      Test ("Group Identification [4.1.4]");
      Comment ("Get_Real_Group_ID");
      Gid := Get_Real_Group_ID;

      Comment ("Get_Effective_Group_ID");
      Assert (Gid = Get_Effective_Group_ID, "A008");

      Comment ("Image and Value on Group_ID");
      declare
         I : String := Image (Gid);
      begin
         --  Assert (I'First = 1);
         --  Do not understand the intent of the above test.
         --  Why should we make sure that the Group ID starts with 1?

         Assert (Value (I) = Gid, "A009");
      exception when E : others => Unexpected_Exception (E, "A010");
      end;

      Comment ("Set_Group_ID");
      Set_Group_ID (Gid);

      Comment ("Set_Group_ID without permission");
      declare
         Newgid : Group_ID;
      begin
         --  use "Image" And "Value" To Define Newgid := Gid + 1;
         Newgid := Value (Integer'Image (Integer'Value (Image (Gid)) + 1));
         Set_Group_ID (Newgid);
         Expect_Exception ("A011");
      exception
      when E1 : POSIX_Error =>
         Check_Error_Code (Operation_Not_Permitted, E1, "A012");
      when E2 : others => Unexpected_Exception (E2, "A013");
      end;

      Comment ("Image and Value on Group_List");
      declare
         List : Group_List := Get_Groups;
      begin
         Comment ("Groups :");
         for I in List'Range loop
            Comment (Image (List (I)));
         end loop;
      exception when E : others => Unexpected_Exception (E, "A014");
      end;

      --  Testing Of process IDs and process groups is covered under
      --  the testing of package POSIX_Process_Primitives.

   exception when E : others =>  Unexpected_Exception (E, "A015");
   end;

   ------------------------------------------------------------------

   Done;
exception when E : others => Fatal_Exception (E, "A016");
end p040100;

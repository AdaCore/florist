------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                             P 0 4 0 1 0 1                                --
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
--  AVAILABLE OR DISCLOSED,  OR THE OWNERSHIP,  MERCHANTABILITY, OR FITNESS  --
--  FOR A PARTICULAR PURPOSE OF SAID MATERIAL.                              --
--                                                                          --
------------------------------------------------------------------------------
--  [$Revision$]

--  Test package POSIX_Process_Identification.

with POSIX;
with POSIX_Process_Identification;
with POSIX_Report;
procedure p040101 is
   use POSIX;
   use POSIX_Process_Identification;
   use POSIX_Report;
begin
   Header ("p040101");
   Test ("package POSIX_Process_Identification [4.1]");

   -----------------------------------------------------------

   Test ("Process Identification Functions [4.1.1]");
   begin
      Comment ("current process ID = " & Image (Get_Process_ID));
      Comment ("parent process ID  = " & Image (Get_Parent_Process_ID));
      Assert (Value (Image (Get_Process_ID)) = Get_Process_ID, "A001");
      Assert (Value
        (Image (Get_Parent_Process_ID)) = Get_Parent_Process_ID, "A002");
   exception when E : others => Unexpected_Exception (E, "A003");
   end;

   -----------------------------------------------------------

   Test ("Process Group Identification [4.1.2]");
   begin
      Comment ("process group ID = " & Image (Get_Process_Group_ID));
      Assert (Value
        (Image (Get_Process_Group_ID)) = Get_Process_Group_ID, "A004");
      Assert (Value (Image (Get_Parent_Process_ID))
        = Get_Parent_Process_ID, "A005");
   exception when E : others => Unexpected_Exception (E, "A006");
   end;

   -----------------------------------------------------------

   Test ("Set_Process_Group_ID [4.1.2]]");
   begin
      Set_Process_Group_ID (Get_Process_ID, Get_Process_Group_ID);
   exception when E : others => Unexpected_Exception (E, "A007");
   end;

   -----------------------------------------------------------

   Test ("Create_Process_Group [4.1.2]");
   declare
      Group : Process_Group_ID;
   begin
      Create_Process_Group (Get_Process_ID, Group);
      Assert (Get_Process_Group_ID = Group, "A008");
      Assert (Value (Image (Group)) = Group, "A009");
   exception
   when E1 : POSIX_Error => Optional (Job_Control_Option, ENOSYS, E1, "A010");
   when E2 : others => Unexpected_Exception (E2, "A011");
   end;

   --  we are now leader of a new process group in the same session

   -----------------------------------------------------------

   Test ("Create_Session [4.1.2]");
   declare
      Group : Process_Group_ID;
   begin
      Create_Session (Group);
      --  should fail since we are already a process group leader
      Assert (False, "A012");
   exception
   when E1 : POSIX_Error => Check_Error_Code
     (Operation_Not_Permitted, E1, "A013");
   when E2 : others => Unexpected_Exception (E2, "A014");
   end;

   -----------------------------------------------------------

   Test ("operations on User_ID [4.1.3]");
   begin
      Assert (Value (Image
        (Get_Real_User_ID)) = Get_Real_User_ID, "A015");
      Comment ("real user ID =" & Image (Get_Real_User_ID));
      Assert (Value (Image
        (Get_Effective_User_ID)) = Get_Effective_User_ID, "A016");
      Comment ("effective user ID =" & Image (Get_Effective_User_ID));
      Set_User_ID (Get_Effective_User_ID);
   exception
   when E : others => Unexpected_Exception (E, "A017");
   end;

   -----------------------------------------------------------

   Test ("operations on Group_ID [4.1.4]");
   begin
      Assert (Value (Image
        (Get_Real_Group_ID)) = Get_Real_Group_ID, "A018");
      Comment ("real group ID =" & Image (Get_Real_Group_ID));
      Assert (Value (Image
        (Get_Effective_Group_ID)) = Get_Effective_Group_ID, "A019");
      Comment ("effective group ID =" & Image (Get_Effective_Group_ID));
      Set_Group_ID (Get_Effective_Group_ID);
   exception
   when E : others => Unexpected_Exception (E, "A020");
   end;

   -----------------------------------------------------------

   Test ("operations on Group_List [4.1.4]");
   begin
      declare
         Groups : constant Group_List := Get_Groups;
      begin
         for I in Groups'Range loop
            Comment ("Groups(" & Group_List_Index'Image (I)
              & ") =" & Image (Groups (I)));
            Assert (Value (Image (Groups (I))) = Groups (I), "A021");
         end loop;
      end;
   exception
   when E : others => Unexpected_Exception (E, "A022");
   end;

   -----------------------------------------------------------

   Done;
exception when E : others => Fatal_Exception (E, "A023");
end p040101;

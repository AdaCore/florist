------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                             P 0 5 0 1 0 0                                --
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

--  Test package POSIX_Permissions,
--  defined in IEEE Std 1003.5b Section 5.1.

--  This test covers only features that depend only on
--  the package itself.
--  See other tests for uses of this package in combination
--  with other features.

--  .... some ideas for additional tests:

--  Compare the value returned by an initial call
--  to Get_Allowed_Process_Permissions against the umask
--  inherited by a process from its parent.

with POSIX_Permissions,
     POSIX_Report;

procedure p050100 is

   use POSIX_Permissions,
       POSIX_Report;

begin
   Header ("p050100");

   ------------------------------------------------

   Test ("Permissions_Set constants [5.1.1]");
   declare

      --  The type Permission shall list the standard POSIX file modes.

      All_Permissions : constant Permission_Set :=
        (Others_Execute | Others_Write | Others_Read |
         Group_Execute | Group_Write | Group_Read |
         Owner_Execute | Owner_Write | Owner_Read |
         Set_Group_ID | Set_User_ID => True);

      Owner_Permissions : constant Permission_Set :=
        (Owner_Read | Owner_Write | Owner_Execute => True,
         others => False);

      Group_Permissions : constant Permission_Set :=
        (Group_Read | Group_Write | Group_Execute => True,
         others => False);

      Others_Permissions : constant Permission_Set :=
        (Others_Read | Others_Write | Others_Execute => True,
         others => False);

      Access_Permissions : constant Permission_Set :=
        (Owner_Read  | Owner_Write  | Owner_Execute => True,
         Group_Read  | Group_Write  | Group_Execute => True,
         Others_Read | Others_Write | Others_Execute => True,
         others => False);

      Set_Group_IDs : constant Permission_Set :=
        (Set_Group_ID => True, others => False);

      Set_User_IDs : constant Permission_Set :=
        (Set_User_ID => True, others => False);

   begin

      --  The constants sets in have the correct members.

      Assert (All_Permissions = All_Permissions, "A001");

      Assert (Owner_Permissions = Owner_Permission_Set, "A002");

      Assert (Group_Permissions = Group_Permission_Set, "A003");

      Assert (Others_Permissions = Others_Permission_Set, "A004");

      Assert (Access_Permissions = Access_Permission_Set, "A005");

      Assert (Set_Group_IDs = Set_Group_ID_Set, "A006");

      Assert (Set_User_IDs = Set_User_ID_Set, "A007");

   exception when E : others => Unexpected_Exception (E, "A008");
   end;

   ------------------------------------------------

   Test ("Get_Allowed_Process_Permissions [5.1.2]");
   declare
      Permissions : Permission_Set;
   begin

      --  Get_Allowed_Process_Permissions can be called,
      --  the result contains only file access permissions,
      --  and it does not raise any exception.

      Permissions := Get_Allowed_Process_Permissions;
      for I in Permissions'Range loop
         --  Check file access permissions
         Assert (Permissions (I) <= Access_Permission_Set (I),
           "A009");
      end loop;

   exception when E : others => Unexpected_Exception (E, "A010");
   end;

   ------------------------------------------------

   Test ("Set_Allowed_Process_Permissions w/two params [5.1.2]");
   declare
      All_Permissions : constant Permission_Set :=
        (Others_Execute | Others_Write | Others_Read |
         Group_Execute | Group_Write | Group_Read |
         Owner_Execute | Owner_Write | Owner_Read |
         Set_Group_ID | Set_User_ID => True);
      Old_Permissions : Permission_Set;
      Permissions : Permission_Set;
   begin

      Old_Permissions := Get_Allowed_Process_Permissions;

      --  Set_Allowed_Process_Permissions can be called,
      --  does not raise any exception,
      --  and returns a value consistent with Get_Allowed_Process_Permissions.

      Set_Allowed_Process_Permissions (All_Permissions, Permissions);
      Assert (Permissions = Old_Permissions, "A011");

      --  Get_Allowed_Process_Permissions returns a result
      --  consistent with what was last set.

      Assert (Get_Allowed_Process_Permissions = Access_Permission_Set,
        "A012");

   exception when E : others => Unexpected_Exception (E, "A013");
   end;

   ------------------------------------------------

   Test ("Set_Allowed_Process_Permissions w/one param [5.1.2]");
   declare
      Some_Permissions : constant Permission_Set :=
        (Others_Execute | Others_Write | Others_Read |
         Group_Execute | Group_Write | Group_Read => True,
         others => False);
   begin

      --  Set_Allowed_Process_Permissions can be called,
      --  and does not raise any exception.

      Set_Allowed_Process_Permissions (Some_Permissions);

      --  Get_Allowed_Process_Permissions returns a result
      --  consistent with what was last set.

      Assert (Get_Allowed_Process_Permissions = Some_Permissions,
        "A014");

   exception when E : others => Unexpected_Exception (E, "A015");
   end;

   ------------------------------------------------

   Done;

exception
when E : others => Fatal_Exception (E, "A016");
end p050100;

------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                             P 0 9 0 1 0 0                                --
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

--  Test package POSIX_User_Database,
--  in IEEE Standard 1003.5b, Section 9.1.

--  The test verifies that each of the subprograms can be called
--  with correct parameters, and that the results are consistent
--  with one another.
--  It assumes that the login name and real user ID of the current
--  process correspond, so the function Get_Login_Name will return the
--  name of the user ID that is returned by Get_Real_User_ID.

with POSIX,
     POSIX_Process_Identification,
     POSIX_Report,
     POSIX_User_Database;

procedure p090100 is

   use POSIX,
       POSIX_Process_Identification,
       POSIX_Report,
       POSIX_User_Database;

   Current_User : User_ID;
   User_ID,
   User_Name : User_Database_Item;

begin

   Header ("p090100");

   -----------------------------------------------------------------

   Test ("Get_Login_Name in [4.1.3]");
   declare
      Login_Name : POSIX_String := Get_Login_Name;
   begin

      -----------------------------------------------------------------

      Test ("Get_Real_User_ID in [4.1.3]");
      begin
         Current_User := Get_Real_User_ID;
      exception when E : others => Unexpected_Exception (E, "A001");
      end;

      -----------------------------------------------------------------

      Test ("Get_User_Database_Item [9.1.2]");
      begin
         User_ID := Get_User_Database_Item (Current_User);
         User_Name := Get_User_Database_Item (Login_Name);
      exception when E : others => Unexpected_Exception (E, "A002");
      end;

      -----------------------------------------------------------------

      Test ("User_Name_Of [9.1.1]");
      begin
         --  Check that user names of current user and login names match
         Assert (User_Name_Of (User_ID) = User_Name_Of (User_Name),
           "A003");
      exception when E : others => Unexpected_Exception (E, "A004");
      end;

      -----------------------------------------------------------------

      Test ("User_ID_Of [9.1.1]");
      begin
         --  Check that user IDs of current user and login name match
         Assert (User_ID_Of (User_ID) = User_ID_Of (User_Name),
           "A005");
      exception when E : others => Unexpected_Exception (E, "A006");
      end;

      -----------------------------------------------------------------

      Test ("Group_ID_Of [9.1.1]");
      begin
         --  Check that group IDs of user ID and login name match
         Assert (Group_ID_Of (User_ID) = Group_ID_Of (User_Name),
           "A007");
      exception when E : others => Unexpected_Exception (E, "A008");
      end;

      -----------------------------------------------------------------

      Test ("Initial_Directory_Of [9.1.1]");
      begin
         --  Check that initial directories of user ID and login name match
         Assert (Initial_Directory_Of (User_ID) =
           Initial_Directory_Of (User_Name),
           "A009");
      exception when E : others => Unexpected_Exception (E, "A010");
      end;

      -----------------------------------------------------------------

      Test ("Initial_Program_Of [9.1.1]");
      begin
         --  Check that initial programs of user ID and login name match
         Assert
           (Initial_Program_Of (User_ID) = Initial_Program_Of (User_Name),
           "A011");
      exception when E : others => Unexpected_Exception (E, "A012");
      end;

   exception when E : others => Unexpected_Exception (E, "A013");
   end;

   -----------------------------------------------------------------

   Done;

exception when E : others => Fatal_Exception (E, "A014");
end p090100;

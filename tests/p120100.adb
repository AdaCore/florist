------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                             P 1 2 0 1 0 0                                --
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

--  Some aspects of POSIX_Memory_Mapping are tested in another program,
--  p120101. P120101 also contains testing for some claimed "Untested Aspects"
--  by VSRT assertions. We make separate programs to improve portability.

with POSIX,
     POSIX_Memory_Locking,
     POSIX_Report;
procedure p120100 is

   use POSIX,
       POSIX_Memory_Locking,
       POSIX_Report;

begin

   Header ("p120100", Root_OK => True);
   Test ("package POSIX_Memory_Locking [12.1]");

   -----------------------------------------------------------------------

   begin
      Test ("Lock_All (Current_Pages) [12.1.1]");
      Lock_All (Current_Pages);
   exception
   when E1 : POSIX.POSIX_Error =>
      --  since this is the first lock operation, we
      --  assume there is no capacity limit
      Privileged (Memory_Locking_Privilege,
        Memory_Locking_Option, Operation_Not_Implemented, E1, "A001");
   when E2 : others => Unexpected_Exception (E2, "A002");
   end;

   -----------------------------------------------------------------------

   begin
      Test ("Unlock_All [12.1.1]");
      Unlock_All;
   exception
   when E1 : POSIX.POSIX_Error =>
      Privileged (Memory_Locking_Privilege,
        Memory_Locking_Option, Operation_Not_Implemented, E1, "A003");
   when E2 : others => Unexpected_Exception (E2, "A004");
   end;

   -----------------------------------------------------------------------

   begin
      Test ("Lock_All (Future_Pages) [12.1.1]");
      Lock_All (Future_Pages);
   exception
   when E1 : POSIX.POSIX_Error =>
      --  Since this is the first lock operation, we
      --  assume there is no capacity limit error.
      Privileged (Memory_Locking_Privilege,
        Memory_Locking_Option, Operation_Not_Implemented, E1, "A005");
   when E2 : others => Unexpected_Exception (E2, "A006");
   end;

   -----------------------------------------------------------------------

   begin
      Test ("Unlock_All [12.1.1]");
      Unlock_All;
   exception
   when E1 : POSIX.POSIX_Error =>
      Privileged (Memory_Locking_Privilege,
        Memory_Locking_Option, Operation_Not_Implemented, E1, "A007");
   when E2 : others => Unexpected_Exception (E2, "A008");
   end;

   -----------------------------------------------------------------------

   begin
      Test ("Unlock memory that hasn't been locked [12.1.1]");
      Unlock_All;
   exception
   when E1 : POSIX.POSIX_Error =>
      Privileged (Memory_Locking_Privilege,
        Memory_Locking_Option, Operation_Not_Implemented, E1, "A009");
   when E2 : others => Unexpected_Exception (E2, "A010");
   end;

   -----------------------------------------------------------------------

   declare
      Options : Memory_Locking_Options :=
        Memory_Locking_Options (POSIX.Empty_Set);
   begin
      Test ("Lock_All (Empty_Set [12.1.1])");
      Lock_All (Options);
      Expect_Exception ("A011");
   exception
   when E1 : POSIX.POSIX_Error =>
      if Get_Error_Code /= Invalid_Argument then
         Privileged (Memory_Locking_Privilege,
           Memory_Locking_Option, Operation_Not_Implemented, E1, "A012");
      end if;
   when E2 : others => Unexpected_Exception (E2, "A013");
   end;

   -----------------------------------------------------------------------

   Done;
   exception when E : others => Fatal_Exception (E, "A014");

end p120100;

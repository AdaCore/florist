------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                             P 1 2 0 2 0 0                                --
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

--  This test does not check that the operation has any effect;
--  just that it can be called.

with POSIX,
     POSIX_Memory_Range_Locking,
     POSIX_Page_Alignment,
     POSIX_Report,
     System.Storage_Elements;
procedure p120200 is
   use POSIX,
       POSIX_Memory_Range_Locking,
       POSIX_Report,
       System.Storage_Elements;

   X : Integer := 10;
   Addr : System.Address := POSIX_Page_Alignment.Truncate_To_Page (X'Address);
   Len  : System.Storage_Elements.Storage_Offset := 100;

begin

   Header ("p120200", Root_OK => True);

   -----------------------------------------------------------------------

   begin
      Test ("Lock_Range (Options) [12.2.1]");
      Lock_Range (Addr, Len);
      --  should not have resource problem
   exception
   when E1 : POSIX.POSIX_Error =>
      Privileged (Memory_Locking_Privilege,
        Memory_Range_Locking_Option, Operation_Not_Implemented, E1, "A001");
   when E2 : others => Unexpected_Exception (E2, "A002");
   end;

   -----------------------------------------------------------------------

   begin
      Test ("Unlock_Range [12.2.1]");
      Unlock_Range (Addr, Len);
   exception
   when E1 : POSIX.POSIX_Error =>
      Privileged (Memory_Locking_Privilege,
        Memory_Range_Locking_Option, Operation_Not_Implemented, E1, "A003");
   when E2 : others => Unexpected_Exception (E2, "A004");
   end;

   -----------------------------------------------------------------------

   Done;
exception when E : others => Fatal_Exception (E, "A005");
end p120200;

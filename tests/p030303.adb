------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                             P 0 3 0 3 0 3                                --
--                                                                          --
--                                B o d y                                   --
--                                                                          --
--                                                                          --
--  Copyright (c) 1998      Florida  State  University  (FSU).  All Rights  --
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

--  This a test of the POSIX_Signals package, and other features of
--  section 3.3 of POSIX.5b.  This test focusses on setups that are
--  likely to kill the program if the test fails.

with POSIX,
     POSIX_Process_Identification,
     POSIX_Report,
     POSIX_Signals;

procedure p030302 is
   use POSIX,
       POSIX_Process_Identification,
       POSIX_Report,
       POSIX_Signals;

begin

   Header ("p030303");

   ---------------------------------------------------------------------
   --  Any occurrences of SIGSEGV that are not identifiable as
   --  corresponding to checks that require some other exception to be
   --  raised are mapped to Program_Error, with Exception_Message
   --  "Signal_Segmentation_Violation".

   Test ("SIGSEGV treatment [3.3.2]");
   begin
      Send_Signal (Get_Process_ID, SIGSEGV);
      Expect_Exception ("A001");
   exception
   when Program_Error =>
     Check_Message (E, "Signal_Segmentation_Violation", "A002");
   when E : others => Unexpected_Exception (E, "A003");
   end;

   ---------------------------------------------------------------------
   --  SIGBUS is translated to Program_Error, with Exception_Message
   --  "Signal_Bus_Error".

   Test ("SIGBUS treatment [3.3.2]");
   begin
      Send_Signal (Get_Process_ID, SIGBUS);
      Expect_Exception ("A004");
   exception
   when Program_Error =>
     Check_Message (E, "Signal_Bus_Error", "A005");
   when E : others => Unexpected_Exception (E, "A006");
   end;
   
   ---------------------------------------------------------------------

   Done;
exception
   when E : others => Fatal_Exception (E, "A008");
end p030303;


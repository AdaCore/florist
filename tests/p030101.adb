------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                             P 0 3 0 1 0 1                                --
--                                                                          --
--                                B o d y                                   --
--                                                                          --
--                                                                          --
--  Copyright (c) 1995-1998 Florida  State  University  (FSU).  All Rights  --
--  Reserved.                                                               --
--                     Copyright (C) 1999-2022, AdaCore                     --
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

--  Test package POSIX_Process_Primitives, which is defined in
--  Section 3.1 of IEEE Std 1003.5b,
--  for consistency with package Ada.Command_Line.

--  Setup:  When this test is run the executable program p030301b must
--  be accessible via the pathname "./bin/p030101b".

with POSIX,
     POSIX_Process_Identification,
     POSIX_Process_Primitives,
     POSIX_Report;

procedure p030101 is

   use POSIX,
       POSIX_Process_Identification,
       POSIX_Process_Primitives,
       POSIX_Report;

   --  Cases to be tested by child processes:

   Normal_Completion : constant := 1;

   Child_Pathname : constant POSIX_String := "./bin/p030101b";

begin

   Header ("p030101");

   ---------------------------------------------------------------------

   Test ("Consistency of Ada and POSIX command line interfaces");
   declare
      Pid : Process_ID;
      Status : Termination_Status;
      Template : Process_Template;
      Args : POSIX_String_List;
   begin
      Open_Template (Template);
      Comment ("Set up argument list");
      POSIX.Append (Args, Child_Pathname);
      POSIX.Append (Args, "-child");
      POSIX.Append (Args, To_POSIX_String (Integer'Image (Normal_Completion)));
      Start_Process (Pid, Child_Pathname, Template, Args);
      Wait_For_Child_Process (Status, Pid);
      Check_Child_Status (Status, Pid, Normal_Exit, "A001");
      Close_Template (Template);
   exception
   when E : others => Unexpected_Exception (E, "A002");
   end;

   --------------------------------------------------------------------

   Done;

exception
when E : others => Fatal_Exception (E, "A003");
end p030101;

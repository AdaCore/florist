------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                             P 1 2 0 5 0 2                                --
--                                                                          --
--                                B o d y                                   --
--                                                                          --
--                                                                          --
--  Copyright (c) 1997-1999 Florida  State  University  (FSU).  All Rights  --
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


--  Test for POSIX_Generic_Shared_Memory_Test package

--  Two processes communicate through a shared memory object
--  containing two counters.  Each process increments its own
--  counter, and then waits until the other process's counter
--  has caught up.  They quit after a fixed number of
--  iterations.

--  Setup: The executable file for program p120502a
--  must be accessible via the path ``./p120502a''.

with POSIX,
     POSIX_Files,
     POSIX_Process_Identification,
     POSIX_Process_Primitives,
     POSIX_Report;

procedure p120502 is
   use POSIX,
       POSIX_Files,
       POSIX_Process_Identification,
       POSIX_Process_Primitives,
       POSIX_Report;

   Child_1 : Process_ID;
   Child_2 : Process_ID;

   Status : Termination_Status;
   Template : Process_Template;
   Args : POSIX_String_List;

   Child_Pathname : constant POSIX_String := "p120502a";
   Child_Exists : Boolean := False;

begin

   Header ("p120501.adb", True);

   declare
   begin
      Child_Exists := Is_Accessible
       (Child_Pathname,
        (Read_Ok | Execute_Ok => True, others => False));
   exception
   when others => null;
   end;

   Assert (Child_Exists,
     "A001 : executable file for child not found");

   Comment ("Creating two child processes");

   Open_Template (Template);
   Make_Empty (Args);
   POSIX.Append (Args, Child_Pathname);
   Pass_Through_Verbosity (Args);
   POSIX.Append (Args, "-child 1");
   Start_Process (Child_1, Child_Pathname, Template, Args);
   Make_Empty (Args);
   POSIX.Append (Args, Child_Pathname);
   Pass_Through_Verbosity (Args);
   POSIX.Append (Args, "-child 2");
   Start_Process (Child_2, Child_Pathname, Template, Args);

   Comment ("Waiting for children to terminate");

   Wait_For_Child_Process (Status, Child_1);
   Check_Child_Status (Status, Child_1, 0, "A002");
   Wait_For_Child_Process (Status, Child_2);
   Check_Child_Status (Status, Child_2, 0, "A003");

   Done;

exception
when E : others => Fatal_Exception (E, "A004");
end p120502;

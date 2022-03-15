------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                             P 0 3 0 2 0 0                                --
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
--  AVAILABLE OR DISCLOSED,  OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS  --
--  FOR A PARTICULAR PURPOSE OF SAID MATERIAL.                              --
--                                                                          --
------------------------------------------------------------------------------
--  [$Revision$]

--  Tests package POSIX_Unsafe_Process_Primitives,
--  in IEEE Std 1003.5b Section 3.2.

--  This test just checks that Fork operation works and
--  does not create more than one task in the child process.

--  ....  We still need to test the Exec operations.

with POSIX_Process_Identification,
     POSIX_Process_Primitives,
     POSIX_Unsafe_Process_Primitives,
     POSIX_Report;

procedure P030200 is

   use POSIX_Process_Identification,
       POSIX_Process_Primitives,
       POSIX_Report,
       POSIX_Unsafe_Process_Primitives;

   Child_Process : Process_ID := Null_Process_ID;

   task T;
   task body T is
   begin
      Comment ("internal task delays for half a second");
      delay 0.5;
      if Child_Process = Null_Process_ID then
         --  Fail because there is more than one task alive in child process
         Fail ("A001");
      end if;
      Comment ("internal task completes");
   end T;

   Status : Termination_Status;

begin

   Header ("p030200");

   -------------------------------------------------------------------------

   Test ("Fork operation [3.2.1]");

   --  Assert:  When the main thread of a process with more than one
   --  active task calls Fork,
   --  only the main thread is left in the child process.

   Child_Process := Fork;
   if Child_Process = Null_Process_ID then
      Comment ("child process completes");
      Exit_Process (0);
      --  Child process cannot exit normally, without hanging,
      --  since it is missing the tasking runtime system.
      --  The problem is that we have already swapped out the
      --  soft links, to make it safe to fork, but exit from
      --  the program will try to use the tasking runtime system.
   else
      Comment ("parent process waits for child");
      Wait_For_Child_Process (Status, Child_Process, True, False);
      Comment ("parent process detects child has terminated");
   end if;

   --------------------------------------------------------------------------

   Done;

exception when E : others => Fatal_Exception (E, "A002");
end P030200;

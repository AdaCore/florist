------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                       T e s t _ P a r a m e t e r s                      --
--                                                                          --
--                                S p e c                                   --
--                                                                          --
--                                                                          --
--  Copyright (c) 1997-1998 Florida  State  University  (FSU).  All Rights  --
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

--  This package contains test-harness details that may need tailoring
--  to fit the local implementation.  For now, we try to keep the tailorable
--  part to the body of the package.

--  The Valid_XX_Name functions must return
--  a distinct name for each positive value N.
--  The Invalid_XX_Name functions should return
--  at least two distinct names, for N=0 and N=1.

with POSIX,
     POSIX_Signals,
     POSIX_Timers;
package Test_Parameters is

   use POSIX;

   function Valid_MQ_Name
     (N : Positive) return POSIX_String;
   --  A string that is a valid message queue name.
   --  Should be different for each value of N.

   function Invalid_MQ_Name
     (N : Positive) return POSIX_String;
   --  A string that is not a valid message queue name.
   --  Should be different for each value of N.

   function Valid_Shared_Memory_Object_Name
     (N : Positive) return POSIX_String;
   --  A string that is a valid shared memory object name.
   --  Should be different for each value of N.

   function Invalid_Shared_Memory_Object_Name
     (N : Positive) return POSIX_String;
   --  A string that is not a valid shared memory object name.
   --  Should be different for each value of N.

   function Valid_Block_Device_Name return POSIX_String;
   --  Name of a file that is a block special device.

   function Valid_Character_Special_File_Name return POSIX_String;
   --  Name of a file that is a character special device.

   function Valid_Nonexistent_File_Name return POSIX_String;
   --  String that is a valid filename, but for which no file exists
   --  in the directory where the tests are run.

   function Is_Reserved_Signal
     (Sig : POSIX_Signals.Signal) return Boolean;
   --  Signals not defined by the standard, that are reserved by the
   --  implementation. See [2.2.2.117].

   function Action_Cannot_Be_Set
     (Sig : POSIX_Signals.Signal) return Boolean;
   --  Signals for which the application cannot set the action.
   --  See [3.3.9.3].

   function Signal_Mask_Is_Process_Wide return Boolean;
   --  See [3.3.1]

   function Valid_Semaphore_Name
     (N : Positive) return POSIX_String;
   --  Returns a valid semaphore name.
   --  The values must be distinct for values of N in the range
   --  1 .. POSIX_Limits.Portable_Semaphores_Maximum.

   function Delay_Unit return Duration;
   --  A value that is at least as large as the minimum delay
   --  granularity, but small enough that delaying this amount
   --  repeatedly will not cause a test to give the impression of
   --  hanging.

   function Short_Watchdog_Timeout return Duration;
   --  A value that is long enough that if a "short" test
   --  test does not complete within it we can assume the test has hung.

   function Unused_Group_Name return POSIX_String;
   --  A string that has no corresponding group.

   function Invalid_Clock_ID return POSIX_Timers.Clock_ID;
   --  An invalid clock ID.

end Test_Parameters;

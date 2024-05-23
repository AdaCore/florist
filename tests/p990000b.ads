------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                            P 9 9 0 0 0 0 b                               --
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
--  AVAILABLE OR DISCLOSED,  OR THE OWNERSHIP,  MECHANTABILITY, OR FITNESS  --
--  FOR A PARTICULAR PURPOSE OF SAID MATERIAL.                              --
--                                                                          --
------------------------------------------------------------------------------
--  [$Revision$]

--  This package supports the versions p9900** set of tests that
--  use multiple processes and shared memory.
--  This package contains code that is common to all these tests.

--  Each of these tests runs a set of periodic tasks/processes
--  that serially share access to an "I/O" object.
--  The tests differ by the mechanisms that they use for:
--    concurrency & scheduling
--      Ada tasks, task priority
--      POSIX processes, process priority
--    timing control
--      Ada clock and delay
--      POSIX clock and timers
--      POSIx clock and pthread_cond_timedwait
--    mutual exclusion and synchronization
--      Ada protected objects
--      POSIX mutexes and CV's (process-shared or otherwise)
--      POSIX semaphores

with Calendar,
     POSIX,
     System;
package p990000b is

   use Calendar,
       POSIX;

   subtype Jobs is Integer range 0 .. 5;

   type Job_Duration_Array is array (Jobs) of Duration;
   type JDA_Ptr is access all Job_Duration_Array;
   type Job_Integer_Array is array (Jobs) of Integer;
   type JIA_Ptr is access all Job_Integer_Array;
   type Job_Time_Array is array (Jobs) of Time;
   type JTA_Ptr is access all Job_Time_Array;
   type Time_Ptr is access all Time;
   type Boolean_Ptr is access all Boolean;

   Priority : constant array (Jobs) of System.Priority :=
     (System.Priority'Last - 1,
      System.Priority'Last - 2,
      System.Priority'Last - 3,
      System.Priority'Last - 4,
      System.Priority'Last - 5,
      System.Priority'Last - 6);

   Shared_Data_Filename : constant POSIX_String := "p990040_data";

   Period : JDA_Ptr;
   --  periods of jobs are set in the body of this package

   Missed_Deadlines : Boolean_Ptr;
   --  ????
   --  Need to do something to insure atomic access to object.

   Computation_Load,
   Input_Load,
   Output_Load : JIA_Ptr;

   Start_Time,
   Stop_Time         : Time_Ptr;
   Next_Request_Time : JTA_Ptr;

   Seconds_To_Run : constant Integer := 10;
   --  number of seconds to run each simulation
   --  during bisection

   procedure Do_Input (Job : Jobs);
   procedure Do_Computation (Job : Jobs);
   procedure Do_Output (Job : Jobs);
   generic
      with procedure Run_Jobs;
   procedure Find_Utilization_Limit;

end p990000b;





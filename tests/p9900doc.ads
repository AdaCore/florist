------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                            P 9 9 0 0 d o c                               --
--                                                                          --
--                                S p e c                                   --
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
--  AVAILABLE OR DISCLOSED,  OR THE OWNERSHIP,  MECHANTABILITY, OR FITNESS  --
--  FOR A PARTICULAR PURPOSE OF SAID MATERIAL.                              --
--                                                                          --
------------------------------------------------------------------------------
--  [$Revision$]

--  This package is a dummy, just to hold comments documenting the overall
--  structure and common elements of the P9900** set of tests.

--  Each of these tests runs a set of periodic tasks or processes
--  that serially share access to an "I/O" object.

--  The tests differ by the mechanisms that they use for:
--    concurrency & priority scheduling
--    timing control
--    mutual exclusion and synchronization
--    access to shared data

--  This set of tests comprises the following files:

--  Common components, used by all P9900** tests
--  -----------------

--  P990000.ads   common declarations
--  P990000.adb

--  P9900x0.ads   generic main program
--  P9900x0.adb

--  Mutual exclusion
--  ----------------

--  P990001a.ads  mutual exclusion, using Ada protected objects
--  P990001a.adb
--  P990001b.ads  mutual exclusion, using POSIX mutexes and condition variables
--  P990001b.adb
--  P990001c.ads  mutual exclusion, using POSIX named semaphores
--  P990001c.adb

--  Timing Control
--  --------------

--  P990002a.ads  scheduling, using Ada clock and delay until statement
--  P990002a.adb
--  P990002b.ads  scheduling, using POSIX Clock_Realtime and Timed_Wait on a CV
--  P990002b.adb
--  P990002c.ads  scheduling, using POSIX_Calendar.Clock and Timed_Wait on a CV
--  P990002c.adb

--  Access to Shared Data
--  ---------------------

--  P990003a.ads  shared variable stuff, single process w/local memory
--  P990003a.adb
--  P990003b.ads  shared variable stuff, multiple processes w/memory mapping
--  P990003b.adb

--  Tests  (N = 1 .. 4)
--  -----

--  P9900N0a.ads  components of test P990010
--  P9900N0.ads   main program of test
--  P9900N0.adb
--  P9900N0b.ads  main program of child process, if applicable

--  Conflict Matrix

---------------------------------------------------------------------------
--           p p p p p p p p p p p p
--           9 9 9 9 9 9 9 9 9 9 9 9
--           9 9 9 9 9 9 9 9 9 9 9 9
--           0 0 0 0 0 0 0 0 0 0 0 0
--           0 x x 0 0 0 0 0 0 0 0 0
--           0 0 0 1 1 1 2 2 2 2 3 3
--               * a b c a b c d a b
-----------------------------------------------------------------------------
--  P990000                                               common declarations
--  P9900x0                                                     generic mains
--  P9900x0*                     X                                *child_main
-----------------------------------------------------------------------------
--  P990001a         X X           X                        protected objects
--  P990001b       X   X           X                    basic mutexes and CVs
--  P990001c       X X                                       named semaphores
-----------------------------------------------------------------------------
--  P990002a               X X X                              clock and delay
--  P990002b             X   X X                Clock_Realtime and Timed_Wait
--  P990002c             X X   X          POSIX_Calendar.Clock and Timed_Wait
--  P990002d             X X X                  Ada.Real_Time.Clock and delay
-----------------------------------------------------------------------------
--  P990003a     X                 X                             local memory
--  P990003b       X X           X                              shared memory
-----------------------------------------------------------------------------

--  Use Matrix

---------------------------------------------------------------------------
--           p p p p p
--           9 9 9 9 9
--           9 9 9 9 9
--           0 0 0 0 0
--           0 0 0 0 0
--           1 2 3 4 5
--           0 0 0 0 0
---------------------------------------------------------------------------
--  P990000  X X X X X                                  common declarations
--  P9900x0  X X X X X                                        generic mains
--  P9900x0*       X X                                          *child_main
---------------------------------------------------------------------------
--  P990001a X                                            protected objects
--  P990001b   X X                                    basic mutexes and CVs
--  P990001c       X X                                     named semaphores
---------------------------------------------------------------------------
--  P990002a X     X                                        clock and delay
--  P990002b   X                              Clock_Realtime and Timed_Wait
--  P990002c     X                      POSIX_Calendar.Clock and Timed_Wait
--  P990002d         X                        Ada.Real_Time.Clock and delay
---------------------------------------------------------------------------
--  P990003a X X X                                             local memory
--  P990003b       X X                                        shared memory
---------------------------------------------------------------------------

package P9900doc is
end P9900doc;

------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                            P 9 9 0 0 4 0 a                               --
--                                                                          --
--                                S p e c                                   --
--                                                                          --
--                                                                          --
--  Copyright (c) 1998 Florida  State  University  (FSU).       All Rights  --
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

--  Simulate mix of periodic jobs with rate monotone priorities.
--  This version uses:
--     POSIX processes and process priorities for concurrency and scheduling
--     Ada delay statements and Ada.Calendar.Clock for timing control
--     POSIX semaphores for mutual exclusion

--  See P9900doc.ads for more detailed explanation.

with P9900doc,
     P9900x0,
     P990001c,  --  POSIX named semaphores
     P990002a,  --  Calendar and delay
     P990003b;  --  interprocess shared data
package P990040a is new P9900x0
  (Version => "40",
   Needs_Clock_Realtime => False,
   Jobs_Are_Processes => True,
   Initialize_Sync => P990001c.Initialize,
   Do_Input => P990001c.Do_Input,
   Do_Output => P990001c.Do_Output,
   Start_All_Jobs => P990001c.Start_All_Jobs,
   Await_All_Jobs_Done => P990001c.Await_All_Jobs_Done,
   Await_Start => P990001c.Await_Start,
   Done_Job => P990001c.Done_Job,
   Finalize_Sync => P990001c.Finalize,
   Initialize_Scheduling => P990002a.Initialize_Scheduling,
   Reschedule => P990002a.Reschedule,
   Finalize_Scheduling => P990002a.Finalize,
   Shared_Data =>  P990003b.Shared_Data,
   Finalize_Shared_Data => P990003b.Finalize
  );

------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                            P 9 9 0 0 9 0 a                               --
--                                                                          --
--                                S p e c                                   --
--                                                                          --
--                                                                          --
--  Copyright (c) 1999      Florida  State  University  (FSU).  All Rights  --
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
--  AVAILABLE OR DISCLOSED,  OR THE OWNERSHIP,  MERCHANTABILITY, OR FITNESS --
--  FOR A PARTICULAR PURPOSE OF SAID MATERIAL.                              --
--                                                                          --
------------------------------------------------------------------------------
--  [$Revision$]

--  Simulate mix of periodic jobs with rate monotone priorities.
--  See P9900doc.ads for more detailed explanation.

with P9900doc,
     P9900x0,
     P990001a,  --  protected objects
     P990002b,  --  Clock_Realtime and Timed_Wait
     P990003a;  --  locally shared data
package P990090a is new P9900x0
  (Version => "90",
   Needs_Clock_Realtime => True,
   Jobs_Are_Processes => False,
   Initialize_Sync => P990001a.Initialize,
   Do_Input => P990001a.Do_Input,
   Do_Output => P990001a.Do_Output,
   Start_All_Jobs => P990001a.Start_All_Jobs,
   Await_All_Jobs_Done => P990001a.Await_All_Jobs_Done,
   Await_Start => P990001a.Await_Start,
   Done_Job => P990001a.Done_Job,
   Finalize_Sync => P990001a.Finalize,
   Initialize_Scheduling => P990002b.Initialize_Scheduling,
   Reschedule => P990002b.Reschedule,
   Finalize_Scheduling => P990002b.Finalize,
   Shared_Data =>  P990003a.Shared_Data,
   Finalize_Shared_Data => P990003a.Finalize
  );


----------------------
-- REVISION HISTORY --
----------------------

--  ----------------------------
--  revision 1.1
--  date: 1998/06/28 21:20:40;  author: baker;  state: Exp;
--  Initial revision
--  ----------------------------
--  revision 1.2  locked by: baker;
--  date: 1998/06/30 13:30:45;  author: baker;  state: Exp;  lines: +4 -3
--  Added finalization.
--  ----------------------------
--  New changes after this line.  Each line starts with: "--  "
--  Added Initialize_Sync.

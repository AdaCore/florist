------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                             P 9 9 0 0 x 0                                --
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
--  AVAILABLE OR DISCLOSED,  OR THE OWNERSHIP,  MERCHANTABILITY, OR FITNESS  --
--  FOR A PARTICULAR PURPOSE OF SAID MATERIAL.                              --
--                                                                          --
------------------------------------------------------------------------------
--  [$Revision$]

--  Simulate mix of periodic jobs with rate monotone priorities.
--  See P9900doc for more detailed explanation.

with P9900doc,
     POSIX,
     P990000;
use  P990000;
generic
   Version : String;
   Needs_Clock_Realtime : Boolean;
   Jobs_Are_Processes : Boolean;
   with procedure Initialize_Sync;
   with procedure Do_Input (Load : Natural);
   with procedure Do_Output (Load : Natural);
   with procedure Start_All_Jobs;
   with procedure Await_All_Jobs_Done;
   with procedure Await_Start;
   with procedure Done_Job;
   with procedure Finalize_Sync;
   with procedure Initialize_Scheduling (Shared_Data : Shared_Data_Ptr);
   with function  Reschedule (Job : Jobs) return Boolean;
   with procedure Finalize_Scheduling;
   with function  Shared_Data return Shared_Data_Ptr;
   with procedure Finalize_Shared_Data;
package P9900x0 is
   procedure Parent_Main;
   procedure Child_Main;
   --  Child_Main is only used for tests that involve creation of
   --  more than one POSIX process.
end P9900x0;

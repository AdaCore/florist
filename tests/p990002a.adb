------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                            P 9 9 0 0 0 2 a                               --
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
--  AVAILABLE OR DISCLOSED,  OR THE OWNERSHIP,  MERCHANTABILITY, OR FITNESS  --
--  FOR A PARTICULAR PURPOSE OF SAID MATERIAL.                              --
--                                                                          --
------------------------------------------------------------------------------
--  [$Revision$]

with Calendar,
     P990000,
     POSIX,
     POSIX_Report;
package body P990002a is

   use Calendar,
       P990000,
       POSIX_Report;

   Data : Shared_Data_Ptr;
   Start_Time,
   Stop_Time : Calendar.Time;
   Next_Request_Time : array (Jobs) of Calendar.Time;

   procedure Initialize_Scheduling (Shared_Data : Shared_Data_Ptr) is
   begin
      Data := Shared_Data;
      Assert (Data.Check = 9999, "A001: P99002a");
      Start_Time := Data.Start_Calendar_Time;
      Comment ("Clock - Start_Time", POSIX.To_Timespec (Clock - Start_Time));
      Stop_Time := Start_Time + Duration (Seconds_To_Run);
      Next_Request_Time := (others => Start_Time);
   exception when E : others => Fatal_Exception (E, "A002: P990002a");
   end Initialize_Scheduling;

   function Reschedule (Job : Jobs) return Boolean is
      Last_Completion_Time : Time;
   begin
      if Data.Missed_Deadlines then
         --  there is at least one task that has already missed its
         --  deadline, so no need to continue anymore
         return False;
      end if;
      Next_Request_Time (Job) := Next_Request_Time (Job) +
        Period (Job);
      Last_Completion_Time := Clock;
      if Next_Request_Time (Job) <= Last_Completion_Time then
         Data.Missed_Deadlines := True;
         Comment ("lateness", POSIX.To_Timespec
           (Last_Completion_Time - Next_Request_Time (Job)));
         return False;
      end if;
      if Next_Request_Time (Job) >= Stop_Time then
         --  if the test has been running for enough time
         return False;
      end if;
      delay until Next_Request_Time (Job);
      --  We should not wake up early.
      Assert (Clock >= Next_Request_Time (Job), "A003: P990002a ");
      return True;
   exception
   when E : others => Fatal_Exception (E, "A004: P990002a");
      return False;
   end Reschedule;

   procedure Finalize is
   begin
      null;
   end Finalize;

end P990002a;

------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                            P 9 9 0 0 0 2 d                               --
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

with Ada.Real_Time,
     P990000,
     POSIX,
     POSIX_Report;
package body P990002d is

   use Ada.Real_Time,
       P990000,
       POSIX_Report;

   Data : Shared_Data_Ptr;
   Start_Time,
   Stop_Time : Ada.Real_Time.Time;
   Next_Request_Time : array (Jobs) of Ada.Real_Time.Time;

   procedure Initialize_Scheduling (Shared_Data : Shared_Data_Ptr) is
   begin
      Comment ("Initialize_Scheduling");
      Data := Shared_Data;
      Start_Time := Data.Start_Real_Time;
      Stop_Time := Start_Time
        + Ada.Real_Time.To_Time_Span (Duration (Seconds_To_Run));
      Next_Request_Time := (others => Start_Time);
      exception when E : others => Fatal_Exception (E, "A001: P990002a");
   end Initialize_Scheduling;

   function Reschedule (Job : Jobs) return Boolean is
      Last_Completion_Time : Time;
      Time_To_Next_Request : Ada.Real_Time.Time_Span;
      Missed_Periods : Integer;
   begin
      Next_Request_Time (Job) :=
        Next_Request_Time (Job) + Ada.Real_Time.To_Time_Span (Period (Job));
      Last_Completion_Time := Clock;
      Time_To_Next_Request := Next_Request_Time (Job) - Last_Completion_Time;
      if Time_To_Next_Request < Ada.Real_Time.Time_Span_Zero then
         if not Data.Missed_Deadlines then
            Data.Missed_Deadlines := True;
            Comment ("lateness", POSIX.To_Timespec
              (Ada.Real_Time.To_Duration (-Time_To_Next_Request)));
         end if;
         Missed_Periods := 0;
         while Time_To_Next_Request < Ada.Real_Time.Time_Span_Zero loop
            Time_To_Next_Request := Time_To_Next_Request
              + Ada.Real_Time.To_Time_Span (Period (Job));
            Missed_Periods := Missed_Periods + 1;
         end loop;
         Next_Request_Time (Job) :=
           Next_Request_Time (Job)
             + Missed_Periods * Ada.Real_Time.To_Time_Span (Period (Job));
      end if;
      if Next_Request_Time (Job) >= Stop_Time then
         return False;
      end if;
      delay until Next_Request_Time (Job);
      --  We should not wake up early.
      Assert (Clock >= Next_Request_Time (Job), "A002: P990002a ");
      return True;
   exception
   when E : others => Fatal_Exception (E, "A003: P990002a");
      return False;
   end Reschedule;

   procedure Finalize is
   begin
      null;
   end Finalize;

end P990002d;

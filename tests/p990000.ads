------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                             P 9 9 0 0 0 0                                --
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

--  This package contains declarations of types, constants,
--  and subprograms that are common to the P9900** series of
--  tests.

--  See file P9900doc.ads for more detailed explanation.

with Calendar,
     Ada.Real_Time,
     POSIX,
     POSIX_Calendar,
     P9900doc,
     System;
package P990000 is

   subtype Jobs is Integer range 0 .. 5;

   type Job_Duration_Array is array (Jobs) of Duration;
   type Job_Integer_Array is array (Jobs) of Integer;

   type Shared_Data_Area is record
      Missed_Deadlines : aliased Boolean;
      Input_Load,
      Computation_Load,
      Output_Load : aliased Job_Integer_Array;
      Start_Calendar_Time : aliased Calendar.Time;
      Start_POSIX_Time : aliased POSIX_Calendar.POSIX_Time;
      Start_Timespec : aliased POSIX.Timespec;
      Start_Real_Time : aliased Ada.Real_Time.Time;
   end record;

   type Shared_Data_Ptr is access all Shared_Data_Area;
   type Job_Procedure_Ptr is access procedure (Job : Jobs);

   Main_Priority : constant System.Priority := System.Priority'Last;

   Priority : constant array (Jobs) of System.Priority :=
     (System.Priority'Last - 1,
      System.Priority'Last - 2,
      System.Priority'Last - 3,
      System.Priority'Last - 4,
      System.Priority'Last - 5,
      System.Priority'Last - 6);

   Rate : constant array (Jobs) of Integer :=
     (16, 8, 6, 4, 2, 1);

   function Period (Job : Jobs) return Duration;
   --  returns 1.0 / Rate (Job)

   Computation_Time : constant array (Jobs) of Float :=
     (0.004,
      0.005,
      0.000025,
      0.0278,
      0.000025,
      0.018);

   Input_Time  : constant array (Jobs) of Float :=
     (others => 0.00000016);

   Output_Time : constant array (Jobs) of Float :=
     (others => 0.00000016);

   --  The following constants determine how long we run some of
   --  the iterative approximations.

   Seconds_To_Run : constant Integer := 10;
   --  number of seconds to run each simulation
   --  during bisection

   Real_Accuracy : constant := 100.0;

   --  Real_Accuracy specifies the number of decimal digits to
   --  which we measure the execution time of procedure that does one unit
   --  of simulated work.

   Accuracy : constant := 100;

   --  Accuracy specifies the relative accuracy to which we determine the
   --  breakdown utilization, i.e., we quit when
   --        Hi - Lo <= (Load_Factor + Accuracy) / Accuracy

   procedure Do_Unit_Work (Dummy : Integer);

   --  One execution of Do_Unit_Work is the unit of simulated work load,
   --  corresponding to one use of the parameter Load
   --  in the procedures below.

   procedure Do_Input (Load : Natural);
   procedure Do_Computation (Load : Natural);
   procedure Do_Output (Load : Natural);

end P990000;

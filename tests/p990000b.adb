------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                            P 9 9 0 0 0 0 b                               --
--                                                                          --
--                                B o d y                                   --
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


with p990000b,
     POSIX,
     POSIX_IO,
     POSIX_Configurable_System_Limits,
     POSIX_Report,
     POSIX_Permissions,
     POSIX_Process_Environment,
     POSIX_Semaphores,
     Test_Parameters;
............
with POSIX_IO,
     POSIX_Files,
     POSIX_Memory_Mapping,
     POSIX_Page_Alignment,
     POSIX_Permissions,
     System,
     Unchecked_Conversion,
     POSIX_Report;

package body p990000b is

   use p990000b,
       POSIX,
       POSIX_IO,
       POSIX_Configurable_System_Limits,
       POSIX_Report,
       POSIX_Permissions,
       POSIX_Process_Environment,
       POSIX_Semaphores,
       Test_Parameters;
............
   use POSIX_IO,
       POSIX_Files,
       POSIX_Memory_Mapping,
       POSIX_Page_Alignment,
       POSIX_Permissions,
       System,
       POSIX_Report;

   procedure Do_Unit_Work (Dummy : Integer);
   procedure Compute_Loads (Load_Factor : Positive);
   function Address_For_Data_Area return System.Address;

   type Shared_Data_Area is record
      Period : aliased Job_Duration_Array;
      Missed_Deadlines : aliased Boolean;
      Computation_Load,
      Input_Load,
      Output_Load : aliased Job_Integer_Array;
      Start_Time,
      Stop_Time : aliased Time;
      Next_Request_Time : aliased Job_Time_Array;
   end record;   

   FD : File_Descriptor;

   function Address_For_Data_Area return System.Address is
   begin

      --  Open or create file to hold shared data.

      begin
         FD := Open_Or_Create
           (Name => Shared_Data_Filename,
            Mode => Read_Write,
            Permissions => Owner_Permission_Set,
            Options => Exclusive);
         --  We are the first to create the file.
      exception
      when POSIX_Error =>
         if Get_Error_Code = File_Exists then
            --  The file already exists.
            FD := Open
              (Name => Shared_Data_Filename,
               Mode => Read_Write);
         end if;
      end;

      --  Map the file into shared memory.

      return Map_Memory
        (Length => Length (Shared_Data_Area'Size),
         Protection => Allow_Read + Allow_Write,
         Mapping => Map_Shared,
         File => FD,
         Offset => 0);   

   end Address_For_Data_Area;

   Data_Address : constant System.Address := Address_For_Data_Area;

   Data : Shared_Data_Area;
   for Data'Address use Data_Address;

   Rate : constant array (Jobs) of Integer := (16, 8, 6, 4, 2, 1);
   Computation_Time : constant array (Jobs) of Float :=
     (0.004,
      0.005,
      0.000025,
      0.0278,
      0.000025,
      0.018);
   Input_Time  : constant array (Jobs) of Float := (others => 0.00000016);
   Output_Time : constant array (Jobs) of Float := (others => 0.00000016);

   --  The following constants determine how long we run some of
   --  the iterative approximations.

   Real_Accuracy : constant := 100.0;
   Accuracy : constant := 100;

   procedure Compute_Loads (Load_Factor : Positive) is
   begin
      for J in Jobs loop
         Input_Load (J)  := Integer (Float (Load_Factor) * Input_Time (J));
         Output_Load (J) := Integer (Float (Load_Factor) * Output_Time (J));
         Computation_Load (J) :=
           Integer (Float (Load_Factor) * Computation_Time (J));
      end loop;
      exception when E : others => Fatal_Exception (E, "A001: p990000a");
   end Compute_Loads;

   --  The following array is used to "confuse" the compiler, so that
   --  is unlikely to optimize away calls to Do_Unit_Work.

   A : array (0 .. 1) of aliased Integer := (0, 1);

   procedure Do_Unit_Work (Dummy : Integer) is
      --  some code that cannot safely be "optimized" away,
      --  whose Computation time is to be used as a measurement of time
      I : Integer := Integer (Dummy mod 2);
      T : Integer := A (I);
      J : Integer := (I + 1) mod 2;
   begin
      A (I) := A (J); A (J) := T;
      exception when E : others => Fatal_Exception (E, "A002: p990000a");
   end Do_Unit_Work;

   --  generic
   --     with procedure Run_Jobs;
   procedure Find_Utilization_Limit is
      T1, T2 : Time;
      Clock_Resolution_Bound, D, Base : Duration;
      K, Hi, Lo, Load_Factor : Integer;
      Total_Utilization,
      Unit_Work_Execution_Time : Float;
   begin

      --  Compute job periods and estimate total utilization.

      Total_Utilization := 0.0;
      for J in Jobs loop
         Period (J) := Duration (1.0 / Float (Rate (J)));
         Total_Utilization := Total_Utilization +
           (Input_Time (J) + Computation_Time (J) + Output_Time (J))
             * Float (Rate (J));
      end loop;

      --  Estimate resolution of Calendar.Clock.

      Clock_Resolution_Bound := 100.0;
      for I in 1 .. 1000 loop
         T1 := Clock;
         loop
            T2 := Clock;
            D := T2 - T1;
            exit when D > 0.0;
            T1 := T2;
         end loop;
         if D < Clock_Resolution_Bound then
            Clock_Resolution_Bound := D;
         end if;
      end loop;
      Comment ("using clock resolution bound of" &
        Integer'Image (Integer (D * 1_000_000)) & "us");

      --  Use Calendar.Clock to measure execution time of
      --  procedure Do_Unit_Work, to a number of decimal
      --  digits specified by the constant Real_Accuracy.
      --  Use dual-loop benchmark method.

      K := 10000;
      loop
         T1 := Clock;
         for J in 1 .. K loop
            Do_Unit_Work (J);
         end loop;
         T2 := Clock;
         Base := T2 - T1;
         exit when Base > Real_Accuracy * Clock_Resolution_Bound;
         K := K * 10;
      end loop;
      T1 := Clock;
      for J in 1 .. K loop
         Do_Unit_Work (J);
         Do_Unit_Work (J);
      end loop;
      T2 := Clock;
      D := (T2 - T1) - Base;
      Unit_Work_Execution_Time := Float (D) / Float (K);
      Comment ("unit_work computation time",
        To_Timespec (Duration (Unit_Work_Execution_Time)));

      --  Initialize lower and upper bounds on achievable
      --  load factor, before bisection.
      --  Upper bound (Hi) must be high enough to cause failure.

      Lo := 1;
      Comment ("finding a breakdown load factor");
      Hi := 1;
      loop
         Compute_Loads (Hi);
         Missed_Deadlines.all := False;
         Run_Jobs;
         if Missed_Deadlines.all then
            Comment ("overloaded  at " & Integer'Image (Hi));
            exit;
         else
            Lo := Hi;
            Comment ("underloaded at " & Integer'Image (Hi));
            Hi := Hi * 16;
         end if;
      end loop;

      --  Zero in on maximum workable load factor, by bisection.

      Comment ("using bisection to find limiting load factor");
      loop
         Load_Factor := (Lo + Hi) / 2;
         --  Lo <= Load_Factor < Hi
         Compute_Loads (Load_Factor);
         Missed_Deadlines.all := False;
         Run_Jobs;
         if Missed_Deadlines.all then
            Hi := Load_Factor;
            Comment ("overloaded  at " & Integer'Image (Load_Factor));
         else
            Lo := Load_Factor;
            Comment ("underloaded at " & Integer'Image (Load_Factor));
         end if;
         exit when Hi - Lo <= (Load_Factor + Accuracy) / Accuracy;
      end loop;

      Comment ("limiting load factor =" & Integer'Image (Load_Factor));

      --  Compute actual effective utilization.

      Total_Utilization := 0.0;
      for J in Jobs loop
         Total_Utilization := Total_Utilization +
           Float ((Input_Load (J) + Output_Load (J) + Computation_Load (J))
             * Rate (J)) * Unit_Work_Execution_Time;
      end loop;
      Comment ("apparent limit utilization =" &
        Integer'Image (Integer (Total_Utilization * 100.0)) & "%");

      if A (1) > 1 then Fatal ("should never happen");
         --  A's values are always either 0 or 1; they are just shuffled
         --  to confuse potentially trivializing optimizations of
         --  time-delay loops.
      end if;
      exception when E : others => Fatal_Exception (E, "A003: p990000a");
   end Find_Utilization_Limit;

   procedure Do_Input (Job : Jobs) is
   begin
      for L in 1 .. Input_Load (Job) loop
         Do_Unit_Work (L);
      end loop;
      exception when E : others => Fatal_Exception (E, "A004: p990000a");
   end Do_Input;

   procedure Do_Output (Job : Jobs) is
   begin
      for L in 1 .. Output_Load (Job) loop
         Do_Unit_Work (L);
      end loop;
      exception when E : others => Fatal_Exception (E, "A005: p990000a");
   end Do_Output;

   procedure Do_Computation (Job : Jobs) is
   begin
      for L in 1 .. Computation_Load (Job) loop
         Do_Unit_Work (L);
      end loop;
      exception when E : others => Fatal_Exception (E, "A006: p990000a");
   end Do_Computation;

   package body IO is

      Mutex : Semaphore_Descriptor;

      procedure Initialize is
      begin
         Optional (Semaphores_Option, "A001: p99001c");
         begin
            Comment ("IO.Initialize: Creating semaphore (1) "
              & To_String (Valid_Semaphore_Name (1)));
            Mutex := Open_Or_Create
             (Name => Valid_Semaphore_Name (1),
              Permissions => Owner_Permission_Set,
              Value => 1,  --  open
              Options => POSIX_IO.Empty_Set);
         exception
         when E1 : POSIX_Error =>
            Optional (Semaphores_Option,
              Operation_Not_Implemented, E1, "A002: p99001c");
         when E2 : others =>
            Unexpected_Exception (E2, "A003: p99001c");
         end;
         exception when E : others => Fatal_Exception (E, "A004: p9900001c");
      end Initialize;

      procedure Input (Self : Jobs) is
      begin
         Comment ("IO.Input");
         Wait (Sem => Mutex, Masked_Signals => RTS_Signals);
         Do_Input (Self);
         Post (Sem => Mutex);
         exception when E : others => Fatal_Exception (E, "A005: p9900001c");
      end Input;

      procedure Output (Self : Jobs) is
      begin
         Comment ("IO.Output");
         Wait (Sem => Mutex, Masked_Signals => RTS_Signals);
         Do_Output (Self);
         Post (Sem => Mutex);
         exception when E : others => Fatal_Exception (E, "A006: p9900001c");
      end Output;

   end IO;

   package body Sync is

      Waiting_To_Start       : Semaphore_Descriptor;
      Waiting_For_Completion : Semaphore_Descriptor;
      Mutex                  : Semaphore_Descriptor;
      Done_Count             : Integer := 0;

      procedure Initialize is
      begin
         Optional (Semaphores_Option, "A007: p99001c");
         begin
            Comment ("Sync.Initialize: Creating semaphore (2) "
              & To_String (Valid_Semaphore_Name (2)));
            Waiting_To_Start := Open_Or_Create
             (Name => Valid_Semaphore_Name (2),
              Permissions => Owner_Permission_Set,
              Value => 0,  --  closed
              Options => POSIX_IO.Empty_Set);
            Comment ("Sync.Initialize: Creating semaphore (3) "
              & To_String (Valid_Semaphore_Name (3)));
            Mutex := Open_Or_Create
             (Name => Valid_Semaphore_Name (3),
              Permissions => Owner_Permission_Set,
              Value => 0,
              Options => POSIX_IO.Empty_Set);
            Comment ("Sync.Initialize: Creating semaphore (4) "
              & To_String (Valid_Semaphore_Name (4)));
            Waiting_For_Completion := Open_Or_Create
             (Name => Valid_Semaphore_Name (4),
              Permissions => Owner_Permission_Set,
              Value => 0,
              Options => POSIX_IO.Empty_Set);
         exception
         when E1 : POSIX_Error =>
            Optional (Semaphores_Option,
              Operation_Not_Implemented, E1, "A008: p99001c");
         when E2 : others =>
            Unexpected_Exception (E2, "A009: p99001c");
         end;
         exception when E : others => Fatal_Exception (E, "A010: p9900001c");
      end Initialize;

      procedure Start_All_Jobs is
      --  called by the main program to start all jobs
      begin
         Comment ("Starting jobs");
         for J in Jobs loop
            Post (Waiting_To_Start);
         end loop;
         exception when E : others => Fatal_Exception (E, "A011: p9900001c");
      end Start_All_Jobs;

      procedure Await_Start is
      --  called once by each process to wait to start
      begin
         Comment ("Awaiting start");
         Wait (Sem => Waiting_To_Start, Masked_Signals => RTS_Signals);
         exception when E : others =>
            Fatal_Exception (E, "A012: p99001c");
      end Await_Start;

      procedure Done is
      --  called once by each process to complete
      begin
         Comment ("Done one job");
         Wait (Sem => Mutex, Masked_Signals => RTS_Signals);
         Done_Count := Done_Count + 1;
         if Done_Count = Jobs'Last - Jobs'First + 1 then
            --  wake up the main program
            Post (Waiting_For_Completion);
         end if;
         Post (Sem => Mutex);
         exception when E : others =>
            Fatal_Exception (E, "A013: p99001c");
      end Done;

      procedure Await_All_Jobs_Done is
      --  called by the main program to wait for all jobs to finish
      begin
         Comment ("Awaiting completion of all jobs");
         Wait (Sem => Waiting_For_Completion, Masked_Signals => RTS_Signals);
         exception when E : others => Fatal_Exception (E, "A014: p9900001c");
      end Await_All_Jobs_Done;

   end Sync;

begin

.........
   IO.Initialize;
   Sync.Initialize;
.........

   --  Initialize individual exported pointers to point to components
   --  of the shared data area.

   Period := Data.Period'Access;
   Missed_Deadlines := Data.Missed_Deadlines'Access;
   Computation_Load := Data.Computation_Load'Access;
   Input_Load := Data.Input_Load'Access;
   Output_Load := Data.Output_Load'Access;
   Start_Time := Data.Start_Time'Access;
   Stop_Time := Data.Stop_Time'Access;
   Next_Request_Time := Data.Next_Request_Time'Access;

end p990000b;


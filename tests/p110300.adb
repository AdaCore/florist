------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                             P 1 1 0 3 0 0                                --
--                                                                          --
--                                B o d y                                   --
--                                                                          --
--                                                                          --
--  Copyright (c) 1995-1998 Florida  State  University  (FSU).  All Rights  --
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

--  Test package POSIX_Condition_Variables

--  This is a very superficial test.
--  It essentially only tests that the interfaces can be called.
--  A better test needs to be written.

with Ada.Calendar,
     POSIX,
     POSIX_Calendar,
     POSIX_Condition_Variables,
     POSIX_Mutexes,
     POSIX_Report;
procedure p110300 is

   use Ada.Calendar,
       POSIX,
       POSIX_Calendar,
       POSIX_Condition_Variables,
       POSIX_Mutexes,
       POSIX_Report;

   Md : POSIX_Mutexes.Mutex_Descriptor;
   M :  POSIX_Mutexes.Mutex;
   Cond1, Cond2 : Condition;
   Producer_Cond : Condition_Descriptor;
   Consumer_Cond : Condition_Descriptor;
   --  number inside critical section
   Inside : Integer := 0;
      pragma Atomic (Inside);
   --  number of last unit consumed
   C_Count : Integer := 0;
      pragma Atomic (C_Count);
   --  number of last unit produced
   P_Count : Integer := 0;
      pragma Atomic (P_Count);
   --  maximum difference between P_Count and C_Count
   Buf_Limit : constant Integer := 3;
   --  total number of units to produce and consume
   Limit : constant Integer := 20;
   Timeout : POSIX.Timespec;

begin

   Header ("p110300");
   Test ("Package POSIX_Condition_Variables [11.3]");
   Initialize (Cond1);
   Initialize (Cond2);
   POSIX_Mutexes.Initialize (M);
   Md := POSIX_Mutexes.Descriptor_Of (M);
   Producer_Cond := Descriptor_Of (Cond1);
   Consumer_Cond := Descriptor_Of (Cond2);
   Timeout := To_Timespec (To_POSIX_Time (Ada.Calendar.Clock + 1.0));

   -----------------------------------------------------------------------

   declare
      task Consumer;
      task body Consumer is
      begin
         Comment ("Consumer starts");
         loop
            POSIX_Mutexes.Lock (Md);
            Comment ("Consumer gets mutex");
            Assert (Inside = 0, "A001");
            Inside := Inside + 1;
            Assert (P_Count >= C_Count and P_Count - C_Count <= Buf_Limit
              and P_Count <= Limit, "A002");
            while C_Count = P_Count loop
               Comment ("Consumer does timed_wait for condition");
               Inside := Inside - 1;
               Assert (Inside = 0, "A003");
               --  wait for producer signal
               Timed_Wait (Consumer_Cond, Md, Timeout);
               Assert (Inside = 0, "A004");
               Inside := Inside + 1;
            end loop;
            C_Count := C_Count + 1;
            Comment ("Consumer consumes unit" & Integer'Image (C_Count));
            Comment ("Consumer releases mutex");
            Inside := Inside - 1;
            Assert (Inside = 0, "A005");
            POSIX_Mutexes.Unlock (Md);
            Comment ("Consumer signals producer");
            Signal (Producer_Cond);
         end loop;
         --  Fail because consumer should exit via timeout
         Fail ("A006");
      exception
      when E : POSIX.POSIX_Error =>
         if POSIX.Get_Error_Code = Timed_Out then
            Comment ("Consumer times out on wait");
            Assert (Inside = 0, "A007");
            Assert (P_Count = Limit and C_Count = Limit, "A008");
            P_Count := Limit;
            C_Count := Limit;
            Signal (Producer_Cond);
            Comment ("Consumer releases mutex");
            POSIX_Mutexes.Unlock (Md);
            Comment ("Consumer exits");
         else
            Signal (Producer_Cond);
            Unexpected_Exception (E, "A009");
         end if;
      end Consumer;

      task producer;
      task body producer is
         Exited : Boolean := False;
      begin
         while P_Count < Limit loop
            POSIX_Mutexes.Lock (Md);
            Comment ("Producer gets mutex");
            Assert (Inside = 0, "A010");
            Inside := Inside + 1;
            P_Count := P_Count + 1;
            Comment ("Producer produces unit" & Integer'Image (P_Count));
            Comment ("Producer signals consumer");
            Signal (Consumer_Cond);
            Assert (P_Count >= C_Count and P_Count - C_Count <= Buf_Limit
              and P_Count <= Limit, "A011");
            while P_Count - C_Count = Buf_Limit loop
               Comment ("Producer waits for condition");
               Inside := Inside - 1;
               Assert (Inside = 0, "A012");
               Wait (Producer_Cond, Md);
               Assert (Inside = 0, "A013");
               Inside := Inside + 1;
            end loop;
            Comment ("Producer releases mutex");
            Inside := Inside - 1;
            Assert (Inside = 0, "A014");
            POSIX_Mutexes.Unlock (Md);
         end loop;
         Comment ("Producer exits");
      exception
      when E : POSIX.POSIX_Error =>
         Unexpected_Exception (E, "A015");
      end producer;

   begin
      Comment ("Main block completes");
   exception
   when E1 : POSIX_Error =>
      Optional (Mutex_Option,  Operation_Not_Implemented, E1, "A016");
      abort producer, Consumer;
   when E2 : others =>
      Unexpected_Exception (E2, "A017");
      abort producer, Consumer;
   end;

   -----------------------------------------------------------------------

   POSIX_Mutexes.Finalize (M);
   Finalize (Cond1);
   Finalize (Cond2);

   Done;

exception
when E : others =>
   Fatal_Exception (E, "A018");
end p110300;

------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                             P 9 9 0 0 0 0                                --
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
--  AVAILABLE OR DISCLOSED,  OR THE OWNERSHIP,  MECHANTABILITY, OR FITNESS  --
--  FOR A PARTICULAR PURPOSE OF SAID MATERIAL.                              --
--                                                                          --
------------------------------------------------------------------------------
--  [$Revision$]

with P9900doc,
     POSIX_Report;
package body P990000 is

   use POSIX_Report;

   function Period (Job : Jobs) return Duration is
   begin
      return Duration (1.0 / Float (Rate (Job)));
   end Period;

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

      if A (1) > 1 then Fatal ("should never happen");
         --  A's values are always either 0 or 1; they are just shuffled
         --  to confuse potentially trivializing optimizations of
         --  time-delay loops.
      end if;

   exception when E : others => Fatal_Exception (E, "A001: P990000a");
   end Do_Unit_Work;
   procedure Do_Input (Load : Natural) is
   begin
      for L in 1 .. Load loop
         Do_Unit_Work (L);
      end loop;
   end Do_Input;

   procedure Do_Computation (Load : Natural) is
   begin
      for L in 1 .. Load loop
         Do_Unit_Work (L);
      end loop;
   end Do_Computation;

   procedure Do_Output (Load : Natural) is
   begin
      for L in 1 .. Load loop
         Do_Unit_Work (L);
      end loop;
   end Do_Output;

end P990000;

------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                            P 9 9 0 0 0 1 a                               --
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

with POSIX_Report,
     P9900doc,
     P990000,
     System;
package body P990001a is

   use POSIX_Report,
       P9900doc,
       P990000;

   protected IO is
      procedure Do_Input (Load : Natural);
      procedure Do_Output (Load : Natural);
   end IO;

   protected Sync is
      procedure Start_All_Jobs;
      entry Await_Start;
      procedure Done_Job;
      entry Await_All_Jobs_Done;
      pragma Priority (System.Priority'Last);
   private
      All_Go : Boolean := False;
      Done_Count : Integer := 0;
   end Sync;

   protected body IO is

      procedure Do_Input (Load : Natural) is
      begin
         P990000.Do_Input (Load);
      exception when E : others => Fatal_Exception (E, "A001: P990001a");
      end Do_Input;

      procedure Do_Output (Load : Natural) is
      begin
         P990000.Do_Output (Load);
      exception when E : others => Fatal_Exception (E, "A002: P990001a");
      end Do_Output;

   end IO;

   protected body Sync is

      procedure Start_All_Jobs is
      begin
         All_Go := True;
      end Start_All_Jobs;

      entry Await_Start when All_Go is
      begin
         null;
      end Await_Start;

      procedure Done_Job is
      begin
         Done_Count := Done_Count + 1;
      end Done_Job;

      entry Await_All_Jobs_Done when Done_Count >= Jobs'Last is
      begin
         All_Go := False;
         Done_Count := 0;
      end Await_All_Jobs_Done;

   end Sync;

   procedure Do_Input (Load : Natural) is
   begin
      IO.Do_Input (Load);
   end Do_Input;

   procedure Do_Output (Load : Natural) is
   begin
      IO.Do_Output (Load);
   end Do_Output;

   procedure Start_All_Jobs is
   begin
      Sync.Start_All_Jobs;
   end Start_All_Jobs;

   procedure Await_All_Jobs_Done is
   begin
      Sync.Await_All_Jobs_Done;
   end Await_All_Jobs_Done;

   procedure Await_Start is
   begin
      Sync.Await_Start;
   end Await_Start;

   procedure Done_Job is
   begin
      Sync.Done_Job;
   end Done_Job;

   procedure Initialize is
   begin
      null;
   end Initialize;

   procedure Finalize is
   begin
      null;
   end Finalize;

end P990001a;

------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                             P 1 1 0 2 0 0                                --
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
--  AVAILABLE OR DISCLOSED,  OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS  --
--  FOR A PARTICULAR PURPOSE OF SAID MATERIAL.                              --
--                                                                          --
------------------------------------------------------------------------------
--  [$Revision$]


--  Test package POSIX_Mutexes.

--  This is a very superficial test.
--  It essentially only tests that the interfaces can be called.

with POSIX,
     POSIX_Mutexes,
     POSIX_Report;
procedure p110200 is

   use POSIX,
       POSIX_Mutexes,
       POSIX_Report;

begin

   Header ("p110200");

   --  This entire test depends on support for the Mutex Option.

   -----------------------------------------------------------------------
   --  If the Process Shared Option is supported, it is possible
   --  to create a mutex with that option, and use the mutex with all
   --  of the mutex operations. [11.2.4]

   declare
      M : Mutex;
      MD : Mutex_Descriptor;
      Attr : Attributes;
   begin
      Test ("Process Shared option");
      Initialize (Attr);
      Set_Process_Shared (Attr, True);
      Assert (Get_Process_Shared (Attr), "A001: process_shared");
      Initialize (M, Attr);
      Finalize (Attr);
      MD := Descriptor_Of (M);
      Lock (MD);
      Unlock (MD);
      Assert (Try_Lock (MD), "A002: try_lock");
      Unlock (MD);
      Finalize (M);
   exception
   when E1 : POSIX_Error =>
      Optional (Mutex_Option, Process_Shared_Option,
        Operation_Not_Implemented, E1, "A003");
   when E2 : others => Unexpected_Exception (E2, "A004");
   end;

   -----------------------------------------------------------------------
   --  If the Mutex Priority Inheritance Option or the Mutex Priority
   --  Ceiling Option is supported, it is possible to create a mutex with
   --  the No_Priority_Inheritance policy, and to use this mutex with
   --  the Lock, Unlock, and Finalize operations. [11.2.5]

   declare
      M : Mutex;
      MD : Mutex_Descriptor;
      Attr : Attributes;
   begin
      Test ("no priority inheritance ");
      Initialize (Attr);
      Set_Locking_Policy (Attr, No_Priority_Inheritance);
      Assert (Get_Locking_Policy (Attr) = No_Priority_Inheritance,
              "A005: (1)");
      Initialize (M, Attr);
      Finalize (Attr);
      MD := Descriptor_Of (M);
      Lock (MD);
      Unlock (MD);
      Assert (Try_Lock (MD), "A006: try_lock");
      Unlock (MD);
      Finalize (M);
   exception
   when E1 : POSIX_Error =>
      if (Is_Supported (Mutex_Option)) then
         Optional (Mutex_Priority_Ceiling_Option,
           Mutex_Priority_Inheritance_Option,
           Operation_Not_Implemented, E1, "A007");
      else
         Optional (Mutex_Option, Operation_Not_Implemented, E1, "A008");
      end if;
   when E2 : others => Unexpected_Exception (E2, "A009");
   end;

   -----------------------------------------------------------------------
   --  If the Mutex Priority Inheritance Option is supported,
   --  it is possible to create a mutex with
   --  the Highest_Blocked_Task policy, and to use this mutex with
   --  the Lock, Unlock, and Finalize operations.  [11.2.5]

   declare
      M : Mutex;
      MD : Mutex_Descriptor;
      Attr : Attributes;
   begin
      Test ("Mutex Priority Inheritance option");
      Initialize (Attr);
      Set_Locking_Policy (Attr, Highest_Blocked_Task);
      Assert (Get_Locking_Policy (Attr) = Highest_Blocked_Task, "A010: (2)");
      Initialize (M, Attr);
      Finalize (Attr);
      MD := Descriptor_Of (M);
      Lock (MD);
      Unlock (MD);
      Assert (Try_Lock (MD), "A011: try_lock");
      Unlock (MD);
      Finalize (M);
   exception
   when E1 : POSIX_Error =>
      Optional (Mutex_Option, Mutex_Priority_Inheritance_Option,
        Operation_Not_Implemented, E1, "A012");
   when E2 : others => Unexpected_Exception (E2, "A013");
   end;

   -----------------------------------------------------------------------
   --  If the Mutex Priority Inheritance Option is supported,
   --  it is possible to create a mutex with
   --  the Highest_Ceiling_Priority policy, and to use this mutex with
   --  the Lock, Unlock, and Finalize operations. [11.2.5]

   declare
      M : Mutex;
      MD : Mutex_Descriptor;
      Attr : Attributes;
   begin
      Test ("Mutex Priority Ceiling option, ");
      Initialize (Attr);
      Set_Locking_Policy (Attr, Highest_Ceiling_Priority);
      Assert (Get_Locking_Policy (Attr) = Highest_Ceiling_Priority,
              "A014: (3)");
      Initialize (M, Attr);
      Finalize (Attr);
      MD := Descriptor_Of (M);
      Lock (MD);
      Unlock (MD);
      Assert (Try_Lock (MD), "A015: try_lock");
      Unlock (MD);
      Finalize (M);
   exception
   when E1 : POSIX_Error =>
      Optional (Mutex_Option, Mutex_Priority_Ceiling_Option,
        Operation_Not_Implemented, E1, "A016");
   when E2 : others => Unexpected_Exception (E2, "A017");
   end;

   -----------------------------------------------------------------------

   declare
      M : Mutex;
      MD : Mutex_Descriptor;
      Attr : Attributes;
   begin
      Test ("get/set ceiling priority of attribute [11.2.7]");
      Initialize (Attr);
      Set_Ceiling_Priority (Attr, 2);
      Assert (Get_Ceiling_Priority (Attr) = 2, "A018");
      Finalize (Attr);
      MD := Descriptor_Of (M);
      Lock (MD);
      Unlock (MD);
      Assert (Try_Lock (MD), "A019: try_lock");
      Unlock (MD);
      Finalize (M);
   exception
   when E1 : POSIX_Error =>
      Optional (Mutex_Option, Mutex_Priority_Ceiling_Option,
        Operation_Not_Implemented, E1, "A020");
   when E2 : others => Unexpected_Exception (E2, "A021");
   end;

   -------------------------------------------------------------------

   declare
      M : Mutex;
      MD : Mutex_Descriptor;
      New_Ceil, Old_Ceil : Ceiling_Priority;
      Attr : Attributes;
   begin
      Test ("get/set ceiling priority of mutex");
      Initialize (Attr);
      Set_Ceiling_Priority (Attr, 2);
      Initialize (M, Attr);
      Finalize (Attr);
      MD := Descriptor_Of (M);
      New_Ceil := 3;
      Set_Ceiling_Priority (MD, New_Ceil, Old_Ceil);
      Assert (Get_Ceiling_Priority (MD) = New_Ceil, "A022");
      Finalize (M);
   exception
   when E1 : POSIX_Error =>
      Optional (Mutex_Option, Mutex_Priority_Ceiling_Option,
        Operation_Not_Implemented, E1, "A023");
   when E2 : others => Unexpected_Exception (E2, "A024");
   end;

   -------------------------------------------------------------------

   Done;

exception
when E : others => Fatal_Exception (E, "A025");
end p110200;

------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                            P 1 2 0 5 0 2 a                               --
--                                                                          --
--                                B o d y                                   --
--                                                                          --
--                                                                          --
--  Copyright (c) 1997-1999 Florida  State  University  (FSU).  All Rights  --
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


--  Child process for test p120502.

--  Two processes communicate through a shared memory object
--  containing two counters.  Each process increments its own
--  counter, and then waits until the other process's counter
--  has caught up.  They quit after a fixed number of
--  iterations.

with POSIX,
     POSIX_IO,
     POSIX_Generic_Shared_Memory,
     POSIX_Memory_Mapping,
     POSIX_Permissions,
     POSIX_Report,
     POSIX_Shared_Memory_Objects,
     Test_Parameters;

procedure p120502a is
   use POSIX,
       POSIX_IO,
       POSIX_Memory_Mapping,
       POSIX_Permissions,
       POSIX_Report,
       POSIX_Shared_Memory_Objects;

   package TP renames Test_Parameters;

   type Window is
      record
        A : Integer;
        B : Integer;
        Done : Integer;
        pragma Volatile (A);
        pragma Volatile (B);
        pragma Volatile (Done);
      end record;

   package P is new POSIX_Generic_Shared_Memory (Window);
   Object_Name : POSIX_String := TP.Valid_Shared_Memory_Object_Name (1);

   Access_Failed : exception;

   Shmd : File_Descriptor;
   Test_Perm : Permission_Set := Owner_Permission_Set;
   Obj : P.Shared_Access;
   Count : Integer := 0;

begin

   Comment ("child: 120502a");
   Assert (Child = 1 or Child = 2,
     "A001: Child =" & Integer'Image (Child));

   begin
      Comment ("child: try to create a new shared memory object");
      Shmd := P.Open_Or_Create_And_Map_Shared_Memory
           (Object_Name, Allow_Write, Test_Perm, Exclusive);
   exception
      when E1 : POSIX_Error =>
         if POSIX.Get_Error_Code = File_Exists then
            Comment ("child: object already exists; try to open it");
            Shmd := P.Open_And_Map_Shared_Memory (Object_Name, Allow_Write);
         else Optional (Shared_Memory_Objects_Option,
                   Operation_Not_Implemented, E1, "A002: child");
         end if;
      when E2 : others =>
         Fatal_Exception (E2, "A003: child");
   end;

   Optional (Shared_Memory_Objects_Option, "A004: child");

   Obj := P.Access_Shared_Memory (Shmd);
   Obj.Done := 0;

   if Child = 1 then
      for I in 0 .. 10 loop
         Obj.A := I;
         Count := 0;
         while Obj.B < I loop
            delay 0.2;
            Count := Count + 1;
            if Count >= 520 then
               raise Access_Failed;
            end if;
         end loop;
      end loop;
   else
      for I in 0 .. 10 loop
         Obj.B := I;
         Count := 0;
         while Obj.A < I loop
            delay 0.2;
            Count := Count + 1;
            if Count >= 520 then
               raise Access_Failed;
            end if;
         end loop;
      end loop;
   end if;

   if Obj.Done = 0 then
      Obj.Done := 1;
      --  The two counters should stop at the same value.
      Assert (Obj.A = Obj.B, "A005: child, values do not match");
   end if;

   begin
      P.Unmap_And_Close_Shared_Memory (Shmd);
      Unlink_Shared_Memory (Object_Name);
   exception
   when POSIX_Error =>
      Check_Error_Code (No_Such_File_Or_Directory, "A006: child");
   end;

   Done;

exception
when Access_Failed =>
   --  The other process may not be executing,
   --  or shared memory may not be working correctly.
   Assert (False, "A007: child, other counter is not changing");
   Done;
when E1 : POSIX_Error =>
   Optional (Shared_Memory_Objects_Option,
     Operation_Not_Implemented, E1, "A008: child");
   Done;
when E2 : others =>
   Unexpected_Exception (E2, "A009: child");
   Done;
end p120502a;

------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                             P 1 1 0 1 0 1                                --
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

--  Test package POSIX_Semaphores,
--  in IEEE Std 1003.5b Section 11.1.

--  This test is of unnamed semaphores.

--  The basic concept of the test is to simulate a bank,
--  via a set of "client" tasks and a
--  smaller set of "teller" tasks (i.e., servers).
--  Each client requires one service.
--  (The clients and tellers are collectively termed the "players".)
--  Semaphores are used to enforce an orderly service discipline,
--  so that each teller serves one client at at time.

with POSIX,
     POSIX_Report,
     POSIX_Semaphores;

procedure p110101 is

   use POSIX,
       POSIX_Report,
       POSIX_Semaphores;

   Masking : constant Signal_Masking := No_Signals;

   Wait_Count       : Semaphore;
   --  value is count of number of tellers available, if positive
   --  value is count of number of customers waiting, if negative

   Master_Lock     : Semaphore;
   --  Master_Lock protects the table of tellers
   --  and the count of players.
   --  value is 1 if table is unlocked
   --  value is count of number of tasks waiting to access the table,
   --  if negative

   Num_Clients : constant := 100;
   Num_Tellers : constant := 5;
   Clients_Alive : Natural := 0;
   --  tells how many players may still be alive
   --  protected by Master_Lock
   Main_Wait : Semaphore;
   --  used by main program to wait for players to terminate

   Null_Player : constant := 0;
   Num_Players : constant := Num_Clients + Num_Tellers + 1;
   type Player_ID is range Null_Player .. Num_Players;

   Main_Program : constant Player_ID := 1;
   subtype Teller_ID is Player_ID
     range  Main_Program + 1 .. Num_Tellers;
   subtype Client_ID is Player_ID
     range Teller_ID'Last + 1 .. Player_ID'Last;

   task type Client is
      entry Start (Self : Client_ID);
   end Client;

   task type Teller is
      entry Start (Self : Teller_ID);
   end Teller;

   Current_Client : array (Teller_ID) of Player_ID
      := (others => Null_Player);

   --  Current_Client tells who each teller is serving, if anybody.
   --  The state is protected by Master_Lock.

   Tellers : array (Teller_ID) of Teller;
   Clients : array (Client_ID) of Client;
   Player_Waits : array (Teller_ID'First .. Client_ID'Last) of Semaphore;

   --------------
   -- Shutdown --
   --------------

   --  Shut down all the Client and Teller tasks.

   procedure Shutdown (Self : Player_ID);

   procedure Shutdown (Self : Player_ID) is
   begin
      for I in Client_ID loop
         if Self /= I then abort Clients (I);
         end if;
      end loop;
      for I in Teller_ID loop
         if Self /= I then abort Tellers (I);
         end if;
      end loop;
      if Self in Teller_ID then abort Tellers (Self);
      elsif Self in Client_ID then abort Clients (Self);
      end if;
      Post (Descriptor_Of (Main_Wait));
   end Shutdown;

   task body Client is
      Self        : Player_ID;
      My_Teller   : Player_ID := Null_Player;
      My_Wait_Ref : Semaphore_Descriptor;
   begin
      --  Wait to be told our ID.
      accept Start (Self : Client_ID) do
         Client.Self := Self;
      end Start;
      My_Wait_Ref := Descriptor_Of (Player_Waits (Self));
      --  Wait for an available teller.
      Wait (Descriptor_Of (Wait_Count), Masking);
      --  choose a specific teller and claim him
      Wait (Descriptor_Of (Master_Lock), Masking);
      for I in Teller_ID loop
         if Current_Client (I) = Null_Player then
            My_Teller := I;
         end if;
      end loop;
      if My_Teller = Null_Player then
         --  This should never happen.
         Post (Descriptor_Of (Master_Lock));
         --  Fail because no teller available
         Fail ("A001");
         Shutdown (Self);
      else
         Current_Client (My_Teller) := Self;
         Post (Descriptor_Of (Master_Lock));
      end if;
      --  Wake up the teller.
      Post (Descriptor_Of (Player_Waits (My_Teller)));
      --  wait for the teller to perform service
      Wait (My_Wait_Ref, Masking);
      --  The service is done;
      --  indicate to main program that we are done.
      Wait (Descriptor_Of (Master_Lock), Masking);
      Clients_Alive := Clients_Alive - 1;
      if Clients_Alive = 0 then
         Post (Descriptor_Of (Main_Wait));
      end if;
      Post (Descriptor_Of (Master_Lock));
   exception
   when E : others =>

      Unexpected_Exception (E, "A002");
      Shutdown (Self);
   end Client;

   task body Teller is
      Self        : Player_ID;
      My_Wait_Ref : Semaphore_Descriptor;
   begin
      --  Wait to be told our ID.
      accept Start (Self : Teller_ID) do
         Teller.Self := Self;
      end Start;
      My_Wait_Ref := Descriptor_Of (Player_Waits (Self));
      loop
         --  Indicate that we are open for business.
         Post (Descriptor_Of (Wait_Count));
         --  Wait for a customer to wake us up.
         Wait (My_Wait_Ref, Masking);
         exit when Current_Client (Self) = Null_Player;
         --  Serve the customer.
         delay Duration (Self) * Duration'(0.001);
         --  Wake up the customer.
         Post (Descriptor_Of (Player_Waits (Current_Client (Self))));
         --  Indicate that we are free again.
         Current_Client (Self) := Null_Player;
      end loop;
   exception
   when E : others =>
      Unexpected_Exception (E, "A003");
      Shutdown (Self);
   end Teller;

begin

   Header ("p110101");

   -----------------------------------------------------------------------

   Test ("Use unnamed semaphores to synchronize Ada tasks");

   begin
      Comment ("Initializing semaphores");
      Initialize (Wait_Count, 0);
      Initialize (Master_Lock, 1);
      Initialize (Main_Wait, 0);
      for I in Player_Waits'Range loop
         Initialize (Player_Waits (I), 0);
      end loop;
   exception
   when E1 : POSIX_Error =>
      Optional (Semaphores_Option, Operation_Not_Implemented, E1, "A004");
   when E2 : others => Unexpected_Exception (E2, "A005");
      Shutdown (Main_Program);
   end;

   -----------------------------------------------------------------------

   begin
      Comment ("Starting clients and tellers");
      Wait (Descriptor_Of (Master_Lock), Masking);
      for I in Clients'Range loop
         Clients (I).Start (I);
         Clients_Alive := Clients_Alive + 1;
      end loop;
      for I in Tellers'Range loop
         Tellers (I).Start (I);
      end loop;
      Post (Descriptor_Of (Master_Lock));
   exception
   when E1 : POSIX_Error =>
      Optional (Semaphores_Option, Operation_Not_Implemented, E1, "A006");
   when E2 : others => Unexpected_Exception (E2, "A007");
      Shutdown (Main_Program);
   end;

   -----------------------------------------------------------------------

   begin
      Comment ("Waiting for all players to finish");
      Wait (Descriptor_Of (Main_Wait), Masking);
   exception
   when E1 : POSIX_Error =>
      Optional (Semaphores_Option, Operation_Not_Implemented, E1, "A008");
   when E2 : others => Unexpected_Exception (E2, "A009");
      Shutdown (Main_Program);
   end;

   -----------------------------------------------------------------------

   begin
      Comment ("Waking up tellers to exit");
      for I in Tellers'Range loop
         Post (Descriptor_Of (Player_Waits (I)));
      end loop;
   exception
   when E1 : POSIX_Error =>
      Optional (Semaphores_Option, Operation_Not_Implemented, E1, "A010");
   when E2 : others => Unexpected_Exception (E2, "A011");
      Shutdown (Main_Program);
   end;

   -----------------------------------------------------------------------

   Done;

exception
when E : others =>
   Shutdown (Main_Program);
   Fatal_Exception (E, "A012");
end p110101;

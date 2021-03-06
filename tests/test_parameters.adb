------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                       T e s t _ P a r a m e t e r s                      --
--                                                                          --
--                                B o d y                                   --
--                                                                          --
--                                                                          --
--  Copyright (c) 1997-1999 Florida  State  University  (FSU).  All Rights  --
--  Reserved.                                                               --
--                     Copyright (C) 2000-2022, AdaCore                     --
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

--  The POSIX standards provide considerable leeway for variation
--  among implementations.  For example, an implementation may impose
--  arbitrary restrictions on the form of the name of a message queue.
--  In order to accommodate these variations, the tests are
--  parameterized.  All the implementation-dependences (such as valid
--  message queue names) are encapsulated in this package body.

--  The specification of this package shall not be modified.
--  The body may be edited to fit each implementation being tested.
--  The only with-clause dependences permitted for the body are on
--  packages that are defined by the Ada language and POSIX/Ada
--  standards.  Whatever modifications are made to the package body
--  shall be consistent with the comments in the package
--  specification.

--  In general, the body of this package will need modification to fit the
--  specifics of the execution environment to be tested.

--  The following is the package body that was used in checking out the
--  tests on Solairs 7 (SunOS 5.7), and also on Linux 2.2 with Linuxthreads
--  using the Gnat 3.12p Ada implementation.  (Not all tests passed.)
--  No one should expect to be able to use this package body as-is for
--  any other combination of compiler, Ada runtime system,
--  POSIX/Ada binding implementation,
--  and execution platform, but it may be useful as an example.

--  The person verifying the administration of the tests should review
--  the version of this package body  to verify that whatever tailoring has
--  been done is consistent with the intent of the comments in the
--  package specification and with the standards.

with Ada.Text_IO,
     Ada.Characters.Handling,
     Ada.Strings.Unbounded,
     POSIX,
     POSIX_Signals,
     Unchecked_Conversion;

package body Test_Parameters is

   use Ada.Text_IO,
       Ada.Characters.Handling,
       Ada.Strings.Unbounded,
       POSIX,
       POSIX_Signals;

   function Valid_MQ_Name
     (N : Positive) return POSIX_String is
      S : constant String := Integer'Image (N);
   begin
      return "/mq_" & To_POSIX_String (S (S'First + 1 .. S'Last));
   end Valid_MQ_Name;

   function Invalid_MQ_Name
     (N : Positive) return POSIX_String is
   begin
      if N = 1 then
         return "(#@!$%^~??||++";
      else
         return "~!@#$%^&*()_++_}{{.//";
      end if;
   end Invalid_MQ_Name;

   function Valid_Shared_Memory_Object_Name
     (N : Positive) return POSIX_String is
      S : constant String := Integer'Image (N);
   begin
      return "/shm_" & To_POSIX_String (S (S'First + 1 .. S'Last));
   end Valid_Shared_Memory_Object_Name;

   function Invalid_Shared_Memory_Object_Name
     (N : Positive) return POSIX_String is
   begin
      if N = 1 then
         return "(#@!$%^~??||++";
      else
         return "~!@#$%^&*()_++_}{{.//";
      end if;
   end Invalid_Shared_Memory_Object_Name;

   function Valid_Block_Device_Name return POSIX_String is
      File : File_Type;
      Buf : String (1 .. 128);
      Last : Integer;
   begin
      begin
         Open (File, In_File, "/etc/mnttab");
      exception
      when others =>
         begin
            Open (File, In_File, "/etc/mtab");
         exception
         when others =>
            return "could_not_find_block_device";
         end;
      end;
      --  Try searching /etc/mnttab or /etc/mtab for a device name.
      loop
         Get_Line (File, Buf, Last);
         if Last >= 4 and Buf (1 .. 4) = "/dev" then
            for I in 1 .. Last loop
               if not Is_Alphanumeric (Buf (I))
                 and then not Is_Special (Buf (I))
               then
                  return To_POSIX_String (Buf (1 .. I - 1));
               end if;
            end loop;
         end if;
      end loop;
   end Valid_Block_Device_Name;

   function Valid_Character_Special_File_Name return POSIX_String is
   begin
      return "/dev/tty";
   end Valid_Character_Special_File_Name;

   function Valid_Nonexistent_File_Name return POSIX_String is
   begin
      return "Nonexistent_File";
   end Valid_Nonexistent_File_Name;

   function Default_Action (Sig : Signal) return Signal_Action is
   begin
      case Sig is
      when SIGCHLD => return Ignore;
      when SIGCONT => return Continue;
      when SIGSTOP | SIGTSTP | SIGTTIN | SIGTTOU => return Stop;
      when SIGHUP  | SIGINT  | SIGKILL | SIGPIPE |
        SIGQUIT | SIGTERM | SIGUSR1 | SIGUSR2 => return Termination;
--! #     if SunOS then
--!       when 19  -- SIGPWR
--!          | 20  -- SIGWINCH
--!          | 21  -- SIGURG
--!          | 32  -- SIGWAITING
--!          | 33  -- SIGLWP
--!          | 34  -- SIGFREEZE
--!          | 35  -- SIGTHAW
--!          | 36  -- SIGCANCEL
--!            => return Ignore;
--!       when others => return Termination;
--! #     else
      when others => return Unspecified;
--! #     end if;
      end case;
   end Default_Action;

   function Is_Reserved_Signal
     (Sig : Signal) return Boolean is
   begin
      case Sig is
      --  The required reserved signals
      when SIGILL  | SIGABRT | SIGFPE  | SIGBUS |
           SIGSEGV | SIGALRM
         => return True;
--! #     if Linux then
      --  Additional reserved signals for GNAT with Leroy Linux threads
      when SIGUSR1 | SIGUSR2 | SIGINT
        |  5 --  SIGTRAP
        | 26 --  SIGVTALRM
        | 31 --  SIGUNUSED
         => return True;
--! #     elsif SunOS then
--! --  The following are correct values for gnat 3.12
--! --  and Solaris 2.7.  They may need modification for other
--! --  configurations.
--!       when 5   --  SIGTRAP
--!         | 29   --  SIGPROF
--!         | 32   --  SIGWAITING
--!         | 33   --  SIGLWP
--!         | 36   --  SIGCANCEL
--!          => return True;
--! #     end if;
      when others => return False;
      end case;
   end Is_Reserved_Signal;

   function Action_Cannot_Be_Set
     (Sig : Signal) return Boolean is
   begin
      case Sig is
      --  The required reserved signals, and SIGNULL
      when SIGILL  | SIGABRT | SIGFPE  | SIGKILL | SIGBUS |
           SIGSEGV | SIGALRM | SIGSTOP | SIGNULL
         => return True;
--! #     if HAVE_Leroy_Threads then
--!   --  Additional reserved signals for GNAT with Leroy Linux threads
--!   when SIGUSR1 | SIGUSR2 | SIGINT
--!     |  5 --  SIGTRAP
--!     | 26 --  SIGVTALRM
--!     | 31 --  SIGUNUSED
--!      => return True;
--! #     elsif SunOS then
--!       --  Additional reserved signals for GNAT with Solaris 2.6
--!       --  The following are correct values for gnat 3.12
--!       --  and Solaris 2.7.  They may need modification for other
--!       --  configurations.
--!       when
--!            5   --  SIGTRAP    -- not named in POSIX.5
--!         | 29   --  SIGPROF    -- not named in POSIX.5
--!         | 32   --  SIGWAITING -- not named in POSIX.5
--!         | 33   --  SIGLWP     -- not named in POSIX.5
--!         | 36   --  SIGCANCEL  -- not named in POSIX.5
--!          => return True;
--! #     end if;
      when others => return False;
      end case;
   end Action_Cannot_Be_Set;

   function Default_Is_Ignore
     (Sig : POSIX_Signals.Signal) return Boolean is
   begin
      case Sig is
      --  The required reserved signals, and SIGNULL
      when SIGCHLD | SIGIO | SIGURG | SIGCONT
        | SIGSTOP | SIGTSTP | SIGTTIN | SIGTTOU
         => return True;
--! #     if SunOS then
--!       when
--!           19   --  SIGPWR
--!         | 20   --  SIGWINCH
--!         | 32   --  SIGWAITING
--!         | 33   --  SIGLWP
--!         | 34   --  SIGFREEZE
--!         | 35   --  SIGTHAW
--!          => return True;
--! #     end if;
      when others => return False;
      end case;
   end Default_Is_Ignore;

   function Signal_Mask_Is_Process_Wide return Boolean is
   begin
      return True;
   end Signal_Mask_Is_Process_Wide;

   function Try_Install_Empty_Handler (Sig : POSIX_Signals.Signal)
     return Boolean is
   begin
      POSIX_Signals.Install_Empty_Handler (Sig);
      return True;
   end Try_Install_Empty_Handler;

   function Valid_Semaphore_Name
     (N : Positive) return POSIX_String is
      S : constant String := Integer'Image (N);
   begin
      return "/sem_" & To_POSIX_String (S (S'First + 1 .. S'Last));
   end Valid_Semaphore_Name;

   function Invalid_Semaphore_Name
     (N : Positive) return POSIX_String is
   begin
      if N = 1 then
         return "(#@!$%^~??||++";
      else
         return "~!@#$%^&*()_++_}{{.//";
      end if;
   end Invalid_Semaphore_Name;

   function Delay_Unit return Duration is
   begin
--! #     if Linux then
      --  For DOS & Linux on PC
      return 0.1;
--! #     elsif SunOS then
--!       return 0.02;
--! #     else
--!       return 0.02;
--! #     end if;
   end Delay_Unit;

   function New_Process_Startup return Duration is
   begin
--! #     if Linux then
      --  For DOS & Linux on PC
      return 0.5;
--! #     elsif SunOS then
--!       return 0.5;
--! #     else
--!       return 0.02;
--! #     end if;
   end New_Process_Startup;

   function Short_Watchdog_Timeout return Duration is
   begin
      return 15.0;
   end Short_Watchdog_Timeout;

   function Unused_Group_Name return POSIX_String is
   begin
      return "not_a_group_name";
   end Unused_Group_Name;

   function Invalid_Clock_ID return POSIX_Timers.Clock_ID is
      use POSIX_Timers;
      type Char_Array is array
        (1 .. Clock_ID'Size / Character'Size) of Character;
      type Clock_ID_Ptr is access all Clock_ID;
      type Char_Array_Ptr is access all Char_Array;
      function To_Clock_ID_Ptr is new
        Unchecked_Conversion (Char_Array_Ptr, Clock_ID_Ptr);
      F : aliased Char_Array;
   begin
      for I in Char_Array'Range loop
         F (I) := Character'Val (I);
      end loop;
      return To_Clock_ID_Ptr (F'Access).all;
      --  By bad luck, this could be a valid clock ID on some system.
      --  If so, replace this by appropriate code for that system.
   end Invalid_Clock_ID;

   function Invalid_Timespec return POSIX.Timespec is
      type Char_Array is array
        (1 .. Timespec'Size / Character'Size) of Character;
      type Timespec_Ptr is access all Timespec;
      type Char_Array_Ptr is access all Char_Array;
      function To_Timespec_Ptr is new
        Unchecked_Conversion (Char_Array_Ptr, Timespec_Ptr);
      F : aliased Char_Array;
   begin
      for I in Char_Array'Range loop
         F (I) := Character'Val (255);
      end loop;
      return To_Timespec_Ptr (F'Access).all;
   end Invalid_Timespec;

   function Valid_Internet_Address return POSIX.POSIX_String
   is
      File : File_Type;
      Buf  : String (1 .. 128);
      Last : Integer;
      Ubuf : Unbounded_String;
   begin
      begin
         Open (File, In_File, "/etc/inet/hosts");
      exception
      when others =>
         begin
            Open (File, In_File, "/etc/hosts");
         exception
         when others =>
            return "127.0.0.1";
         end;
      end;
      --  Try searching /etc/inet/hosts or /etc/hosts for a device name.
      loop
         Get_Line (File, Buf, Last);
         Ubuf := To_Unbounded_String (Buf);
         if Last > 4
           and Ada.Strings.Unbounded.Count (Ubuf, "localhost") = 0
         then
            Ubuf := To_Unbounded_String (Slice (Ubuf, 1, Last));
            if Index (Ubuf, ".") /= 0 then
               if Index (Ubuf, "" & ASCII.HT) /= 0 then
                  Ubuf := To_Unbounded_String
                     (Slice (Ubuf, 1, Index (Ubuf, "" & ASCII.HT) - 1));
               elsif Index (Ubuf, " ") /= 0 then
                  Ubuf := To_Unbounded_String
                     (Slice (Ubuf, 1, Index (Ubuf, " ") - 1));
               end if;
               Close (File);
               return To_POSIX_String (To_String (Ubuf));
            end if;
         end if;
      end loop;
   end Valid_Internet_Address;

   function Valid_Internet_Name return POSIX.POSIX_String is
      File : File_Type;
      Buf  : String (1 .. 128);
      Last : Integer;
      Temp : Unbounded_String;
      Ubuf : Unbounded_String;
   begin
      begin
         Open (File, In_File, "/etc/inet/hosts");
      exception
      when others =>
         begin
            Open (File, In_File, "/etc/hosts");
         exception
         when others =>
            return "localhost";
         end;
      end;
      --  Try searching /etc/inet/hosts or /etc/hosts for a device name.
      loop
         Get_Line (File, Buf, Last);
         Ubuf := To_Unbounded_String (Buf);
         if Last > 4
           and then Ada.Strings.Unbounded.Count (Ubuf, "localhost") = 0
         then
            Ubuf := To_Unbounded_String (Slice (Ubuf, 1, Last));
            if Index (Ubuf, ".") /= 0 then
               Temp := To_Unbounded_String
                  (Slice (Ubuf, Index (Ubuf, " ",
                   Going => Ada.Strings.Backward) + 1, Last));
               Temp := To_Unbounded_String (Slice (Temp, 1, Length (Temp)));
               Close (File);
               return To_POSIX_String (To_String (Temp));
            end if;
         end if;
      end loop;
   end Valid_Internet_Name;

   function Continue_Generates_Signal return Boolean is
   begin
      return True;
   end Continue_Generates_Signal;

end Test_Parameters;

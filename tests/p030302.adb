------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                             P 0 3 0 3 0 2                                --
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
--  AVAILABLE OR DISCLOSED,  OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS  --
--  FOR A PARTICULAR PURPOSE OF SAID MATERIAL.                              --
--                                                                          --
------------------------------------------------------------------------------
--  [$Revision$]

--  This a test of the POSIX_Signals package, and other features of
--  section 3.3 of POSIX.5b.  This test focusses on requirements that
--  involve more than one process.

with Ada.Streams,
     Ada_Task_Identification,
     POSIX,
     POSIX_Asynchronous_IO,
     POSIX_Files,
     POSIX_IO,
     POSIX_Message_Queues,
     POSIX_Permissions,
     POSIX_Process_Identification,
     POSIX_Process_Primitives,
     POSIX_Report,
     POSIX_Signals,
     POSIX_Timers,
     POSIX_Unsafe_Process_Primitives,
     System,
     System.Storage_Elements,
     Unchecked_Conversion;

procedure p030302 is
   use  Ada.Streams,
        Ada_Task_Identification,
        POSIX,
        POSIX_Asynchronous_IO,
        POSIX_Files,
        POSIX_IO,
        POSIX_Message_Queues,
        POSIX_Permissions,
        POSIX_Process_Identification,
        POSIX_Process_Primitives,
        POSIX_Report,
        POSIX_Signals,
        POSIX_Timers,
        POSIX_Unsafe_Process_Primitives,
        System;

   function To_Signal_Data is
     new Unchecked_Conversion (Integer, Signal_Data);
   function To_Integer is
     new Unchecked_Conversion (Signal_Data, Integer);

begin

   Header ("p030302");

   ---------------------------------------------------------------------
   --  [3.1.2] The initial mask of the environment task
   --  shall be the set specified by the Signal Mask attribute of
   --  the process template.

   ---------------------------------------------------------------------
   --  [3.3.1] When any stop signal (Signal_Stop,
   --  Signal_Terminal_Stop, Signal_Terminal_Input,
   --  Signal_Terminal_Output) is generated for a process, any
   --  pending Signal_Continue signals for that process shall be
   --  discarded.  Conversely, when Signal_Continue is generated for
   --  a process, all pending stop signals for that process shall be
   --  discarded.  When Signal_Continue is generated for a process
   --  that is stopped, the process shall be continued, even if the
   --  Signal_Continue is blocked or ignored.  If Signal_Continue is
   --  blocked and not ignored, it shall remain pending until it is
   --  either unblocked or a stop signal is generated for the
   --  process.

   ---------------------------------------------------------------------
   --  When multiple unblocked signals, all in the range
   --  Realtime_Signal are pending, the behavior shall be as if the
   --  implementation delivers the pending, unblocked signal with the
   --  lowest signal number within that range. No other ordering of
   --  signal delivery is specified.


   ---------------------------------------------------------------------
   --  [3.3.4] The default action for the required signals that are not
   --  reserved is to terminate the process abnormally.
   --  The required non-reserved signals are:
   --   SIGHUP,  SIGINT,  SIGKILL, SIGPIPE, SIGQUIT, SIGTERM, SIGUSR1,
   --   SIGUSR2, SIGCHLD
   --  If the Job Control option is supported the following also are
   --  required:
   --   SIGCONT, SIGSTOP, SIGTSTP, SIGTTIN, SIGTTOU
   --  If the Realtime Signals option is supported, the realtime signals
   --  are required to be supported.

   ---------------------------------------------------------------------
   --  [3.3.4] The default action for Signal_Child is to ignore it.

   ---------------------------------------------------------------------
   --  [3.3.4] The default action for Signal_Continue is to continue.

   ---------------------------------------------------------------------
   --  [3.3.4] The default action for Signal_Stop, Signal_Terminal_Stop,
   --  Signal_Terminal_Input, and Signal_Terminal_Output is to stop the
   --  process.

   ---------------------------------------------------------------------
   --  [3.3.6] Signals that are ignored shall not affect the behavior of
   --  any operation.

   ---------------------------------------------------------------------
   --  [3.3.6] Signals that are blocked shall not affect the behavior of
   --  any operation until they are delivered.  (This cannot literally be
   --  true, since they must affect the value returned by Pending_Signals.

   ---------------------------------------------------------------------
   --  [3.3.6] Return from a call to an interruptible POSIX operation
   --  shall be an abort completion point.

   ---------------------------------------------------------------------
   --  [3.3.6] When a task is blocked in an interruptible operation with
   --  masking No_Signals or RTS_Signals, abort of the task shall cause
   --  the operation to be interrupted.

   Done;
exception
   when E : others => Fatal_Exception (E);
end p030302;

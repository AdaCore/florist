------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                           P 1 5 0 1 0 0 b                                --
--                                                                          --
--                                B o d y                                   --
--                                                                          --
--                                                                          --
--  Copyright (c) 1995-1999 Florida  State  University  (FSU).  All Rights  --
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

--  Test for POSIX_Message_Queues package, supplentary main program
--  for child process.

with Ada_Streams,
     POSIX,
     POSIX_IO,
     POSIX_Message_Queues,
     POSIX_Process_Primitives,
     POSIX_Report,
     Test_Parameters;

procedure p150100b is

   use Ada_Streams,
       POSIX,
       POSIX_IO,
       POSIX_Message_Queues,
       POSIX_Process_Primitives,
       POSIX_Report;

   package TP renames Test_Parameters;

   Mqd : Message_Queue_Descriptor;
   Msg : Stream_Element_Array (1 .. 10);
   Last : Stream_Element_Offset;
   Prio : Message_Priority;

begin

   Mqd := Open (TP.Valid_MQ_Name (7), Read_Only);
   Receive (Mqd, Msg, Last, Prio);
   Assert (Last = 10, "A001");
   Assert (Msg = To_Stream_Element_Array ("Hello....."), "A002: Child");
   Close (Mqd);
   Exit_Process (Normal_Exit);

exception
when E1 : POSIX_Error =>
   Optional
     (Message_Queues_Option, Operation_Not_Implemented, E1, "A003: Child");
when E2 : others => Unexpected_Exception (E2, "A004: Child");
end p150100b;

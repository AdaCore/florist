------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--                                  D E P S                                 --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                                                                          --
--  Copyright (c)  1996, 1997           Florida  State  University  (FSU),  --
--  All Rights Reserved.                                                    --
--                                                                          --
--  This file is a component of FLORIST, an  implementation of an  Ada API  --
--  for the POSIX OS services, for use with  the  GNAT  Ada  compiler  and  --
--  the FSU Gnu Ada Runtime Library (GNARL).   The  interface  is intended  --
--  to be close to that specified in  IEEE STD  1003.5: 1990  and IEEE STD  --
--  1003.5b: 1996.                                                          --
--                                                                          --
--  FLORIST is free software;  you can  redistribute  it and/or  modify it  --
--  under terms of the  GNU  General  Public  License as  published by the  --
--  Free Software Foundation;  either version  2, or (at  your option) any  --
--  later version.  FLORIST is distributed  in  the hope  that  it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without  even the implied  warranty  --
--  of MERCHANTABILITY or FITNESS FOR A PARTICULAR  PURPOSE.  See  the GNU  --
--  General Public License for more details.  You  should have  received a  --
--  copy of the GNU General Public License  distributed  with  GNARL;  see  --
--  file  COPYING.  If not,  write to  the  Free  Software  Foundation, 59  --
--  Temple Place - Suite 330, Boston, MA 02111-1307, USA.                   --
--                                                                          --
--  As a special exception, if other files instantiate generics from  this  --
--  unit, or you link this unit with other files to produce an  executable, --
--  this  unit does not by itself cause the  resulting  executable  to  be  --
--  covered  by the  GNU  General  Public License. This exception does not  --
--  however invalidate any other  reasons why the executable file might be  --
--  covered by the GNU Public License.                                      --
--                                                                          --
------------------------------------------------------------------------------
--  [$Revision$]

--  This procedure is introduced to compile all the POSIX.5b implementation
--  packages, but not the POSIX.5c (draft) files.

pragma Warnings (Off);
with Ada_Streams;
with Ada_Task_Identification;
with POSIX;
with POSIX_Asynchronous_IO;
with POSIX_Calendar;
with POSIX_Condition_Variables;
with POSIX_Configurable_File_Limits;
with POSIX_Configurable_System_Limits;
with POSIX_File_Locking;
with POSIX_File_Status;
with POSIX_Files;
with POSIX_Generic_Shared_Memory;
with POSIX_Group_Database;
with POSIX_IO;
with POSIX_Limits;
with POSIX_Memory_Locking;
with POSIX_Memory_Mapping;
with POSIX_Memory_Range_Locking;
with POSIX_Message_Queues;
with POSIX_Mutexes;
with POSIX_Options;
with POSIX_Page_Alignment;
with POSIX_Permissions;
with POSIX_Process_Environment;
with POSIX_Process_Identification;
with POSIX_Process_Primitives;
with POSIX_Process_Scheduling;
with POSIX_Process_Times;
with POSIX_Semaphores;
with POSIX_Shared_Memory_Objects;
with POSIX_Signals;
with POSIX_Supplement_to_Ada_IO;
with POSIX_Terminal_Functions;
with POSIX_Timers;
with POSIX_Unsafe_Process_Primitives;
with POSIX_User_Database;
pragma Warnings (On);

procedure deps is
begin
   null;
end deps;

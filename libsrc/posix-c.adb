------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--                               P O S I X . C                              --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                                                                          --
--             Copyright (C) 1996-1997 Florida State University             --
--                     Copyright (C) 1998-2014, AdaCore                     --
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

with System.Storage_Elements;
package body POSIX.C is

   use System.Storage_Elements;

   ---------------
   --  Advance  --
   ---------------

   procedure Advance (Ptr : in out char_ptr) is
   begin
      Ptr := To_Ptr (To_Address (Ptr) + char'Size / System.Storage_Unit);
   end Advance;

   procedure Advance (Ptr : in out char_ptr_ptr) is
   begin
      Ptr := To_Ptr (To_Address (Ptr) + char_ptr'Size / System.Storage_Unit);
   end Advance;

   -------------------------
   --  Form_POSIX_String  --
   -------------------------

   function strlen (str : char_ptr) return size_t;
   pragma Import (C, strlen, "strlen");

   function Form_POSIX_String (Str : char_ptr)
      return POSIX.POSIX_String is
   begin
      if Str = null then
         return "";
      end if;
      declare
         Result : constant POSIX.POSIX_String (1 .. Integer (strlen (Str)));
         for Result'Address use Str.all'Address;
         pragma Import (Ada, Result);
      begin
         return Result;
      end;
   end Form_POSIX_String;

end POSIX.C;

------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--                               P O S I X . C                              --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                                                                          --
--  Copyright (c) 1996, 1997            Florida  State  University  (FSU),  --
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


with Unchecked_Conversion;
with System;
with System.Storage_Elements;
package body POSIX.C is

   use System;
   use System.Storage_Elements;

   ---------------
   --  Advance  --
   ---------------

   function To_Address is new Unchecked_Conversion (char_ptr, Address);
   function To_Ptr is new Unchecked_Conversion (Address, char_ptr);

   procedure Advance (Ptr : in out char_ptr) is
   begin
      Ptr := To_Ptr (To_Address (Ptr) + char'Size / System.Storage_Unit);
   end Advance;

   function To_Address is new Unchecked_Conversion (char_ptr_ptr, Address);
   function To_Ptr is new Unchecked_Conversion (Address, char_ptr_ptr);

   procedure Advance (Ptr : in out char_ptr_ptr) is
   begin
      Ptr := To_Ptr (To_Address (Ptr) + char_ptr'Size / System.Storage_Unit);
   end Advance;

   -------------------------
   --  Form_POSIX_String  --
   -------------------------

   function strlen (str : in char_ptr) return size_t;
   pragma Import (C, strlen, "strlen");

   function Form_POSIX_String (Str : in char_ptr)
      return POSIX.POSIX_String is
   begin
      if Str = null then return ""; end if;
      declare
         subtype POSIX_Substring is POSIX.POSIX_String
           (1 .. Integer (strlen (Str)));
         type POSIX_Substring_Ptr is access POSIX_Substring;
         function char_ptr_to_pssptr is new Unchecked_Conversion
           (char_ptr, POSIX_Substring_Ptr);
      begin
         return char_ptr_to_pssptr (Str).all;
      end;
   end Form_POSIX_String;

end POSIX.C;

------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--            P O S I X . S U P P L E M E N T _ T O _ A D A _ I O           --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                                                                          --
--  Copyright (c) 1996 Florida State University (FSU), All Rights Reserved. --
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
--  [Revision: 1.1]

--  .... This cannot be fully implemented without special support
--  from the implementation of the package Ada.Text_IO.
--  The present version is just a stub.  A true implementation
--  will apparently need to coordinate with
--  the implementation of the standard Ada IO packages, e.g.

--     System.File_Control_Block
--     System.File_IO
--     System.Direct_IO
--     System.Sequential_IO
--     Interfaces.C_Streams

--  The modifications we need to make to those packages will
--  need coordination with ACT.

with Ada.IO_Exceptions,
     Ada.Text_IO,
     Interfaces.C_Streams,
     POSIX.IO,
     POSIX.Implementation,
     POSIX.Permissions,
     System.File_Control_Block,
     System.File_IO,
     Unchecked_Conversion;

package body POSIX.Supplement_to_Ada_IO is

   use Interfaces.C_Streams;
   use POSIX.Implementation;

   subtype System_File_Type is System.File_Control_Block.AFCB_Ptr;

   function Form_String (Val : Form_Values_for_Open) return String is
   begin
      Raise_POSIX_Error (Operation_Not_Supported);
      return "";
   end Form_String;

   function Form_Value (Str : String) return Form_Values_for_Open is
      A : Form_Values_for_Open;
   begin
      Raise_POSIX_Error (Operation_Not_Supported);
      return A;
   end Form_Value;

   function Form_String (Val : Form_Values_for_Create) return String is
   begin
      Raise_POSIX_Error (Operation_Not_Supported);
      return "";
   end Form_String;

   function Form_Value (Str : String) return Form_Values_for_Create is
      A : Form_Values_for_Create;
   begin
      Raise_POSIX_Error (Operation_Not_Supported);
      return A;
   end Form_Value;

   --  .... We may be able to implement Flush_All, using the open file
   --  chain, which is maintained by System.File_IO.

   procedure Flush_All is
   begin
      Raise_POSIX_Error (Operation_Not_Supported);
   end Flush_All;

   procedure Flush_Text_IO (File : in  Ada.Text_IO.File_Type) is
   begin
      Ada.Text_IO.Flush (File);
   end Flush_Text_IO;

   procedure Flush_Sequential_IO (File : File_Type) is
      function To_SFT is new
        Unchecked_Conversion (File_Type, System_File_Type);

      F   : System_File_Type;
      Ret : int;

   begin
      F := To_SFT (File);
      System.File_IO.Check_File_Open (F);
      Ret := fflush (F.Stream);
   end Flush_Sequential_IO;

   procedure Flush_Direct_IO (File : File_Type) is
      function To_SFT is new
        Unchecked_Conversion (File_Type, System_File_Type);

      F   : System_File_Type;
      Ret : int;

   begin
      F := To_SFT (File);
      System.File_IO.Check_File_Open (F);
      Ret := fflush (F.Stream);
   end Flush_Direct_IO;

end POSIX.Supplement_to_Ada_IO;

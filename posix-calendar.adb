------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--                        P O S I X . C A L E N D A R                       --
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
--  [$Revision: 1.1]

--  ....Change POSIX.5?????
--  This package is rather ugly, since it was modeled on early Ada 83
--  implementations where the Ada Calendar.Clock function was
--  implemented using the C time() function.  Thus, for correct
--  interaction with file times (see Set_File_Times), we have to
--  limit the precision of the time representation here to match
--  the precision of the C type time_t.  That is, all times must be
--  truncated to the nearest second.

--  At one point we used gettimeofday, on systems where that
--  function is supported, to provide more accurate time.
--  As mentioned above, that created inconsistencies in file
--  access times versus clock values.

with POSIX.C,
     Unchecked_Conversion;
package body POSIX.Calendar is

   package AC renames Standard.Calendar;

   use Standard.Calendar,
       POSIX.C;

   function Duration_To_POSIX_Time is new Unchecked_Conversion
      (Duration, POSIX_Time);
   function POSIX_Time_To_Duration is new Unchecked_Conversion
      (POSIX_Time, Duration);

   function Truncate (D : Duration) return Duration;

   function Truncate (D : Duration) return Duration is
   begin
      return Duration (time_t (D));
   end Truncate;

   -------------
   --  Clock  --
   -------------

   function c_time (t : time_t_var_ptr) return time_t;
   pragma Import (C, c_time, time_LINKNAME);

   --  Warning, don't use Epoch and Time_Of here,
   --  since Time_Of converts from GMT to local time!!!!

   function Clock return POSIX_Time is
   begin
      return Duration_To_POSIX_Time (Duration (c_time (null)));
   end Clock;

   ---------------
   --  To_Time  --
   ---------------

   function To_Time (Date : POSIX_Time) return AC.Time is
   begin
      return AC.Time (Date);
   end To_Time;

   ---------------------
   --  To_POSIX_Time  --
   ---------------------

   function To_POSIX_Time (Date : AC.Time) return POSIX_Time is
   begin
      return Duration_To_POSIX_Time (Duration (time_t
        (POSIX_Time_To_Duration (POSIX_Time (Date)))));
   end To_POSIX_Time;

   ------------
   --  Year  --
   ------------

   function Year (Date : POSIX_Time) return Year_Number is
   begin
      return Year_Number (AC.Year (AC.Time (Date)));
   end Year;

   -------------
   --  Month  --
   -------------

   function Month (Date : POSIX_Time) return Month_Number is
   begin
      return AC.Month (AC.Time (Date));
   end Month;

   -----------
   --  Day  --
   -----------

   function Day (Date : POSIX_Time) return Day_Number is
   begin
      return AC.Day (AC.Time (Date));
   end Day;

   ---------------
   --  Seconds  --
   ---------------

   function Seconds (Date : POSIX_Time) return Day_Duration is
   begin
      return AC.Seconds (AC.Time (Date));
   end Seconds;

   -------------
   --  Split  --
   -------------

   procedure Split
     (Date    : in POSIX_Time;
      Year    : out Year_Number;
      Month   : out Month_Number;
      Day     : out Day_Number;
      Seconds : out Day_Duration) is
   begin
      AC.Split (AC.Time (Date), Year, Month, Day, Seconds);
   end Split;

   ---------------
   --  Time_Of  --
   ---------------

   function Time_Of
     (Year    : Year_Number;
      Month   : Month_Number;
      Day     : Day_Number;
      Seconds : Day_Duration := 0.0) return POSIX_Time is
   begin
      return POSIX_Time (AC.Time_Of (Year, Month, Day,
        Truncate (Seconds)));
   end Time_Of;

   -----------
   --  "+"  --
   -----------

   function "+" (L : POSIX_Time; R : Duration) return POSIX_Time is
   begin
      return POSIX_Time (AC.Time (L) + Truncate (R));
   end "+";

   -----------
   --  "+"  --
   -----------

   function "+" (L : Duration; R : POSIX_Time) return POSIX_Time is
   begin
      return POSIX_Time (Truncate (L) + AC.Time (R));
   end "+";

   -----------
   --  "-"  --
   -----------

   function "-" (L : POSIX_Time; R : Duration) return POSIX_Time is
   begin
      return POSIX_Time (AC.Time (L) - R);
   end "-";

   -----------
   --  "-"  --
   -----------

   function "-" (L : POSIX_Time; R : POSIX_Time) return Duration is
   begin
      return AC.Time (L) - AC.Time (R);
   end "-";

   -----------
   --  "<"  --
   -----------

   function "<" (L, R : POSIX_Time) return Boolean is
   begin
      return AC.Time (L) < AC.Time (R);
   end "<";

   ------------
   --  "<="  --
   ------------

   function "<=" (L, R : POSIX_Time) return Boolean is
   begin
      return AC.Time (L) <= AC.Time (R);
   end "<=";

   -----------
   --  ">"  --
   -----------

   function ">" (L, R : POSIX_Time) return Boolean is
   begin
      return AC.Time (L) > AC.Time (R);
   end ">";

   ------------
   --  ">="  --
   ------------

   function ">=" (L, R : POSIX_Time) return Boolean is
   begin
      return AC.Time (L) >= AC.Time (R);
   end ">=";

   ---------------------
   --  To_POSIX_Time  --
   ---------------------

   function To_POSIX_Time (Date : POSIX.Timespec) return POSIX_Time is
   begin
      return Duration_To_POSIX_Time (Truncate (POSIX.To_Duration (Date)));
   end To_POSIX_Time;

   -------------------
   --  To_Timespec  --
   -------------------

   function To_Timespec
     (Date : POSIX_Time) return POSIX.Timespec is
   begin
      return POSIX.To_Timespec (POSIX_Time_To_Duration (Date));
   end To_Timespec;

end POSIX.Calendar;

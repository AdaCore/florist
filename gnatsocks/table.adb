--  file : table.adb [$Revision$]

--  See package body (file tables.adb) for header comments.

--  This implementation of the package uses a fixed-size array
--  representation.  What about other representations?

--  Ted Baker, 13 January 1997

PACKAGE BODY TABLE IS

  Table_Size : constant := 100;
  Last_Entry : Natural := 0;
  type Pair is record
     Key : Key_String;
     Value : Value_String;
  end record;
  type Table_Type is array (1..Table_Size) of Pair;
  The_Table: Table_Type;

  procedure Set_Value
    (Key   : in Key_String;
     Value : in Value_String) is
  begin
    for I in The_Table'First .. Last_Entry loop
       if The_Table(I).Key = Key then
          The_Table(I).Value := Value;
          return;
       end if;
    end loop;
    if Last_Entry = Table_Size then raise Overflow;
    end if;
    Last_Entry := Last_Entry + 1;
    The_Table(Last_Entry) := (Key=> Key, Value=> Value);
  end Set_Value;

  function  Has_Value
    (Key   : in Key_String) return Boolean is
  begin
    for I in The_Table'First .. Last_Entry loop
       if The_Table(I).Key = Key then return True;
       end if;
    end loop;
    return False;
  end Has_Value;

  function  Value
    (Key   : in Key_String) return Value_String is
  begin 
    for I in The_Table'First .. Last_Entry loop
       if The_Table(I).Key = Key then return The_Table(I).Value;
       end if;
    end loop;
    raise Not_Found;
  end Value;

  -- generic
  --   with procedure Action (Key : in Key_String; Value : in Value_String);
  procedure Enumerate is
  begin
    for I in The_Table'First .. Last_Entry loop
       Action (The_Table(I).Key, The_Table(I).Value);
    end loop;
  end Enumerate;

END TABLE;


----------------------
-- REVISION HISTORY --
----------------------

--  ----------------------------
--  revision 1.1
--  date: 1997/02/26 15:29:22;  author: baker;  state: Exp;
--  Initial revision
--  ----------------------------
--  New changes after this line.  Each line starts with: "--  "
--  This version is just to check how the "checkin" and "checkout" scripts
--  are working.

-----------------------------------------------------------------------
--  file : table.ads [$Revision$]
-----------------------------------------------------------------------

--  See file READ.ME for explanation of this and the other examples.
--  Uppercase characters are used for emphasis of new features
--  in this example, as compared with the other examples.

package table is
  subtype key_string is string (1..10);
  subtype value_string is string (1..16);
  procedure set_value
    (key   : in key_string;
     value : in value_string);
  function  has_value
    (key   : in key_string) return boolean;
  function  value
    (key   : in key_string) return value_string;
  generic
    with procedure action (key : in key_string; value : in value_string);
  procedure enumerate;
  not_found : exception;
  overflow : exception;
end table;

--  Table is an abstract data object that provides the
--  ability to define a mapping from a finite set of strings
--  of a given fixed length to strings of another fixed length.

--  Set_Value attempts to modify Table, so that Key is mapped to Value.
--  It raises Overflow if there is not enough space left in Table.

--  Has_Value returns True iff Table contains a mapping for Key.
--  It does not raise any exceptions.

--  Value returns the value to which Key is mapped by Table, if
--  Table contains a mapping for Key.
--  Otherwise, Value raises Not_Found.

--  Generic procedure Enumerate must be instantiated with a specific
--  Action procedure to create a callable subprogram.  The instantiation
--  is a procedure that takes no argument, and calls the
--  Action procedure for each (Key, Value) pair in Table.

-----------------------------------------------------------------------------
--  file : socketest.adb  [$Revision$]
-----------------------------------------------------------------------------

--  This is an indirect translation into GNAT Ada
--  of the Java example in file "socketest.java".

with ada.exceptions;
with ada.text_io;
with sockets;
with sockets.internet;
procedure sockettest is
   t : sockets.stream_socket;
   ins : sockets.input_stream_ptr;
   lf : constant character := character'val (10);
   ch : character := ' ';
begin
   sockets.open (t, sockets.internet.new_address (12, "www.cs.fsu.edu"));
   ins := sockets.get_input_stream (t);
   while ch /= lf loop
      character'read (ins,ch);
      ada.text_io.put (ch);
   end loop;
exception when e : others =>
   ada.text_io.put_line ("error " & ada.exceptions.exception_name (e));
end sockettest;



--------------------------------------------------------------------------
--  file : echoserver.adb [$Revision$]
--------------------------------------------------------------------------

--  This is a direct translation into GNAT Ada
--  of the Java example in file "echoserver.java".

--  waits for a client to attach to port 8189, then
--  reads input from the client, a line at a time, and echos it.
--  To run this program, type "java EchoServer", then put the job in
--  the background or switch to another window and type
--  "telnet <hostname> 8189", replacing <hostname> by the name of
--  the host on which you are running the echo-server.
--  This will connect you to the server.

with ada.characters.latin_1;
with ada.exceptions;
with ada.text_io;
with sockets;
with sockets.internet;
procedure echoserver is

   s : sockets.server_socket;
   connection : sockets.stream_socket;
   ins : sockets.input_stream_ptr;
   outs : sockets.output_stream_ptr;
   peer : sockets.internet.internet_socket_address;
   lf : constant character := ada.characters.latin_1.lf;
   -- line-feed
   cr : constant character := ada.characters.latin_1.cr;
   -- carriage-return

   procedure writeln (s : string) is
   begin
      string'write (outs, s);
      character'write (outs, cr);
      character'write (outs, lf);
   end writeln;

   function readln return string is
      buf : string (1 .. 1024);
      i : integer := 1;
   begin
      loop
         character'read (ins, buf (i));
         exit when buf (i) = lf;
         i := i + 1;
      end loop;
      return buf (1 .. i-2);
   end readln;

begin
   sockets.open (s, sockets.internet.new_address (8189, "dad.cs.fsu.edu"));
   sockets.accept_connection (s, connection, peer);
   ins := sockets.get_input_stream (connection);
   outs := sockets.get_output_stream (connection);
   writeln ("Hello! Enter BYE to exit.");
   loop
     declare
       str : string := readln;
     begin 
       exit when str (1..3) = "BYE";
       writeln ("Echo: """ & str & '"');
     end;
   end loop;
exception when e : others =>
   ada.text_io.put_line (ada.exceptions.exception_name (e)
      & ": " & ada.exceptions.exception_message (e));
end echoserver;

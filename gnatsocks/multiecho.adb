--------------------------------------------------------------------------
--  file : multiecho.adb [$Revision$]
--------------------------------------------------------------------------

--  This is a modification of "echoserver.adb", to allow multiple
--  connections, using Ada tasks.

--  waits for a client to attach to port 8189, then
--  reads input from the client, a line at a time, and echos it.
--  To run this program, type "java EchoServer",
--  then put the job in the background
--  or switch to another window, and type
--  "telnet <hostname> <portnumber>",
--  replacing <hostname> by the name of
--  the host on which you are running the echo-server
--  and <portnumber> by the port number that the server printed
--  out when it started up.
--  This will connect you to the server.
--  The server should be able to handle up to 4 concurrent connections.
--  Use control-C to kill the server.

with ada.characters.latin_1;
with ada.exceptions;
with ada.task_identification;
with ada.text_io;
with sockets;
with sockets.internet;
procedure multiecho is

   type connection_id is range 1..4;
   connections : array (connection_id) of sockets.stream_socket;

   procedure shut_down (e : ada.exceptions.exception_occurrence);
   main_task : ada.task_identification.task_id := 
       ada.task_identification.current_task;
   --  used to shut down the entire program

   task type server_task;
   servers : array (connections'range) of server_task;

   lf : constant character := ada.characters.latin_1.lf;
   -- line-feed
   cr : constant character := ada.characters.latin_1.cr;
   -- carriage-return

   protected server_pool is
      entry await_turn;
      procedure next_turn;
   private
      turn : boolean := false;
   end server_pool;

   protected body server_pool is
      entry await_turn when turn is
      begin
         turn := false;
      end await_turn;
      procedure next_turn is
      begin
         turn := true;
      end next_turn;
   end server_pool;

   procedure writeln (outs : sockets.output_stream_ptr; s : string) is
   begin
      string'write (outs, s);
      character'write (outs, cr);
      character'write (outs, lf);
   end writeln;

   function readln (ins : sockets.input_stream_ptr) return string is
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

   peer : sockets.internet.internet_socket_address;
   s   : sockets.server_socket;

   task body server_task is
      connection : sockets.stream_socket;
      ins : sockets.input_stream_ptr;
      outs : sockets.output_stream_ptr;
   begin
      loop
         server_pool.await_turn;
         sockets.accept_connection (s, connection, peer);
         server_pool.next_turn;
         ins := sockets.get_input_stream (connection);
         outs := sockets.get_output_stream (connection);
         writeln (outs, "Hello! Enter BYE to exit.");
         loop
            declare
               str : string := readln (ins);
            begin 
               exit when str (1..3) = "BYE";
               writeln (outs, "Echo: """ & str & '"');
            end;
         end loop;
         sockets.close(connection);
      end loop;
   exception when e : others => shut_down (e);
   end server_task;

   procedure shut_down (e : ada.exceptions.exception_occurrence) is
   begin
      ada.text_io.put_line ("main: " & ada.exceptions.exception_name (e)
         & ": " & ada.exceptions.exception_message (e));
      sockets.close (s);
      ada.task_identification.abort_task (main_task);
   end shut_down;

begin
   sockets.open (s, sockets.internet.new_address
     (sockets.internet.any_port, sockets.internet.all_local_addresses));
   ada.text_io.put_line ("serving at: " 
       & sockets.internet.get_addressstring (
--     & sockets.internet.get_hostbyaddr (
       sockets.internet.get_internet_address (
       sockets.internet.get_address (s)))
     & " port "
     & sockets.internet.port_number'image (
       sockets.internet.get_port (
       sockets.internet.get_address (s))));
   server_pool.next_turn;
exception when e : others => shut_down (e);
end multiecho;



--------------------------------------------------------------------------
--  file : multidb.adb [$Revision$]
--------------------------------------------------------------------------

--  This is a modification of "multiecho.adb", to implement instead
--  a simple database.  It illustrates the use of Ada direct I/O.

--  The present version has at least one flaw.  The socket input '!'
--  is supposed to shut down the server, by aborting the main task.
--  This is not working correctly, probably due to a bad interaction
--  between the "accept" operation and the task abort.  I need to look
--  into this more, but you should not need this capability for your
--  projects.

--  Ted Baker

with ada.characters.latin_1;
with ada.exceptions;
with ada.task_identification;
with ada.text_io;
with ada.direct_io;
with sockets;
with sockets.internet;
with table;
procedure multidb is

   use table;

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

   is_letter : array (character) of boolean := 
     ('a'..'z' | 'A'..'Z' => true, others => false);

   procedure skipln (ins : sockets.input_stream_ptr) is
      c : character := ' ';
   begin 
      while c /= lf loop
         character'read (ins, c);
      end loop;
   end skipln;

   procedure get_string
     (ins : sockets.input_stream_ptr;
      outs : sockets.output_stream_ptr;
      s : out string) is
      i : integer := s'first -1;
      c : character;
   begin
      string'write (outs, "enter a string of up to" &
        integer'image (s'length) & " letters: ");
      while i < s'last loop
         character'read (ins, c);
         exit when not is_letter (c);
         i := i + 1; s (i) := c;
      end loop;
      while i <  s'last loop
         i := i + 1; s (i) := ' ';
      end loop;
      skipln (ins);
   end get_string;

   peer : sockets.internet.internet_socket_address;
   s   : sockets.server_socket;

   task db_task is
      entry store (key : key_string; value : value_string);
      entry fetch (key : key_string; value : out value_string);
   end db_task;

   task body server_task is
      connection : sockets.stream_socket;
      ins : sockets.input_stream_ptr;
      outs : sockets.output_stream_ptr;
      ch : character;
      key : key_string;
      value : value_string;
   begin
      loop
         begin
            server_pool.await_turn;
            sockets.accept_connection (s, connection, peer);
            server_pool.next_turn;
            ins := sockets.get_input_stream (connection);
            outs := sockets.get_output_stream (connection);
            writeln (outs, "Hello!");
            loop
               string'write (outs, "enter +, ?, ., or ! ");
               character'read (ins, ch);
               skipln (ins);
               case ch is 
               when '+' => 
                  get_string (ins, outs, key);
                  get_string (ins, outs, value);
                  db_task.store (key, value);
                  writeln (outs, "ok.");
               when '?' => 
                  get_string (ins, outs, key);
                  db_task.fetch (key, value);
                  writeln (outs, "value = " & value & '.');
               when '.' =>
                  writeln (outs, "bye.");
                  exit;
               when '!' =>
                  writeln (outs, "bye.");
                  sockets.close (connection);
                  sockets.close (s);
                  abort db_task;
                  ada.task_identification.abort_task (main_task);
               when others => null;
               end case;
            end loop;
         exception when others => null;
         end;
         sockets.close(connection);
      end loop;
   exception when e : others => shut_down (e);
   end server_task;

   task body db_task is
   begin
      loop
         begin
            select
            accept store (key : key_string; value : value_string) do
               set_value (key, value);            
            end store;
            or accept fetch (key : key_string; value : out value_string) do
                value := table.value (key);
            end fetch;
            or terminate;
            end select;
         exception when others => null;
         end;
      end loop;
   end db_task;

   procedure shut_down (e : ada.exceptions.exception_occurrence) is
   begin
      ada.text_io.put_line ("main: " & ada.exceptions.exception_name (e)
         & ": " & ada.exceptions.exception_message (e));
      sockets.close (s);
      abort db_task;
      ada.task_identification.abort_task (main_task);
   end shut_down;

begin
   sockets.open (s, sockets.internet.new_address
     (sockets.internet.any_port, sockets.internet.all_local_addresses));
   ada.text_io.put_line ("serving at: " 
       & sockets.internet.get_addressstring (
       sockets.internet.get_internet_address (
       sockets.internet.get_address (s)))
     & " port "
     & sockets.internet.port_number'image (
       sockets.internet.get_port (
       sockets.internet.get_address (s))));
   server_pool.next_turn;
exception when e : others => shut_down (e);
end multidb;





with GNAT.Sockets; use GNAT.Sockets;

with Ada.Streams;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Ada.Containers.Vectors; use Ada.Containers;

with ib_ada.communication.incomming;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.IO_Exceptions;

with Interfaces; use Interfaces;
with System; use System;

with Ada.Unchecked_Conversion;

package body ib_ada.connection is

   function to_length is new Ada.Unchecked_Conversion (source => string, target => integer);

   task body socket_connection is
      use Ada.Streams;
      client     : Socket_Type;
      address    : Sock_Addr_Type;
      channel    : Stream_Access;
      offset     : Stream_Element_Count;
   begin
      loop
         delay 0.01;
         -- throttle 100 calls/sec max.
         -- interactive broker maximum allowed frequency
         select
            accept setup (session : session_type) do
               GNAT.Sockets.Initialize;
               Create_Socket (Client);

               set_socket_option (client, socket_level, (reuse_address, true));

               Address.Addr := Inet_Addr ("127.0.0.1");
               Address.Port := ports (session);

               Connect_Socket (Client, Address);
               Channel := Stream (Client);

            end setup;
         or
            accept disconnect;
            exit;
         or
            accept send (req : req_type; resp : out resp_type) do
               declare
                  msg_header : string(1..4);
                  msg_header_swap : string(1..4);
                  msg_length : integer;
               begin
                  if req.msg /= "" then

                     -- client call
                     string'write(channel, +req.msg);

                     -- and listen..?
                     if req.and_listen then
                        loop -- read server messages until a handler says its over
                           -- read header
                           string'read(channel, msg_header);
                           msg_header_swap := (msg_header(msg_header'last), msg_header(msg_header'last-1), msg_header(msg_header'last-2), msg_header(msg_header'last-3));
                           msg_length := to_length (msg_header_swap);
                           declare
                              msg_buffer: Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset(msg_length));
                              message_tokens : msg_vector.vector;
                              server_msg : unbounded_String;
                           begin
                              -- read message
                              Ada.Streams.Read (channel.All, msg_buffer, offset);
                              for i in msg_buffer'range loop
                                 append (server_msg, Character'Val (msg_buffer (I)));
                              end loop;
                              resp := ib_ada.communication.incomming.handle_message (req, server_msg);
                           end;

                           exit when not resp.and_listen;

                        end loop;
                     end if;
                  end if;
               end;
            end send;
         or
              terminate;
         end select;
      end loop;
      Close_Socket (client);
   exception
      when others =>
         Ada.Text_IO.Put_Line ("!!!!!!!!!!! Error !!!!!!!!!!!!");
   end;
end ib_ada.connection;


--
--  with GNAT.Sockets; use GNAT.Sockets;
--  with Ada.Streams;
--
--  --with Ada.Text_IO; use Ada.Text_IO;
--
--  --with Ada.IO_Exceptions;
--  with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--  --With Ada.Text_IO.Unbounded_IO;
--  with Ada.Containers.Vectors; use Ada.Containers;
--
--  with ib_ada.communication; use ib_ada.communication;
--  with ib_ada.communication.incomming;
--
--  package body ib_ada.connection is
--
--     task body socker_sender is
--        my_connection : GNAT.Sockets.Socket_Type;
--        my_client     : GNAT.Sockets.Sock_Addr_Type;
--        my_channel    : GNAT.Sockets.Stream_Access;
--     begin
--        loop
--           delay 0.01; -- throttle 100 calls/sec max
--           select
--              accept Setup (connection : GNAT.Sockets.Socket_Type;
--                            client  : GNAT.Sockets.Sock_Addr_Type;
--                            channel : GNAT.Sockets.Stream_Access) do
--                 my_connection := connection;
--                 my_client     := client;
--                 my_channel    := channel;
--              end;
--           or
--              accept send (msg : in string) do
--                 string'write(my_channel, msg);
--              end;
--           end select;
--        end loop;
--     end;
--
--     task body socket_listener is
--        use Ada.Streams;
--        client_socket  : Socket_Type;
--        Address : Sock_Addr_Type;
--        Channel : Stream_Access;
--     begin
--
--        GNAT.Sockets.Initialize;
--        Create_Socket (client_socket);
--        Address.Addr := Inet_Addr ("127.0.0.1");
--        Address.Port := ports (TWS_LIVE);
--
--        Connect_Socket (client_socket, Address);
--        Channel := Stream (client_socket);
--        client.setup (client_socket, address, channel);
--
--        loop
--           declare
--              msg_length_buffer : Ada.Streams.Stream_Element_Array (1 .. 4);
--              msg : unbounded_string;
--              char : character;
--              offset : Ada.Streams.Stream_Element_Count;
--              continue_listening : boolean := true;
--           begin
--              Ada.Streams.Read (channel.all, msg_length_buffer, offset);
--              char := character'val (msg_length_buffer(msg_length_buffer'last));
--              declare
--                 msg_buffer: Ada.Streams.Stream_Element_Array (1 .. character'pos(char));
--                 message_tokens : msg_vector.vector;
--              begin
--                 Ada.Streams.Read (Channel.All, msg_buffer, Offset);
--                 for i in msg_buffer'range loop
--                    append (msg, Character'Val (msg_buffer (I)));
--                 end loop;
--                 continue_listening := ib_ada.communication.incomming.handle_message (msg);
--              end;
--           end;
--        end loop;
--     end;
--
--     listener : socket_listener;
--
--  end ib_ada.connection;
--
--  --suio.Put_line (msg);
--  --Put_Line("");
--  --message_tokens := parse_message (msg);
--  --put_line (count_type'image(message_tokens.length));
--  --suio.Put_line (msg);
--  --msg_buffer.print_msgs_to_stdout;
--  --Put_Line(offset'image);
--  --package suio renames Ada.Text_IO.Unbounded_IO;

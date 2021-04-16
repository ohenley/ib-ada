with gnat.sockets;
------------------------------------
with ada.streams;
with ada.strings.unbounded;
with ada.containers.vectors;
with ada.text_io;
with ada.io_exceptions;
with ada.unchecked_conversion;
------------------------------------
with interfaces;
with system;
------------------------------------
with ib_ada.communication.incomming;

use gnat.sockets;
------------------------------------
use ada.strings.unbounded;
use ada.containers;
use ada.text_io;
------------------------------------
use interfaces;
use system;

package body ib_ada.connection is

   function to_length is new ada.unchecked_conversion (source => string, target => integer);

   task body socket_connection is
      use ada.streams;
      client     : socket_type;
      address    : sock_addr_type;
      channel    : stream_access;
      offset     : stream_element_count;
   begin
      loop
         delay 0.01;
         -- throttle 100 calls/sec max.
         -- interactive broker maximum allowed frequency
         select
            accept setup (session : session_type) do
               gnat.sockets.initialize;
               create_socket (client);

               set_socket_option (client, socket_level, (reuse_address, true));

               address.addr := inet_addr ("127.0.0.1");
               address.port := ports (session);

               connect_socket (client, address);
               channel := stream (client);

            end setup;
         or
            accept disconnect;
            exit;
         or
            accept send (req : req_type; resp : out resp_type) do
               declare
                  msg_header      : string(1..4);
                  msg_header_swap : string(1..4);
                  msg_length      : integer;
               begin
                  if req.msg /= "" then
                     string'write(channel, +req.msg);                             -- client msg call
                     if req.and_listen then                                       -- and listen..?
                        loop                                                      -- read server messages until a handler says its over
                           string'read(channel, msg_header);                      -- read header
                           msg_header_swap := (msg_header(msg_header'last), msg_header(msg_header'last-1), msg_header(msg_header'last-2), msg_header(msg_header'last-3));
                           msg_length := to_length (msg_header_swap);
                           declare
                              msg_buffer     : stream_element_array (1 .. ada.streams.stream_element_offset(msg_length));
                              message_tokens : msg_vector.vector;
                              server_msg     : unbounded_string;
                           begin
                              ada.streams.read (channel.all, msg_buffer, offset); -- read message
                              for i in msg_buffer'range loop
                                 append (server_msg, character'val (msg_buffer (i)));
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
      close_socket (client);
   exception
      when others =>
         ada.text_io.put_line ("<------ exception ------->");
   end;
end ib_ada.connection;

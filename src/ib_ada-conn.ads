with ib_ada.communication; use ib_ada.communication;

package ib_ada.conn is

   task type socket_connection is
      entry setup (session : session_type);
      entry disconnect;
      entry send (req : req_type; resp : out resp_type);
   end;

   client : socket_connection;

end ib_ada.conn;

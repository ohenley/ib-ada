with Ada.Containers.Vectors;
use Ada.Containers;

package ib_ada.communication.incomming is

   type msg_handler_type;

   type handling_func_type is access function (req : req_type; msg_tokens : in out msg_vector.vector; msg_handler : msg_handler_type) return resp_type;

   type msg_handler_type is record
      resp_id : resp_id_type;
      codes : code_vector.vector;
      handling_func : handling_func_type;
   end record;

   package message_map is new indefinite_hashed_maps
       (Key_Type        => unbounded_string,
        Element_Type    => msg_handler_type,
        Hash            => Ada.Strings.Unbounded.Hash,
        Equivalent_Keys => "=");

   msg_definitions : message_map.map;

   function handle_message (req : req_type; msg : Unbounded_String) return resp_type;

end ib_ada.communication.incomming;

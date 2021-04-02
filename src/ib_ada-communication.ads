
with Ada.Containers.Vectors; use Ada.Containers;
--with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Ada.Calendar;

package ib_ada.communication is

   package code_vector is new vectors(Index_Type => natural, element_type => integer);
   type variadic_integer_array is array(positive range <>) of integer;
   function codes (elements: variadic_integer_array) return code_vector.vector;

   package msg_vector is new vectors(Index_Type => natural, element_type => unbounded_string);

   function get_serialized_msg (msg : string) return string;

   type req_id_type is
     (handshake,
      start_api,
      positions,
      account_summary,
      pnl_single,
      pnl_cancel_single,
      place_order,
      cancel_order,
      open_orders);

   type req_type is
      record
         msg : unbounded_string := +"";
         and_listen : boolean;
         req_id : req_id_type;
      end record;

   type resp_id_type is
     (server_infos,
      managed_accounts,
      next_valid_id,
      error,
      positions,
      positions_end,
      account_summary,
      account_summary_end,
      pnl_single,
      cancel_order,
      open_order,
      order_status,
      open_orders_end);

   type resp_type is
      record
         and_listen : boolean;
         resp_id : resp_id_type;
      end record;

   procedure handshake;
   procedure start_api;
   procedure account_summary (tag : tag_type);
   procedure positions;
   procedure pnl (account_id : string; contract_id : integer);
   function place_order (contract : contract_type; order : order_type) return integer;
   function buy_order (symbol : string; quantity : integer; at_price_type : order_at_price_type) return integer;
   function sell_order (symbol : string; quantity : integer; at_price_type : order_at_price_type) return integer;
   procedure cancel_order (request_id : integer);
   procedure open_orders;

end ib_ada.communication;



   --  package msg_queue_vector is new vectors(Index_Type => natural, element_type => message_type);
   --
   --  protected msg_queue_monitor is
   --     procedure add_message_to_queue (msg : message_type);
   --     procedure consume_message_from_queue (msg : in out message_type);
   --  private
   --     msg_queue : msg_queue_vector.vector;
   --  end;





--  type request_type is tagged
--        record
--           date_time : Ada.calendar.time;
--        end record;
--
--     type pnl_single_request is new request_type with
--        record
--           account_id : unbounded_string;
--        end record;
--
--     --type request_access is access request_type'class;
--
--     function integer_hash (key : integer) return hash_type is
--       (Ada.Containers.Hash_Type (key));
--
--     package pnl_single_request_map is new indefinite_hashed_maps
--       (Key_Type        => integer,
--        Element_Type    => pnl_single_request,
--        Hash            => integer_hash,
--        Equivalent_Keys => "=");
--
--     protected requests is
--        procedure cache_request (req_id : integer; req : pnl_single_request);
--        procedure consume_request (req_id : integer; req : in out pnl_single_request);
--     private
--        pnl_single_requests : pnl_single_request_map.map;
--     end;





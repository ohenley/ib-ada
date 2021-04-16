
with Ada.Containers.Vectors;
with Ada.Calendar;

use Ada.Containers;

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
      account_summary_cancel,
      pnl_single,
      pnl_cancel_single,
      place_order,
      fake_order,
      cancel_order,
      open_orders,
      market_data,
      undefined);

   type req_type is
      record
         request_number : integer := -1;
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
      fake_order,
      open_orders_end,
      undefined);

   type resp_type is
      record
         and_listen : boolean;
         resp_id : resp_id_type;
      end record;


   type cached_request_type is abstract tagged null record;

   type pnl_cached_request_type is new cached_request_type with
      record
         account_id : unbounded_string;
         contract_id : integer;
      end record;

   type commission_cached_request_type is new cached_request_type with
      record
         commission : safe_float;
      end record;

   package cached_request_map is new indefinite_hashed_maps
     (Key_Type        => string,
      Element_Type    => cached_request_type'class,
      Hash            => Ada.strings.hash,
      Equivalent_Keys => "=");

   protected cached_requests is
      procedure cache_request (req_id : integer; req : cached_request_type'class);
      procedure consume_request (req_id : integer; req : in out cached_request_type'class);

      function length return count_type;
   private
      cached_requests : cached_request_map.map;
   end;

   procedure handshake;
   procedure start_api;
   procedure accounts_summary (tag : tag_type);
   procedure positions;
   procedure pnl (account_id : string; contract_id : integer);
   procedure pnls;
   function place_order (contract : contract_type; order : order_type) return integer;
   function place_order (side: order_side_type; symbol : string; quantity : integer; at_price_type : order_at_price_type) return integer;
   function place_fake_order (side: order_side_type; symbol : string; quantity : integer; at_price_type : order_at_price_type) return integer;
   procedure cancel_order (request_number : integer);
   procedure open_orders;


   procedure market_data (symbol : string; contract_id : integer);
   function get_commission (request_number : integer) return safe_float;

end ib_ada.communication;



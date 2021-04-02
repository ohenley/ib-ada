package ib_ada.communication.outgoing is

   protected unique_id is
      function get_unique_id (next_valid_id : integer) return integer;
   end;

   function build_handshake_msg return string;
   function build_start_api_msg return string;
   function build_positions_msg return string;
   function build_account_summary_msg (account_tag : tag_type) return string;
   function build_pnl_msg (request_id : integer; account : string; contract_id : integer) return string;
   function build_cancel_pnl_msg (request_id : integer) return string;
   function build_place_order_msg (request_id : integer; contract : contract_type; order : order_type) return string;
   function build_cancel_order_msg (request_id : integer) return string;
   function build_open_orders_msg return string;

end ib_ada.communication.outgoing;

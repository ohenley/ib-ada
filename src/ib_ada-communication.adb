with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Calendar;

with Ada.text_io;
with Ada.Strings.Fixed;

with ib_ada.communication.outgoing;
with ib_ada.connection;

use Ada.Containers;
use Ada.Text_IO;
use Ada.Strings.Fixed;


package body ib_ada.communication is


   protected body cached_requests is
      procedure cache_request (req_id : integer; req : cached_request_type'class) is
         req_id_str : string := trim(req_id'image, Ada.Strings.Left);
      begin
         cached_requests.include (req_id_str, req);
      end;

      procedure consume_request (req_id : integer; req : in out cached_request_type'class) is
         req_id_str : string := trim(req_id'image, Ada.Strings.Left);
      begin
         if cached_requests.contains (req_id_str) then
            req := cached_requests.element (req_id_str);
         end if;
         cached_requests.exclude (req_id_str);
      end;

      function length return count_type is
      begin
         return cached_requests.length;
      end;
   end;

   function codes (elements: variadic_integer_array) return code_vector.vector is
      codes : code_vector.vector;
   begin
      for i in elements'range loop
         codes.append(elements(i));
      end loop;
      return codes;
   end;

   function get_serialized_msg (msg : string) return string is
      header : string(1..4);
   begin
      for i in header'first .. header'last-1 loop
         header(i) := ascii.nul;
      end loop;
      header(header'last) := character'val(msg'length);
      return header & msg;
   end;

   procedure clear_accounts_content is
      account_id : unbounded_string;
   begin
      for account in accounts.iterate loop
         account_id := account_map.key(account);
         accounts(account_id).positions.clear;
         accounts(account_id).open_orders.clear;
         accounts(account_id).summaries.clear;
      end loop;
   end;

   procedure handshake is
      use ib_ada.communication.outgoing;
      handshake_msg : req_type := (-1, +build_handshake_msg, true, handshake);
      resp : resp_type;
   begin
      ib_ada.connection.client.send (handshake_msg, resp);
   end;

   procedure start_api is
      use ib_ada.communication.outgoing;
      start_api_msg : req_type := (-1, +build_start_api_msg, true, start_api);
      resp : resp_type;
   begin
      ib_ada.connection.client.send (start_api_msg, resp);
   end;

   procedure accounts_summary (tag : tag_type) is
      use ib_ada.communication.outgoing;
      request_number : integer := unique_id.get_unique_id (next_valid_request_id);
      account_summary_msg : req_type := (request_number , +build_accounts_summary_msg (request_number , tag), true, account_summary);
      cancel_account_summary_msg : req_type := (request_number, +build_cancel_accounts_summary_msg (request_number), false, account_summary_cancel);
      resp : resp_type;
   begin
      clear_accounts_content;
      ib_ada.connection.client.send (account_summary_msg, resp);
      ib_ada.connection.client.send (cancel_account_summary_msg, resp);
   end;

   procedure positions is
      use ib_ada.communication.outgoing;
      positions_msg : req_type := (-1, +build_positions_msg, true, positions);
      resp : resp_type;
   begin
      clear_accounts_content;
      ib_ada.connection.client.send (positions_msg, resp);
   end;

   procedure pnl (account_id : string; contract_id : integer) is
      use ib_ada.communication.outgoing;
      request_number : integer := unique_id.get_unique_id (next_valid_request_id);
      pnl_msg : req_type := (request_number, +build_pnl_msg (request_number, account_id, contract_id), true, pnl_single);
      cancel_pnl : req_type := (request_number, +build_cancel_pnl_msg (request_number), false, pnl_cancel_single);
      resp : resp_type;
      cache_request : pnl_cached_request_type;
   begin
      cache_request.account_id := +account_id;
      cache_request.contract_id := contract_id;
      cached_requests.cache_request (request_number, cache_request);

      ib_ada.connection.client.send (pnl_msg, resp);

      if resp.resp_id /= error then
         ib_ada.connection.client.send (cancel_pnl, resp);
      end if;
   end;

   procedure pnls is
      use ib_ada.communication.outgoing;

      type pnl_argument is
         record
            account_id : unbounded_string;
            contract_id : integer;
         end record;

      package pnl_argument_vector is new vectors (natural, pnl_argument);
      pnl_arguments : pnl_argument_vector.vector;
      account_id : unbounded_string;
      contract_id : integer;
   begin
      for account in accounts.iterate loop
         account_id := account_map.key(account);
         for pos in accounts(account_id).positions.iterate loop
            contract_id := position_map.element(pos).contract.contract_id;
            pnl_arguments.append ((account_id, contract_id));
         end loop;
      end loop;

      for argument of pnl_arguments loop
         pnl(+argument.account_id, argument.contract_id);
      end loop;
   end;


   function place_order (contract : contract_type; order : order_type) return integer is
      use ib_ada.communication.outgoing;
      request_number : integer := unique_id.get_unique_id (next_valid_request_id);
      place_order_msg : req_type;
      req_id : req_id_type;
      resp : resp_type;
   begin
      if order.what_if then
         req_id := fake_order;
      else
         req_id := place_order;
      end if;
      place_order_msg := (request_number, +build_place_order_msg (request_number, contract, order), true, req_id);
      ib_ada.connection.client.send (place_order_msg, resp);
      return request_number;
   end;

   function place_order (side: order_side_type; symbol : string; quantity : integer; at_price_type : order_at_price_type) return integer is
      contract : ib_ada.contract_type := ib_ada.prepare_contract (symbol   => symbol,
                                                                  security => ib_ada.STK,
                                                                  currency => ib_ada.USD,
                                                                  exchange => ib_ada.SMART);
      order : ib_ada.order_type := ib_ada.prepare_order (side           => side,
                                                         quantity       => quantity,
                                                         at_price_type  => at_price_type);
      request_number : integer := ib_ada.communication.place_order (contract, order);
   begin
      return request_number;
   end;

   function place_fake_order (side: order_side_type; symbol : string; quantity : integer; at_price_type : order_at_price_type) return integer is
      contract : ib_ada.contract_type := ib_ada.prepare_contract (symbol   => symbol,
                                                                  security => ib_ada.STK,
                                                                  currency => ib_ada.USD,
                                                                  exchange => ib_ada.SMART);
      order : ib_ada.order_type := ib_ada.prepare_order (side           => side,
                                                         quantity       => quantity,
                                                         at_price_type  => at_price_type,
                                                         what_if        => true);
      request_number : integer := ib_ada.communication.place_order (contract, order);
   begin
      return request_number;
   end;

   procedure cancel_order (request_number : integer) is
      use ib_ada.communication.outgoing;
      cancel_order_msg : req_type := (request_number, +build_cancel_order_msg (request_number), true, cancel_order);
      resp : resp_type;
   begin
      ib_ada.connection.client.send (cancel_order_msg, resp);
   end;

   procedure open_orders is
      use ib_ada.communication.outgoing;
      open_orders_msg : req_type := (-1, +build_open_orders_msg, true, open_orders);
      resp : resp_type;
   begin
      clear_accounts_content;
      ib_ada.connection.client.send (open_orders_msg, resp);
   end;

   procedure market_data (symbol : string; contract_id : integer) is
      use ib_ada.communication.outgoing;
      request_number : integer := unique_id.get_unique_id (next_valid_request_id);
      contract : contract_type;
      resp : resp_type;
   begin
      contract.contract_id := contract_id;
      contract.symbol := +symbol;
      contract.security := STK;
      contract.exchange := SMART;
      contract.currency := USD;
      declare
         market_data_msg : req_type := (request_number, +build_market_data_msg (request_number, contract), true, market_data);
      begin
         ib_ada.connection.client.send (market_data_msg, resp);
      end;

   end;

   function get_commission (request_number : integer) return safe_float is
      cache_request : commission_cached_request_type;
   begin
      cached_requests.consume_request (request_number, cache_request);
      return cache_request.commission;
   end;

end ib_ada.communication;

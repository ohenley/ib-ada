with Ada.Containers.Indefinite_Hashed_Maps; use Ada.Containers;
with Ada.Calendar;

with Ada.text_io; use Ada.Text_IO;

with ib_ada.communication.outgoing;
with ib_ada.conn;

package body ib_ada.communication is

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

   procedure handshake is
      use ib_ada.communication.outgoing;
      handshake_msg : req_type := (+build_handshake_msg, true, handshake);
      resp : resp_type;
   begin
      ib_ada.conn.client.send (handshake_msg, resp);
   end;

   procedure start_api is
      use ib_ada.communication.outgoing;
      start_api_msg : req_type := (+build_start_api_msg, true, start_api);
      resp : resp_type;
   begin
      ib_ada.conn.client.send (start_api_msg, resp);
   end;

   procedure account_summary (tag : tag_type) is
      use ib_ada.communication.outgoing;
      account_summary_msg : req_type := (+build_account_summary_msg (tag), true, account_summary);
      resp : resp_type;
   begin
      ib_ada.conn.client.send (account_summary_msg, resp);
   end;

   procedure positions is
      use ib_ada.communication.outgoing;
      positions_msg : req_type := (+build_positions_msg, true, positions);
      resp : resp_type;
   begin
      ib_ada.conn.client.send (positions_msg, resp);
   end;

   procedure pnl (account_id : string; contract_id : integer) is
      use ib_ada.communication.outgoing;
      req_id : integer := unique_id.get_unique_id (next_valid_request_id);
      pnl_msg : req_type := (+build_pnl_msg (req_id, account_id, contract_id), true, pnl_single);
      cancel_pnl : req_type := (+build_cancel_pnl_msg (req_id), false, pnl_cancel_single);
      resp : resp_type;
   begin
      ib_ada.conn.client.send (pnl_msg, resp);

      if resp.resp_id /= error then
         ib_ada.conn.client.send (cancel_pnl, resp);
      end if;
   end;

   function place_order (contract : contract_type; order : order_type) return integer is
      use ib_ada.communication.outgoing;
      req_id : integer := unique_id.get_unique_id (next_valid_request_id);
      place_order_msg : req_type := (+build_place_order_msg (req_id, contract, order), true, place_order);
      resp : resp_type;
   begin
      ib_ada.conn.client.send (place_order_msg, resp);
      return req_id;
   end;

   function buy_order (symbol : string; quantity : integer; at_price_type : order_at_price_type) return integer is
      contract : ib_ada.contract_type := ib_ada.prepare_contract (symbol   => symbol,
                                                                  security => ib_ada.STK,
                                                                  currency => ib_ada.USD,
                                                                  exchange => ib_ada.SMART);
      order : ib_ada.order_type := ib_ada.prepare_order (action      => ib_ada.BUY,
                                                         quantity    => quantity,
                                                         at_price_type  => at_price_type);
      req_id : integer := ib_ada.communication.place_order (contract, order);
   begin
      return req_id;
   end;

   function sell_order (symbol : string; quantity : integer; at_price_type : order_at_price_type) return integer is
      contract : ib_ada.contract_type := ib_ada.prepare_contract (symbol   => symbol,
                                                                  security => ib_ada.STK,
                                                                  currency => ib_ada.USD,
                                                                  exchange => ib_ada.SMART);
      order : ib_ada.order_type := ib_ada.prepare_order (action      => ib_ada.SELL,
                                                         quantity    => quantity,
                                                         at_price_type  => at_price_type);
      req_id : integer := ib_ada.communication.place_order (contract, order);
   begin
      return req_id;
   end;

   procedure cancel_order (request_id : integer) is
      use ib_ada.communication.outgoing;
      cancel_order_msg : req_type := (+build_cancel_order_msg (request_id), true, cancel_order);
      resp : resp_type;
   begin
      ib_ada.conn.client.send (cancel_order_msg, resp);
   end;

   procedure open_orders is
      use ib_ada.communication.outgoing;
      open_orders_msg : req_type := (+build_open_orders_msg, true, open_orders);
      resp : resp_type;
   begin
      ib_ada.conn.client.send (open_orders_msg, resp);
   end;



end ib_ada.communication;

   --  protected body requests is
   --     procedure cache_request (req_id : integer; req : pnl_single_request) is
   --     begin
   --        pnl_single_requests.include (req_id, req);
   --     end;
   --     procedure consume_request (req_id : integer; req : in out pnl_single_request) is
   --     begin
   --        if pnl_single_requests.contains (req_id) then
   --           req := pnl_single_requests.element (req_id);
   --           pnl_single_requests.delete (req_id);
   --        end if;
   --     end;
   --  end;
   --
   --
   --  protected body msg_queue_monitor is
   --     procedure add_message_to_queue (msg : message_type) is
   --     begin
   --        msg_queue.prepend (msg);
   --     end;
   --
   --     procedure consume_message_from_queue (msg : in out message_type) is
   --     begin
   --        if msg_queue.Length > 0 then
   --           msg := msg_queue.last_element;
   --           msg_queue.delete_last;
   --        end if;
   --     end;
   --  end;

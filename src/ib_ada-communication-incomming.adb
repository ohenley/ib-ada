with Ada.Text_IO; use Ada.Text_IO;
With Ada.Text_IO.Unbounded_IO;
with Ada.Characters.Handling, Ada.Text_IO;
use  Ada.Characters.Handling, Ada.Text_IO;

with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with ib_ada.communication.outgoing; use ib_ada.communication.outgoing;


package body ib_ada.communication.incomming is
   package suio renames Ada.Text_IO.Unbounded_IO;

   procedure print_msg_tokens (msg_tokens : msg_vector.vector) is
   begin
      for e of msg_tokens loop
         --suio.Put ("*" & e);
         suio.Put (" " & e);
      end loop;
      new_line;
   end;

   procedure filter_codes (msg_tokens : in out msg_vector.vector; codes : code_vector.vector) is
      i : msg_vector.extended_index;
   begin
      for c of codes loop
         declare
            code_image : string := trim(c'image, Ada.Strings.Left);
         begin
            i := msg_tokens.find_index (+code_image);
            if i /= msg_vector.no_index then
               msg_tokens.delete (i);
            end if;
         end;
      end loop;
   end;

   function handle_session_datetime_msg (req : req_type; msg_tokens : in out msg_vector.vector; msg_handler : msg_handler_type) return resp_type is
      resp : resp_type;
   begin
      server_version := integer'value(+msg_tokens.first_element);
      resp.and_listen := false;
      resp.resp_id := server_infos;
      return resp;
   end;

   function handle_managed_accounts_msg (req : req_type; msg_tokens : in out msg_vector.vector; msg_handler : msg_handler_type) return resp_type is
      new_account : act_type;
      resp : resp_type;
   begin
      resp.and_listen := true;
      resp.resp_id := managed_accounts;
      for e of msg_tokens loop
         ib_ada.accounts.include (e, new_account);
      end loop;
      return resp;
   end;

   function handle_next_valid_id_msg (req : req_type; msg_tokens : in out msg_vector.vector; msg_handler : msg_handler_type) return resp_type is
      resp : resp_type;
   begin
      resp.and_listen := true;
      resp.resp_id := next_valid_id;
      ib_ada.next_valid_request_id := integer'value(+msg_tokens.first_element);
      return resp;
   end;

   function handle_error_msg (req : req_type; msg_tokens : in out msg_vector.vector; msg_handler : msg_handler_type) return resp_type is
      resp : resp_type;
   begin
      if req.req_id = start_api then
         if msg_tokens.first_element = "2104" or msg_tokens.first_element = "2106" then
            resp.and_listen := true;
         end if;
      elsif req.req_id = pnl_single then
         resp.and_listen := true;
      elsif req.req_id = place_order then
         resp.and_listen := true;
      elsif req.req_id = cancel_order then
         resp.and_listen := false;
      else
         resp.and_listen := false;
      end if;

      resp.resp_id := error;

      return resp;
   end;

   function handle_position_msg (req : req_type; msg_tokens : in out msg_vector.vector; msg_handler : msg_handler_type) return resp_type is
      account : unbounded_string := msg_tokens.first_element;
      position : position_type;
      contract : contract_type;
      resp : resp_type;
   begin
      resp.and_listen := true;
      resp.resp_id := positions;
      contract.contract_id := integer'value(+msg_tokens (msg_tokens.first_index + 1));
      contract.symbol := msg_tokens (msg_tokens.first_index + 2);
      contract.security := security_type'value (+msg_tokens (msg_tokens.first_index + 3));
      contract.exchange := exchange_type'value (+msg_tokens (msg_tokens.first_index + 5));
      contract.currency := currency_type'value (+msg_tokens (msg_tokens.first_index + 6));
      position.contract := contract;
      position.number := integer'value(+msg_tokens (msg_tokens.first_index + 9));
      position.average_cost := float'value(+msg_tokens (msg_tokens.first_index + 10));
      ib_ada.accounts(account).positions.include (contract.symbol, position);

      return resp;
   end;

   function handle_position_end_msg (req : req_type; msg_tokens : in out msg_vector.vector; msg_handler : msg_handler_type) return resp_type is
      resp : resp_type;
   begin
      resp.and_listen := false;
      resp.resp_id := positions_end;
      return resp;
   end;

   function handle_account_summary_msg (req : req_type; msg_tokens : in out msg_vector.vector; msg_handler : msg_handler_type) return resp_type is
      resp : resp_type;
   begin
      resp.and_listen := true;
      resp.resp_id := account_summary;
      return resp;
   end;

   function handle_account_summary_end_msg (req : req_type; msg_tokens : in out msg_vector.vector; msg_handler : msg_handler_type) return resp_type is
      resp : resp_type;
   begin
      resp.and_listen := false;
      resp.resp_id := account_summary_end;
      return resp;
   end;

   function handle_pnl_single_msg (req : req_type; msg_tokens : in out msg_vector.vector; msg_handler : msg_handler_type) return resp_type is

      function get_safe_float (token : string) return safe_float is
         value : float := float'value(token);
      begin
         if value'valid then
            return value;
         end if;
         return safe_float'last;
      end;

      resp : resp_type;
      request_id : integer := integer'value(+msg_tokens (msg_tokens.first_index));
      number : integer := integer'value(+msg_tokens (msg_tokens.first_index + 1));

      pnl_daily : safe_float := get_safe_float(+msg_tokens (msg_tokens.first_index + 2));
      pnl_unrealized : safe_float := get_safe_float(+msg_tokens (msg_tokens.first_index + 3));
      pnl_realized : safe_float := get_safe_float(+msg_tokens (msg_tokens.first_index + 4));
      value : safe_float := get_safe_float(+msg_tokens (msg_tokens.first_index + 5));

   begin
      resp.and_listen := false;
      resp.resp_id := pnl_single;
      return resp;
   end;

   function handle_cancel_order_msg (req : req_type; msg_tokens : in out msg_vector.vector; msg_handler : msg_handler_type) return resp_type is
      resp : resp_type;
   begin
      resp.and_listen := false;
      resp.resp_id := cancel_order;
      return resp;
   end;

   function handle_open_order_msg (req : req_type; msg_tokens : in out msg_vector.vector; msg_handler : msg_handler_type) return resp_type is
      resp : resp_type;
   begin
      resp.and_listen := true;
      resp.resp_id := open_order;
      return resp;
   end;

   function handle_order_status_msg (req : req_type; msg_tokens : in out msg_vector.vector; msg_handler : msg_handler_type) return resp_type is
      resp : resp_type;
   begin
      if req.req_id = place_order then
         resp.and_listen := false;
      else
         resp.and_listen := true;
      end if;
      resp.resp_id := order_status;
      return resp;
   end;

   function handle_open_orders_end_msg (req : req_type; msg_tokens : in out msg_vector.vector; msg_handler : msg_handler_type) return resp_type is
      resp : resp_type;
   begin
      if req.req_id = start_api then
         resp.and_listen := true;
      else
         resp.and_listen := false;
      end if;
      resp.resp_id := open_orders_end;
      return resp;
   end;


   function parse_message (msg : Unbounded_String) return msg_vector.vector is
      message_tokens : msg_vector.vector;
      char : character := element (msg, 1);
   begin
      message_tokens.append(null_unbounded_string);
      for i in 1.. length(msg) loop
         char := element (msg, i);
         if char = ascii.nul then
            message_tokens.append(null_unbounded_string);
         else
            append(message_tokens(message_tokens.last_index), char);
         end if;
      end loop;

      declare
         i : msg_vector.extended_index;
      begin
         loop
            i := message_tokens.find_index (null_unbounded_string);
            exit when i = msg_vector.no_index;
            message_tokens.delete (i);
         end loop;
      end;
      return message_tokens;
   end;

   function handle_message (req : req_type; msg : Unbounded_String) return resp_type is
      message_tokens : msg_vector.vector := parse_message (msg);
      raw_message : string := +msg;
      resp : resp_type;
   begin
      -- patching ill formed protocol
      if req.req_id = handshake then
         message_tokens.prepend(+"0");
      end if;

      if msg_definitions.contains (message_tokens.first_element) then
         declare
            msg_handler : msg_handler_type := msg_definitions (message_tokens.first_element);
            msg_id_str : string := "<" & msg_handler.resp_id'image & ">";
         begin
            put (to_lower(msg_id_str));
            filter_codes (message_tokens, msg_handler.codes);
            print_msg_tokens (message_tokens);
            resp := msg_handler.handling_func.all (req, message_tokens, msg_handler);
         end;
      end if;
      return resp;
   end;

begin

   msg_definitions.include (+"0", (server_infos, codes((1 => 0)), handle_session_datetime_msg'access));
   msg_definitions.include (+"15", (managed_accounts, codes((1 => 15, 2 => 1)), handle_managed_accounts_msg'access));
   msg_definitions.include (+"9", (next_valid_id, codes((1 => 9, 2 => 1)), handle_next_valid_id_msg'access));
   msg_definitions.include (+"4", (error, codes((1 => 4, 2 => 2, 3 => -1)), handle_error_msg'access));
   msg_definitions.include (+"61", (positions, codes((1 => 61,  2 => 3)), handle_position_msg'access));
   msg_definitions.include (+"62", (positions_end, codes((1 => 62,  2 => 1)), handle_position_end_msg'access));
   msg_definitions.include (+"63", (account_summary, codes((1 => 63, 2 => 1)), handle_account_summary_msg'access));
   msg_definitions.include (+"64", (account_summary_end, codes((1 => 64, 2 => 1)), handle_account_summary_end_msg'access));
   msg_definitions.include (+"95", (pnl_single, codes((1 => 95)), handle_pnl_single_msg'access));
   msg_definitions.include (+"5", (open_order, codes((1 => 5)), handle_open_order_msg'access));
   msg_definitions.include (+"3", (order_status, codes((1 => 3)), handle_order_status_msg'access));
   msg_definitions.include (+"53", (open_orders_end, codes((1 => 53)), handle_open_orders_end_msg'access));

end ib_ada.communication.incomming;

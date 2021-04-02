--with ib_ada.connection;
--with ib_ada.conn;

with Ada.Direct_IO;
with Ada.Directories;

with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;

package body ib_ada.communication.outgoing is

   function "-" (field : string) return String is
   begin
      return field & ascii.nul;
   end;

   function "-" (field: unbounded_string) return string is
      str : string := +field;
   begin
      return -str;
   end;

   function "-" (field: integer) return string is
   begin
      if field = integer'last then
         return -"";
      else
         return -trim(field'image, Ada.Strings.Left);
      end if;
   end;

   function "-" (field: safe_float) return string is
   begin
      if field = safe_float'last then
         return -"";
      else
         return -trim(field'image, Ada.Strings.Left);
      end if;
   end;

   function "-" (field: boolean) return string is
   begin
      if field = true then
         return -"1";
      else
         return -"0";
      end if;
   end;

   function "-" (field: security_type) return string is
   begin
      if field = UNDEFINED then
         return -"";
      else
         return -field'image;
      end if;
   end;

   function "-" (field: security_identifier_type) return string is
   begin
      if field = UNDEFINED then
         return -"";
      else
         return -field'image;
      end if;
   end;

   function "-" (field: right_type) return string is
   begin
      if field = UNDEFINED then
         return -"";
      else
         return -field'image;
      end if;
   end;

   function "-" (field: multiplier_type) return string is
   begin
      if field = UNDEFINED then
         return -"";
      else
         return -field'image;
      end if;
   end;

   function "-" (field: exchange_type) return string is
   begin
      if field = UNDEFINED then
         return -"";
      else
         return -field'image;
      end if;
   end;

   function "-" (field: currency_type) return string is
   begin
      if field = UNDEFINED then
         return -"";
      else
         return -field'image;
      end if;
   end;

   function "-" (field: trading_class_type) return string is
   begin
      if field = UNDEFINED then
         return -"";
      else
         return -field'image;
      end if;
   end;

   function "-" (field: order_action_type) return string is
   begin
      if field = UNDEFINED then
         return -"";
      else
         return -field'image;
      end if;
   end;

   function "-" (field: order_at_price_type) return string is
   begin
      if field = UNDEFINED then
         return -"";
      else
         return -field'image;
      end if;
   end;

   function "-" (field: time_in_force_type) return string is
   begin
      if field = UNDEFINED then
         return -"";
      else
         return -field'image;
      end if;
   end;

   function "-" (field: open_close_type) return string is
   begin
      if field = UNDEFINED then
         return -"";
      else
         return -field'image;
      end if;
   end;

   function "-" (field: rule_80a_type) return string is
   begin
      if field = UNDEFINED then
         return -"";
      else
         return -field'image;
      end if;
   end;

   function "-" (field: hedge_value_type) return string is
   begin
      if field = UNDEFINED then
         return -"";
      else
         return -field'image;
      end if;
   end;

   function "-" (field: clearing_intent_type) return string is
   begin
      if field = UNDEFINED then
         return -"";
      else
         return -field'image;
      end if;
   end;

   function "-" (field: algo_strategy_type) return string is
   begin
      if field = UNDEFINED then
         return -"";
      else
         return -field'image;
      end if;
   end;

   package integer_io is new Ada.direct_io (integer);

   protected body unique_id is
      function get_unique_id (next_valid_id : integer) return integer is
         use integer_io;
         use ada.directories;
         i : integer_io.File_Type;
         file_name : constant String := "unique_id.bin";
         id : integer := 0;

         procedure create_file is
         begin
            create (i, out_file, file_name);
            write (i, 1);
            close (i);
         end;

         function get_next_id return integer is
            db_id : integer;
         begin
            open (i, in_file, file_name);
            while not end_of_file (i) loop
               read (i, db_id);
            end loop;
            close (i);

            db_id := db_id + 1;

            if db_id >= next_valid_id then
               return db_id;
            end if;

            return next_valid_id;
         end;

         procedure persist_id (db_id : integer) is
         begin
            open (i, out_file, file_name);
            write (i, db_id);
            close (i);
         end;

      begin
         if not exists (file_name) then
            create_file;
         end if;
         id := get_next_id;
         persist_id (id);
         return id;
      end;
   end;


   function build_handshake_msg return string is
      v100_prefix : string := -"API";
      min_version : string := "142";
      max_version : string := "152";
      v100_version : string := "v" & min_version & ".." & max_version;
      handshake_msg : string := v100_prefix & get_serialized_msg (v100_version);
   begin
      return handshake_msg;
   end;

   function build_start_api_msg return string is
      start_api : string := -"71";
      version : string := -"2";
      client_id : string := -"1";
      opt_capabilities : string := -"";
      start_api_msg : string := get_serialized_msg (start_api & version & client_id & opt_capabilities);
   begin
      return start_api_msg;
   end;

   function build_positions_msg return string is
      request_positions : string := -"61";
      version : string := -"1";
      positions_msg : string := get_serialized_msg (request_positions & version);
   begin
      return positions_msg;
   end;

   function build_account_summary_msg (account_tag : tag_type) return string is
      request_account_summary : string := -"62";
      code_1 : string := -"1";
      request_id : string := -unique_id.get_unique_id (next_valid_request_id);
      group : string := -"All";
      tag : string := -tag_image (account_tag);
      account_summary_msg : string := get_serialized_msg (request_account_summary & code_1 & request_id & group & tag);
   begin
      return account_summary_msg;
   end;


   function build_pnl_msg (request_id : integer; account : string; contract_id : integer) return string is
      request_pnl : string := -"94";
      req_id : string := -request_id;
      acnt : string := -account;
      model_code : string := -"";
      conid : string := -contract_id;
      pnl_msg : string := get_serialized_msg (request_pnl & req_id & acnt & model_code & conid);
   begin
      return pnl_msg;
   end;

   function build_cancel_pnl_msg (request_id : integer) return string is
      request_cancel_pnl : string := -"95";
      req_id : string := -request_id;
      cancel_pnl_msg : string := get_serialized_msg (request_cancel_pnl & req_id);
   begin
      return cancel_pnl_msg;
   end;

   function build_place_order_msg (request_id : integer; contract : contract_type; order : order_type) return string is
      -- note to myself: this is a form of madness.
      request_place_order : string := -"3";
      req_id : string := -request_id;

      contract_id : string := -contract.contract_id;
      symbol : string := -contract.symbol;
      security : string := -contract.security;
      last_trade_date_or_contract_month : string := -contract.last_trade_date_or_contract_month;
      strike : string := -contract.strike;
      right : string := -contract.right;
      multiplier : string := -contract.multiplier;
      exchange : string := -contract.exchange;
      primary_exchange : string := -contract.primary_exchange;
      currency : string := -contract.currency;
      local_symbol : string := -contract.local_symbol;
      trading_class : string := -contract.trading_class;
      security_id_type : string := -contract.security_id_type;
      security_id : string := -contract.security_id;

      action : string := -order.action;
      quantity : string := -order.quantity;
      at_price_type : string := -order.at_price_type;
      limit_price : string := -order.limit_price;
      aux_price : string := -order.aux_price;
      time_in_force : string := -order.time_in_force;
      one_cancel_all_group_identifier : string := -order.one_cancel_all_group_identifier;
      account : string := -order.account;
      open_close : string := -order.open_close;
      origin : string := -order.origin;
      order_ref : string := -order.order_ref;
      transmit : string := -order.transmit;
      parent_id : string := -order.parent_id;
      block_order : string := -order.block_order;
      sweep_to_fill : string := -order.sweep_to_fill;
      display_size : string := -order.display_size;
      trigger_method : string := -order.trigger_method;
      outside_regular_trading_hours : string := -order.outside_regular_trading_hours;
      hidden : string := -order.hidden;

      total_madness_filler : string := -"";

      discretionary_amount : string := -order.discretionary_amount;
      good_after_time : string := -order.good_after_time;
      good_till_date : string := -order.good_till_date;
      financial_advisor_group : string := -order.financial_advisor_group;
      financial_advisor_method : string := -order.financial_advisor_method;
      financial_advisor_percentage : string := -order.financial_advisor_percentage;
      financial_advisor_profile : string := -order.financial_advisor_profile;
      model_code : string := -order.model_code;
      short_sale_slot : string := -order.short_sale_slot;
      designated_location : string := -order.designated_location;
      exempt_code : string := -order.exempt_code;
      one_cancel_all_type : string := -order.one_cancel_all_type;
      rule_80a : string := -order.rule_80a;
      settling_firm : string := -order.settling_firm;
      all_or_none : string := -order.all_or_none;
      min_quantity : string := -order.min_quantity;
      percent_offset : string := -order.percent_offset;
      etrade_only : string := -order.etrade_only;
      firm_quote_only : string := -order.firm_quote_only;
      national_best_bid_offer_price_cap : string := -order.national_best_bid_offer_price_cap;
      auction_strategy : string := -order.auction_strategy;
      starting_price : string := -order.starting_price;
      stock_reference_price : string := -order.stock_reference_price;
      order_delta : string := -order.order_delta;
      stock_range_lower : string := -order.stock_range_lower;
      stock_range_upper : string := -order.stock_range_upper;
      override_percentage_constraints : string := -order.override_percentage_constraints;
      volatility : string := -order.volatility;
      volatility_type : string := -order.volatility_type;
      delta_neutral_order_type : string := -order.delta_neutral_order_type;
      delta_neutral_aux_price : string := -order.delta_neutral_aux_price;

      continuous_update : string := -order.continuous_update;
      reference_price_type : string := -order.reference_price_type;
      trail_stop_price : string := -order.trail_stop_price;
      trailing_percent : string := -order.trailing_percent;
      scale_init_level_size : string := -order.scale_init_level_size;
      scale_subs_level_size : string := -order.scale_subs_level_size;
      scale_price_increment : string := -order.scale_price_increment;

      scale_table : string := -order.scale_table;
      active_start_time : string := -order.active_start_time;
      active_stop_time : string := -order.active_stop_time;
      hedge_type : string := -order.hedge_type;

      opt_out_smart_routing : string := -order.opt_out_smart_routing;
      clearing_account : string := -order.clearing_account;
      clearing_intent: string := -order.clearing_intent;
      not_held : string := -order.not_held;

      delta_neutral_contract : string := -contract.delta_neutral_contract;

      algo_strategy : string := -order.algo_strategy;

      algo_id : string := -order.algo_id;
      what_if : string := -order.what_if;
      order_misc_options : string := -order.order_misc_options;
      solicited : string := -order.solicited;
      randomize_size : string := -order.randomize_size;
      randomize_price : string := -order.randomize_price;

      conditions : string := -order.conditions;

      adjusted_order_type : string := -order.adjusted_order_type;
      trigger_price : string := -order.trigger_price;
      limit_price_offset : string := -order.limit_price_offset;
      adjusted_stop_price : string := -order.adjusted_stop_price;
      adjusted_stop_limit_price : string := -order.adjusted_stop_limit_price;
      adjusted_trailing_amount : string := -order.adjusted_trailing_amount;
      adjustable_trailing_unit : string := -order.adjustable_trailing_unit;
      ext_operator : string := -order.ext_operator;
      soft_dollar_tier_name : string := -order.soft_dollar_tier_name;
      soft_dollar_tier_val : string := -order.soft_dollar_tier_val;
      cash_quantity : string := -order.cash_quantity;

      mifid_to_decision_maker : string := -order.mifid_to_decision_maker;
      mifid_to_decision_algo : string := -order.mifid_to_decision_algo;
      mifid_to_execution_trader : string := -order.mifid_to_execution_trader;
      mifid_to_execution_algo : string := -order.mifid_to_execution_algo;
      dont_use_auto_price_for_hedge : string := -order.dont_use_auto_price_for_hedge;
      is_oms_container : string := -order.is_oms_container;
      discretionary_up_to_limit_price : string := -order.discretionary_up_to_limit_price;
      use_price_management_algo : string := -order.use_price_management_algo;


      place_order_msg : string := get_serialized_msg (
      request_place_order &
      req_id &
      contract_id &
      symbol &
      security &
      last_trade_date_or_contract_month &
      strike &
      right &
      multiplier &
      exchange &
      primary_exchange &
      currency &
      local_symbol &
      trading_class &
      security_id_type &
      security_id &
      action &
      quantity &
      at_price_type &
      limit_price &
      aux_price &
      time_in_force &
      one_cancel_all_group_identifier &
      account &
      open_close &
      origin &
      order_ref &
      transmit &
      parent_id &
      block_order &
      sweep_to_fill &
      display_size &
      trigger_method &
      outside_regular_trading_hours &
      hidden &
      total_madness_filler &
      discretionary_amount &
      good_after_time &
      good_till_date &
      financial_advisor_group &
      financial_advisor_method &
      financial_advisor_percentage &
      financial_advisor_profile &
      model_code &
      short_sale_slot &
      designated_location &
      exempt_code &
      one_cancel_all_type &
      rule_80a &
      settling_firm &
      all_or_none &
      min_quantity &
      percent_offset &
      etrade_only &
      firm_quote_only &
      national_best_bid_offer_price_cap  &
      auction_strategy &
      starting_price &
      stock_reference_price &
      order_delta &
      stock_range_lower &
      stock_range_upper &
      override_percentage_constraints &
      volatility &
      volatility_type &
      delta_neutral_order_type &
      delta_neutral_aux_price &
      continuous_update &
      reference_price_type &
      trail_stop_price &
      trailing_percent &
      scale_init_level_size &
      scale_subs_level_size &
      scale_price_increment &
      scale_table &
      active_start_time &
      active_stop_time &
      hedge_type &
      opt_out_smart_routing &
      clearing_account &
      clearing_intent &
      not_held &
      delta_neutral_contract &
      algo_strategy &
      algo_id &
      what_if &
      order_misc_options &
      solicited &
      randomize_size &
      randomize_price &
      conditions &
      adjusted_order_type &
      trigger_price &
      limit_price_offset &
      adjusted_stop_price &
      adjusted_stop_limit_price &
      adjusted_trailing_amount &
      adjustable_trailing_unit &
      ext_operator &
      soft_dollar_tier_name &
      soft_dollar_tier_val &
      cash_quantity &
      mifid_to_decision_maker &
      mifid_to_decision_algo &
      mifid_to_execution_trader &
      mifid_to_execution_algo &
      dont_use_auto_price_for_hedge &
      is_oms_container &
      discretionary_up_to_limit_price &
      use_price_management_algo);

   begin
      return place_order_msg;
   end;

   function build_cancel_order_msg (request_id : integer) return string is
      request_cancel_order : string := -"4";
      version : string := -"2";
      req_id : string := -request_id;
      cancel_order_msg : string := get_serialized_msg (request_cancel_order & version & req_id);
   begin
      return cancel_order_msg;
   end;

   function build_open_orders_msg return string is
      request_open_orders : string := -"5";
      version : string := -"1";
      open_orders_msg : string := get_serialized_msg (request_open_orders & version);
   begin
      return open_orders_msg;
   end;




end ib_ada.communication.outgoing;

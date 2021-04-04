with GNAT.Sockets;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Ada.Containers.Indefinite_Hashed_Maps; use Ada.Containers;

with Ada.Strings.Unbounded.Hash;

with Ada.Strings.Hash;

package ib_ada is

   subtype safe_float is float range float'range;

   function "+" (value : unbounded_string) return string is
     (to_string(value));
   function "+" (value : string) return unbounded_string is
     (to_unbounded_string(value));

   type security_type is (STK, OPT, FUT, IND, FOP, CASH, BAG, WAR, BOND, CMDTY, NEWS, FUND, UNDEFINED);
   type security_identifier_type is (ISIN, CUSIP, UNDEFINED);
   type right_type is (PUT, CALL, UNDEFINED);
   type multiplier_type is (OPTIONS, FUTURES, UNDEFINED);

   -- exchange list @ https://www.interactivebrokers.ca/en/index.php?f=1562
   type exchange_type is (SMART, NASDAQ, NYSE, AMEX, IDEALPRO, ONE, GLOBEX, CBOE, UNDEFINED);
   type currency_type is (USD, CAD, UNDEFINED);
   type trading_class_type is (NMS, SCM, UNDEFINED);

   type session_type is (TWS_LIVE, TWS_PAPER, IB_LIVE, IB_PAPER, UNDEFINED);
   type session_port_type is array (session_type) of GNAT.Sockets.port_type;

   ports : session_port_type := (TWS_LIVE  => 7496,
                                 TWS_PAPER => 7497,
                                 IB_LIVE   => 4001,
                                 IB_PAPER  => 4002,
                                 UNDEFINED => 0000);

   type contract_type is record
      contract_id : integer := 0;
      symbol      : unbounded_string := +"STONK";
      security    : security_type := UNDEFINED;
      last_trade_date_or_contract_month : unbounded_string := +"";
      strike      : safe_float := 0.0;
      right       : right_type := UNDEFINED;
      multiplier  : multiplier_type := UNDEFINED;
      exchange    : exchange_type := UNDEFINED;
      primary_exchange : exchange_type := UNDEFINED;
      currency    : currency_type := UNDEFINED;
      local_symbol : unbounded_string := +"";
      trading_class : trading_class_type := UNDEFINED;
      include_expired : boolean := false;
      security_id_type : security_identifier_type := UNDEFINED;
      security_id : security_identifier_type := UNDEFINED;
      delta_neutral_contract : boolean := false;


   end record;

   type order_action_type is (BUY, SELL, UNDEFINED);
   type order_at_price_type is (MKT, MIDPRICE, UNDEFINED);

   type time_in_force_type is (DAY, GTC, IOC, GTD, OPG, FOK, DTC, UNDEFINED);
   type open_close_type is (O, C, UNDEFINED);
   type rule_80a_type is (INDIVIDUAL, AGENCY, AGENT_OTHER_MEMBER,
                          INDIVIDUAL_P_T_I_A, AGENCY_P_T_I_A, AGENT_OTHER_MEMBER_P_T_I_A,
                          INDIVIDUAK_P_T, AGENCY_P_T, AGENT_OTHER_MEMBER_P_T, UNDEFINED);
   type hedge_value_type is (D, B, F, P, UNDEFINED);
   type clearing_intent_type is (I_B, AWAY, P_T_A, UNDEFINED);
   type algo_strategy_type is (ARRIVAL_P_X, DARK_ICE, PCT_VOL, TWAP, VWAP, UNDEFINED);

   type order_type is record
      action : order_action_type := UNDEFINED;
      quantity : integer := 0;
      at_price_type : order_at_price_type := UNDEFINED;
      limit_price : safe_float := safe_float'last;
      aux_price : safe_float := safe_float'last;
      time_in_force : time_in_force_type := UNDEFINED;
      one_cancel_all_group_identifier : unbounded_string := +"";
      account : unbounded_string := +"";
      open_close : open_close_type := O;
      origin : integer := 0;
      order_ref : unbounded_string := +"";
      transmit : boolean := true;
      parent_id : integer := 0;
      block_order : boolean := false;
      sweep_to_fill : boolean := false;
      display_size : integer := 0;
      trigger_method : integer := 0;
      outside_regular_trading_hours : boolean := false;
      hidden : boolean := false;
      discretionary_amount : safe_float := 0.0;
      good_after_time : unbounded_string := +"";
      good_till_date : unbounded_string := +"";
      financial_advisor_group : unbounded_string := +"";
      financial_advisor_method : unbounded_string := +"";
      financial_advisor_percentage : safe_float := safe_float'last;
      financial_advisor_profile : unbounded_string := +"";
      model_code : unbounded_string := +"";
      short_sale_slot : integer := 0;
      designated_location : unbounded_string := +"";
      exempt_code : integer := -1;
      one_cancel_all_type : integer := 0;
      rule_80a : rule_80a_type := UNDEFINED;
      settling_firm : unbounded_string := +"";
      all_or_none : boolean := false;
      min_quantity : integer := integer'last;
      percent_offset : safe_float := safe_float'last;
      etrade_only : boolean := true;
      firm_quote_only : boolean := true;
      national_best_bid_offer_price_cap : safe_float := safe_float'last;
      auction_strategy : integer := 0;
      starting_price : safe_float := safe_float'last;
      stock_reference_price : safe_float := safe_float'last;
      order_delta : safe_float := safe_float'last;
      stock_range_lower : safe_float := safe_float'last;
      stock_range_upper : safe_float := safe_float'last;
      override_percentage_constraints : boolean := false;
      volatility : safe_float := safe_float'last;
      volatility_type : integer := integer'last;
      delta_neutral_order_type : unbounded_string := +"";
      delta_neutral_aux_price : safe_float := safe_float'last;
      continuous_update : boolean := false;
      reference_price_type : integer := integer'last;
      trail_stop_price : safe_float := safe_float'last;
      trailing_percent : safe_float := safe_float'last;
      scale_init_level_size : integer := integer'last;
      scale_subs_level_size : integer := integer'last;
      scale_price_increment : safe_float := safe_float'last;
      scale_table : unbounded_string := +"";
      active_start_time : unbounded_string := +"";
      active_stop_time : unbounded_string := +"";
      hedge_type : hedge_value_type := UNDEFINED;
      opt_out_smart_routing : boolean := false;
      clearing_account : unbounded_string := +"";
      clearing_intent: clearing_intent_type := UNDEFINED;
      not_held : boolean := false;
      algo_strategy : algo_strategy_type := UNDEFINED;
      algo_id : unbounded_string := +"";
      what_if : boolean := false;
      order_misc_options : unbounded_string := +"";
      solicited : boolean := false;
      randomize_size : boolean := false;
      randomize_price : boolean := false;
      conditions : integer := 0;
      adjusted_order_type : unbounded_string := +"";
      trigger_price : safe_float := safe_float'last;
      limit_price_offset : safe_float := safe_float'last;
      adjusted_stop_price : safe_float := safe_float'last;
      adjusted_stop_limit_price : safe_float := safe_float'last;
      adjusted_trailing_amount : safe_float := safe_float'last;
      adjustable_trailing_unit : integer := 0;
      ext_operator : unbounded_string := +"";
      soft_dollar_tier_name : unbounded_string := +"";
      soft_dollar_tier_val : unbounded_string := +"";
      cash_quantity : safe_float := safe_float'last;
      mifid_to_decision_maker : unbounded_string := +"";
      mifid_to_decision_algo : unbounded_string := +"";
      mifid_to_execution_trader : unbounded_string := +"";
      mifid_to_execution_algo : unbounded_string := +"";
      dont_use_auto_price_for_hedge : boolean := false;
      is_oms_container : boolean := false;
      discretionary_up_to_limit_price : boolean := false;
      use_price_management_algo : boolean := true;
   end record;

   type position_type is record
      contract       : contract_type;
      quantity         : integer := -1;
      average_cost   : safe_float := safe_float'last;
      pnl_unrealized : safe_float := safe_float'last;
      pnl_realized   : safe_float := safe_float'last;
      pnl_daily      : safe_float := safe_float'last;
      value          : safe_float := safe_float'last;
   end record;

   package position_map is new indefinite_hashed_maps
       (Key_Type        => unbounded_string,
        Element_Type    => position_type,
        Hash            => Ada.Strings.Unbounded.Hash,
        Equivalent_Keys => "=");

   type tag_type is (ACCOUNT_TYPE,
                     NET_LIQUIDATION,
                     TOTAL_CASH_VALUE,
                     SETTLED_CASH,
                     BUYING_POWER,
                     EQUITY_WITH_LOAN_VALUE,
                     AVAILABLE_FUNDS,
                     EXCESS_LIQUIDITY,
                     DAY_TRADES_REMAINING,
                     LEVERAGE,
                     UNDEFINED);

   function tag_image (tag : tag_type) return string;
   function tag_value (tag : string) return tag_type;

   function prepare_contract (symbol : string; security : security_type; currency : currency_type; exchange : exchange_type) return contract_type;
   function prepare_order (action : order_action_type; quantity : integer; at_price_type : order_at_price_type; time_in_force : time_in_force_type := DAY; limit_price : safe_float := 0.0) return order_type;

   type summary_type is record
      value : float := 0.0;
      currency : currency_type := UNDEFINED;
   end record;

   function tag_hash (tag : tag_type) return hash_type is
      (Ada.Strings.hash (tag'image));

   package summary_map is new indefinite_hashed_maps
       (Key_Type        => tag_type,
        Element_Type    => summary_type,
        Hash            => tag_hash,
        Equivalent_Keys => "=");

   type act_type is record
      positions : position_map.map;
      summaries : summary_map.map;
   end record;

   package account_map is new indefinite_hashed_maps
       (Key_Type        => unbounded_string,
        Element_Type    => act_type,
        Hash            => Ada.Strings.Unbounded.Hash,
        Equivalent_Keys => "=");

   server_version : integer;
   accounts : account_map.map;
   next_valid_request_id : integer;

end ib_ada;

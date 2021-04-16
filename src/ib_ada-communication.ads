--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2021 ohenley <olivier.henley@gmail.com>
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.

with ada.containers.vectors;
with ada.calendar;

use ada.containers;

package ib_ada.communication is

   type req_id_type is
     (handshake,
      start_api,
      positions,
      account_summary,
      account_summary_cancel,
      profit_and_loss_single,
      cancel_profit_and_loss_single,
      place_order,
      fake_order,
      cancel_order,
      open_orders,
      market_data,
      undefined);

   type req_type is
      record
         request_number : integer := -1;
         msg            : unbounded_string := +"";
         and_listen     : boolean;
         req_id         : req_id_type;
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
      profit_and_loss_single,
      cancel_order,
      open_order,
      order_status,
      fake_order,
      open_orders_end,
      undefined);

   type resp_type is
      record
         and_listen : boolean;
         resp_id    : resp_id_type;
      end record;


   type cached_request_type is abstract tagged null record;

   type profit_and_loss_cached_request_type is new cached_request_type with
      record
         account_id  : unbounded_string;
         contract_id : integer;
      end record;

   type commission_cached_request_type is new cached_request_type with
      record
         commission : safe_float;
      end record;

   package cached_request_map is new indefinite_hashed_maps
     (key_type        => string,
      element_type    => cached_request_type'class,
      hash            => ada.strings.hash,
      equivalent_keys => "=");

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
   procedure profit_and_loss (account_id : string; contract_id : integer);
   procedure profits_and_losses;
   function place_order (contract : contract_type; order : order_type) return integer;
   function place_order (side: order_side_type; symbol : string; quantity : integer; at_price_type : order_at_price_type) return integer;
   function place_fake_order (side: order_side_type; symbol : string; quantity : integer; at_price_type : order_at_price_type) return integer;
   procedure cancel_order (request_number : integer);
   procedure open_orders;
   function get_commission (request_number : integer) return safe_float;

   -- [wip] cannot test, because I am not subscribed.
   procedure market_data (symbol : string; contract_id : integer);

end ib_ada.communication;



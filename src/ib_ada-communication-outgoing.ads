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

package ib_ada.communication.outgoing is

   protected unique_id is
      function get_unique_id (next_valid_req_number : integer) return integer;
   end;

   function build_handshake_msg return string;
   function build_start_api_msg return string;
   function build_positions_msg return string;
   function build_accounts_summary_msg (request_id : integer; account_tag : tag_type) return string;
   function build_cancel_accounts_summary_msg (request_id : integer) return string;
   function build_profit_and_loss_msg (request_id : integer; account : string; contract_id : integer) return string;
   function build_cancel_profit_and_loss_msg (request_id : integer) return string;
   function build_place_order_msg (request_id : integer; contract : contract_type; order : order_type) return string;
   function build_cancel_order_msg (request_id : integer) return string;

   -- [wip] cannot test, because I am not subscribed.
   function build_open_orders_msg return string;
   function build_market_data_msg (request_id : integer; contract : contract_type) return string;

end ib_ada.communication.outgoing;

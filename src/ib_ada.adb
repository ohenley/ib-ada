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

with ada.characters.handling;

use ada.characters.handling;

package body ib_ada is

   function tag_image (tag : tag_type) return string is
      tag_raw_image : string  := tag'image;
      tag_image     : unbounded_string;
      capital       : boolean := false;
      c             : character;
   begin
      append(tag_image, to_upper(tag_raw_image(tag_raw_image'first)));
      for i in tag_raw_image'first + 1 .. tag_raw_image'last loop
         c := tag_raw_image(i);
         if c = '_' then
            capital := true;
         else
            if capital then
               append(tag_image, to_upper(c));
               capital := false;
            else
               append(tag_image, to_lower(c));
            end if;
         end if;
      end loop;

      return +tag_image;
   end;

   function tag_value (tag : string) return tag_type is
      tag_image : unbounded_string;
   begin
      append(tag_image, to_upper(tag(tag'first)));
      for i in tag'first + 1 .. tag'last loop
         if is_upper(tag(i)) then
            append(tag_image, "_");
         end if;
         append(tag_image, to_upper(tag(i)));
      end loop;
      return tag_type'value(+tag_image);
   end;

   function order_status_image (order_status : order_status_type) return string is
      order_status_raw_image : string  := order_status'image;
      order_status_image     : unbounded_string;
      capital                : boolean := false;
      c                      : character;
   begin
      append(order_status_image, to_upper(order_status_raw_image(order_status_raw_image'first)));
      for i in order_status_raw_image'first + 1 .. order_status_raw_image'last loop
         c := order_status_raw_image(i);
         if c = '_' then
            capital := true;
         else
            if capital then
               append(order_status_image, to_upper(c));
               capital := false;
            else
               append(order_status_image, to_lower(c));
            end if;
         end if;
      end loop;

      return +order_status_image;
   end;

   function order_status_value (order_status : string) return order_status_type is
       order_status_image : unbounded_string;
   begin
      append (order_status_image, to_upper(order_status(order_status'first)));
      for i in order_status'first + 1 .. order_status'last loop
         if is_upper(order_status(i)) then
            append(order_status_image, "_");
         end if;
         append(order_status_image, to_upper(order_status(i)));
      end loop;
      return order_status_type'value(+order_status_image);
   end;



   function prepare_contract (symbol   : string;
                              security : security_type;
                              currency : currency_type;
                              exchange : exchange_type) return contract_type is
      contract : contract_type;
   begin
      contract.symbol   := +symbol;
      contract.security := security;
      contract.currency := currency;
      contract.exchange := exchange;
      return contract;
   end;

   function prepare_order (side          : order_side_type;
                           quantity      : integer;
                           at_price_type : order_at_price_type;
                           time_in_force : time_in_force_type := DAY;
                           limit_price   : safe_float         := 0.0;
                           what_if       : boolean            := false) return order_type is
      order : order_type;
   begin
      order.side          := side;
      order.quantity      := quantity;
      order.at_price_type := at_price_type;
      order.time_in_force := time_in_force;
      order.limit_price   := limit_price;
      order.what_if       := what_if;
      return order;
   end;

end ib_ada;

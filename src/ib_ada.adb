with Ada.Characters.Handling; use Ada.Characters.Handling;

package body ib_ada is

   function tag_image (tag : tag_type) return string is
      tag_raw_image : string := tag'image;
      tag_image : unbounded_string;
      capital : boolean := false;
      c : character;
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
      append (tag_image, to_upper (tag(tag'first)));
      for i in tag'first + 1 .. tag'last loop
         if is_upper(tag(i)) then
            append (tag_image, "_");
         end if;
         append (tag_image, to_upper (tag(i)));
      end loop;
      return tag_type'value(+tag_image);
   end;


   function prepare_contract (symbol : string; security : security_type; currency : currency_type; exchange : exchange_type) return contract_type is
      contract : contract_type;
   begin
      contract.symbol := +symbol;
      contract.security := security;
      contract.currency := currency;
      contract.exchange := exchange;
      return contract;
   end;

   function prepare_order (side : order_side_type; quantity : integer; at_price_type : order_at_price_type; time_in_force : time_in_force_type := DAY; limit_price : safe_float := 0.0) return order_type is
      order : order_type;
   begin
      order.side := side;
      order.quantity := quantity;
      order.at_price_type := at_price_type;
      order.time_in_force := time_in_force;
      order.limit_price := limit_price;
      return order;
   end;

end ib_ada;

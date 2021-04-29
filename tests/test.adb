with Ada.text_io; use Ada.Text_IO;

with ib_ada;
with ib_ada.connection;
with ib_ada.communication;


procedure test is
   resp : ib_ada.communication.resp_type;
   use ib_ada;
begin

   ib_ada.connection.client.setup(ib_ada.IB_PAPER);

   resp := ib_ada.communication.handshake;
   put_line(+resp.message);

   resp := ib_ada.communication.start_api;
   put_line(+resp.message);

   resp := ib_ada.communication.accounts_summary (ib_ada.BUYING_POWER);
   put_line(+resp.message);

   resp := ib_ada.communication.positions;
   put_line(+resp.message);

   resp := ib_ada.communication.profits_and_losses;
   put_line(+resp.message);

--     resp := ib_ada.communication.place_order (side          => BUY,
--                                               symbol        => "IBM",
--                                               quantity      => 10,
--                                               at_price_type => MKT);
--     put_line(+resp.message);

--     resp := ib_ada.communication.cancel_order (resp.req_number);
--     put_line(+resp.message);
--
--     resp := ib_ada.communication.open_orders;
--     put_line(+resp.message);

--     resp := ib_ada.communication.place_order (side          => SELL,
--                                               symbol        => "IBM",
--                                               quantity      => 10,
--                                               at_price_type => MKT);
--     put_line(+resp.message);

   ib_ada.connection.client.disconnect;

end test;

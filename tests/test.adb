with Ada.text_io; use Ada.Text_IO;

with ib_ada;
with ib_ada.conn;
with ib_ada.communication;


procedure test is

   request_id : integer;

begin

   ib_ada.conn.client.setup(ib_ada.IB_PAPER);

   ib_ada.communication.handshake;
   ib_ada.communication.start_api;

   --  ib_ada.communication.pnl ("DU3689337", 265598); -- AAPL
   --  ib_ada.communication.account_summary (ib_ada.BUYING_POWER);
   --  ib_ada.communication.positions;
   --
   --  request_id := ib_ada.communication.buy_order ("QS", 10, ib_ada.MIDPRICE);
   --
   ib_ada.communication.open_orders;

   --ib_ada.communication.cancel_order (278);
   --ib_ada.communication.cancel_order (275);
   --ib_ada.communication.cancel_order (284);

   --ib_ada.communication.cancel_order (request_id);


   --ib_ada.comminucation.sell ("AAPL", 10, ib_ada.MKT);

   ib_ada.conn.client.disconnect;

end test;

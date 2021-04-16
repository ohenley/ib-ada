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

with ib_ada.communication;

use ib_ada.communication;

package ib_ada.connection is

   task type socket_connection is
      entry setup (session : session_type);
      entry disconnect;
      entry send (req : req_type; resp : out resp_type);
   end;

   client : socket_connection;

end ib_ada.connection;

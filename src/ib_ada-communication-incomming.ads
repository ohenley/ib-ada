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

use ada.containers;

package ib_ada.communication.incomming is

   type msg_handler_type;

   type handling_func_type is access function (req         : req_type;
                                               msg_tokens  : in out msg_vector.vector;
                                               msg_handler : msg_handler_type) return resp_type;

   type msg_handler_type is record
      resp_id       : resp_id_type;
      codes         : code_vector.vector;
      handling_func : handling_func_type;
   end record;

   package message_map is new indefinite_hashed_maps
       (key_type        => unbounded_string,
        element_type    => msg_handler_type,
        hash            => ada.strings.unbounded.hash,
        equivalent_keys => "=");

   msg_definitions : message_map.map;

   function handle_message (req : req_type; msg : unbounded_string) return resp_type;

end ib_ada.communication.incomming;

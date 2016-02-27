(*
 * Parser
 *
 * Copyright (C) 2015  Xavier Van de Woestyne <xaviervdw@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
*)

let id x = x

type 'a base =
  | Success of 'a
  | Failure of string


let either result fsuccess ffailure =
  match result with
  | Success x -> fsuccess x
  | Failure x -> ffailure x

let fail x = Failure x

module Requirement = Olmi.Make.WithBind(struct

    type 'a t = 'a base
    let return x = Success x
    let bind x f = either x f fail

  end)

include Olmi.Make.Monad (Requirement)

let succeed = return

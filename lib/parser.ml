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

type 'a result = ('a, string) Result.t
type 'a t = Parser of (string -> 'a result)

open Result
open Util

let run parser input =
  let Parser f = parser in
  f input

let expected =
  Printf.sprintf "Excepted %c, got %c"

let char c =
  let aux input =
    let len = String.length input in
    if len == 0 then fail "Empty string"
    else if input.[0] == c
    then return (c, String.sub input 1 (pred len))
    else fail (expected c input.[0])
  in Parser aux

let followed parser1 parser2 =
  let aux input =
    match run parser1 input with
    | Failure x as failure-> failure
    | Success (x, xs) -> begin
        match run parser2 xs with
        | Failure x as failure-> failure
        | Success (y, ys) -> return ((x, y), ys)
      end
  in Parser aux

let disjunction parser1 parser2 =
  let aux input =
    match run parser1 input with
    | (Success x) as result -> result
    | _ -> run parser2 input
  in Parser aux


let choice parsers  =
  List.reduce disjunction parsers

let ( >& ) = followed
let ( >| ) = disjunction

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


type 'a result = ('a *string, string) Result.t
type 'a t = Parser of (string -> 'a result)
open Util

let run parser input =
  let Parser f = parser in
  f input

let return x =
  Parser (fun input -> Result.Success (x, input))

let expected =
  Printf.sprintf "Excepted %c, got %c"

let map f parser =
  let open Result in
  let aux input =
    match run parser input with
    | Success (x, xs) -> return (f x, xs)
    | Failure err -> fail err
  in Parser aux

let char c =
  let open Result in
  let aux input =
    let len = String.length input in
    if len == 0 then fail "Empty string"
    else if input.[0] == c
    then return (c, String.sub input 1 (pred len))
    else fail (expected c input.[0])
  in Parser aux

let followed parser1 parser2 =
  let open Result in
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
  let open Result in
  let aux input =
    match run parser1 input with
    | (Success x) as result -> result
    | _ -> run parser2 input
  in Parser aux


let choice parsers  =
  List.reduce disjunction parsers

let one_of chars =
  chars
  |> List.map char
  |> choice

let lowercase = one_of Char.lowers
let uppercase = one_of Char.uppers
let digit = one_of Char.digits
let alphanumeric = one_of Char.alphanumerics

let ( >& ) = followed
let ( >| ) = disjunction
let ( >|=) x f = map f x


let apply fp xp = (fp >& xp) >|= (fun (f, x) -> f x)
let (<*>) = apply
let lift2 f parser1 parser2 = return f <*> parser1 <*> parser2

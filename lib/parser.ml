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



open Util

type 'a base =
  | Parser of (string -> ('a * string) Result.t)

let run parser input =
  let Parser f = parser in
  f input


include Olmi.Make.Monad(Olmi.Make.WithBind(
    struct
      type 'a t = 'a base
      let return x = Parser (fun input -> Result.Success (x, input))
      let bind parser f =
        let aux input =
          let result = run parser input in
          match result with
          | Result.Failure err -> Result.fail err
          | Result.Success (x, xs) -> run (f x) xs
        in Parser aux
    end)
    )


let expected =
  Printf.sprintf "Excepted %c, got %c"

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
  parser1 >>= fun result1 ->
  parser2 >>= fun result2 ->
  return (result1, result2)

let disjunction parser1 parser2 =
  let open Result in
  let aux input =
    match run parser1 input with
    | (Success x) as result -> result
    | _ -> run parser2 input
  in Parser aux

let seq parsers =
  let cons = liftM2 List.cons in
  let rec aux = function
    | [] -> return []
    | x::xs -> cons x (aux xs)
  in aux parsers

let choice parsers  =
  List.reduce disjunction parsers

let one_of chars =
  chars
  |> List.map char
  |> choice

let rec zom parser input =
  match run parser input with
  | Result.Failure err -> ([], input)
  | Result.Success (x, xs) ->
    let xx, xxs = zom parser xs in
    (x::xx, xxs)

let zero_or_more parser =
  Parser (fun input -> Result.return (zom parser input))

let one_or_more parser =
  parser >>= fun x ->
  zero_or_more parser >>= fun xs ->
  return (x :: xs)

let lowercase = one_of Char.lowers
let uppercase = one_of Char.uppers
let digit = one_of Char.digits
let alphanumeric = one_of Char.alphanumerics
let whitespace = one_of [' '; '\n'; '\t' ]

let string str =
  str
  |> List.of_string
  |> List.map char
  |> seq
  |> fmap String.of_list

let next_int =
  let to_i list = int_of_string (String.of_list list) in
  one_or_more (digit)
  |> fmap to_i

let ( >& ) = followed
let ( >| ) = disjunction

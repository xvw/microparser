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

module List =
struct

  include List

  let reduce f = function
    | [] -> raise (Failure "Empty list")
    | x::xs -> List.fold_left f x xs

  let range fpred fsucc x y =
    let f = if x < y then fpred else fsucc in
    let rec aux acc = function
      | n when n = (f x) -> acc
      | n -> aux (n :: acc) (f n)
    in aux [] y

  let seed = range pred succ

  module Requirement = Olmi.Make.WithJoin(struct
      type 'a t = 'a list
      let return x = [x]
      let join = flatten
      let fmap = map
    end)

  include Olmi.Make.Plus (Requirement) (struct
      type 'a t = 'a list
      let mempty = []
      let mplus = append
    end)

  let cons x xs = x::xs

end

module Char =
struct

  include Char

  let to_int = code
  let of_int = chr

  let move f c = of_int (f (to_int c))
  let pred = move pred
  let succ = move succ

  let seed = List.range pred succ
  let digits = seed '0' '9'
  let lowers = seed 'a' 'z'
  let uppers = seed 'A' 'Z'
  let letters = lowers @ uppers
  let alphanumerics = letters @ digits



end

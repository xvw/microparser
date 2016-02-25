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

type ('success, 'failure) t =
  | Success of 'success
  | Failure of 'failure

let either result fsuccess ffailure =
  match result with
  | Success x -> fsuccess x
  | Failure x -> ffailure x


let return x = Success x
let succeed = return
let fail x = Failure x

let bind x f = either x f fail

let ( >>= ) = bind
let ( >>  ) m k = m >>= (fun _ -> k)
let ( >=> ) s1 s2 x = s1 >> (x >>= s2)

let join x = x >>= id
let lift f x = x >>= (fun y -> return (f y))
let fmap f result = result >>= ( fun x -> return (f x) )
let ( <$> ) = fmap
let ( <*> ) fs rs =
  fs >>= fun f ->
  rs >>= fun x -> return (f x)

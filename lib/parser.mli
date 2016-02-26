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
type 'a t

val run : 'a t -> string -> 'a result
val return : 'a -> 'a t
val expected : char -> char -> string

val map : ('a -> 'b) -> 'a t -> 'b t
val apply : ('a -> 'b) t -> 'a t -> 'b t
val lift2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

val followed : 'a t -> 'b t -> ('a * 'b) t
val disjunction : 'a t -> 'a t -> 'a t

val char : char -> char t
val choice : 'a t list -> 'a t
val one_of : char list -> char t
val lowercase : char t
val uppercase : char t
val digit : char t
val alphanumeric : char t

val ( >& ) : 'a t -> 'b t -> ('a * 'b) t
val ( >| ) : 'a t -> 'a t -> 'a t
val ( >|= ) : 'a t -> ('a -> 'b) -> 'b t
val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t

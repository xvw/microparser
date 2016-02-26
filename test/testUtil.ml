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


open OUnit
open Util

let testCharMove1 _ =
  assert_equal (Char.succ 'A') 'B';
  assert_equal (Char.succ 'B') 'C';
  assert_equal (Char.succ '0') '1'

let testCharMove2 _ =
  assert_equal (Char.pred 'B') 'A';
  assert_equal (Char.pred 'C') 'B';
  assert_equal (Char.pred '9') '8'

let testSeed1 _ =
  match Char.seed 'A' 'C' with
  | a::b::[c] ->
    assert_equal a 'A';
    assert_equal b 'B';
    assert_equal c 'C'
  | _ -> assert_failure "testSeed fail"

let suite = [
  "TestCharMove1"   >:: testCharMove1
; "TestCharMove2"   >:: testCharMove2
; "TestSeed1"       >:: testSeed1
]

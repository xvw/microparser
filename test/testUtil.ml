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


let testDigits _ =
  let l = Char.digits in
  assert_equal 10 (List.length l);
  match l with
  | '0'::'1'::'2'::'3'::'4'::'5'::'6'::'7'::'8'::['9'] -> ()
  | _ -> assert_failure "Invalid list"

let testListInit1 _ =
  let l = List.init 5 (fun x -> x) in
  assert_equal 5 (List.length l);
  match l with
  | 0 :: 1 :: 2 :: 3 :: 4 :: [] -> ()
  | _ -> assert_failure "Invalid list"

let testListOfStr1 _ =
  match List.of_string "" with
  | [] -> ()
  | _ -> assert_failure "Invalid list"

let testListOfStr2 _ =
  match List.of_string "foo" with
  | 'f'::'o'::'o'::[] -> ()
  | _ -> assert_failure "Invalid list"

let testListOfStr3 _ =
  match List.of_string "fBo" with
  | 'f'::'B'::'o'::[] -> ()
  | _ -> assert_failure "Invalid list"

let testStrOfList1 _ = assert_equal (String.of_list []) ""
let testStrOfList2 _ = assert_equal (String.of_list ['d']) "d"
let testStrOfList3 _ = assert_equal (String.of_list ['d'; 'F']) "dF"

let suite = [
  "TestCharMove1"   >:: testCharMove1
; "TestCharMove2"   >:: testCharMove2
; "TestSeed1"       >:: testSeed1
; "TestDigits"      >:: testDigits
; "TestListInit"    >:: testListInit1
; "TestListOfStr1"  >:: testListOfStr1
; "TestListOfStr2"  >:: testListOfStr2
; "TestListOfStr3"  >:: testListOfStr3
; "TestStrOfList1"  >:: testStrOfList1
; "TestStrOfList2"  >:: testStrOfList2
; "TestStrOfList2"  >:: testStrOfList3
]

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

let parseA = Parser.char 'A'
let parseB = Parser.char 'B'
let parseAandB = Parser.(parseA >& parseB)
let parseAorB = Parser.(parseA >| parseB)
let parserabZ = Parser.(choice [
    (char 'a');
    (char 'b');
    (char 'Z')]
  )

let parseABCD = Parser.one_of ['A'; 'B'; 'C'; 'D' ]

let extract_success = function
  | Result.Success x -> x
  | _ -> failwith "Improbable case"

let extract_failure = function
  | Result.Failure x -> x
  | _ -> failwith "Improbable case"

let testParseChar1 _ =
  let (c, remaining) =
    Parser.run parseA "AHA"
    |> extract_success
  in
  assert_equal c 'A';
  assert_equal remaining "HA"

let testParseChar2 _ =
  let message =
    Parser.run parseA "BHA"
    |> extract_failure
   in assert_equal (Parser.expected 'A' 'B') message

let testFollowed1 _ =
  let ((a, b), remaining) =
    Parser.run parseAandB "ABFOO"
    |> extract_success
  in assert_equal ((a, b), remaining) (('A', 'B'), "FOO")

let testFollowed2 _ =
  let message  =
    Parser.run parseAandB "AZFOO"
    |> extract_failure
  in
  let message2 =
    Parser.run parseAandB "BAFOO"
    |> extract_failure
  in
  assert_equal (Parser.expected 'B' 'Z') message;
  assert_equal (Parser.expected 'A' 'B') message2

let testDisjunction1 _ =
  let (c, rem) =
    Parser.run parseAorB "BETA"
    |> extract_success
  in assert_equal (c, rem) ('B', "ETA")

let testDisjunction2 _ =
  let (c2, rem2) =
    Parser.run parseAorB "ALPHA"
    |> extract_success
  in assert_equal (c2, rem2) ('A', "LPHA")

let testDisjunction3 _ =
  let message =
    Parser.run parseAorB "ZETA"
    |> extract_failure
  in assert_equal (Parser.expected 'B' 'Z') message

let testChoice1 _ =
  let (c, rem) =
    Parser.run parserabZ "areste"
    |> extract_success
  in assert_equal (c, rem) ('a', "reste")

let testChoice1 _ =
  let (c, rem) =
    Parser.run parserabZ "areste"
    |> extract_success
  in assert_equal (c, rem) ('a', "reste")

let testChoice2 _ =
  let (c, rem) =
    Parser.run parserabZ "breste"
    |> extract_success
  in assert_equal (c, rem) ('b', "reste")

let testChoice3 _ =
  let (c, rem) =
    Parser.run parserabZ "Zreste"
    |> extract_success
  in assert_equal (c, rem) ('Z', "reste")

let testChoice4 _ =
  let message =
    Parser.run parserabZ "zReste"
    |> extract_failure
  in assert_equal (Parser.expected 'Z' 'z') message

let testOneOf1 _ =
  let (c, rem) =
    Parser.run parseABCD "AFOO"
    |> extract_success
  in assert_equal (c, rem) ('A', "FOO")

let testOneOf2 _ =
  let (c, rem) =
    Parser.run parseABCD "BFOO"
    |> extract_success
  in assert_equal (c, rem) ('B', "FOO")

let testOneOf3 _ =
  let (c, rem) =
    Parser.run parseABCD "CFOO"
    |> extract_success
  in assert_equal (c, rem) ('C', "FOO")

let testOneOf4 _ =
  let (c, rem) =
    Parser.run parseABCD "DFOO"
    |> extract_success
  in assert_equal (c, rem) ('D', "FOO")

let suite =
  "OUnit tests for Parser" >::: [
    "testParseChar1"   >:: testParseChar1
  ; "testParseChar2"   >:: testParseChar2
  ; "testFollowed1"    >:: testFollowed1
  ; "testFollowed2"    >:: testFollowed2
  ; "testDisjunction1" >:: testDisjunction1
  ; "testDisjunction2" >:: testDisjunction2
  ; "testDisjunction3" >:: testDisjunction3
  ; "testChoice1"      >:: testChoice1
  ; "testChoice2"      >:: testChoice2
  ; "testChoice3"      >:: testChoice3
  ; "testChoice4"      >:: testChoice4
  ; "testOneOf1"       >:: testOneOf1
  ; "testOneOf1"       >:: testOneOf1
  ; "testOneOf2"       >:: testOneOf2
  ; "testOneOf3"       >:: testOneOf3
  ; "testOneOf4"       >:: testOneOf4
  ]

let _ = run_test_tt_main suite


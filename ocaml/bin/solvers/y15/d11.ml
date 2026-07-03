open Advent

let inc_pw pw =
  let len = String.length pw in
  let inc_char c = c |> Char.code |> Int.add 1 |> Char.chr in
  let rec inci ?(i = len - 1) () =
    let c = pw.[i] in
    if c <> 'z' then
      let c = inc_char c in
      let pre = String.drop_last (len - i) pw in
      let post = String.make (len - (i + 1)) 'a' in
      pre ^ String.of_char c ^ post
    else if i = 0 then String.make len 'a'
    else inci ~i:(i - 1) ()
  in
  inci ()

let rec has_straight ?(i = 0) pw =
  let i = ref 0 in
  let found = ref false in
  let is_straight i =
    let d = int_of_char pw.[i] in
    int_of_char pw.[i + 1] = d + 1 && int_of_char pw.[i + 2] = d + 2
  in
  while !i < String.length pw - 2 && not !found do
    found := is_straight !i;
    i := !i + 1
  done;
  !found

let has_ilo pw =
  let i = ref 0 in
  let found = ref false in
  let is_ilo c = match c with 'i' | 'l' | 'o' -> true | _ -> false in
  while !i < String.length pw && not !found do
    found := is_ilo pw.[!i];
    i := !i + 1
  done;
  !found

let has_two_pair pw =
  let first = ref None in
  let found = ref false in
  let i = ref 0 in
  while !i < String.length pw - 1 && not !found do
    if pw.[!i] = pw.[!i + 1] then
      match !first with
      | None -> first := Some pw.[!i]
      | Some c -> if c <> pw.[!i] then found := true else ()
    else ();
    i := !i + 1
  done;
  !found

let is_valid pw = has_straight pw && (not (has_ilo pw)) && has_two_pair pw

let rec next_valid pw =
  let pw = inc_pw pw in
  if is_valid pw then pw else next_valid pw

let solve input =
  let pw1 = next_valid input in
  let pw2 = next_valid pw1 in
  (Ans.Str pw1, Ans.Str pw2)

module S = struct
  type t = string

  let parse input = Input.one_line input
  let solve = solve
end

let () = Registry.register (15, 11) (module S : Solver)

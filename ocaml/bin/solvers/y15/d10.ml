open Advent

let next_group b i =
  if i = Bytes.length b then None
  else
    let c = Bytes.get b i in
    let j = ref i in
    while !j < Bytes.length b && Bytes.get b !j = c do
      j := !j + 1
    done;
    Some (c, !j - i)

let size_next b =
  let i = ref 0 in
  let size = ref 0 in
  let grp = ref (next_group b !i) in
  while Option.is_some !grp do
    let _, n = Option.get !grp in
    i := !i + n;
    size := !size + 2;
    grp := next_group b !i
  done;
  !size

let look_say cur =
  let next = cur |> size_next |> Bytes.create in
  let icur = ref 0 in
  let inext = ref 0 in
  let grp = ref (next_group cur !icur) in
  while Option.is_some !grp do
    let c, n = Option.get !grp in
    Bytes.set next !inext (Char.Ascii.digit_of_int n);
    Bytes.set next (!inext + 1) c;
    icur := !icur + n;
    inext := !inext + 2;
    grp := next_group cur !icur
  done;
  next

let solve input =
  let b = ref input in
  for i = 1 to 40 do
    b := look_say !b
  done;
  let p1 = Bytes.length !b in
  for i = 1 to 10 do
    b := look_say !b
  done;
  (Ans.Int p1, Ans.Int (Bytes.length !b))

module S = struct
  type t = bytes

  let parse input = input |> Input.one_line |> Bytes.of_string
  let solve = solve
end

let () = Registry.register (15, 10) (module S : Solver)

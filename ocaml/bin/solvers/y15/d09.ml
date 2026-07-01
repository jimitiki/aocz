open Advent

let add_edge edges src dest dist =
  let subtbl =
    match Hashtbl.find_opt edges src with
    | Some t -> t
    | None ->
        let t = Hashtbl.create 10 in
        Hashtbl.add edges src t;
        t
  in
  Hashtbl.replace subtbl dest dist

let parse_edge edges s =
  match String.split_all ~sep:" " s with
  | [ src; "to"; dest; "="; dist ] ->
      let d = try int_of_string dist with Failure s -> raise (Bad_input s) in
      add_edge edges src dest d;
      add_edge edges dest src d
  | _ -> raise (Bad_input s)

let rec parse edges input = List.iter (parse_edge edges) input

module StrOrd = struct
  type t = string

  let compare = String.compare
end

module StrSet = Set.Make (StrOrd)

type path = string list * int
type optimizer = { choose : path -> path -> path; worst : int }

let rec best_path edges choose unvisited cur =
  if StrSet.is_empty unvisited then ([], 0)
  else
    let candidates =
      match cur with
      | None -> Hashtbl.to_seq_keys edges |> Seq.map (fun dest -> (dest, 0))
      | Some src ->
          let is_candidate (dest, _) =
            unvisited |> StrSet.find_opt dest |> Option.is_some
          in
          Hashtbl.(find edges src |> to_seq) |> Seq.filter is_candidate
    in
    let f (dest, dist) =
      let unvisited = StrSet.remove dest unvisited in
      let pbest, dbest = best_path edges choose unvisited (Some dest) in
      (dest :: pbest, dbest + dist)
    in
    candidates |> Seq.map f |> choose

type comparator = Min | Max

let choose cmp (paths : path Seq.t) =
  let op = match cmp with Min -> ( < ) | Max -> ( > ) in
  let first, rest = paths |> Seq.uncons |> Option.get in
  let f (pcur, dcur) (pnext, dnext) =
    if op dnext dcur then (pnext, dnext) else (pcur, dcur)
  in
  Seq.fold_left f first rest

let solve edges =
  let cities = Hashtbl.to_seq_keys edges in
  let unvisited = StrSet.(empty |> add_seq cities) in
  let _, dshort = best_path edges (choose Min) unvisited None in
  let _, dlong = best_path edges (choose Max) unvisited None in
  (Ans.Int dshort, Ans.Int dlong)

module S = struct
  type t = (string, (string, int) Hashtbl.t) Hashtbl.t

  let parse input =
    let edges = Hashtbl.create 10 in
    parse edges input;
    edges

  let solve = solve
end

let () = Registry.register (15, 09) (module S : Solver)

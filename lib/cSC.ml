type nonzero = {
  i : int;
  j : int;
  x : float;
}

(* suppose k = ps[c] for a column c, where 0 <= c <= (n-1). Then the matrix has
   a nonzero value [fst (List.nth xis k)] at column c, row [snd (List.nth xis
   k)] *)
type compressed = {
  xis : (float * int) list;
  ps : int list;
  num_nz : int;  (** number of non-zero values; length of [xs] and [is] *)
}

let cmp_nz { i = i1; j = j1; _ } { i = i2; j = j2; _ } =
  match Int.compare j1 j2 with 0 -> Int.compare i1 i2 | c -> c

(* [prepend ~value:x ~count:c l] prepends value [x] to list [l] [c] times. *)
let rec prepend ~value ~count list =
  if count = 0 then list else prepend ~value ~count:(count - 1) (value :: list)

let check_bounds_exn ~m ~n { i; j; _ } =
  if i < 0 || i >= m then failwith "row index out of bounds";
  if j < 0 || j >= n then failwith "column index out of bounds"

let eps = sqrt Float.epsilon
let is_small x = -.eps <= x && x <= eps

type structure =
  | Plain
  | Upper  (** lower triangular *)
  | Lower  (** upper triangular *)

let create ~m ~n ?(col1 = true) =
  let rec loop check_structure xis ps prev_nz k = function
    | ({ i; j; x } as nz) :: rest ->
      (* because nonzeros are sorted *)
      assert (prev_nz.j <= j);

      check_bounds_exn ~m ~n nz;
      check_structure nz;

      if prev_nz.j = j && prev_nz.i = i then failwith "duplicate nonzeros";

      (* some columns may be empty; still, an empty column still requires a
         value index [k] (an index pointing to the arrays [xs] and [is]) . for
         an empty column, this [k] will equal the [k] associated with the
         previous column. *)
      let ps = prepend ~value:k ~count:(j - prev_nz.j) ps in

      if is_small x then
        (* x is near zero; skip *)
        loop check_structure xis ps nz k rest
      else
        let xis = (x, i) :: xis in
        let k = k + 1 in
        loop check_structure xis ps nz k rest
    | [] ->
      let last_j = n - 1 in
      (* it is possible that the there are no nonzeros in the last few columns,
         so we extend [k] to those columns: *)
      let ps = prepend ~value:k ~count:(last_j - prev_nz.j) ps in
      let xis = List.rev xis in
      let ps =
        let ps =
          (* length of ps is one more than the number of columns *)
          if col1 then k :: ps else ps
        in
        List.rev ps
      in
      assert (
        let len_xis = List.length xis in
        let len_ps = List.length ps in
        (len_ps = n + if col1 then 1 else 0) && len_xis = k);
      { xis; ps; num_nz = k }
  in

  (* skip all leading zeros; returning the first nonzero *)
  let rec first_nonzero = function
    | [] -> None
    | nz :: rest ->
      check_bounds_exn ~m ~n nz;
      if is_small nz.x then (* [nz.x] is near zero; skip *)
        first_nonzero rest
      else Some (nz, rest)
  in

  fun nonzeros structure ->
    let check_structure_exn =
      match structure with
      | Upper ->
        fun { i; j; _ } -> if i > j then failwith "not upper-triangular"
      | Lower ->
        fun { i; j; _ } -> if i < j then failwith "not lower-triangular"
      | Plain -> fun _ -> ()
    in

    let nonzeros = List.sort cmp_nz nonzeros in
    match first_nonzero nonzeros with
    | None -> { xis = []; ps = []; num_nz = 0 }
    | Some (nz, rest) ->
      let xis = [ (nz.x, nz.i) ] in
      (* it is possible that there are no nonzeros in the first few columns, so
         we extend the value [k=0] to these columns: *)
      let ps = prepend ~value:0 ~count:(nz.j + 1) [] in
      loop check_structure_exn xis ps nz 1 rest

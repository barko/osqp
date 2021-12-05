type nonzero = {
  i : int;
  j : int;
  x : float
}

let cmp_nz {i = i1; j = j1; _} {i = i2; j = j2; _} =
  match Int.compare j1 j2 with
  | 0 -> Int.compare i1 i2
  | c -> c

let rec prepend ~value ~count list =
  if count = 0 then
    list
  else
    prepend ~value ~count:(count - 1) (value :: list)

let check_bounds_exn ~m ~n {i; j; _} =
  if i < 0 || i >= m then
    failwith "row index out of bounds";
  if j < 0 || j >= n then
    failwith "column index out of bounds"


let create ~m ~n =
  let rec loop check_structure_exn xs is ps prev_nz k = function
    | ({i; j; x} as nz) :: rest ->
      (* because nonzeros are sorted *)
      assert (prev_nz.j <= j);

      check_bounds_exn ~m ~n nz;

      if prev_nz.j = j && prev_nz.i = i then
        failwith "duplicate nonzeros";

      check_structure_exn nz;

      let xs = x :: xs in
      let is = i :: is in
      let ps = prepend ~value:k ~count:(j - prev_nz.j) ps in
      let k = k + 1 in
      loop check_structure_exn xs is ps nz k rest


    | [] ->
      let xs = List.rev xs in
      let is = List.rev is in
      let ps = List.rev (k :: ps) in
      assert (List.length xs = List.length is);
      assert (List.length ps = n + 1);
      xs, is, ps
  in
  fun nonzeros structure ->
    let nonzeros = List.sort cmp_nz nonzeros in
    let check_structure_exn =
      match structure with
      | `Upper ->
        fun {i; j; _} ->
          if i > j then
            failwith "not upper-triangular"
      | `Lower ->
        fun {i; j; _} ->
          if i < j then
            failwith "not lower-triangular"
      | `None ->
        fun _ -> ()
    in
    match nonzeros with
    | [] -> [], [], [0]
    | nz :: rest ->
      check_bounds_exn ~m ~n nz;
      let xs = [nz.x] in
      let is = [nz.i] in
      let ps = [0] in
      loop check_structure_exn xs is ps nz 1 rest

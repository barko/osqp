type nonzero = {
  i : int;  (** row index *)
  j : int;  (** col index *)
  x : float;  (** matrix value *)
}

type compressed = {
  xis : (float * int) list;
      (** suppose [p = List.nth ps c] for a column c, where 0 <= c <=
          (n-1). Then the matrix has a nonzero value [fst (List.nth xis k)]
          at column c, row [snd (List.nth xis k)] *)
  ps : int list;
      (** if [List.nth ps c = p] then the nonzeros of column [c] 
          starts with the [p]-th element of [xis] *)
  num_nz : int;  (** number of non-zero values; length of [xs] and [is] *)
}

type structure =
  | Plain  (** no particular structure *)
  | Upper  (** lower triangular *)
  | Lower  (** upper triangular *)

val create :
  m:int (** number of rows *) ->
  n:int (** number of columns *) ->
  ?col1:bool
    (** should the size of [ps] be one greater than the number of column? *) ->
  nonzero list ->
  structure ->
  compressed

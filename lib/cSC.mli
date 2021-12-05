type nonzero = {
  i : int;
  j : int;
  x : float
}

val create: m:int -> n:int -> nonzero list -> [ `Lower | `None | `Upper ]
  -> float list * int list * int list

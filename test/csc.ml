open Osqp.CSC

let test1 () =
  let nz = [{i=0; j=0; x=1.}; {i=0; j=1; x=2.}; {i=1; j=0; x=3.}; {i=2; j=1; x=4.}] in
  let xs, is, ps = create ~m:3 ~n:2 nz `None in
  assert ( xs = [1.; 3.; 2.; 4.    ] );
  assert ( is = [0 ; 1 ; 0 ; 2 ] );
  assert ( ps = [0 ; 2 ; 4     ] )

let test2 () =
  let nz = [{i=0; j=0; x=1.}; {i=0; j=3; x=2.}; {i=1; j=0; x=3.}; {i=2; j=3; x=4.}] in
  let xs, is, ps = create ~m:3 ~n:4 nz `None in
  assert ( xs = [1.; 3.; 2.; 4.    ] );
  assert ( is = [0 ; 1 ; 0 ; 2     ] );
  assert ( ps = [0 ; 2 ; 2 ; 2 ; 4 ] )

let test3 () =
  let nz = [{i=0; j=0; x=1.}; {i=0; j=3; x=2.}; {i=1; j=1; x=3.}] in
  let xs, is, ps = create ~m:3 ~n:4 nz `Upper in
  assert ( xs = [1.; 3.; 2.        ] );
  assert ( is = [0 ; 1 ; 0         ] );
  assert ( ps = [0 ; 1 ; 2 ; 2 ; 3 ] )

let _ =
  test1 ();
  test2 ();
  test3 ()



open Osqp.CSC

let test1 () =
  let nz = [{i=0; j=0; x=1.}; {i=0; j=1; x=2.}; {i=1; j=0; x=3.}; {i=2; j=1; x=4.}] in
  let { xis; ps; num_nz } = create ~m:3 ~n:2 nz Plain in
  assert ( num_nz = 4 );
  assert ( xis = [1., 0; 3., 1; 2., 0; 4., 2] );
  assert ( ps = [0 ; 2 ; 4 ] )

let test2 () =
  let nz = [{i=0; j=0; x=1.}; {i=0; j=3; x=2.}; {i=1; j=0; x=3.}; {i=2; j=3; x=4.}] in
  let { xis; ps; num_nz } = create ~m:3 ~n:4 nz Plain in
  assert (num_nz = 4);
  assert (xis = [1., 0; 3., 1; 2., 0; 4., 2]);
  assert ( ps = [0 ; 2 ; 2 ; 2 ; 4 ] )

let test3 () =
  let nz = [{i=0; j=0; x=1.}; {i=0; j=3; x=2.}; {i=1; j=1; x=3.}] in
  let { xis; ps; num_nz } = create ~m:3 ~n:4 nz Upper in
  assert (num_nz = 3);
  assert (xis = [1., 0; 3., 1; 2., 0]);
  assert ( ps = [0 ; 1 ; 2 ; 2 ; 3 ] )

let _ =
  test1 ();
  test2 ();
  test3 ()



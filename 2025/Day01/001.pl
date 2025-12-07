
:- use_module( library( dcg/basics)). % whites, eol, integer, blanks
:- use_module( library( pure_input)). % phrase_from_file

parse( [ ( Direction , Count) | T]) --> blanks, [D], integer(Count), { atom_codes( Direction, [D])}, parse( T), !, blanks.
parse( []) --> [].

dial( L, State, ZeroCount) :- L = [], !, State=_, ZeroCount= 0.

dial( L, State, ZeroCount) :- L = [ ( Direction , Count) | T ], !
, ( Direction == 'L' -> C is - Count ; Direction == 'R' -> C is Count ; $(false))
, State_Next is ( State + C) mod 100
, dial( T, State_Next, ZeroCount_Next)
, ( State_Next == 0 -> ZeroCount is ZeroCount_Next + 1; ZeroCount is ZeroCount_Next)
.

dial( L, ZeroCount) :- dial( L, 50, ZeroCount).

solution_001 :- true
, phrase_from_file( parse( L), 'input.txt')
% , phrase_from_file( parse( L), 'example.txt')
% , writeln( L)
, dial( L, ZeroCount)
, writeln( ZeroCount)
.

dial2( L, State, ZeroCount) :- L = [], !, State=_, ZeroCount= 0.

dial2( L, State, ZeroCount) :- L = [ ( Direction , Count) | T ], !
, ( Direction == 'L' -> C is - Count ; Direction == 'R' -> C is Count ; $(false))
, State_C is State + C
% , State_Next is ( State_C) mod 100
, divmod( State_C, 100, Q, State_Next)
% , writeln( divmod( State_C, 100, Q, State_Next) )
, QA is abs(Q)
, dial2( T, State_Next, ZeroCount_Next)
, ( 
    State==0, State_Next == 0, Direction == 'L' -> ZeroCount is ZeroCount_Next + QA ; 
    State\==0, State_Next == 0, Direction == 'L' -> ZeroCount is ZeroCount_Next + QA + 1 ; 
    State==0, State_Next == 0, Direction == 'R' -> ZeroCount is ZeroCount_Next + QA ; 
    State\==0, State_Next == 0, Direction == 'R' -> ZeroCount is ZeroCount_Next + QA ; 
    State \== 0, \+ between( 0, 100, State_C) -> ZeroCount is ZeroCount_Next + QA ;
    State == 0, Direction == 'L', \+ between( 0, 100, State_C) -> ZeroCount is ZeroCount_Next + QA -1 ;
    State == 0, Direction == 'R', \+ between( 0, 100, State_C) -> ZeroCount is ZeroCount_Next + QA  ;
    ZeroCount is ZeroCount_Next
  )
.

dial2( L, ZeroCount) :- dial2( L, 50, ZeroCount).

test_001 :- true
, dial2( [('L', 49)], 50, Z0), writeln( z0-Z0-0)
, dial2( [('L', 50)], 50, Z1), writeln( z1-Z1-1)
, dial2( [('L', 51)], 50, Z2), writeln( z2-Z2-1)
, dial2( [('R', 49)], 50, Z3), writeln( z3-Z3-0)
, dial2( [('R', 50)], 50, Z4), writeln( z4-Z4-1) 
, dial2( [('R', 51)], 50, Z5), writeln( z5-Z5-1)
.

test_002 :- true
, dial2( [('L', 149)], 50, Z0), writeln( z0-Z0-1)
, dial2( [('L', 150)], 50, Z1), writeln( z1-Z1-2)
, dial2( [('L', 151)], 50, Z2), writeln( z2-Z2-2)
, dial2( [('R', 149)], 50, Z3), writeln( z3-Z3-1)
, dial2( [('R', 150)], 50, Z4), writeln( z4-Z4-2) 
, dial2( [('R', 151)], 50, Z5), writeln( z5-Z5-2)
.

test_003 :- true
, dial2( [('L', 49)], 0, Z0), writeln( z0-Z0-0)
, dial2( [('L', 50)], 0, Z1), writeln( z1-Z1-0)
, dial2( [('L', 51)], 0, Z2), writeln( z2-Z2-0)
, dial2( [('R', 49)], 0, Z3), writeln( z3-Z3-0)
, dial2( [('R', 50)], 0, Z4), writeln( z4-Z4-0) 
, dial2( [('R', 51)], 0, Z5), writeln( z5-Z5-0)
.

test_004 :- true
, dial2( [('L', 99)], 0, Z0), writeln( z0-Z0-0)
, dial2( [('L', 100)], 0, Z1), writeln( z1-Z1-1) 
, dial2( [('L', 101)], 0, Z2), writeln( z2-Z2-1)
, dial2( [('R', 99)], 0, Z3), writeln( z3-Z3-0)
, dial2( [('R', 100)], 0, Z4), writeln( z4-Z4-1) 
, dial2( [('R', 101)], 0, Z5), writeln( z5-Z5-1) 
.

test_005 :- true
, dial2( [('L', 199)], 0, Z0), writeln( z0-Z0-1)
, dial2( [('L', 200)], 0, Z1), writeln( z1-Z1-2) 
, dial2( [('L', 201)], 0, Z2), writeln( z2-Z2-2)
, dial2( [('R', 199)], 0, Z3), writeln( z3-Z3-1)
, dial2( [('R', 200)], 0, Z4), writeln( z4-Z4-2) 
, dial2( [('R', 201)], 0, Z5), writeln( z5-Z5-2) 
.

solution_002 :- true
, phrase_from_file( parse( L), 'input.txt')
% , phrase_from_file( parse( L), 'example2.txt')
% , writeln( L)
, dial2( L, ZeroCount)
, writeln( ZeroCount)
.




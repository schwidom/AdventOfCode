:- use_module( library( dcg/basics)). % integer
:- use_module( library( dcg/high_order)). % sequence
:- use_module( library( pure_input)). % phrase_from_file( grammar, file)
:- use_module( library( ugraphs)). % top_sort, vertices_edges_to_ugraph
:- use_module( library( aggregate)). % aggregate_all
:- use_module( library( clpfd)). % for normalize

parse_field( F) --> sequence( parse_line, F), eos, !.
parse_line( L) --> string_without( `\n`, L), `\n`.

:- meta_predicate normalize( ?, ?, ?, ?).

normalize( DIRECTION, REMAINS, (ROW, COL), NORMALIZED) :- DIRECTION = 0'^, NORMALIZED #= 0 -ROW, REMAINS = ( _, COL).
normalize( DIRECTION, REMAINS, (ROW, COL), NORMALIZED) :- DIRECTION = 0'v, NORMALIZED #= 0 +ROW, REMAINS = ( _, COL).
normalize( DIRECTION, REMAINS, (ROW, COL), NORMALIZED) :- DIRECTION = 0'>, NORMALIZED #= 0 +COL, REMAINS = ( ROW, _).
normalize( DIRECTION, REMAINS, (ROW, COL), NORMALIZED) :- DIRECTION = 0'<, NORMALIZED #= 0 -COL, REMAINS = ( ROW, _).


:- dynamic field/3 . % ROW, COL, CHAR

fill_field( F) :- true
, TERM = ( nth0( ROW0, F, L), nth0( COW0, L, C))
, forall( TERM, assertz( field( ROW0, COW0, C)))
.

guard_rotation( `^>v<`).

:- table rotate_guard/2.

rotate_guard( G0, G1) :- guard_rotation( GR), append( GR, GR, GR2)
, nextto( G0, G1, GR2)
, !
.

find_guard( ROW0, COL0, C) :- guard_rotation( GR), member( C, GR), field( ROW0, COL0, C), !.

next_pos(ROW0, COL0, C0, ROW1, COL1) :- NORMALIZE = normalize( C0, _REMAINS)
, call( NORMALIZE, (ROW0, COL0), NORM0)
, call( NORMALIZE, (ROW1, COL1), NORM1)
, plus( NORM0, 1, NORM1)
.

:- dynamic stone/2.

fill_stones( F) :- true
, TERM = ( nth0( ROW0, F, L), nth0( COW0, L, C), C == 0'#)
, forall( TERM, assertz( stone( ROW0, COW0)))
.

towards_1_stone( ROW_STONE, COL_STONE, ROW0, COL0, D0) :- true
, next_pos( ROW0, COL0, D0, ROW_STONE, COL_STONE)
, field( ROW0, COL0, _)
.

bump_to_stone( ROW0, COL0, D0, ROW1, COL1) :- true
, NORMALIZE = normalize( D0, REMAINS)
, call( NORMALIZE, ( ROW0, COL0), NORM0)
, aggregate_all( min( NORM1), 
   ( REMAINS = ( ROW1_STONE, COL1_STONE), stone( ROW1_STONE, COL1_STONE)
      , call( NORMALIZE, ( ROW1_STONE, COL1_STONE), NORM1), NORM1 > NORM0)
  , NORM1MIN)
, NORM1 is NORM1MIN - 1
, call( NORMALIZE, ( ROW1, COL1), NORM1)
.

bump_to_end( ROW0, COL0, D0, ROW2, COL2) :- true
, ( next_pos( ROW0, COL0, D0, ROW1, COL1) , field( ROW1, COL1, _) )
  -> bump_to_end( ROW1, COL1, D0, ROW2, COL2)
  ; ( ROW0, COL0) = ( ROW2, COL2)
.


:- table edge/6.

edge( ROW0, COL0, D0, ROW1, COL1, D1) :- true
, stone( ROW0_STONE, COL0_STONE)
, towards_1_stone( ROW0_STONE, COL0_STONE, ROW0, COL0, D0)
, \+ stone( ROW0, COL0) % must not be on another stone
, rotate_guard( D0, D1)
, bump_to_stone( ROW0, COL0, D1, ROW1, COL1)
.

:- dynamic route_path/3 . % ROW, COL, DIRECTION

track_guard :- true
, find_guard( ROW0, COL0, D0)
, assertz( route_path( ROW0, COL0, D0))
, bump_to_stone( ROW0, COL0, D0, ROW1, COL1)
, assertz( route_path( ROW1, COL1, D0))
, track_guard( ROW1, COL1, D0)
.

track_guard( ROW0, COL0, D0) :- true
, ( edge( ROW0, COL0, D0, ROW1, COL1, D1) -> 
    ( true
    , ( route_path( ROW1, COL1, D1) -> throw( loop) ; assertz( route_path( ROW1, COL1, D1)))
    , track_guard( ROW1, COL1, D1)
    )
    ; true
    , rotate_guard( D0, D2)
    , bump_to_end( ROW0, COL0, D2, ROW2, COL2)
    , assertz( route_path( ROW2, COL2, D2))
  )
.

rbetween( B, A, C) :- AA is -A, BB is -B, between( AA, BB, CC), C is -CC.

between_row_col( (ROW, COL), (ROW, COL), RC) :- !, (ROW, COL) = RC.
between_row_col( (ROW, COL1), (ROW, COL2), RC) :- COL1 < COL2, !, (ROW, COL) = RC, between( COL1, COL2, COL).
between_row_col( (ROW, COL2), (ROW, COL1), RC) :- COL1 < COL2, !, (ROW, COL) = RC, rbetween( COL1, COL2, COL).
between_row_col( (ROW1, COL), (ROW2, COL), RC) :- ROW1 < ROW2, !, (ROW, COL) = RC, between( ROW1, ROW2, ROW).
between_row_col( (ROW2, COL), (ROW1, COL), RC) :- ROW1 < ROW2, !, (ROW, COL) = RC, rbetween( ROW1, ROW2, ROW).
between_row_col( _, _, _) :- $( false).

:- dynamic route_positions/2.
:- dynamic route_positions_uniq/2.

route_path_to_positions2 :- true
, findall( ( ROW0,COL0) , route_path( ROW0, COL0, _), L)
, TERM1 = ( nth1( I, L, E0) , nth0( I, L, E1), between_row_col( E0, E1, RC) )
, TERM2 = ( RC = (R, C), assertz( route_positions( R, C)), T = route_positions_uniq( R, C), ( T -> true ; assertz( T)))
, forall( TERM1, TERM2)
.

:- dynamic route_positions/3.
:- dynamic route_positions_uniq/3.

route_path_to_positions3 :- true
, findall( ( ROW, COL, C) , route_path( ROW, COL, C), L)
, TERM1 = ( true
   , nth1( I, L, E0), E0 = ( ROW0, COL0, _C0)
   , nth0( I, L, E1), E1 = ( ROW1, COL1, C1)
   , between_row_col( ( ROW0, COL0), ( ROW1, COL1), RC) 
   , RC \== ( ROW0, COL0)
  )
, TERM2 = ( RC = (R, C)
   , assertz( route_positions( R, C, C1)), T = route_positions_uniq( R, C, C1), ( T -> true ; assertz( T))
   )
, forall( TERM1, TERM2)
.

:- dynamic loopdata/3.

find_loop( Obstacle) :- Obstacle = ( ROW_O, COL_O, _D_O)
, $( \+ stone( ROW_O, COL_O ) ) % just in case
, $( \+ find_guard( ROW_O, COL_O, _) ) % just in case
, find_guard( ROW0, COL0, D1)
, bump_to_stone( ROW0, COL0, D1, ROW1, COL1)
, retractall( loopdata( _, _, _))
, track_loop( ROW_O, COL_O, ROW1, COL1, D1)
.

track_loop( ROW_O, COL_O, ROW0, COL0, D0) :- true
, TERM = loopdata( ROW0, COL0, D0)
, ( TERM -> true
  ; assertz( TERM)
  , ( edge( ROW0, COL0, D0, ROW1, COL1, D1) -> true
    , (
        between_row_col( ( ROW0, COL0), ( ROW1, COL1), ( ROW_O, COL_O)) % gq3xnuzuao
        -> ( true
           , next_pos( ROW2, COL2, D1, ROW_O, COL_O) % xjl49adms0
           , rotate_guard( D1, D2)
           , bump_to_stone( ROW2, COL2, D2, ROW3, COL3)
           , track_loop( ROW_O, COL_O, ROW3, COL3, D2)
           )
        ; track_loop( ROW_O, COL_O, ROW1, COL1, D1)
      )
      ; true
      , rotate_guard( D0, D1)
      , bump_to_end( ROW0, COL0, D1, ROW1, COL1)
      , between_row_col( ( ROW0, COL0), ( ROW1, COL1), ( ROW_O, COL_O)) % gq3xnuzuao
      , next_pos( ROW2, COL2, D1, ROW_O, COL_O) % xjl49adms0
      , rotate_guard( D1, D2)
      , bump_to_stone( ROW2, COL2, D2, ROW3, COL3)
      , track_loop( ROW_O, COL_O, ROW3, COL3, D2)
    )
  )
.

cleanup :- true
, abolish_all_tables
, retractall( route_path( _, _, _))
, retractall( route_positions( _, _))
, retractall( route_positions_uniq( _, _))
, retractall( route_positions( _, _, _))
, retractall( route_positions_uniq( _, _, _))
, retractall( field( _, _, _))
, retractall( stone( _, _)) % 0'#
, retractall( solution_002_results( _, _))
.


prepare( FNAME) :- true
, cleanup
, phrase_from_file( parse_field( F), FNAME)
, fill_field( F)
, fill_stones( F)
.


solution_001_demo :- solution_001_fname( 'demo_input.txt').
solution_001:- solution_001_fname( 'input.txt').

solution_001_fname( FNAME) :- true
, prepare( FNAME)
, track_guard
, route_path_to_positions2
, aggregate_all( count, route_positions_uniq( _, _), COUNT)
, writeln( positions-COUNT)
.

:- dynamic solution_002_results/2.

solution_002_demo :- solution_002_fname( 'demo_input.txt').
solution_002:- solution_002_fname( 'input.txt').

assertz_uniq( FACT) :- FACT -> true ; assertz( FACT).

solution_002_fname( FNAME) :- true
, prepare( FNAME)
, track_guard
, route_path_to_positions3
, TERM1 = ( route_positions( R, C, D), \+ find_guard( R, C, _), find_loop( ( R, C, D) ) )
, TERM2 = ( assertz_uniq( solution_002_results( R, C)) )
, forall( TERM1, TERM2)
, aggregate_all( count, solution_002_results( R, C), COUNT)
, writeln( count-COUNT)
.


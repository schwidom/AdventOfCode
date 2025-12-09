
:- set_prolog_flag(table_space, 10 737 418 240).

:- use_module( library( dcg/basics)).
:- use_module( library( pure_input)).


parse( []) --> eos, !.
parse( [H|T]) --> parse_pair( H), parse( T).

parse_pair( X-Y) --> integer(X), `,`, integer(Y), eol.

:- dynamic fgcr/2. % floor_grid_cols_rows

fill_floor( L) :- true
, retractall( fgcr( _, _))
, forall( member( C - R, L), assertz( fgcr( C, R)))
.

:- dynamic rectangles/3.

range( A, B, C) :- C is 1 + abs( A - B).

find_largest_rectangle :- true
, retractall( rectangles( _, _, _))
, F1= fgcr( C1, R1)
, F2= fgcr( C2, R2)
, TERM = ( F1, F2, range( C1, C2, CR), range( R1, R2, RR), AREA is CR * RR)
, forall( TERM, assertz( rectangles( F1, F2, AREA)))
, findall( A, rectangles(_,_,A), LA)
, max_list( LA, MAX)
, writeln( MAX)
.

window2( L, A, B) :- L = [C, D| T], ( ( A, B) = ( C, D)  ; window2( [ D| T], A, B) ).

:- dynamic rectangle_grid_c1_r1_c2_r2/4. % left lesser, right bigger

create_rectangle_grid :- true
, retractall( rectangle_grid_c1_r1_c2_r2( _, _, _, _))
, FGCR = fgcr( C, R)
, setof( C, FGCR^FGCR, LC)
, setof( R, FGCR^FGCR, LR)
% , writeln( LC)
% , writeln( LR)
, TERM = ( window2( [0| LC], C1, C2), window2( [0| LR], R1, R2))
, forall( TERM, assertz( rectangle_grid_c1_r1_c2_r2( C1, R1, C2, R2)))
.

:- dynamic rectangle_state/2. % rectangle_state( rectangle_grid_c1_r1_c2_r2( _, _, _, _), STATE)

create_rectangle_state :- true
, retractall( rectangle_state( _, _))
, create_rectangle_state( 0, 0)
.

flip_side( outside, inside).
flip_side( inside, outside).

:- table one_hash_side/4.

one_hash_side( A, B, C, D) :- forall( member( M, [A,B,C]), nonvar( M))
, flip_side( E, F)
, permutation( [E, E, E, F], [A, B, C, D])
, !
.

:- table one_no_hash_side/4.

one_no_hash_side( A, B, C, D) :- forall( member( M, [A,B,C]), nonvar( M))
, ( maplist( =(_), [A, B, C, D]), ! ; flip_side( E, F), permutation( [E, E, F, F], [A, B, C, D]), !)
.

create_rectangle_state( C1, R1) :- writeln( create_rectangle_state( C1, R1)), false.

create_rectangle_state( C1, R1) :- true
, TERM = rectangle_grid_c1_r1_c2_r2( C1, R1, C2, R2)
, TERM % nur 1x
, TERM0 = ( (C1, R1) = ( 0, 0))
, ARS = assertz( rectangle_state( TERM, SIDE)) % outside, inside
, TERM_R10 = ( R1 = 0, rectangle_state( rectangle_grid_c1_r1_c2_r2( _, R1, C1, _), SIDE_LEFT))
, TERM_C10 = ( C1 = 0, rectangle_state( rectangle_grid_c1_r1_c2_r2( C1, _, _, R1), SIDE_UP))
, TERM_3_SIDES = ( true
   , rectangle_state( rectangle_grid_c1_r1_c2_r2( _, R1, C1, _), SIDE_LEFT)
   , rectangle_state( rectangle_grid_c1_r1_c2_r2( C1, _, _, R1), SIDE_UP)
   , rectangle_state( rectangle_grid_c1_r1_c2_r2( _, _, C1, R1), SIDE_LEFT_UP)
   )
, (
   fgcr( C1, R1) 
    -> (
       TERM0 -> SIDE = inside, ARS;
       TERM_R10 -> flip_side( SIDE_LEFT, SIDE), ARS;
       TERM_C10 -> flip_side( SIDE_UP, SIDE), ARS;
       TERM_3_SIDES -> one_hash_side( SIDE_LEFT_UP, SIDE_UP, SIDE_LEFT, SIDE), ARS;
       false 
    )
    ;  (
       TERM0 -> SIDE = outside, ARS;
       TERM_R10 -> =( SIDE_LEFT, SIDE), ARS;
       TERM_C10 -> =( SIDE_UP, SIDE), ARS;
       TERM_3_SIDES -> one_no_hash_side( SIDE_LEFT_UP, SIDE_UP, SIDE_LEFT, SIDE), ARS;
       false 
    )

  )
, ignore( create_rectangle_state( C1, R2))
, ignore( create_rectangle_state( C2, R1))
.

:- table check_tiles_along_c/3.

% check_tiles_along_c( C1, R1, C2) :- writeln( check_tiles_along_c( C1, R1, C2)), false.

check_tiles_along_c( C1, R1, C2) :- true
, rectangle_state( rectangle_grid_c1_r1_c2_r2( C1, R1, C3, _), inside)
, ( 
   C3 == C2 -> true ;
   C3 > C2 -> false ;
    check_tiles_along_c( C3, R1, C2)
  )
.

% check_tiles( C1, R1, C2, R2) :- writeln( check_tiles( C1, R1, C2, R2)), false.

:- table check_tiles/4.

check_tiles( C1, R1, C2, R2) :- true
, check_tiles_along_c( C1, R1, C2)
, rectangle_state( rectangle_grid_c1_r1_c2_r2( C1, R1, _, R3), inside)
, ( 
   R3 == R2 -> true ;
   R3 > R2 -> false ;
    check_tiles( C1, R3, C2, R2)
  )
.

biggest_rectangle :- true
, reset_gensym( biggest_rectangle_)
, TERM = ( true
   , FGCR1 = fgcr( C1, R1)
   , FGCR2 = fgcr( C2, R2)
   , FGCR1
   , FGCR2
   , FGCR1 \= FGCR2
   , C1 < C2
   , msort( [R1, R2], [RX1, RX2])
   , gensym( biggest_rectangle_, GS)
   , writeln( GS)
   , check_tiles( C1, RX1, C2, RX2) % see rectangle_grid_c1_r1_c2_r2
   % , writeln( check_tiles( C1, RX1, C2, RX2))
   , AREA is ( 1 + abs( C2 - C1)) * ( 1 + abs( R2 - R1))
  )
, aggregate_all( max( AREA, check_tiles( C1, RX1, C2, RX2)), TERM, MAX)
, writeln( MAX)
.

solution_001_demo :- solution_001( 'spec_data.txt').
solution_001_input :- solution_001( 'input.txt').

solution_001( FNAME) :- true
, phrase_from_file( parse( L), FNAME)
, fill_floor( L)
, find_largest_rectangle
.

solution_002_demo :- solution_002( 'spec_data.txt').
solution_002_input :- solution_002( 'input.txt').

solution_002( FNAME) :- true
, phrase_from_file( parse( L), FNAME)
, fill_floor( L)
, create_rectangle_grid
, create_rectangle_state
, biggest_rectangle
.




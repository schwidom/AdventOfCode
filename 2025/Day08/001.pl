
:- use_module( library( pure_input)). % phrase_from_file
:- use_module( library( dcg/basics)). % eos, eol, integer
:- use_module( library( aggregate)). % aggregate, aggregate_all
:- use_module( library( apply)). % foldl

parse( []) --> eos, !.
parse( [H|T]) --> parse_box( H), parse( T).

parse_box( [I1, I2, I3]) --> integer( I1), `,`, integer(I2), `,`, integer(I3), eol.

:- dynamic grid/3.

feed_grid( L) :- true
, retractall( grid(_,_,_))
, forall( ( member( M, L), TERM=..[grid|M]), assertz( TERM))
.

minus( A, B, C) :- C is A - B.

distance_grid( GRID1, GRID2, DIST) :- GRID1 =.. [grid|PT1], GRID2 =.. [grid|PT2], distance_list3( PT1, PT2, DIST).

distance_list3( GRID1, GRID2, DIST) :- true
, maplist( minus, GRID1, GRID2, DIFF)
, DIFF = [A,B,C]
, DIST is sqrt( A**2 + B**2 + C**2)
.

:- dynamic distance/3.

grid_id( GRID, ID) :- true
, functor( GRID, grid, 3)
% , clause( GRID, true, ID) % 0.002 seconds, 4.882 seconds 
% , clause( GRID, true, _), ID = GRID % 0.005 seconds, 5.279 seconds
, GRID, ID = GRID % 0.002 seconds, 4.798 seconds
.


create_distances :- true
, reset_gensym( create_distances_count_)
, retractall( distance( _GRID1, _GRID2, _DIST))
, TERM = ( true
    , grid_id( GRID1, REF1)
    , grid_id( GRID2, REF2)
    % , REF1 \= REF2
    % , \+ distance( REF2, REF1, _) % too slow
    , REF1 @< REF2
    , distance_grid( GRID1, GRID2, DIST) 
    , gensym( create_distances_count_, CDC)
    % , aggregate_all( count, distance( _, _, _), COUNT) % too slow
    % , writeln( create_distances-count-COUNT)
    , writeln( CDC)
  )
, forall( TERM, assertz( distance( REF1, REF2, DIST)))
.

:- dynamic circuit/2.
:- dynamic circuit_same_ids_register/2.

register_same_ids( ID1, ID2) :- true
, CS1 = circuit_same_ids_register( ID1, IDX1)
, CS2 = circuit_same_ids_register( ID2, IDX2)
, AR1 = assertz( circuit_same_ids_register( ID1, ID))
, AR2 = assertz( circuit_same_ids_register( ID2, ID))
, ( 
    CS1, IDX1 = IDX2, CS2 -> true ;
    CS1, \+ CS2, IDX1 = ID -> AR2 ;
    \+ CS1, CS2, IDX2 = ID -> AR1 ;
    \+ CS1, \+ CS2 -> gensym( dist_, ID), AR1, AR2 ;
    
    CS1, CS2 -> register_same_ids( IDX1, IDX2) ;

    $( false )
  )
.

circuit_same_ids( ID1, ID2) :- true
, circuit_same_ids_register( ID1, ID1_NEXT)
, circuit_same_ids_register( ID2, ID2_NEXT)
, (
    ID1_NEXT = ID2_NEXT -> true ; 
    circuit_same_ids( ID1, ID2_NEXT) -> true ;
    circuit_same_ids( ID1_NEXT, ID2) -> true ;
    circuit_same_ids( ID1_NEXT, ID2_NEXT)
  )
.

circuit_new_id( ID, ID_NEW) :- true
, circuit_same_ids_register( ID, IDX), circuit_new_id( IDX, ID_NEW)
; true
, once( circuit_same_ids_register( _, ID)), \+ circuit_same_ids_register( ID, _), =( ID, ID_NEW)
.

circuit_size( ID_NEW, SIZE) :- true
, TERM = ( circuit_new_id( ID, ID_NEW), circuit( C, ID) )
, aggregate( count, (ID,C)^TERM, SIZE)
; TERM = ( circuit( C, ID_NEW), \+ circuit_new_id( ID_NEW, _) )
, aggregate( count, C^TERM, SIZE)
.

circuit_size_3_biggest( RES) :- true
, findall( SIZE, circuit_size( _, SIZE), L)
, sort( 0, @>=, L, LS)
, length( RES, 3)
, append( RES, _, LS)
.

connect( 0, _) :- !.
connect( COUNT, L) :- L = [DISTANCE|T]
, DISTANCE = distance( REF1, REF2, _)
, writeln( DISTANCE)
, C1 = circuit( REF1, ID1)
, C2 = circuit( REF2, ID2)
, AR1 = assertz( circuit( REF1, ID))
, AR2 = assertz( circuit( REF2, ID))
, (
   C1, \+ C2, ID1 = ID -> AR2 ;
   \+ C1, C2, ID2 = ID -> AR1 ;
   C1, ID1 = ID2, C2 -> true ;
   C1, C2, circuit_same_ids( ID1, ID2) -> true ;
   C1, C2, \+ circuit_same_ids( ID1, ID2) -> register_same_ids( ID1, ID2);
   \+ C1, \+ C2 -> gensym( dist_, ID), AR1, AR2 ;
   $( false )
  )
, COUNT_NEXT is COUNT - 1
, $
, connect( COUNT_NEXT, T)
.

connect_continue( 0, L) :- connect_continue( continued, L) , !.

connect_continue( COUNT, L) :- L = [DISTANCE|T]
, DISTANCE = distance( REF1, REF2, _)
% , writeln( DISTANCE)
, C1 = circuit( REF1, ID1)
, C2 = circuit( REF2, ID2)
, AR1 = assertz( circuit( REF1, ID))
, AR2 = assertz( circuit( REF2, ID))
, (
   C1, \+ C2, ID1 = ID -> AR2 ;
   \+ C1, C2, ID2 = ID -> AR1 ;
   C1, ID1 = ID2, C2 -> true ;
   C1, C2, circuit_same_ids( ID1, ID2) -> true ;
   C1, C2, \+ circuit_same_ids( ID1, ID2) -> register_same_ids( ID1, ID2);
   \+ C1, \+ C2 -> gensym( dist_, ID), AR1, AR2 ;
   $( false )
  )
, ( COUNT == continued -> ( true
   , aggregate_all( count, circuit_size( _, _), CS_COUNT)
   , aggregate_all( count, ( grid_id( _, GID), \+ circuit( GID, _)), G_COUNT)
   % , writeln( 'CS_COUNT' - CS_COUNT)
   % , writeln( 'G_COUNT' - G_COUNT)
   , ( 1 == CS_COUNT, 0 == G_COUNT  -> nb_setval( connect_continue_last_distance, DISTANCE) 
                                    ; connect_continue( continued, T)) 
   )
   ; ( true
   , COUNT_NEXT is COUNT - 1
   % , $
   , connect_continue( COUNT_NEXT, T) )
  )
.
   
connect_N( N, L) :- true
, retractall( circuit(_,_))
, retractall( circuit_same_ids_register( _, _))
, reset_gensym( dist_)
, connect( N, L)
.

connect_N_continue( N, L) :- true
% , write_to_file( 'file_connect_continue.txt', '')
, retractall( circuit(_,_))
, retractall( circuit_same_ids_register( _, _))
, reset_gensym( dist_)
, connect_continue( N, L)
.


prod( A, B, C) :- C is A * B.

solution_001_demo :- solution_001( 'spec_data.txt', 10).
solution_001_input :- solution_001( 'input.txt', 1000).

solution_001( FNAME, N) :- true
, phrase_from_file( parse(L), FNAME)
, writeln( phrase_from_file)
, feed_grid( L)
, writeln( feed_grid)
, create_distances
, writeln( create_distances)
, functor( DISTANCE, distance, 3)
, findall( DISTANCE, DISTANCE, L2)
, sort( 3, @=<, L2, L2S)
, connect_N( N, L2S) 
, circuit_size_3_biggest( LBIGGEST)
, writeln( LBIGGEST)
, foldl( prod, LBIGGEST, 1, PROD)
, writeln( prod-PROD)
.

diff( LI, LO) :- LI = [], !, LO = [].
diff( LI, LO) :- LI = [_], !, LO = [].
diff( LI, LO) :- LI = [H1,H2|T], !, H is H2 - H1, diff( [H2|T], TO), LO = [H|TO].

solution_002_demo :- solution_002( 'spec_data.txt', 10).
solution_002_input :- solution_002( 'input.txt', 1000).

solution_002( FNAME, N) :- true
, phrase_from_file( parse(L), FNAME)
, writeln( phrase_from_file)
, feed_grid( L)
, writeln( feed_grid)
, create_distances
, writeln( create_distances)
, functor( DISTANCE, distance, 3)
, findall( DISTANCE, DISTANCE, L2)

, sort( 3, @=<, L2, L2S)

, connect_N_continue( N, L2S)

, nb_getval( connect_continue_last_distance, DISTANCE)
, DISTANCE = distance( grid( X1, _, _), grid( X2, _, _), _)
, PROD is X1 * X2
, writeln( prod-PROD)
.


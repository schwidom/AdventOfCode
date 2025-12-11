
:- use_module( library( dcg/basics)). % string_without, blanks
:- use_module( library( pure_input)). % phrase_from_file
:- use_module( library( ugraphs)).
:- use_module( library( apply)). % foldl

parse( []) --> eos, !.
parse( [H|T]) --> parse_line( H), parse( T).

parse_line( [H|T]) --> string_without( `:`, C), { atom_codes( H, C) }, `:`, whites, parse_line_rest( T).

parse_line_rest( []) --> eol, !.
parse_line_rest( [H|T]) --> string_without( ` \n`, C), { atom_codes( H, C) }, whites, parse_line_rest( T).

:- dynamic edge/2.

delete_graph :- retractall( edge( _,_)).
insert_graph( []) :- !.
insert_graph( [H|T]) :- insert_line( H), insert_graph( T).
create_graph( L) :- delete_graph, insert_graph( L).

insert_line( [_]) :- !.
insert_line( [FROM, TOH| TOT]) :- true
, ETERM= edge( FROM, TOH)
, $( \+ ETERM)
, assertz( ETERM)
, insert_line( [FROM | TOT])
.

:- dynamic path/2. % NODE, COUNT

add_to_path_count( NAME, N) :- true
, P1 = path( NAME, COUNT)
, P2 = path( NAME, COUNT_NEW)
, ( P1 -> retract( P1), COUNT_NEW is COUNT + N ; COUNT_NEW is N)
, assertz( P2)
.

insert_path_starting_from( L) :- mytraces, writeln( insert_path_starting_from( L)), false.

insert_path_starting_from( []) :- !.
insert_path_starting_from( L) :- true
, TERM_001 = ( member( M, L), path( M, STARTNR), edge( M, M2))
, TERM_002 = add_to_path_count( M2, STARTNR)
, TERM = ( TERM_001, TERM_002)
, ( setof( M2, TERM^TERM , LM2) -> insert_path_starting_from( LM2) ; true)
.

calculate_pathcount :- true
, retractall( path( _, _))
, assertz( path( you, 1))
, insert_path_starting_from( [you])
.

% pre, id, bw, if, post ... pre dac, in dac, between, in fft, post fft
add_typed( C1, C2, C3) :- true
, append( C1, C2, CA)
, msort( CA, CAS)
, group_pairs_by_key( CAS, CASG)
, findall( K-V, ( member( K-VL, CASG), sum_list( VL, V)), C3)
.

rename_keys( FROM, TO, LI, LO) :- true
% , pairs_keys_values( LI, Ks, Vs)
, TERM = ( member(K-V, LI), ( K==FROM -> KNEW = TO ; KNEW=K))
, findall( KNEW-V, TERM, LO)
.


monadify(IN,OUT) --> [PRED], { call( PRED,IN,VALUE2) }, ( { OUT=VALUE2 } ; monadify( VALUE2,OUT) ).

add_typed( NAME, C1, C2, C3) :- true
, add_typed( C1, C2, C3PRE)
, ( 
   NAME == dac -> phrase( monadify( C3PRE, C3), [rename_keys( pre, id1), rename_keys( bw2, id2), rename_keys( if2, id2)]) ;
   NAME == fft -> phrase( monadify( C3PRE, C3), [rename_keys( pre, if2), rename_keys( bw1, if1), rename_keys( id1, if1)]) ;

       phrase( monadify( C3PRE, C3), [rename_keys( id1, bw1), rename_keys( if2, bw2), rename_keys( if1, post), rename_keys( id2, post)])

  )
, !
, ( mytraces -> writeln( add_typed( NAME, C1, C2, C3)) ; true)
.

% mytraces :- true.
mytraces :- false.

insert_path :- true
, path( M, _)
, edge( M, M2)
, \+ path( M2, _)
, findall( MBACK, edge( MBACK, M2), LMBACK)
, findall( S, ( member( E, LMBACK), path( E, S)), LS)
, same_length( LMBACK, LS)
, foldl( add_typed( M2), LS, [], COUNT_NEW)
, assertz( path( M2, COUNT_NEW))
, ( mytraces -> writeln( path( M2, COUNT_NEW)) ; true)
.

:- dynamic pathqueue/1.

calculate_pathcount_002 :- true
, retractall( path( _, _))
, assertz( path( svr, [pre-1]))
, ignore( forall( repeat , insert_path))
.


test_min_max( 1, 16).

test(001) :- create_graph( [[svr, out]]), calculate_pathcount_002, path(out, SUM), writeln( sum-SUM), SUM=[pre-1].
test(002) :- create_graph( [[svr, a, out]]), calculate_pathcount_002, path(out, SUM), writeln( sum-SUM), SUM=[pre-1].
test(003) :- create_graph( [[svr, dac], [dac, out]]), calculate_pathcount_002, path(out, SUM), writeln( sum-SUM), SUM=[bw1-1].
test(004) :- create_graph( [[svr, fft], [fft, out]]), calculate_pathcount_002, path(out, SUM), writeln( sum-SUM), SUM=[bw2-1].
test(005) :- create_graph( [[svr, dac, out], [dac, out]]), calculate_pathcount_002, path(out, SUM), writeln( sum-SUM), SUM=[bw1-1,pre-1].
test(006) :- true.
test(007) :- create_graph( [[svr, fft, out], [fft, out]]), calculate_pathcount_002, path(out, SUM), writeln( sum-SUM), SUM=[bw2-1,pre-1].
test(008) :- create_graph( [[svr, dac], [dac, a], [a,fft], [fft, out]]), calculate_pathcount_002, path(out, SUM), writeln( sum-SUM), SUM=[post-1].
test(009) :- create_graph( [[svr, dac], [dac, fft], [fft, out]]), calculate_pathcount_002, path(out, SUM), writeln( sum-SUM), SUM=[post-1].

test(010) :- create_graph( [[svr, dac,out], [dac, fft], [fft, out]]), calculate_pathcount_002, path(out, SUM), writeln( sum-SUM), SUM=[post-1,pre-1].
test(011) :- create_graph( [[svr, a], [ a, dac], [dac, fft], [fft, out]]), calculate_pathcount_002, path(out, SUM), writeln( sum-SUM), SUM=[post-1].
test(012) :- create_graph( [[svr, a,b], [b,dac],[ a, dac], [dac, fft], [fft, out]]), calculate_pathcount_002, path(out, SUM), writeln( sum-SUM), SUM=[post-2].
test(013) :- create_graph( [[svr, a,b], [b,dac],[ a, dac], [dac, n,m], [n,fft],[m,fft], [fft, out]]), calculate_pathcount_002, path(out, SUM), writeln( sum-SUM), SUM=[post-4].
test(014) :- create_graph( [[svr, a,b], [b,dac],[ a, dac], [dac, n,m], [n,fft],[m,fft], [fft, x,y], [x,out], [y,out] ]), calculate_pathcount_002, path(out, SUM), writeln( sum-SUM), SUM=[post-8].
test(015) :- create_graph( [[svr, a,b, out], [b,dac],[ a, dac], [dac, n,m], [n,fft],[m,fft], [fft, x,y], [x,out], [y,out] ]), calculate_pathcount_002, path(out, SUM), writeln( sum-SUM), permutation( SUM,[pre-1,post-8]).
test(016) :- create_graph( [[svr, a,b, out], [b,fft],[ a, fft], [fft, n,m], [n,dac],[m,dac], [dac, x,y], [x,out], [y,out] ]), calculate_pathcount_002, path(out, SUM), writeln( sum-SUM), permutation( SUM,[pre-1,post-8]).

gensym_before( BASE, RES) :- gensym( BASE, GS), atom_concat(BASE, NR, GS)
, atom_number( NR, N)
, NR_BEFORE is N - 1
, atom_concat(BASE, NR_BEFORE, RES)
.

test_all :- true
, reset_gensym( testcount_)
, reset_gensym( testcount_ok_)
, reset_gensym( testcount_fail_)
, test_min_max( FROM, TO)
, TERM = ( true
 , between( FROM, TO, B)
 , gensym( testcount_, _)
 , writeln( test(B))
 , GSNAME= test_all_variation_
 , ( reset_gensym( GSNAME), test(B) 
        *-> gensym( GSNAME, GS), gensym( testcount_ok_,_), writeln( ok-GS) 
        ; gensym( testcount_fail_, _), writeln( fail)
   )
 )
, aggregate_all( count, TERM, COUNT)
% , TESTCOUNT is 1 + TO - FROM
, gensym_before( testcount_, TESTCOUNT)
, gensym_before( testcount_ok_, TESTCOUNT_OK)
, gensym_before( testcount_fail_, TESTCOUNT_FAIL)
, writeln( TESTCOUNT)
, writeln( TESTCOUNT_OK)
, writeln( TESTCOUNT_FAIL)
, writeln( count(COUNT))
.

write_graph :- true
, open( 'graph.gv', write, FD)
, format( FD, 'digraph mygraph {~n', [])
, E = edge( N1, N2)
% , ( setof( N, E^( E, member( N, [N1,N2])), LNODES) -> true ; LNODES = [])
, ( setof( N-PI, E^( E, member( N, [N1,N2]), path( N, PI)), LNODES) -> true ; LNODES = [])
, forall( ( member( N-PI, LNODES), term_to_atom( N-PI, LABEL)), format( FD, ' ~w [ label = "~w" ] ;~n', [N, LABEL]))
, forall( E, format( FD, ' ~w -> ~w ~n', [N1, N2]))
, format( FD, '}~n', [])
, close(FD)
.

node( N) :- setof( 1, ( (N1,N2)^( edge( N1, N2), member( N, [N1,N2]))), _).

create_ugraph( U) :- true

, TERM= ( node( N1), findall( N2, edge( N1, N2), L))
, findall( N1-L, TERM, U)
.

check_graph_reachable(U) :- true
, setof( N1, (N0,N2)^(edge( N0, N1), \+ edge( N1, N2)), BOTTOMNODES)
, writeln( check_graph_reachable-bottomnodes( BOTTOMNODES))
, setof( N1, (N0,N2)^(edge( N1, N0), \+ edge( N2, N1)), TOPNODES)
, writeln( check_graph_reachable-topnodes( TOPNODES))
, TOPNODES = [TOP|_]
, reachable( TOP, U, LREACH)
, findall( E, (member( E, LREACH), \+ node(E)), L2)
, writeln( reachable_nodes_which_are_not_nodes( L2))
, findall( E, ( node(E), \+ memberchk( E, LREACH)), L3)
, writeln( nodes_which_are_not_reachable( L3))
.


check_graph :- true
, aggregate_all( count, node(_), N_COUNT)
, writeln( number_nodes( N_COUNT))
, E = edge( _, _)
, aggregate_all( count, E, E_COUNT)
, writeln( number_edges( E_COUNT))
, findall( E, ( aggregate( count, E, COUNT), COUNT> 1), LE)
, writeln( double_edges(LE))
, create_ugraph( U)
, ( top_sort( U, _) -> writeln( 'top_sort possible') ; writeln( 'top_sort impossible') )
, check_graph_reachable(U)
.

write_edges :- true
, E = edge( N1, N2)
, forall( E, format( '~w ~w ', [N1, N2])) % tsort : no warnings
, nl
.


solution_001_demo :- solution_001( 'spec_data.txt').
solution_001_input :- solution_001( 'input.txt').

solution_001( FNAME) :- true
, phrase_from_file( parse( L), FNAME)

, create_graph( L)
, calculate_pathcount
, path(out, SUM)
, writeln( sum-SUM)
.

solution_002_demo :- solution_002( 'spec_data_002.txt').
solution_002_demo_a :- solution_002( 'spec_data_002_a.txt').
solution_002_input :- solution_002( 'input.txt').

solution_002( FNAME) :- true
, phrase_from_file( parse( L), FNAME)
, create_graph( L)
, calculate_pathcount_002
, path(out, SUM)
, writeln( sum-SUM)
.

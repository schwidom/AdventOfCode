:- use_module( library( dcg/basics)). % integer
:- use_module( library( dcg/high_order)). % sequence
:- use_module( library( pure_input)). % phrase_from_file( grammar, file)
:- use_module( library( ugraphs)). % top_sort, vertices_edges_to_ugraph

parse_rules_updates( [RULES, UPDATES]) --> parse_rules( RULES), `\n`, parse_updates( UPDATES), eos, !.

parse_rules( RULES) --> sequence( parse_rule, RULES).
parse_rule( RULE) --> { RULE = A-B}, integer(A), `|`, integer( B), `\n`.

parse_updates( UPDATES) --> sequence( parse_updateline, UPDATES).
parse_updateline( UPDATE) --> sequence( integer, `,`, UPDATE), `\n`.

:- dynamic rule/2.

middle_of_list( L, E) :- length( L, LEN)
, LENH is LEN // 2
, length( START, LENH)
, length( END, LENH)
, MID=[E]
, $, append( [START, MID, END], L)
.

check_update( L) :- L = [].
check_update( L) :- L = [_], !.
check_update( L) :- true
, TERM = ( nth0( I1, L, E1), nth0( I2, L, E2), I1 < I2)
, forall( TERM, rule( E1, E2))
.

solution_001_demo :- solution_001_fname( 'demo_input.txt').
solution_001:- solution_001_fname( 'input.txt').

solution_001_fname(FNAME) :- true
, phrase_from_file( parse_rules_updates( RULES_UPDATES), FNAME)
, [ RULES, UPDATES] = RULES_UPDATES
, retractall( rule( _, _))
, forall( member( A-B, RULES), assertz( rule( A, B)))
, TERM = ( member( UPDATE, UPDATES), check_update( UPDATE), middle_of_list( UPDATE, M))
, findall( M, TERM, LIST_OF_MIDDLES)
, sum_list( LIST_OF_MIDDLES, SUM)
, writeln( sum-SUM)
.

sort_update( UPDATE, UPDATE_SORTED) :- L = UPDATE
, TERM = ( nth0( I1, L, E1), nth0( I2, L, E2), I1 < I2)
, TERM2 = ( rule( E1, E2) -> ( E1, E2) = ( F1, F2) ; rule( E2, E1) -> ( E2, E1) = ( F1, F2) ; $( false))
, findall( F1-F2, ( TERM, TERM2), EDGES) 
, vertices_edges_to_ugraph( [], EDGES, UGRAPH)
, top_sort( UGRAPH, UPDATE_SORTED)
.

solution_002:- true
, phrase_from_file( parse_rules_updates( RULES_UPDATES), 'input.txt')
, [ RULES, UPDATES] = RULES_UPDATES
, retractall( rule( _, _))
, forall( member( A-B, RULES), assertz( rule( A, B)))
, TERM = ( member( UPDATE, UPDATES), \+ check_update( UPDATE)
   , sort_update( UPDATE, UPDATE_SORTED)
   , $( check_update( UPDATE_SORTED) )
   , middle_of_list( UPDATE_SORTED, M)
  )
, findall( M, TERM, LIST_OF_MIDDLES)
, sum_list( LIST_OF_MIDDLES, SUM)
, writeln( sum-SUM)
.



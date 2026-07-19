:- use_module( library( dcg/basics)). % integer
:- use_module( library( dcg/high_order)). % sequence
:- use_module( library( pure_input)). % phrase_from_file( grammar, file)


parse_input( I) --> sequence( parse_line, I), eos, !.
parse_line( LINE) --> { LINE = (RESULT, NUMBERS) }, integer( RESULT), `: `, sequence( integer, ` `, NUMBERS), `\n`.

ops( (+)).
ops( (*)).

find_ops( RESULT, LIST_OF_NUMBERS) :- [RESULT] == LIST_OF_NUMBERS, !.
find_ops( RESULT, LIST_OF_NUMBERS) :- [H1, H2 | T] = LIST_OF_NUMBERS 
, ops( OP)
, ISTERM =.. [ OP, H1, H2]
, call( is, INTERMEDIATE, ISTERM)
, find_ops( RESULT, [ INTERMEDIATE | T ])
, !
.

ops3( A, B, C) :- ops( OP), ISTERM =.. [ OP, A, B], call( is, C, ISTERM).
ops3( A, B, C) :- atomic_concat( A, B, AB), atom_number( AB, C).

find_ops2( RESULT, LIST_OF_NUMBERS) :- [RESULT] == LIST_OF_NUMBERS, !.
find_ops2( RESULT, LIST_OF_NUMBERS) :- [H1, H2 | T] = LIST_OF_NUMBERS 
, ops3( H1, H2, INTERMEDIATE)
, find_ops2( RESULT, [ INTERMEDIATE | T ])
, !
.

solution_001_demo :- solution_001_fname( 'demo_input.txt').
solution_001:- solution_001_fname( 'input.txt').

solution_001_fname( FNAME) :- true
, phrase_from_file( parse_input(I), FNAME)
% , maplist( writeln, I)
, TERM = ( member( ( RESULT, LIST_OF_NUMBERS), I), find_ops( RESULT, LIST_OF_NUMBERS))
, findall( RESULT, TERM, L)
, sum_list( L, SUM)
, writeln( sum - SUM)
.


solution_002_demo :- solution_002_fname( 'demo_input.txt').
solution_002:- solution_002_fname( 'input.txt').

solution_002_fname( FNAME) :- true
, phrase_from_file( parse_input(I), FNAME)
% , maplist( writeln, I)
, TERM = ( member( ( RESULT, LIST_OF_NUMBERS), I), find_ops2( RESULT, LIST_OF_NUMBERS))
, findall( RESULT, TERM, L)
, sum_list( L, SUM)
, writeln( sum - SUM)
.



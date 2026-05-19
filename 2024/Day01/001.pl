
:- use_module( library( pure_input)). % phrase_from_file
:- use_module( library( dcg/basics)). % integer, whites
:- use_module( library( dcg/high_order)). % sequence

line_of_digits( ( A, B) ) --> integer(A), whites, integer(B), `\n`.
list_of_lines( L) --> sequence( line_of_digits, L), !.


split_line_tuple( (A, B), A, B).
diff( A, B, D) :- D is abs( A - B).

solution_001_demo :- true
, phrase_from_file( list_of_lines( L), "input_demo.txt")
, maplist( split_line_tuple, L, LA, LB)
% , writeln( (LA, LB))
, sort( 0, @=<, LA, LAS)
, sort( 0, @=<, LB, LBS)
, maplist( diff, LAS, LBS, LD)
, sum_list( LD, RESULT)
, writeq( RESULT)
.

solution_001:- true
, phrase_from_file( list_of_lines( L), "input.txt")
, maplist( split_line_tuple, L, LA, LB)
% , writeln( (LA, LB))
, sort( 0, @=<, LA, LAS)
, sort( 0, @=<, LB, LBS)
, maplist( diff, LAS, LBS, LD)
, sum_list( LD, RESULT)
, writeq( RESULT)
.

occurences( LBS_C_A, NUM, RES) :- get_assoc( NUM, LBS_C_A, TIMES) -> RES is TIMES * NUM ; RES = 0.

solution_002_demo :- true
, phrase_from_file( list_of_lines( L), "input_demo.txt")
, maplist( split_line_tuple, L, LA, LB)
, sort( 0, @=<, LA, LAS)
, sort( 0, @=<, LB, LBS)
, clumped( LBS, LBS_C)
, ord_list_to_assoc( LBS_C, LBS_C_A)
, maplist( occurences( LBS_C_A), LAS, LSA_O)
, sum_list( LSA_O, RES)
, writeln( RES)
.

solution_002 :- true
, phrase_from_file( list_of_lines( L), "input.txt")
, maplist( split_line_tuple, L, LA, LB)
, sort( 0, @=<, LA, LAS)
, sort( 0, @=<, LB, LBS)
, clumped( LBS, LBS_C)
, ord_list_to_assoc( LBS_C, LBS_C_A)
, maplist( occurences( LBS_C_A), LAS, LSA_O)
, sum_list( LSA_O, RES)
, writeln( RES)
.


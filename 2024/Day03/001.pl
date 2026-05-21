

:- use_module( library( 'pure_input')). % phrase_from_file
:- use_module( library( 'dcg/basics')). % string_without
:- use_module( library( 'dcg/high_order')). % sequence

match_mul(MUL) --> `mul(`, integer(I1), `,`, integer(I2), `)`, !, { MUL=mul( I1, I2) }.
match_do(DO) --> `do()`, !, { DO=do() }.
match_dont(DONT) --> `don't()`, !, { DONT=dont() }. % '

filter_data( MUL) --> string(_), match_mul(MUL), !.

filter_data_list( LMUL) --> sequence( filter_data, LMUL), !, remainder(_).

filter_data2( OP) --> string(_), ( match_mul(OP) ; match_do(OP); match_dont(OP)), !.
filter_data2_list( LOPS) --> sequence( filter_data2, LOPS), !, remainder(_).


mul( A, B, C) :- C is A*B.

solution_001_gen(FNAME) :- true
, phrase_from_file( filter_data_list( LMUL), FNAME)
, maplist( call, LMUL, PL)
, sum_list( PL, SUM)
, writeln( SUM)
.

solution_001_demo :- true
, solution_001_gen( 'input_demo.txt')
.

solution_001:- true
, solution_001_gen( 'input.txt')
.

filter_ops( LMUL) --> filter_ops( true, LMUL).

filter_ops( _, LMUL) --> [do()], !, ( filter_ops( true, LMUL), !; !).
filter_ops( _, LMUL) --> [dont()], !, ( filter_ops( false, LMUL), !; !).
filter_ops( ON, LMUL) --> [H], !, ( filter_ops( ON, T); { T=[]}), !, { ON -> LMUL=[H|T] ; LMUL = T }.

solution_002_gen(FNAME) :- true
, phrase_from_file( filter_data2_list( LOPS), FNAME)
, phrase( filter_ops( LMUL), LOPS)
, maplist( call, LMUL, PL)
, sum_list( PL, SUM)
, writeln( SUM)
.

solution_002_demo :- true
, solution_002_gen( 'input_demo2.txt')
.

solution_002:- true
, solution_002_gen( 'input.txt')
.



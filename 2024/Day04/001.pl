:- use_module( library( 'pure_input')). % phrase_from_file( grammar, file)
:- use_module( library( 'dcg/basics')). % string_without
:- use_module( library( 'dcg/high_order')). % sequence
:- use_module( library( 'clpfd')). % transpose

line( L) --> string_without( `\n`, L), `\n`.
lines( L) --> sequence( line, L), !.

xmas( `XMAS`).

dynamic_predicates( char_index/2).

fill_database( LINES) :- true
, forall( dynamic_predicates(DP), ( abolish( DP), dynamic( DP) ) )
, xmas( XMAS)
, TERM1 = ( nth0( ROW, LINES, LINE), nth0( COL, LINE, CHAR), memberchk( CHAR, XMAS), IDX_R_C = ( ROW, COL))
, TERM2 = ( assertz( char_index( CHAR, IDX_R_C)) )
, forall( TERM1, TERM2)
.

direction8( IDX_R_C) :- IDX_R_C = ( ROW, COL)
, FACTORS=[-1,0,1]
, member( ROW, FACTORS)
, member( COL, FACTORS)
, IDX_R_C \= ( 0, 0)
.

stretch8( IDX_R_C_OFFSET, IDX_R_C, L) :- IDX_R_C_OFFSET = ( ROW_O, COL_O), IDX_R_C = ( ROW, COL)
, TERM = ( between( 1, 3, F), R is ROW_O + F*ROW, C is COL_O + F*COL)
, findall( (R, C), TERM, L)
.

check_mas( IDX_R_C, COUNT) :- true
, xmas( [_|MAS])
, TERM1 = ( direction8( D) , stretch8( IDX_R_C, D, IDX_R_C_LIST),
   transpose( [IDX_R_C_LIST,MAS], IDX_R_C__CHAR__LIST))
, TERM2 = forall( member( [IDX_R_C_INNER, CHAR], IDX_R_C__CHAR__LIST), 
   char_index( CHAR, IDX_R_C_INNER))
, findall( 1, ( TERM1, TERM2), L)
, length( L, COUNT)
.

count_database(COUNT) :- true
, xmas( [X|_])
, TERM = ( char_index( X, IDX_R_C), check_mas( IDX_R_C, COUNT) )
, findall( COUNT, TERM, L)
, sum_list(L, COUNT)
.

solution_001_gen( FNAME) :- true
, phrase_from_file( lines( LINES), FNAME)
, fill_database( LINES)
, count_database(COUNT)
, writeln( COUNT)
.


solution_001_demo :- solution_001_gen( 'input_demo.txt').
solution_001:- solution_001_gen( 'input.txt').

edges_rot( IDX_R_C) :- member( IDX_R_C, [(-1, -1), (-1, 1), (1, 1), (1, -1)]).

idx_r_c_add( (ROW1, COL1), (ROW2, COL2), (ROW3, COL3)) :- true
, ROW3 is ROW2 + ROW1
, COL3 is COL2 + COL1
.

pick_edges_rot_values( IDX_R_C_OFFSET, CHARS) :- true
, TERM = ( edges_rot( IDX_R_C), idx_r_c_add( IDX_R_C_OFFSET, IDX_R_C, IDX_R_C_TO_PICK), char_index( CHAR, IDX_R_C_TO_PICK))
, findall( CHAR, TERM, CHARS)
.

check_m_a_s_cross( IDX_R_C) :- true
, xmas( [_,M,_,S])
, pick_edges_rot_values( IDX_R_C, CHARS)
, append( CHARS, CHARS, CHARS2) 
, append( [_,[M,M,S,S],_], CHARS2), !
.

count_database2(COUNT) :- true
, xmas( [_,_,A,_])
, TERM = ( char_index( A, IDX_R_C), check_m_a_s_cross( IDX_R_C) )
, findall( 1, TERM, L)
, sum_list(L, COUNT)
.


solution_002_gen( FNAME) :- true
, phrase_from_file( lines( LINES), FNAME)
, fill_database( LINES)
, count_database2(COUNT)
, writeln( COUNT)
.


solution_002_demo :- solution_002_gen( 'input_demo.txt').
solution_002:- solution_002_gen( 'input.txt').




:- use_module( library( dcg/basics)). % integer, string
:- use_module( library( dcg/high_order)). % sequence
:- use_module( library( pure_input)). % phrase_from_file(:Grammar, +File) 

myeol --> ( `\r\n`; `\n`).

integer_line( L) --> sequence( integer, (white, whites), L), myeol, !.
integer_lines( L) --> sequence( integer_line, L), eos, !.

minmax_diff( [A,B], MIN, MAX) :- DIFF is B - A, between( MIN, MAX, DIFF).

minmax_diff_list( [A,B], MIN, MAX) :- !, minmax_diff( [A,B], MIN, MAX).
minmax_diff_list( [A,B|T], MIN, MAX) :- minmax_diff( [A,B], MIN, MAX), minmax_diff_list( [B|T], MIN, MAX).

list_inc_or_dec_by_1_3( L) :- minmax_diff_list( L, 1, 3) ; minmax_diff_list( L, -3, -1).

solution_001_gen(FNAME) :- true
, phrase_from_file( integer_lines( L), FNAME)
, findall( true, ( member( E, L), list_inc_or_dec_by_1_3( E)), RES)
, length( RES, LEN)
, writeln( LEN)
.

create_err_state_maxerr_1( 1, 0, _).
check_err_state( MAXERR, ERRIN, ERROUT) :- var( ERROUT), ERRIN =< MAXERR.
end_err_state( _MAXERR, ERRIN, ERROUT) :- =(ERRIN, ERROUT).

c3( NAME, TUPLE) :- TUPLE=(A,B,C), call( NAME, A, B, C).

inc_err_state( (MAXERR, ERRIN, ERROUT), (MAXERR, ERRMID, ERROUT)) :- ERRMID is 1 + ERRIN.

% don't call it directly, use minmax_diff_list_maxerr_start
minmax_diff_list_maxerr( [A,B], MIN, MAX, ERR_STATE) :- !
, c3( check_err_state, ERR_STATE)
, (
   minmax_diff( [A,B], MIN, MAX) , c3( end_err_state, ERR_STATE), !
   ; true
    , inc_err_state( ERR_STATE, ERR_STATE_END)
    , c3( check_err_state, ERR_STATE_END)
    , c3( end_err_state, ERR_STATE_END)
  )
.

% don't call it directly, use minmax_diff_list_maxerr_start
minmax_diff_list_maxerr( [A,B|T], MIN, MAX, ERR_STATE) :- true
, c3(check_err_state, ERR_STATE)
, (
   minmax_diff( [A,B], MIN, MAX), minmax_diff_list_maxerr( [B|T], MIN, MAX, ERR_STATE) , !
   ; true
    , inc_err_state( ERR_STATE, ERR_STATE_NEXT)
    , T = [C|T2]
    , minmax_diff_list_maxerr( [A,C|T2], MIN, MAX, ERR_STATE_NEXT), !
  )
.

minmax_diff_list_maxerr_start( [A,B|T], MIN, MAX, ERR_STATE) :- true
, c3( create_err_state_maxerr_1, ERR_STATE)
,( true
   , minmax_diff_list_maxerr( [A,B|T], MIN, MAX, ERR_STATE), !
   ; true
   , inc_err_state( ERR_STATE, ERR_STATE_NEXT)
   , minmax_diff_list_maxerr( [B|T], MIN, MAX, ERR_STATE_NEXT), !
 )
.


list_inc_or_dec_by_1_3_maxerr_1( L) :- true
, ( minmax_diff_list_maxerr_start( L, 1, 3, _ERR_STATE) ; minmax_diff_list_maxerr_start( L, -3, -1, _ERR_STATE) )
.

solution_002_gen(FNAME) :- true
, phrase_from_file( integer_lines( L), FNAME)
, findall( true, ( member( E, L), list_inc_or_dec_by_1_3_maxerr_1( E)), RES)
, length( RES, LEN)
, writeln( LEN)
.

solution_001_demo :- solution_001_gen( 'input_demo.txt').
solution_001:- solution_001_gen( 'input.txt').

solution_002_demo :- solution_002_gen( 'input_demo.txt').
solution_002:- solution_002_gen( 'input.txt').



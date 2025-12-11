:- set_prolog_flag(table_space, 10737418240).

:- set_prolog_flag( answer_write_options, [quoted(true), portray(true), max_depth(50), spacing(next_argument)]).
:- set_prolog_flag( backtrace_goal_depth, 20).


:- use_module( library( dcg/basics)). % blanks
:- use_module( library( pure_input)). % phrase_from_file
:- use_module( library( apply)). % foldl
:- use_module( library( janus)). % py_call/1, py_call/2

parse( []) --> eos, !.
parse( [H|T]) --> parse_line( H), parse( T).

parse_line( [LIGHTS,BUTTONS,JOLTAGE]) --> parse_lights( LIGHTS), blanks, parse_buttons_list( BUTTONS), blanks, parse_joltage( JOLTAGE), blanks.

parse_lights( LIGHTS) --> `[`, parse_lights2(LIGHTS), `]`.

parse_light( A) --> [C], { memberchk( C, `.#`), atom_codes( A, [C])}.
parse_lights2( [H]) --> parse_light( H).
parse_lights2( [H|T]) --> parse_light( H), parse_lights2( T).

parse_buttons_list( [H]) --> parse_buttons(H).
parse_buttons_list( [H|T]) --> parse_buttons(H), blanks, parse_buttons_list( T).

parse_buttons( BUTTONS) --> `(`, parse_buttons2(BUTTONS), `)`.
parse_buttons2( [H]) --> integer(H).
parse_buttons2( [H|T]) --> integer(H), `,`, parse_buttons2(T).

parse_joltage( BUTTONS) --> `{`, parse_joltage2(BUTTONS), `}`.
parse_joltage2( [H]) --> integer(H).
parse_joltage2( [H|T]) --> integer(H), `,`, parse_joltage2(T).

lights_to_integer( [H], I) :- ( H='#' -> I=1 ; H='.' -> I=0 ; $( false )), !.
lights_to_integer( [H|T], I) :- lights_to_integer( [H], I1), lights_to_integer( T, I2), I is I1 + I2*2.

buttons_to_integer( [H], I) :- I is 2 ** H, !.
buttons_to_integer( [H|T], I) :- I1 is 2 ** H, buttons_to_integer( T, I2), I is I1 + I2.

combn( _, 0, []) :- !.

combn( Individuals, N, L) :- true
, N > 0, !
, NNext is N - 1
, append( _, [M|IRT], Individuals)
, L=[M|LT]
, combn( IRT, NNext, LT)
.

xor( A, B, C) :- C is A xor B.

find_presses( LIGHTS_INTEGER, BUTTONS_INTEGER, PRESSES) :- true
, length( BUTTONS_INTEGER, LEN)
, between( 0, LEN, B)
% , writeln( b-B)
, combn( BUTTONS_INTEGER, B, L)
% , writeln( l-L)
, foldl( xor, L, LIGHTS_INTEGER, RES)
% , writeln( res-RES)
, RES = 0
, !
, PRESSES = B
.

test_001 :- true
, find_presses( 1, [1], 1)
, find_presses( 1, [1,1], 1)
, find_presses( 1, [2,3], 2)
, find_presses( 2, [2,3], 1)
.

solution_001_demo :- solution_001( 'spec_data.txt').
solution_001_input :- solution_001( 'input.txt').

solution_001( FNAME) :- true
, phrase_from_file( parse(L), FNAME), !
% , length( L, LEN)
% , writeln( len-LEN)
, TERM = ( member( M, L), M = [ LIGHTS, BUTTONS, _JOLTAGE]
   , lights_to_integer( LIGHTS, LIGHTS_INTEGER)
   , maplist( buttons_to_integer, BUTTONS, BUTTONS_INTEGER)
   )

, ( true 
     -> ( true
     , aggregate_all( sum(PRESSES), ( TERM, find_presses( LIGHTS_INTEGER, BUTTONS_INTEGER, PRESSES)), SUM)
     , writeln( sum-SUM)
     )
     
     ; ( true
     , TERM
     , writeln( LIGHTS_INTEGER - BUTTONS_INTEGER)
     , find_presses( LIGHTS_INTEGER, BUTTONS_INTEGER, PRESSES)
     , writeln( PRESSES)
     )
  )

.

add_button_joltage( BUTTON, JOLTAGE_I, JOLTAGE_O) :- true
, TERM = ( nth0( IDX, JOLTAGE_I, E), ( memberchk( IDX, BUTTON) -> E_NEW is E + 1 ; E_NEW is E))
, findall( E_NEW, TERM, JOLTAGE_O)
.

sub_button_joltage( BUTTON, JOLTAGE_I, JOLTAGE_O) :- true
, TERM = ( nth0( IDX, JOLTAGE_I, E), ( memberchk( IDX, BUTTON) -> E_NEW is E - 1 ; E_NEW is E))
, findall( E_NEW, TERM, JOLTAGE_O)
.

:- table find_joltage/5.
% :- table find_joltage/4 as private.
% :- table find_joltage/4 as subsumptive.
% :- table find_joltage( min, _, _, _).

% find_joltage( PRESSES, START_JOLTAGE, BUTTONS, JOLTAGE) :- writeln( find_joltage( PRESSES, START_JOLTAGE, BUTTONS, JOLTAGE)), false.

% :- table button_b1_le_b2/2.

button_b1_le_b2( B1, B2) :- B1 \= B2 , ! ; button_b1_lt_b2( B1, B2).

% :- table button_b1_lt_b2/2.

button_b1_lt_b2( B1, B2) :- B1 = [], !, B2 \= [].

button_b1_lt_b2( B1, B2) :- length( B1, B1LEN), length( B2, B2LEN)
, (
   B1LEN > B2LEN -> true ;
   B1LEN < B2LEN -> false ;
    B1 @< B2
  )
.

% :- table button_sort_pred/3.

button_sort_pred( Delta, B1, B2) :- true
, ( 
   button_b1_lt_b2( B1, B2) -> Delta = '<' ;
   button_b1_lt_b2( B2, B1) -> Delta = '>' ;
   $( false )
  )
.

% :- table button_sort/2.

button_sort( BL1, BL2) :- predsort( button_sort_pred, BL1, BL2).

% too slow
find_joltage( PRESSES, START_JOLTAGE, _, BUTTONS, JOLTAGE) :- PRESSES < 0, !, ( START_JOLTAGE, BUTTONS, JOLTAGE) = _, false .
find_joltage( PRESSES, START_JOLTAGE, _, BUTTONS, JOLTAGE) :- PRESSES = 0, !, START_JOLTAGE=JOLTAGE, BUTTONS=_.
find_joltage( PRESSES, START_JOLTAGE, _, BUTTONS, JOLTAGE) :- maplist( '>', START_JOLTAGE, JOLTAGE), !, ( PRESSES, BUTTONS) = _, false .
find_joltage( PRESSES, START_JOLTAGE, LAST_BUTTON, BUTTONS, JOLTAGE) :- true

% , joltage_diff( START_JOLTAGE, JOLTAGE, JOLTAGE_DIFF)
% , maplist( potential_keyed( JOLTAGE_DIFF), BUTTONS, BUTTONS_LKP)
% , sort( 0, @>=, BUTTONS_LKP, BUTTONS_LKP_S)
% , pairs_keys_values( BUTTONS_LKP_S, _, BUTTONS_S)
, =( BUTTONS, BUTTONS_S)

, member( BUTTON, BUTTONS_S)

% , LAST_BUTTON @=< BUTTON

, button_b1_le_b2( LAST_BUTTON, BUTTON)

% , writeln( buttons_s-BUTTONS_S)
, add_button_joltage( BUTTON, START_JOLTAGE, START_JOLTAGE_NEXT)
, PRESSES_NEXT is PRESSES - 1
, find_joltage( PRESSES_NEXT, START_JOLTAGE_NEXT, BUTTON, BUTTONS, JOLTAGE)
, !
.

find_joltage( BUTTONS, JOLTAGE, PRESSES) :- true
% , flatten( BUTTONS, BUTTONS_FLAT)
% , max_list( BUTTONS_FLAT, MAX)
% , LEN is MAX + 1
% , length( START_JOLTAGE, LEN)
, same_length( JOLTAGE, START_JOLTAGE)
, maplist( =(0), START_JOLTAGE)
% , writeln( START_JOLTAGE- BUTTONS- JOLTAGE)
, between( 0, inf, PRESSES)
% , writeln( presses-PRESSES)
% , joltage_diff( START_JOLTAGE, JOLTAGE, JOLTAGE_DIFF)

% , maplist( length_keyed, BUTTONS, LEN_BUTTONS)
% , sort( 0, @>=, LEN_BUTTONS, LEN_BUTTONS_S)
% , pairs_keys_values( LEN_BUTTONS_S, _, BUTTONS_S)

% , sort( 0, @=<, BUTTONS, BUTTONS_S)
, button_sort( BUTTONS, BUTTONS_S)
, writeln( buttons_s-BUTTONS_S)
% kein unterschied bzgl. BUTTONS feststellbar
, LAST_BUTTON=[]
, find_joltage( PRESSES, START_JOLTAGE, LAST_BUTTON, BUTTONS_S, JOLTAGE) 
, !
.

minus( A, B, C) :- C is A - B.

joltage_diff( START_JOLTAGE, JOLTAGE, DIFF) :- maplist( minus, JOLTAGE, START_JOLTAGE, DIFF).

% :- table length_keyed/2.

length_keyed( L, LEN-L) :- length( L, LEN).

% :- table joltage_button_factor/3.

joltage_button_factor( JOLTAGE, BUTTON, FACTOR) :- true
, findall( E, ( member( IDX, BUTTON), nth0( IDX, JOLTAGE, E)), L)
, min_list( L, MIN)
, length( L, LEN)
, FACTOR is MIN * LEN
.

% :- table potential_keyed/2.

potential_keyed( JOLTAGE_DIFF, L, KEY-L) :- true
, joltage_button_factor( JOLTAGE_DIFF, L, FACTOR)
, length( L, LEN)
% , KEY = (LEN,FACTOR)
, KEY = (FACTOR, LEN)
.


solution_002_demo :- solution_002( 'spec_data.txt').
solution_002_input :- solution_002( 'input.txt').

solution_002( FNAME) :- true % too slow
, abolish_all_tables
, phrase_from_file( parse(L), FNAME), !
, TERM = ( true
   , member( M, L), M = [ _LIGHTS, BUTTONS, JOLTAGE]
   , writeln( find_joltage( BUTTONS, JOLTAGE, PRESSES))
   , find_joltage( BUTTONS, JOLTAGE, PRESSES)
   , writeln( find_joltage( BUTTONS, JOLTAGE, PRESSES))
  )
, aggregate_all( sum( PRESSES), TERM, SUM)
, writeln( sum-SUM)
.

:- use_module( library( clpfd)).

% clpfd_sum( L, SUM) :- L = [], !, SUM = 0.
% clpfd_sum( L, SUM) :- L = [H|T], clpfd_sum( T, SREST), SUM = H + SREST.

% add_button_joltage
% joltage_button_factor
max_button_joltage( JOLTAGE, BUTTON, FACTOR) :- true
, findall( E, ( member( IDX, BUTTON), nth0( IDX, JOLTAGE, E)), L)
, min_list( L, MIN)
% , length( L, LEN)
, FACTOR is MIN 
.

calculate_min( BUTTONS, JOLTAGE, PRESSES) :- true
, same_length( BUTTONS, VARS)
% , VARS ins 0..10 % educated guess

, TERM_PRE = ( nth0( BIDX, BUTTONS, BUTTON), nth0( BIDX, VARS, VAR), max_button_joltage( JOLTAGE, BUTTON, FACTOR))
, bagof( VAR in 0..FACTOR, (BIDX,BUTTON)^TERM_PRE, L_PRE)
, maplist( call, L_PRE)

, TERM = ( true
   , nth0( JIDX, JOLTAGE, JOLTAGE_VALUE)
   , TERM_002 = ( true
     , nth0( BIDX, BUTTONS, BUTTON)
     , memberchk( JIDX, BUTTON)
     , nth0( BIDX, VARS, BVAR)
     )
     , bagof( BVAR , (BIDX,BUTTON)^TERM_002, BVAR2)
   )
  
, bagof( sum( BVAR2, #= , JOLTAGE_VALUE), (TERM_002,BIDX,BUTTON,JIDX,JOLTAGE_VALUE)^TERM, L)
, writeln( vars-VARS)
, writeln( l-L)
, maplist( call, L)
, sum( VARS , #=, SUM)
, aggregate_all( min( SUM), ( labeling( [], [SUM|VARS]), writeln( VARS)), PRESSES)
.

solution_002_a_demo :- solution_002_a( 'spec_data.txt').
solution_002_a_input :- solution_002_a( 'input.txt'). % too slow

solution_002_a( FNAME) :- true 
, abolish_all_tables
, phrase_from_file( parse(L), FNAME), !
, TERM = ( true
   , member( M, L), M = [ _LIGHTS, BUTTONS, JOLTAGE]
   , writeln( calculate_min( BUTTONS, JOLTAGE, PRESSES))
   , $( calculate_min( BUTTONS, JOLTAGE, PRESSES) )
   , writeln( calculate_min( BUTTONS, JOLTAGE, PRESSES))
  )
, aggregate_all( sum( PRESSES), TERM, SUM)
, writeln( sum-SUM)
.

calculate_min2( BUTTONS, JOLTAGE, PRESSES) :- true % kommt nicht ans Ziel
, same_length( BUTTONS, VARS)
% , VARS ins 0..10 % educated guess

, TERM_PRE = ( nth0( BIDX, BUTTONS, BUTTON), nth0( BIDX, VARS, VAR), max_button_joltage( JOLTAGE, BUTTON, FACTOR))
, bagof( VAR in 0..FACTOR, (BIDX,BUTTON)^TERM_PRE, L_PRE)
, maplist( call, L_PRE)

, TERM = ( true
   , nth0( JIDX, JOLTAGE, JOLTAGE_VALUE)
   , TERM_002 = ( true
     , nth0( BIDX, BUTTONS, BUTTON)
     , memberchk( JIDX, BUTTON)
     , nth0( BIDX, VARS, BVAR)
     )
     , bagof( BVAR , (BIDX,BUTTON)^TERM_002, BVAR2)
   )
  
, bagof( sum( BVAR2, #= , JOLTAGE_VALUE), (TERM_002,BIDX,BUTTON,JIDX,JOLTAGE_VALUE)^TERM, L)
% , bagof( BVAR2, (TERM_002,BIDX,BUTTON,JIDX,JOLTAGE_VALUE,BVAR2LEN)^( TERM, length( BVAR2, BVAR2LEN), BVAR2LEN=< 5), BVAR2_5GROUPS)
, writeln( vars-VARS)
, writeln( l-L)
, maplist( call, L)
, sum( VARS , #=, SUM)
% , BVAR2_5GROUPS= [BVAR2_5GROUP|_] % kann failen
% , setof( BVAR2_5GROUP, ( label( BVAR2_5GROUP), writeln( 'BVAR2_5GROUP'-BVAR2_5GROUP)), BVAR2_5GROUP_LIST)
, setof( SUM, ( labeling( [], [SUM]), writeln( sum1-SUM)), SUMLIST)
, aggregate_all( min( SUM), ( member( SUM, SUMLIST), writeln( sum2-SUM), labeling( [], [SUM|VARS]), writeln( VARS), !), PRESSES)
.

fill_gensym( BASE, LIST) :- findall( M, ( member( M, LIST), gensym( BASE, M)), LIST).

write_vars( VARS) :- true
, TERM = ( member( M, VARS))
, TERM_002 = ( format( '~w = Int(\'~w\')~n', [M, M]) )
, forall( TERM, TERM_002)
.

write_vars( MySolver, VARS) :- true
, TERM = ( member( M, VARS))
, TERM_002 = ( py_call( MySolver:add_int( M)))
, forall( TERM, TERM_002)
.

write_l_pre( L_PRE) :- true
, TERM = ( member( M, L_PRE), M = ( VAR in A..B ))
, TERM_002 = ( format( 'solver.add( ~w >= ~w ) ~n', [ VAR, A] ), format( 'solver.add( ~w >= ~w ) ~n', [ B, VAR] ))
, forall( TERM, TERM_002)
.

write_l_pre( MySolver, L_PRE) :- true
, TERM = ( member( M, L_PRE), M = ( VAR in A..B ))
, TERM_002 = ( py_call( MySolver:add_ge( VAR, A)), py_call( MySolver:add_ge( B, VAR)))
, forall( TERM, TERM_002)
.

z3_sumlist( [H], H) :- !.
z3_sumlist( [H1,H2|T], H) :- true
, z3_sumlist( [H2|T], T2)
, atom_concat( H1, '+', M)
, atom_concat( M, T2, H)
.


write_sums( L) :- true
, TERM = ( member( M, L), M = sum( SUML, #=, VALUE), z3_sumlist( SUML, Z3SUM))
, TERM_002 = ( format( 'solver.add( ~w == ~w )\n', [VALUE, Z3SUM]))
, forall( TERM, TERM_002)
.

write_sums( MySolver, L) :- true
, TERM = ( member( M, L), M = sum( SUML, #=, VALUE))
, TERM_002 = ( py_call( MySolver:add_eq_list( VALUE, SUML)))
, forall( TERM, TERM_002)
.

% als z3 subroutine für calculate_min3
calculate_min3_a( BUTTONS, JOLTAGE, PRESSES) :- true

, py_call( z3solver:'MySolver'(), MySolver)

, reset_gensym( z3_button_)
, same_length( BUTTONS, VARS) % z3
, fill_gensym( z3_button_, VARS)
% , writeln( VARS)
, write_vars( MySolver, VARS)
, TERM_PRE = ( nth0( BIDX, BUTTONS, BUTTON), nth0( BIDX, VARS, VAR), max_button_joltage( JOLTAGE, BUTTON, FACTOR))
, bagof( VAR in 0..FACTOR, (BIDX,BUTTON)^TERM_PRE, L_PRE)
% , writeln( l_pre-L_PRE)
, write_l_pre( MySolver, L_PRE)

, TERM = ( true
   , nth0( JIDX, JOLTAGE, JOLTAGE_VALUE)
   , TERM_002 = ( true
     , nth0( BIDX, BUTTONS, BUTTON)
     , memberchk( JIDX, BUTTON)
     , nth0( BIDX, VARS, BVAR)
     )
     , bagof( BVAR , (BIDX,BUTTON)^TERM_002, BVAR2)
   )
  
, bagof( sum( BVAR2, #= , JOLTAGE_VALUE), (TERM_002,BIDX,BUTTON,JIDX,JOLTAGE_VALUE)^TERM, L)
% , writeln( vars-VARS)
% , writeln( l-L)
, write_sums( MySolver, L)
% , writeln( write_sums( MySolver, [sum( VARS , #=, PRESSES)]))
, write_sums( MySolver, [sum( VARS , #=, PRESSES)])
, INFO = PRESSES
, py_call( MySolver:solve(INFO), RES)
, py_free( MySolver)
, RES = @true
.

calculate_min3( BUTTONS, JOLTAGE, PRESSES) :- true 
, same_length( BUTTONS, VARS)
% , VARS ins 0..10 % educated guess

, TERM_PRE = ( nth0( BIDX, BUTTONS, BUTTON), nth0( BIDX, VARS, VAR), max_button_joltage( JOLTAGE, BUTTON, FACTOR))
, bagof( VAR in 0..FACTOR, (BIDX,BUTTON)^TERM_PRE, L_PRE)
, maplist( call, L_PRE)

, TERM = ( true
   , nth0( JIDX, JOLTAGE, JOLTAGE_VALUE)
   , TERM_002 = ( true
     , nth0( BIDX, BUTTONS, BUTTON)
     , memberchk( JIDX, BUTTON)
     , nth0( BIDX, VARS, BVAR)
     )
     , bagof( BVAR , (BIDX,BUTTON)^TERM_002, BVAR2)
   )
  
, bagof( sum( BVAR2, #= , JOLTAGE_VALUE), (TERM_002,BIDX,BUTTON,JIDX,JOLTAGE_VALUE)^TERM, L)
% , bagof( BVAR2, (TERM_002,BIDX,BUTTON,JIDX,JOLTAGE_VALUE,BVAR2LEN)^( TERM, length( BVAR2, BVAR2LEN), BVAR2LEN=< 5), BVAR2_5GROUPS)
, writeln( vars-VARS)
, writeln( l-L)
, maplist( call, L)
, sum( VARS , #=, SUM)
% , BVAR2_5GROUPS= [BVAR2_5GROUP|_] % kann failen
% , setof( BVAR2_5GROUP, ( label( BVAR2_5GROUP), writeln( 'BVAR2_5GROUP'-BVAR2_5GROUP)), BVAR2_5GROUP_LIST)
, setof( SUM, ( labeling( [], [SUM]), writeln( sum1-SUM)), SUMLIST)
% , aggregate_all( min( SUM), ( member( SUM, SUMLIST), writeln( sum2-SUM), labeling( [], [SUM|VARS]), writeln( VARS), !), PRESSES)
, findall( PRESSES, ( member( PRESSES, SUMLIST), calculate_min3_a( BUTTONS, JOLTAGE, PRESSES), !), [PRESSES])
.



solution_002_b_demo :- solution_002_b( 'spec_data.txt').
solution_002_b_input :- solution_002_b( 'input.txt'). % worked, but slow

solution_002_b( FNAME) :- true
, abolish_all_tables
, phrase_from_file( parse(L), FNAME), !
, TERM = ( true
   , member( M, L), M = [ _LIGHTS, BUTTONS, JOLTAGE]
   , writeln( calculate_min3( BUTTONS, JOLTAGE, PRESSES))
   , $( time( calculate_min3( BUTTONS, JOLTAGE, PRESSES)) )
   , writeln( calculate_min3( BUTTONS, JOLTAGE, PRESSES))
  )
, aggregate_all( sum( PRESSES), TERM, SUM)
, writeln( sum-SUM)
.


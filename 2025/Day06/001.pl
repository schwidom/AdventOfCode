
:- use_module( library( dcg/basics)).
:- use_module( library( pure_input)).
:- use_module( library( clpfd)). % transpose
:- use_module( library( apply)). % foldl
:- use_module( library( yall)). % >>

parse( [H|T]) --> parse_integers( H), !, parse( T).
parse( [L]) --> parse_operators( L), !, eos.

parse_integers( []) --> blanks_to_nl.
parse_integers( [H|T]) --> whites, integer(H), parse_integers( T).

parse_operators( []) --> blanks_to_nl.
parse_operators( [H|T]) --> whites, [C], { memberchk( C, `+ *`), atom_codes( H, [C])}, parse_operators( T).

parse2( [L]) --> parse_operators( L), !, eos, !.
parse2( [H|T]) --> parse_digit_lol( H), !, parse2( T).

parse_digit_lol( []) --> blanks_to_nl, !.
parse_digit_lol( [H|T]) --> whites, parse_digit_list(H), parse_digit_lol( T).

parse_digit_list( [H|T]) --> digit(D), !, { number_codes( H, [D])}, parse_digit_list( T).
parse_digit_list( []) --> [].

parse3( [L]) --> parse_operators( L), !, eos.
% parse3( [H|T]) --> string_without( `\n`, C), { atom_codes( H, C)}, eol, parse3( T).
parse3( [H|T]) --> string_without( `\n`, H), eol, parse3( T).

op_null_element( '*', 1).
op_null_element( '+', 0).

calculate3( Op, Is, RES) :- true
, op_null_element( Op, NULL)
, foldl( {Op}/[A,B,C]>>( F =.. [Op,A,B], C is F), Is, NULL, RES)
.

solution_001 :- true
% , phrase_from_file( parse( L), 'spec_data.txt')
, phrase_from_file( parse( L), 'input.txt')
, append( Is, [Ops], L)
, !
, transpose( Is, IsT)
, maplist( calculate3, Ops, IsT, RES)
, sum_list( RES, SUM)
, writeln( sum-SUM)
.

phrase_parse_integers( Is, Codes) :- phrase( parse_integers( Is), Codes), !.


group( IsI, GROUP_START, GROUP, IsO) :- IsI = [], !
, ( GROUP_START \== GROUP -> IsO=[GROUP_START], GROUP=[] ; IsO=[], (GROUP_START, GROUP)=_)
.

group( IsI, GROUP_START, GROUP, IsO) :- IsI = [HI|TI]
, ( HI = [] , GROUP_START \== GROUP -> GROUP = [], IsO = [GROUP_START|TO], group( TI, GS, GS, TO) ;
    HI = [] -> group( TI, GROUP_START, GROUP, IsO) ;
    GROUP = [HI|GT], group( TI, GROUP_START, GT, IsO)
  )
.

group( IsI, IsO) :- group( IsI, GS, GS, IsO).

split( IsI, IsO) :- group( IsI, IsM), maplist( flatten, IsM, IsO).

solution_002 :- true
% , phrase_from_file( parse3( L), 'spec_data.txt')
, phrase_from_file( parse3( L), 'input.txt')
, writeln( l-L)
, append( ICodes, [Ops], L)
, !
, transpose( ICodes, ICodesT)
, writeln( icodest-ICodesT)
, maplist( phrase_parse_integers, Is, ICodesT)
, writeln( is-Is)
, split( Is, IsS)
, writeln( iss-IsS)
, writeln( ops-Ops)
, maplist( calculate3, Ops, IsS, RES)
, sum_list( RES, SUM)
, writeln( sum-SUM)
.



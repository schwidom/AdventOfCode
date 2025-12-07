
:- use_module( library( dcg/basics)).

parse( [(A-B)|T]) --> integer(A) ,`-`, integer(B), parse_comma(T).
parse_comma( L) --> `,`, !, parse(L).
parse_comma( []) --> `\n`, [].

integer_digits( I, Ds) :- integer(I), !
, atom_chars( I, Cs)
, maplist( atom_number, Cs, Ds)
.

integer_digits( I, Ds) :- ground( Ds)
, maplist( atom_number, Cs, Ds)
, atom_chars( A, Cs)
, atom_number( A, I)
.

solution_001 :- true
% , phrase_from_file( parse(L), 'spec_data.txt')
, phrase_from_file( parse(L), 'input.txt')
, TERM= ( member( A-B, L), between( A, B, C), integer_digits( C, Cs), append( Ds, Ds, Cs))
, findall( C, TERM, LC)
, writeln( LC)
, sum_list( LC, SUM)
, writeln( SUM)
.

solution_002 :- true
% , phrase_from_file( parse(L), 'spec_data.txt')
, phrase_from_file( parse(L), 'input.txt')
, aggregate_all( max(BLEN), ( member( _-B, L), integer_digits( B, Bs), length( Bs, BLEN)), MAX_LEN)
% , writeln( MAX_LEN) % 10
, TERM_002= ( between( 2, MAX_LEN, L2LEN), length( L2, L2LEN), maplist( =(_), L2), append( L2, Cs))
, TERM= ( member( A-B, L), between( A, B, C), integer_digits( C, Cs), once( TERM_002))
, findall( C, TERM, LC)
, writeln( LC)
, sum_list( LC, SUM)
, writeln( SUM)
.




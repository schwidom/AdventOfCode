
:- use_module( library( dcg/basics)). % 
:- use_module( library( pure_input)). % 

parse( Ranges, Ingredients) --> parse_ranges(Ranges), eol, parse_ingredients( Ingredients), eos, !.

parse_ranges( [F-T|Tail]) --> integer(F), `-`, integer(T), eol, parse_ranges( Tail).
parse_ranges( []) --> [].

parse_ingredients( [I|T]) --> integer( I), eol, parse_ingredients( T).
parse_ingredients( []) --> [].

solution_001 :- true % takes too long
% , phrase_from_file( parse( Rs,Is), 'spec_data.txt')
, phrase_from_file( parse( Rs,Is), 'input.txt') % too long, see solution_001_a
, TERM= ( member( F-T, Rs), between( F, T, B))
, setof( B, TERM^TERM, FRESHNUMBERS)
, writeln( FRESHNUMBERS)
, TERM_002= ( member( I, Is), memberchk( I, FRESHNUMBERS))
, aggregate_all( count, TERM_002, COUNT)
, writeln( count-COUNT)
.

:- use_module( library( clpfd)).

clpfd_or( [], D) :- D= 0 .. -1 .
clpfd_or( [H|T], D) :- clpfd_or( T, D2), D = H \/ D2.

solution_001_a :- true
% , phrase_from_file( parse( Rs,Is), 'spec_data.txt')
, phrase_from_file( parse( Rs,Is), 'input.txt') 
, TERM= ( member( F-T, Rs), $( F =< T ), B = F..T)
, findall( B, TERM, BL)
, clpfd_or( BL, FRESHNUMBERS_DOMAIN)
, writeln( FRESHNUMBERS_DOMAIN)
, TERM_002= ( member( I, Is), I in FRESHNUMBERS_DOMAIN)
, aggregate_all( count, TERM_002, COUNT)
, writeln( count-COUNT)
.

solution_002 :- true % too long
% , phrase_from_file( parse( Rs, _Is), 'spec_data.txt')
, phrase_from_file( parse( Rs, _Is), 'input.txt') % too long, see solution_002_a
, TERM= ( member( F-T, Rs), $( F =< T ), B = F..T)
, findall( B, TERM, BL)
, clpfd_or( BL, FRESHNUMBERS_DOMAIN)
, writeln( FRESHNUMBERS_DOMAIN)
, TERM_002= ( I in FRESHNUMBERS_DOMAIN, label([I]))
, aggregate_all( count, TERM_002, COUNT)
, writeln( count-COUNT)
.

solution_002_a :- true 
% , phrase_from_file( parse( Rs, _Is), 'spec_data.txt')
, phrase_from_file( parse( Rs, _Is), 'input.txt') % 
, TERM= ( member( F-T, Rs), $( F =< T ), B = F..T)
, findall( B, TERM, BL)
, clpfd_or( BL, FRESHNUMBERS_DOMAIN)
, I in FRESHNUMBERS_DOMAIN
, fd_size(I,D) 
, writeln( count-D)
.


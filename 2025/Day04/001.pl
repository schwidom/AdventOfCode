
:- use_module( library( dcg/basics)).
:- use_module( library( pure_input)).
:- use_module( library( aggregate)).

parse( []) --> eos, ! .
parse( [Line|T]) --> parse_line( Line), parse( T).

parse_line( []) --> eol, !.
parse_line( [C|T]) --> [C], { $( memberchk( C, `.@`))}, parse_line( T).

:- dynamic grid/3.


insert_line_into_grid( CountRows, CountCols, Chars) :- Chars = [], !, ( CountRows, CountCols) = _.
insert_line_into_grid( CountRows, CountCols, Chars) :- Chars = [ H|T], !, CountCols_Next is CountCols + 1
, atom_codes( A, [H])
, assertz( grid( CountRows, CountCols, A))
, insert_line_into_grid( CountRows, CountCols_Next, T)
.

insert_into_grid( Count, Lines) :- Lines = [], !, Count = _.
insert_into_grid( Count, Lines) :- Lines = [H|T], !, Count_Next is Count + 1
, insert_line_into_grid( Count, 0, H)
, insert_into_grid( Count_Next, T)
.

insert_into_grid( Lines) :- true
, retractall( grid(_,_,_))
, insert_into_grid( 0, Lines)
.

:- use_module( library( clpfd)).

adjacent_8(RI, CI, RO, CO) :- $( integer( RI)), $( integer( CI))
, [R,C] ins -1..1
, RO #= RI + R
, CO #= CI + C
, OUTS=[RO,CO]
, label( OUTS)
, \+ maplist( =(0), [R,C])
.

% grid_free( R, C) :- \+ grid( R, C, _), ! ; grid( R, C, '.') .
% grid_adjacents_free( R, C) :- true
% forall( adjacent_8(R, C, RO, CO), grid_free( RO, CO))
% .

grid_has_paper( R, C) :- grid( R, C, @).
grid_fewer_than_four_papers_adjacent( R, C) :- true
, TERM = ( adjacent_8(R, C, RO, CO), grid_has_paper( RO, CO))
, aggregate_all( count, TERM, Count)
, Count < 4
.

count_accessible_rolls( Count) :- true
, TERM= ( grid( R, C, '@'), grid_fewer_than_four_papers_adjacent( R, C)) % wtebz0qgke
, aggregate_all( count, TERM, Count)
.

solution_001 :- true
% , phrase_from_file( parse( G), 'spec_data.txt')
, phrase_from_file( parse( G), 'input.txt')
, insert_into_grid( G)
, count_accessible_rolls( Count)
, writeln( Count)
.

remove_papers_1_step( Count) :- true
, GRID = grid( R, C, '@')
, TERM= ( GRID, grid_fewer_than_four_papers_adjacent( R, C)) % wtebz0qgke
, TERM_002 = ( retract( GRID))
, aggregate_all( count, ( TERM, TERM_002), Count)
.

remove_papers( Count) :- true
, remove_papers_1_step( Count1)
, ( Count1 \= 0 -> ( Count = Count1 ; remove_papers( Count)))
.


solution_002 :- true
% , phrase_from_file( parse( G), 'spec_data.txt')
, phrase_from_file( parse( G), 'input.txt')
, insert_into_grid( G)
, TERM = ( remove_papers( Count), writeln( Count))
, aggregate_all( sum(Count), TERM, SUM)
, writeln( SUM)
.


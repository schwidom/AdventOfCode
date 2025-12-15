
:- use_module( library( dcg/basics)).
:- use_module( library( pure_input)). % phrase_from_file


parse( (SHAPELIST, PRESENTPLACE)) --> []
, parse_shapelist( SHAPELIST)
, parse_presentplace( PRESENTPLACE), eos, !.

parse_shapelist( [H1,H2|T]) --> parse_shape_with_header( H1), parse_shapelist( [H2|T]).
parse_shapelist( [H1]) --> parse_shape_with_header( H1).

parse_shape_header( I) --> integer(I), `:`, `\n`.

parse_shape_with_header( O) --> parse_shape_header( I), parse_shape( S), { O = I-S}.

parse_shape( [H1]) --> parse_shape_line( H1), `\n`, !.
parse_shape( [H1,H2|T]) --> parse_shape_line( H1) , parse_shape( [H2|T]).


parse_shape_char( [H]) --> [C], { memberchk( C, `#.`), atom_codes( H, [C])}.

parse_shape_line( [H1]) --> parse_shape_char( [H1]), `\n`, !.
parse_shape_line( [H1,H2|T]) --> parse_shape_char( [H1]), parse_shape_line( [H2|T]).

parse_presentplace( [H1]) --> parse_presentline( H1).
parse_presentplace( [H1,H2|T]) --> parse_presentline( H1), parse_presentplace( [H2|T]).

parse_presentline( (( WIDE,LONG),SLOTS)) --> integer( WIDE), `x`, integer(LONG), `: ` , parse_slotlist( SLOTS).

parse_slotlist( [I1]) --> integer(I1), `\n`, !.
parse_slotlist( [I1,I2|T]) --> integer(I1), ` `, parse_slotlist( [I2|T]).


% see clumped
unclumped( PAIRLIST, LIST) :- PAIRLIST=[], !, LIST=[].
unclumped( PAIRLIST, LIST) :- PAIRLIST= [ K-V|T]
, findall( K, between( 1, V, _), L)
, unclumped( T, TL)
, append( L, TL, LIST)
.

:- dynamic shape/2.

create_shapes_db( L) :- true
, must_be( ground, L)
, S = shape( IDX0, E)
, retractall( S)
, forall( member( IDX0-E, L), assertz( S))
.

shape_width_height( S, WIDTH, HEIGHT) :- length( S, HEIGHT), S=[L|_], length( L, WIDTH).

:- dynamic presentplace/3.

create_presentplace_db( PRESENTPLACE) :- true
, PP = presentplace( W, H, L)
, retractall( PP)
, forall( member( ((W, H), L), PRESENTPLACE), assertz( PP))
.

expected_density( W, H, SHAPES, DENSITY) :- true
, maplist( shape_nonpoints, SHAPES, NP)
, sum_list( NP, SUMNP)
, A is W * H
, DENSITY is SUMNP / A
.

select_shape_ids( L, SHAPEIDS) :- true
, length( L, LEN)
, LEN0 is LEN - 1
, numlist( 0, LEN0, SHAPEIDXs)
, pairs_keys_values( CLUMPED, SHAPEIDXs, L)
, unclumped( CLUMPED, UNCLUMPED)
, SHAPEIDS = UNCLUMPED
.

select_shapes( COUNTS, SHAPES) :- select_shape_ids( COUNTS, IDs), maplist( shape, IDs, SHAPES).

expected_density_from_counts( W, H, COUNTS, DENSITY) :- true
, select_shapes( COUNTS, SHAPES)
, expected_density( W, H, SHAPES, DENSITY)
.

shape_nonpoints( S, NP) :- true
, must_be( ground, S)
, TERM = ( member( L, S), member( E, L), E \='.')
, aggregate_all( count, TERM, NP)
.

solution_001_input_a :- true
, FNAME = 'input.txt'
, DATA = (SHAPELIST, PRESENTPLACE)
, phrase_from_file( parse( DATA), FNAME)

, create_shapes_db( SHAPELIST)
, create_presentplace_db( PRESENTPLACE)
, PP = presentplace( W, H, COUNTS)

, TERM = ( expected_density_from_counts( W, H, COUNTS, DENSITY))
, aggregate_all( count, ( PP, TERM, DENSITY < 1) , COUNT )
, writeln( count-COUNT)
.



:- use_module( library( dcg/basics)). % 
:- use_module( library( pure_input)). % phrase_from_file

% parse_beam_char( C) --> [C], { $( memberchk( C, `.S^`) ) }.
parse_beam_char( A) --> [C], { $( memberchk( C, `.S^`) ), atom_codes( A, [C]) }.

parse( []) --> eos, !.
parse( [H|T]) --> parse_line( H), parse( T).

parse_line( []) --> eol, !.
parse_line( [H|T]) --> parse_beam_char( H), parse_line( T).

:- dynamic grid/3.

feed_grid( L) :- true
, retractall( grid( _, _, _))
, TERM= ( true
    , nth0( ROWIDX, L, ROW)
    , nth0( COLIDX, ROW, COLELEMENT)
    % , memberchk( COLELEMENT, ['S','^'])
    % , writeln( ROWIDX-COLIDX-COLELEMENT)
  )
, forall( TERM, assertz( grid( ROWIDX, COLIDX, COLELEMENT)))
.

split( R, Cs, CsNEW, MatchLen) :- true
, findall( C, grid( R, C, '^'), CsSplit)
, subtract( Cs, CsSplit, Nomatch)
, intersection( Cs, CsSplit, Match)
, length( Match, MatchLen)
, TERM = ( member( M, Match), ( N is M - 1; N is M + 1), grid( R, N, _))
, findall( N, TERM, NL)
, union( Nomatch, NL, CsNEWU)
, sort( CsNEWU, CsNEW)
% , writeln( union( Nomatch, NL, CsNEW))
.

split_timelines( R, Cs, CsNEW, MatchLen) :- true
, findall( C, grid( R, C, '^'), CsSplit)
, subtract( Cs, CsSplit, Nomatch)
, intersection( Cs, CsSplit, Match)
, length( Match, MatchLen)
, TERM = ( member( M, Match), ( N is M - 1; N is M + 1), grid( R, N, _))
, findall( N, TERM, NL)
, union( Nomatch, NL, CsNEWU)
, sort( CsNEWU, CsNEWS)
% , writeln( union( Nomatch, NL, CsNEW))
, member( CsNEWSM, CsNEWS)
, CsNEW = [CsNEWSM]
.

search_splitters( R, Cs, RES_CURRENT, RES) :- true
, R_NEXT is R + 1
, ( \+ grid( R_NEXT, _, _) -> =( RES_CURRENT, RES) ; true
   , split( R_NEXT, Cs, CsNEW, MatchLen)
   , RES_CURRENT_NEXT is MatchLen + RES_CURRENT
   , search_splitters( R_NEXT, CsNEW, RES_CURRENT_NEXT, RES)
  )
.

search_splitters( RES) :- grid( R, C, 'S'), search_splitters( R, [C], 0, RES) .

search_splitters_timelines( R, Cs, RES_CURRENT, RES) :- true
, R_NEXT is R + 1
, ( \+ grid( R_NEXT, _, _) -> =( RES_CURRENT, RES) ; true
   , split_timelines( R_NEXT, Cs, CsNEW, MatchLen)
   , RES_CURRENT_NEXT is MatchLen + RES_CURRENT
   , search_splitters_timelines( R_NEXT, CsNEW, RES_CURRENT_NEXT, RES)
  )
.

search_splitters_timelines( RES) :- grid( R, C, 'S'), search_splitters_timelines( R, [C], 0, RES) .

split_timelines2( R, CsTl, CsNEW, MatchLen) :- true
, findall( C, grid( R, C, '^'), CsSplit)
% , pairs_keys_values( CsTl, Cs, Tl)
, pairs_keys_values( CsTlSplit, CsSplit, _)
, subtract( CsTl, CsTlSplit, Nomatch)
, intersection( CsTl, CsTlSplit, Match)
, length( Match, MatchLen)
, TERM = ( member( M-Tl, Match), ( N is M - 1; N is M + 1), grid( R, N, _))
, findall( N-Tl, TERM, NL)
% , union( Nomatch, NL, CsNEWU)
, append( Nomatch, NL, CsNEWA)
% , writeln( 'CsNEWA'-CsNEWA) % [6-1,8-1]
, msort( CsNEWA, CsNEWAS)
, group_pairs_by_key( CsNEWAS, CsNEWASG)
, findall( Cs2-Tl, ( member( Cs2-TlL, CsNEWASG), sum_list( TlL, Tl)), CsNEW)
% , sort( CsNEWU, CsNEW)
% , writeln( union( Nomatch, NL, CsNEW))
.

search_splitters_timelines2( R, CsTl, RES) :- true
, R_NEXT is R + 1
, ( \+ grid( R_NEXT, _, _) -> true
    , pairs_keys_values( CsTl, _, Timelines)
    , sum_list( Timelines, RES)

   ; true
   , split_timelines2( R_NEXT, CsTl, CsNEW, _MatchLen)
   % , RES_CURRENT_NEXT is MatchLen + RES_CURRENT
   , search_splitters_timelines2( R_NEXT, CsNEW, RES)
  )
.

search_splitters_timelines2( RES) :- grid( R, C, 'S'), search_splitters_timelines2( R, [C-1], RES) .

solution_001 :- true
% , phrase_from_file( parse(L), 'spec_data.txt')
, phrase_from_file( parse(L), 'input.txt')
% , writeln( L)
, feed_grid( L)
, search_splitters( RES)
, writeln( RES)
.

tests_001 :- true
, phrase_from_file( parse(L), 'spec_data.txt')
, feed_grid( L)
, split( 2, [7], [6,8], 1)
.

solution_002 :- true % takes too long, works only with testdata
, phrase_from_file( parse(L), 'spec_data.txt')
% , phrase_from_file( parse(L), 'input.txt')
% , writeln( L)
, feed_grid( L)
, aggregate_all( count, search_splitters_timelines( _), COUNT)
, writeln( COUNT)
.

solution_002_a :- true
% , phrase_from_file( parse(L), 'spec_data.txt')
, phrase_from_file( parse(L), 'input.txt')
% , writeln( L)
, feed_grid( L)
, search_splitters_timelines2( RES)
, writeln( RES)
.

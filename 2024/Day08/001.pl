
:- use_module( library( dcg/basics)). % integer, string_without
:- use_module( library( dcg/high_order)). % sequence
:- use_module( library( pure_input)). % phrase_from_file( grammar, file)
:- use_module( library( aggregate)). % aggregate_all

parse_input( I) --> sequence( parse_line, I), eos, !.
parse_line( L) --> string_without(`\n`, L), `\n`, !.

traverse_input( INPUT, GOAL, ROWIDX0, COLIDX0, CHAR) :- true
, TERM_ROW = nth0( ROWIDX0, INPUT, LINE)
, TERM_COL = nth0( COLIDX0, LINE, CHAR)
, forall( TERM_ROW, forall( TERM_COL, GOAL))
.

:- dynamic field/3. % ROWIDX0, COLIDX0, CHAR
:- dynamic antennas/3. % ROWIDX0, COLIDX0, CHAR
:- dynamic antinodes/2. % ROWIDX0, COLIDX0
:- dynamic antinodes2/2. % ROWIDX0, COLIDX0

:- table rows_cols/2.

rows_cols( ROWS, COLS) :- true
, TERM = field( R, C, _)
, setof( R, TERM^TERM, ROWS)
, setof( C, TERM^TERM, COLS)
.

cleanup :- true
, abolish_all_tables
, retractall( field( _, _, _))
, retractall( antennas( _, _, _))
, retractall( antinodes( _, _))
, retractall( antinodes2( _, _))
.

assertz_uniq( FACT) :- FACT -> true ; assertz( FACT).

calculate_antinodes :- true
, TERM_1 = ( antennas( AR, AC, X), antennas( BR, BC, X), ( AR, AC) \== ( BR, BC) )
, TERM_2 = ( double_range( AR, AC, BR, BC, CR, CC), field( CR, CC, _CHAR)) % antinode can be located on another antenna
, TERM_3 = assertz_uniq( antinodes( CR, CC))
, forall( ( TERM_1, TERM_2), TERM_3)
.

double_range_chain( AR, AC, BR, BC, CR2, CC2) :- true
, double_range( AR, AC, BR, BC, CR, CC) , field( CR, CC, CHAR)
, ( [CHAR] == `.`, (CR2, CC2) = ( CR, CC) % this time an antinode can't be located on another antenna
  ; true
  , double_range_chain( BR, BC, CR, CC, CR2, CC2)
  )
.

calculate_antinodes2 :- true
, TERM_1 = ( antennas( AR, AC, X), antennas( BR, BC, X), ( AR, AC) \== ( BR, BC) )
, TERM_2 = ( double_range_chain( AR, AC, BR, BC, CR, CC)) % this time an antinode can't be located on another antenna
, TERM_3 = assertz_uniq( antinodes2( CR, CC))
, forall( ( TERM_1, TERM_2), TERM_3)
.

double_range( A, B, C) :- C is B + B - A.
double_range( AR, AC, BR, BC, CR, CC) :- true
, double_range( AR, BR, CR)
, double_range( AC, BC, CC)
.

solution_001_demo :- solution_001_fname( 'demo_input.txt').
solution_001:- solution_001_fname( 'input.txt').

solution_001_fname( FNAME) :- true
, cleanup
, phrase_from_file( parse_input(INPUT), FNAME)
% , maplist( writeln, I)
, GOAL1 = assertz( field( ROWIDX0, COLIDX0, CHAR))
, GOAL2 = ( [CHAR] \== `.` -> assertz( antennas( ROWIDX0, COLIDX0, CHAR)); true)
, traverse_input( INPUT, (GOAL1, GOAL2) , ROWIDX0, COLIDX0, CHAR)
, calculate_antinodes
, aggregate_all( count, antinodes( _, _), COUNT)
, writeln( antinodes-COUNT)
.

show_template( GOAL, ROW, COL) :- true
, rows_cols( ROWS, COLS)
, TERMROW = ( member( ROW, ROWS), forall( TERMCOL, true))
, TERMCOL = ( member( COL, COLS), GOAL)
, forall( TERMROW, nl)
.

show_antinodes :- true
, GOAL1 = ( antennas( ROW, COL, CHAR) -> CHAR2 = CHAR ; 
   antinodes( ROW, COL) -> [CHAR2] = `#` ;
   [CHAR2] = `.`
  )
, GOAL2 = format( '~c', [CHAR2])
, show_template( ( GOAL1, GOAL2), ROW, COL)
.

solution_002_demo :- solution_002_fname( 'demo_input.txt').
solution_002:- solution_002_fname( 'input.txt').

solution_002_fname( FNAME) :- true
, cleanup
, phrase_from_file( parse_input(INPUT), FNAME)
% , maplist( writeln, I)
, GOAL1 = assertz( field( ROWIDX0, COLIDX0, CHAR))
, GOAL2 = ( [CHAR] \== `.` -> assertz( antennas( ROWIDX0, COLIDX0, CHAR)); true)
, traverse_input( INPUT, (GOAL1, GOAL2) , ROWIDX0, COLIDX0, CHAR)
, calculate_antinodes2
, aggregate_all( count, antinodes2( _, _), COUNT1)
, aggregate_all( count, antennas( _, _, _), COUNT2)
, COUNT is COUNT1 + COUNT2
, writeln( antinodes2-COUNT)
.

show_antinodes2 :- true
, GOAL1 = ( antennas( ROW, COL, CHAR) -> CHAR2 = CHAR ; 
   antinodes2( ROW, COL) -> [CHAR2] = `#` ;
   [CHAR2] = `.`
  )
, GOAL2 = format( '~c', [CHAR2])
, show_template( ( GOAL1, GOAL2), ROW, COL)
.

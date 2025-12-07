
:- use_module( library( dcg/basics)).
:- use_module( library( pure_input)). % phrase_from_file

parse_digit_per_line( []) --> eol, !.
parse_digit_per_line( [D|T]) --> digit( C), { atom_codes( A, [C]), atom_number( A, D)}, parse_digit_per_line(T).

parse_lines( []) --> eos, !.
parse_lines( [H|T]) --> parse_digit_per_line( H), parse_lines( T).

integer_digits( I, Ds) :- integer(I), !
, atom_chars( I, Cs)
, maplist( atom_number, Cs, Ds)
.

integer_digits( I, Ds) :- ground( Ds)
, maplist( atom_number, Cs, Ds)
, atom_chars( A, Cs)
, atom_number( A, I)
.

% debug
% max_jolt_per_bank( Bank, UpperDigit, LEN, MAX) :- writeln( max_jolt_per_bank( Bank, UpperDigit, LEN, MAX)), false .

max_jolt_per_bank( Bank, UpperDigit, LEN, MAX) :- LEN = 0, !, Bank = _, UpperDigit= _, MAX = [].

max_jolt_per_bank( Bank, UpperDigit, LEN, MAX) :- true
, Bank = [BH|BT]
, (
   BH == UpperDigit -> MAX = [UpperDigit|MAX_NEXT], LEN_NEXT is LEN - 1, Bank_NEXT = BT 
    , max_jolt_per_bank( Bank_NEXT, LEN_NEXT, MAX_NEXT)

     ;

    max_jolt_per_bank( BT, LEN, MAX)
  )

.

% debug
% max_jolt_per_bank( Bank, LEN, MAX) :- writeln( max_jolt_per_bank( Bank, LEN, MAX)), false.

max_jolt_per_bank( Bank, LEN, MAX) :- LEN = 0, !, Bank = _, MAX=[].
max_jolt_per_bank( Bank, LEN, MAX) :- true

, LEN0 is LEN - 1
, length( L0, LEN0)
, append( Bank0, L0, Bank)

% , sort( 0, @>=, Bank0, UpperDigits)
, max_list( Bank0, UpperDigit)
, max_jolt_per_bank( Bank, UpperDigit, LEN, MAX)
.

solution_001 :- true
% , phrase_from_file( parse_lines(L), 'spec_data.txt')
, phrase_from_file( parse_lines(L), 'input.txt')
% , writeln( L)
, findall( MAX,  ( member( LINE, L), max_jolt_per_bank( LINE, 2, MAX)), MAXBANKS)
% , writeln( MAXBANKS) % [[9,8],[8,1],[9,2]]
, maplist( integer_digits, Is, MAXBANKS)
% , writeln( Is)
, sum_list( Is, SUM)
, writeln( sum-SUM)
.

solution_002 :- true
% , phrase_from_file( parse_lines(L), 'spec_data.txt')
, phrase_from_file( parse_lines(L), 'input.txt')
% , writeln( L)
, findall( MAX,  ( member( LINE, L), max_jolt_per_bank( LINE, 12, MAX)), MAXBANKS)
% , writeln( MAXBANKS) % [[9,8],[8,1],[9,2]]
, maplist( integer_digits, Is, MAXBANKS)
% , writeln( Is)
, sum_list( Is, SUM)
, writeln( sum-SUM)
.

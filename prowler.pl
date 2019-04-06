/***************************************************************************
 *   prowler.pl                               Version 20190405.211933      *
 *                                                                         *
 *   Text adventure                                                        *
 *   Copyright (C) 2019         by Ruben Carlo Benante                     *
 ***************************************************************************
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License.        *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************
 *   To contact the author, please write to:                               *
 *   Ruben Carlo Benante                                                   *
 *   Email: rcb@beco.cc                                                    *
 *   Webpage: http://www.beco.cc                                           *
 *   Phone: +55 (81) 3184-7555                                             *
 ***************************************************************************/

/* ---------------------------------------------------------------------- */
/* Files, dynamic clauses, modules, etc. */
/**
 * @ingroup GroupUnique
 * @brief Defining dynamic clauses
 * @param[in] A List of clauses
 * @retval TRUE on success.
 * @retval FALSE on fail.
 */
:- dynamic([verbosecounter/1, have/1, here/1, dead/0, done/0, thing/2]).

/* ---------------------------------------------------------------------- */
/* Facts and Rules */
/**
 * @ingroup GroupUnique
 * @brief main is det
 * @details Initializes memory and other tasks
 * @retval TRUE If it can prove main.
 * @retval FALSE If main can't be proven.
 */
main :-
    respond(['Initializing...']),
    initialize,
    explain,
    gameloop,
    !.

initialize :-
    retractall(have(_)),
    retractall(local(_)),
    retractall(dead),
    retractall(done),
    assert(have([])),
    assert(local([1,1])),
    put_things.

explain :-
    respond(['Move with go north, go south, go east or go west']).

% main loop of the game
gameloop :-
    repeat,
    whereami,
    whatishere,
    exec_command,
    check_dead,
    gameover.

% return true if the game is over
gameover :-
    dead ; done.

check_dead :-
    local(X),
    at(X, dwater),
    respond(['You died in the deep waters of the Ekofiume river']),
    assert(dead).

whereami :-
    local(Pos),
    respond(['Your position is ', Pos]),
    getat(Pos, Place),
    namesq(Place, Name),
    respond(['You are at the ', Name]).

whatishere :-
    local(Pos),
    getat(Pos, Place),
    respond(['Here you can see: ']),
    whatlist(Place).

whatlist(Place) :-
    listhouses(Place),
    listpeople(Place),
    listthings(Place),
    nl.

listhouses(Place) :-
    house(Place, House),
    namesq(House, Name),
    write(Name),
    write(', '),
    fail.

listhouses(_).

listpeople(Place) :-
    people(Place, Person),
    namesq(Person, Name),
    write(Name),
    write(', '),
    fail.

listpeople(_).

listthings(Place) :-
    thing(Place, Thing),
    namesq(Thing, Name),
    write(Name),
    write(', '),
    fail.

listthings(_).

exec_command :-
    respond(['What do you want to do?']),
    get_command(Comm),
    nl,
    respond([Comm]),
    do(Comm).

do(go(X)) :-
    go(X), !.
do(talk(X)) :-
    talk(X), !.
do(take(X)) :-
    take(X), !.
do(drop(X)) :-
    drop(X), !.
do(eat(X)) :-
    eat(X), !.
do(fight(X)) :-
    fight(X), !.
do(inspect(X)) :-
    inspect(X), !.
do(explore(X)) :-
    explore(X), !.
do(enter(X)) :-
    enter(X), !.
do(run(X)) :-
    run(X), !.
do(rest(X)) :-
    rest(X), !.
do(play(X)) :-
    play(X), !.
do(list(X)) :-
    list(X), !.
do(opens(X)) :-
    opens(X), !.
do(closes(X)) :-
    closes(X), !.
do(quit(X)) :-
    quit(X), !.
do(help(X)) :-
    help(X), !.
do(_) :-
    respond(['Sorry, I didn\'t understand that.']), !.

% the real actions go here
go([H|_]) :-
    respond(['You are trying to go ', H]),
    trymove(H),
    !.

go(_).

trymove(north) :-
    local([X0, Y0]),
    Y1 is Y0 + 1,
    Npos = [X0, Y1],
    accessible([X0, Y0], Npos),
    respond(['You moved north']),
    retractall(local(_)),
    assert(local(Npos)),
    !.

trymove(south) :-
    local([X0, Y0]),
    Y1 is Y0 - 1,
    Npos = [X0, Y1],
    accessible([X0, Y0], Npos),
    respond(['You moved south']),
    retractall(local(_)),
    assert(local(Npos)),
    !.

trymove(east) :-
    local([X0, Y0]),
    X1 is X0 + 1,
    Npos = [X1, Y0],
    accessible([X0, Y0], Npos),
    respond(['You moved east']),
    retractall(local(_)),
    assert(local(Npos)),
    !.

trymove(west) :-
    local([X0, Y0]),
    X1 is X0 - 1,
    Npos = [X1, Y0],
    accessible([X0, Y0], Npos),
    respond(['You moved west']),
    retractall(local(_)),
    assert(local(Npos)),
    !.

trymove(_) :-
    respond(['The passage is blocked by a wall']),
    !,
    fail.





talk([H|_]) :-
    respond(['You tried to talk to ', H]),
    !.
take([H|_]) :-
    respond(['You tried to take a ', H]),
    !.
drop([H|_]) :-
    respond(['You tried to drop a ', H]),
    !.
eat([H|_]) :-
    respond(['You tried to eat a ', H]),
    !.
fight([H|_]) :-
    respond(['You tried to fight the ', H]),
    !.
inspect([H|_]) :-
    respond(['You tried to look better at ', H]),
    !.
explore(_) :-
    respond(['You tried to explore your surrounds']),
    !.
enter([H|_]) :-
    respond(['You tried to enter ', H]),
    !.
run(_) :-
    respond(['You tried to run away']),
    !.
rest(_) :-
    respond(['You tried to rest for a while']),
    !.
play([H|_]) :-
    respond(['You tried to play with ', H]),
    !.
list(_) :-
    respond(['You tried to list your inventory']),
    !.
opens([H|_]) :-
    respond(['You tried to open a ', H]),
    !.
closes([H|_]) :-
    respond(['You tried to close a ', H]),
    !.
quit(_) :-
    respond(['Good bye! Thanks for playing...']),
    assert(done),
    !.
help(_) :-
    respond(['Your objective is...']),
    !.


% names of the important squares, plazas, objects
namesq(witch, "famine witch's yard").
namesq(smoke, "smoked forest").
namesq(dforest, "deep forest").
namesq(forest, "enchanted forest").
namesq(water, "shallow waters of the Ekofiume river").
namesq(dwater, "deep waters of the Ekofiume river").
namesq(margin, "margin of Ekofiume river").
namesq(church, "St. Eklasius Church").
namesq(cemitery, "Dom Doom Cemitery").
namesq(plaza, "Councilor Lustre Plaza").
namesq(gate, "Entrance gates of Ekofype City").
namesq(lroad, "EK101 regional road").
namesq(road, "EK303 city road").
namesq(rroad, "EK666 cursed road").
namesq(bridge, "Ekofy Fireater Bridge").
namesq(mroad, "King's Mountain Road").
namesq(froad, "Queen's Forest Road").
namesq(neighbor4, "Four Mosquitoes neighborhood").
namesq(knights3, "Three Knights neighborhood").
namesq(palace, "Ekofype Great Palace").
namesq(fountain, "Robinlot's Wife Saudades Fountain").
namesq(fishermans2, "Fisherman's Friends neighborhood").
namesq(fishermans3, "Fisherman's Old Village").
namesq(fishermans1, "Ermit Fisherman's yard").
namesq(intersection, "Three-way Eko-Mountain-Forest intersection (3EMFI)").
namesq(castle, "Red Castle of Firearms").
namesq(cave, "Ermit Caesirus cave").
namesq(nstreet, "north street").
namesq(estreet, "east street").
namesq(woods, "city woods").
namesq(dwoods, "creepy deep woods").
namesq(mountain, "Great Mountains of King Kalavareko").
namesq(dmountain, "Deep Cursed  Mountains").
namesq(loach, "Caesirus hidden place at the river margin").

% names of houses
namesq(hwitch, "Madaleine witch's house").
namesq(hchurch, "main hall of St. Eklasius Church").
namesq(tomb, "tomb of Dom Doom").
namesq(mosquito1, "first mosquito's house").
namesq(mosquito2, "second mosquito's house").
namesq(mosquito3, "third mosquito's house").
namesq(mosquito4, "fourth mosquito's house").
namesq(knight1, "Greminlot's house").
namesq(knight2, "Robinlot's house").
namesq(knight3, "Lancelot's house").
namesq(hpalace, "main hall of the Ekofype Great Palace").
namesq(fisherfriend1, "first fisherman's house").
namesq(fisherfriend2, "second fisherman's house").
namesq(fisherold1, "Old Claudius' house").
namesq(fisherold2, "Old Otavius' house").
namesq(fisherold3, "Old Julius's house").
namesq(fisher1, "Ermit fisherman Caexius' house").
namesq(hcastle, "hall of the Red Castle of Firearms").
namesq(hcave, "main hollow of the Ermit Caesirus cave").

% names of people
namesq(madaleine, "Madaleine the witch").
namesq(eklasius, "Eklasius the saint").
namesq(domdoom, "Dom Doom the cursed").
namesq(lustre, "Colonel Lustre").
namesq(guard, "Justus the Guard of the Gate").
namesq(collector, "Jurus the Coin Collector").
namesq(liljohn, "Liljohn the First Mosquito").
namesq(lilly, "Lilly the Second Ladybug").
namesq(lilbil, "Lilbil the Third Mosquito").
namesq(lilove, "Lillove the Fourth Ladybug").
namesq(greminlot, "Sir Greminlot the First Knight").
namesq(robinlot, "Sir Robinlot the Second Knight").
namesq(lancelot, "Sir Lancelot the Third Knight").
namesq(kalavareko, "ghost of the dead king Kalavareko").
namesq(ekofype, "ghost of the city founder Ekofype"). 
namesq(saudadesmirage, "mirage of Saudades the Robinlot's wife").
namesq(titus, "Titus the friendly fisherman").
namesq(pitus, "Pitus the friendly fisherman").
namesq(claudius, "Claudius the old fisherman").
namesq(otavius, "Otavius the old fisherman").
namesq(julius, "Julius the old fisherman").
namesq(caexius, "Caexius the great fisherman's brother"). % Caesirus' brother
namesq(tukernook, "Tukernook the Red Dragon").
namesq(saudades, "Saudades the princess").
namesq(caesirus, "Caesirus the great fisherman").
namesq(wghost, "Ghost of the deep woods").
namesq(mghost, "Ghost of the deep mountains").

% names of things
namesq(wall, "Great Wall of Eklotan").
namesq(sword, "Kalista the dragon killer sword").

% indoors
% squares that have houses or things you can enter
house(witch, hwitch).
house(church, hchurch).
house(cemitery, tomb).
house(neighbor4, mosquito1).
house(neighbor4, mosquito2).
house(neighbor4, mosquito3).
house(neighbor4, mosquito4).
house(knights3, knight1).
house(knights3, knight2).
house(knights3, knight3).
house(fishermans2, fisherfriend1).
house(fishermans2, fisherfriend2).
house(fishermans3, fisherold1).
house(fishermans3, fisherold2).
house(fishermans3, fisherold3).
house(fishermans1, fisher1).
house(cave, hcave).
house(castle, hcastle).

% people on houses or squares
people(hwitch, madaleine).
people(hchurch, eklasius).
people(tomb, domdoom).
people(plaza, lustre).
people(gate, guard).
people(bridge, collector).
people(mosquito1, liljohn).
people(mosquito2, lilly).
people(mosquito3, lilbil).
people(mosquito4, lilove).
people(knight1, greminlot).
people(knight2, robinlot).
people(knight3, lancelot).
people(hpalace, kalavareko).
people(hpalace, ekofype). 
people(fountain, saudadesmirage).
people(fisherfriend1, titus).
people(fisherfriend2, pitus).
people(fisherold1, claudius).
people(fisherold2, otavius).
people(fisherold3, julius).
people(fisher1, caexius). % Caesirus' brother
people(hcastle, tukernook).
people(hcastle, saudades).
people(hcave, caesirus).
people(dwoods, wghost).
people(dmountain, mghost).

% movable things in place
put_things :-
    retractall(thing(_, _)),
    assert(thing(smoke, sword)). % place, thing

% get only the first
getat(X, P) :-
    at(X, P),
    !.

% outdoors
% map from [X,Y]=[0,0] to [X,Y]=[10,6]
at([0, 0], witch).
at([0, 1], smoke).
at([0, 2], forest).
at([0, 3], lroad).
at([0, 4], woods).
at([0, 5], church).
at([0, 6], cemitery).
at([1, 0], forest).
at([1, 1], forest).
at([1, 2], forest).
at([1, 3], gate).
at([1, 4], plaza).
at([1, 5], nstreet).
at([1, 6], fountain).
at([2, 0], forest).
at([2, 1], forest).
at([2, 2], forest).
at([2, 3], road).
at([2, 4], estreet).
at([2, 5], neighbor4).
at([2, 6], palace).
at([3, 0], forest).
at([3, 1], forest).
at([3, 2], forest).
at([3, 3], road).
at([3, 4], estreet).
at([3, 5], knights3).
at([3, 6], dwoods).
at([4, 0], margin).
at([4, 1], margin).
at([4, 2], margin).
at([4, 3], road).
at([4, 4], fishermans2).
at([4, 5], fishermans3).
at([4, 6], fishermans1).
at([5, 0], water).
at([5, 1], water).
at([5, 2], water).
at([5, 3], road).
at([5, 4], water).
at([5, 5], water).
at([5, 6], water).
at([6, 0], dwater).
at([6, 1], dwater).
at([6, 2], dwater).
at([6, 3], bridge).
at([6, 4], dwater).
at([6, 5], dwater).
at([6, 6], dwater).
at([7, 0], water).
at([7, 1], water).
at([7, 2], water).
at([7, 3], rroad).
at([7, 4], water).
at([7, 5], water).
at([7, 6], water).
at([8, 0], margin).
at([8, 1], margin).
at([8, 2], margin).
at([8, 3], rroad).
at([8, 4], margin).
at([8, 5], margin).
at([8, 6], loach).
at([9, 0], dmountain).
at([9, 1], dwater).
at([9, 2], mountain).
at([9, 3], rroad).
at([9, 4], forest).
at([9, 5], forest).
at([9, 6], forest).
at([10, 0], cave).
at([10, 1], mroad).
at([10, 2], mroad).
at([10, 3], intersection).
at([10, 4], froad).
at([10, 5], froad).
at([10, 6], castle).

at([X, Y], dforest) :-
    (X < 4, Y < 0);
    (X < 0, Y < 3);
    (X < 0, Y > 3);
    (X < 4, Y > 6);
    (X > 8, Y > 6);
    (X > 10, Y > 3).

at([X, Y], mountain) :-
    (X > 10, Y =< 3);
    (X > 8, Y < 0 ).

at([X, 3], lroad) :-
    X < 0.

at([4, Y], margin) :-
    Y < 0 ; Y > 6.

at([5, Y], water) :-
    Y < 0 ; Y > 6.

at([6, Y], dwater) :-
    Y < 0 ; Y > 6.

at([7, Y], water) :-
    Y < 0 ; Y > 6.

at([8, Y], margin) :-
    Y < 0 ; Y > 6.

% this paths are blocked by walls
wall([X0, Y0], [X1, Y1]) :-
    % city walls
    (X0 = 0, Y0 = 3, X1 = 0, Y1 = 4);
    (X0 > 1, X0 < 5,  Y0 = 3, X1 > 1, X1 < 5, Y1 = 4);
    (X0 = -1, Y0 > 3, Y0 < 7, X1 = 0, Y1 > 3, Y1 < 7);
    (X0 > -1 , X0 < 5, Y0 = 7, X1 > -1, X1 < 5, Y1 = 6);
    % castle walls
    (X0 = 9,  Y0 = 6, X1 = 10, Y1 = 6);
    (X0 = 11,  Y0 = 6, X1 = 10, Y1 = 6);
    (X0 = 10,  Y0 = 7, X1 = 10, Y1 = 6);
    % cave walls
    (X0 = 9,  Y0 = 0, X1 = 10, Y1 = 0);
    (X0 = 11,  Y0 = 0, X1 = 10, Y1 = 0);
    (X0 = 10,  Y0 = -1, X1 = 10, Y1 = 0);
    % loach walls
    (X0 = 9,  Y0 = 6, X1 = 8, Y1 = 6);
    (X0 = 8,  Y0 = 5, X1 = 8, Y1 = 6);
    (X0 = 8,  Y0 = 7, X1 = 8, Y1 = 6).

% wall blocked by both sides (symmetric relation)
blocked(P0, P1) :- 
    wall(P0, P1).
blocked(P0, P1) :- 
    wall(P1, P0).
% positive thinking
accessible(P0, P1) :-
    not(blocked(P0, P1)), !.


% vocabulary(saia).
% vocabulary(va).
% vocabulary(fale).
% vocabulary(pegue).
% vocabulary(solte).
% vocabulary(coma).
% vocabulary(lute).
% vocabulary(olhe_em_volta).
% vocabulary(olhe_para).
% vocabulary(entre).
% vocabulary(fuja).
% vocabulary(descanse).
% vocabulary(toque).
% vocabulary(liste).
% vocabulary(abra).
% vocabulary(desista).
% vocabulary(ajuda).
% vocabulary(feche).

% vocabulary(go). % synonym get(out)
% vocabulary(talk).
% vocabulary(take). % synonym get(something)
% vocabulary(drop). % synonym release(something)
% vocabulary(eat).
% vocabulary(fight).
% vocabulary(inspect). % synonyms look(something), search(something)
% vocabulary(explore). % synonyms look(around), search(around)
% vocabulary(enter).
% vocabulary(run).
% vocabulary(rest).
% vocabulary(play).
% vocabulary(list). % synonym inventory()
% vocabulary(opens).
% vocabulary(closes).
% vocabulary(quit). % synonym give(up)
% vocabulary(help).

% get user input and convert to "command([arguments])"
get_command(C) :-
    read_command(RC),
    synonymous(RC, C).

synonymous(get([out|T]), go([out|T])) :- !.
synonymous(get([H|T]), take([H|T])) :- not(H == out), !.
synonymous(release(L), drop(L)) :- !.
synonymous(look([around|T]), explore(T)) :- !.
synonymous(look([H|T]), inspect([H|T])) :- not(H == around), !.
synonymous(search([around|T]), explore(T)) :- !.
synonymous(search([H|T]), inspect([H|T])) :- not(H == around), !.
synonymous(enter([inside|T]), enter(T)).
synonymous(inventory(L), list(L)).
synonymous(give([up|_]), quit([])).
synonymous(C, C).

read_command(C) :-
    readlist(L),
    remdet(L, Ld),
    Ld = [Co | Tail],
    Cr = [Co, Tail],
    C =.. Cr, !.

% remove determinants the, a, to, at, in, inside, under, of
% remove on and off, but change turn on to turnon and turn off to turnoff first
remdet(La, L0) :-
    delete(La, the, Lb),
    delete(Lb, a, Lc),
    delete(Lc, to, Ld),
    delete(Ld, at, Le),
    delete(Le, in, Lf),
    delete(Lf, inside, Lg),
    delete(Lg, under, Lh),
    delete(Lh, of, Li),
    delete(Li, me, Lj),
    delete(Lj, my, Lk),
    delete(Lk, mine, Ll),
    delete(Ll, you, Lm),
    delete(Lm, your, Ln),
    delete(Ln, yours, Lo),
    delete(Lo, it, Lp),
    delete(Lp, its, Lq),
    delete(Lq, him, Lr),
    delete(Lr, his, Ls),
    delete(Ls, her, Lt),
    delete(Lt, hers, Lu),
    delete(Lu, us, Lv),
    delete(Lv, our, Lw),
    delete(Lw, ours, Lx),
    delete(Lx, them, Ly),
    delete(Ly, their, Lz),
    delete(Lz, theirs, L1),
    remon(L1, L2),
    delete(L2, on, L3),
    delete(L3, off, L4),
    delete(L4, some, L5),
    delete(L5, any, L0).

% remove determinat on/off if not turn on, turn off
remon(La, Lo) :-
    replace2(turn, on, turnon, La, Lb),
    replace2(turn, off, turnoff, Lb, Lo).

% replace two consecutive words with a single one
replace2(W1, W2, New, La, Lb) :-
    repl2(W1, W2, New, La, Lb), !.

repl2(_, _, _, [], []).

repl2(_, _, _, [H|[]], [H|[]]).

repl2(W1, W2, New, [W1, W2 | T], [New | T2]) :-
    replace2(W1, W2, New, T, T2).

repl2(W1, W2, New, [H | T], [H | T2]) :-
    dif(H, W1),
    replace2(W1, W2, New, T, T2).

repl2(W1, W2, New, [W1, H2 | T], [W1, H2 | T2]) :-
    dif(H2, W2),
    replace2(W1, W2, New, T, T2).

% replace element in list 
replace(_, _, [], []).

replace(O, R, [O|T], [R|T2]) :-
    replace(O, R, T, T2).

replace(O, R, [H|T], [H|T2]) :-
    dif(H, O),
    replace(O, R, T, T2).


% read user input and return a list of atoms
readlist(L) :-
    % read(I), % read terms, end with dot.
    read_string(user_input, "\n", "\r\t ", _, I),
    string_lower(I, Il),
    remapo(Il, SI),
    split_string(SI, ". !?,;\":$%&*()_-=+`´^~{[}]></", ". !?,;\":$%&*()_-=+`´^~{[}]></", SSP),
    maplist(string_to_atom, SSP, L).


% remove \' from string
remapo(SC, SS) :-
    string_codes(SC, C), 
    doselect(C, D),
    atom_codes(A, D),
    atom_string(A, SS).

doselect(C, D) :-
    select(39, C, F),
    doselect(F, D),
    !.

doselect(C, C).

% respond simplifies writing a mixture of literals and variables
respond([]) :-
    nl.
respond([H|T]) :-
    write(H),
    respond(T).

/* ---------------------------------------------------------------------- */
/**prowler.pl
 * @ingroup GroupUnique
 * @brief verbose is det
 * @details Increases verbose level by one.
 * @return TRUE always.
 */
verbose :-
    verbosecounter(X),
    retractall(verbosecounter(_)),
    Y is X + 1,
    assert(verbosecounter(Y)).

/* ---------------------------------------------------------------------- */
/**
 * @ingroup GroupUnique
 * @brief verbose0 is det
 * @details Sets verbose level to zero.
 * @return TRUE always.
 */
verbose0 :-
    retractall(verbosecounter(_)),
    assert(verbosecounter(0)).

/* ---------------------------------------------------------------------- */
/**
 * @ingroup GroupUnique
 * @brief version is a fact
 * @details It will return the version number of program prowler.
 * @param[out] A It's a string with the version number.
 * @return TRUE always.
 */
version('20190405.211933').

/* ----------------------------------------------------------------------- */
/* vi: set ai et ts=4 sw=4 tw=0 wm=0 fo=croql : PL config for Vim modeline */
/* Template by Dr. Beco <rcb at beco dot cc>       Version 20150620.224740 */


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
:- dynamic([verbosecounter/1, have/1, here/1]).

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
    writeln('Initializing...'),
    initialize,
    explain,
    gameloop.

initialize :-
    retractall(have(_)),
    retractall(local(_)),
    assert(have([])),
    assert(local([1,1])).

explain :-
    writeln('Move with north, south, east or west').

gameloop :-
    whereami,
    whatishere,
    getcommand.

whereami :-
    local(X),
    format('You are at: ~w\n', [X]).
    % writef('You are at: %w\n', [X]).

whatishere :-
    local(X),
    getat(X, P),
    writeln("Here you can see: "),
    things(P, T),
    writeln(T).
    % fail.

whatishere.

getcommand :-
    write("What do you want to do?").

namesq(witch, "famine witch's yard").
namesq(hwitch, "Madaleine witch's house").
namesq(smoke, "smoked forest").
namesq(dforest, "deep forest").
namesq(forest, "forest").
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
namesq(mosquito1, "first mosquito's house").
namesq(mosquito2, "second mosquito's house").
namesq(mosquito3, "third mosquito's house").
namesq(mosquito4, "fourth mosquito's house").
namesq(knights3, "Three Knights neighborhood").
namesq(knight1, "Greminlot's house").
namesq(knight2, "Robinlot's house").
namesq(knight3, "Lancelot's house").
namesq(palace, "Ekofype Great Palace House").
namesq(fountain, "Robinlot's Wife Saudades Fountain").
namesq(fishermans2, "Fisherman's Friends neighborhood").
namesq(fishermans3, "Fisherman's Old Village").
namesq(fishermans1, "Ermit Fisherman's yard").
namesq(fisherfriend1, "first fisherman's house").
namesq(fisherfriend2, "second fisherman's house").
namesq(fisherold1, "Old Claudius' house").
namesq(fisherold2, "Old Otavius' house").
namesq(fisherold3, "Old Julius's house").
namesq(fisher1, "Ermit fisherman Caexius' house").
namesq(intersection, "Three-way Eko-Mountain-Forest intersection (3EMFI)").
namesq(castle, "Red Castle of Firearms").
namesq(hcastle, "hall of the Red Castle of Firearms").
namesq(cave, "Ermit Caesirus cave").
namesq(hcave, "main hollow of the Ermit Caesirus cave").
namesq(wall, "Great Wall of Eklotan").
namesq(hchurch, "main hall of St. Eklasius Church").
namesq(nstreet, "north street").
namesq(estreet, "east street").
namesq(woods, "city woods").
namesq(mountain, "Great Mountains of King Kalavareko").

% indoors
% places have things on it
things(witch, hwitch).
things(church, hchurch).
things(neighbor4, mosquito1).
things(neighbor4, mosquito2).
things(neighbor4, mosquito3).
things(neighbor4, mosquito4).
things(knights3, knight1).
things(knights3, knight2).
things(knights3, knight3).
things(fishermans2, fisherfriend1).
things(fishermans2, fisherfriend2).
things(fishermans3, fisherold1).
things(fishermans3, fisherold2).
things(fishermans3, fisherold3).
things(fishermans1, fisher1).
things(cave, hcave).
things(cave, hcastle).

getat(X, P) :-
    at(X, P),
    !.

% outdoors
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
at([3, 6], woods).
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
at([8, 6], margin).
at([9, 0], mountain).
at([9, 1], mountain).
at([9, 2], mountain).
at([9, 3], rroad).
at([9, 4], mountain).
at([9, 5], mountain).
at([9, 6], mountain).
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



/* ---------------------------------------------------------------------- */
/**
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


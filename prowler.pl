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
:- dynamic([have/1, coord/1, local/2, here/1, dead/0, done/0, pl_thing/2, last_command/1]).

/* ---------------------------------------------------------------------- */
/* Facts and Rules */
/**
 * @ingroup GroupUnique
 * @brief main is det
 * @details Initializes memory and other tasks
 * @retval TRUE If it can prove main.
 * @retval FALSE If main can't be proven.
 */
start :-
    % respond(['Initializing...']),
    initialize,
    explain,
    gameloop,
    !.

initialize :-
    % clear old memory
    retractall(have(_)),
    retractall(coord(_)),
    retractall(local(_, _)),
    retractall(here(_)),
    retractall(dead),
    retractall(done),
    retractall(pl_thing(_,_)),
    retractall(last_command(_)),
    % initializes a new game
    assert(have([])),
    assert(coord([1,1])),
    getat([1,1], Place),
    assert(local(player, Place)),
    % assert(here([])),
    put_things,
    put_people,
    assert(last_command(go([north]))).

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
    coord(X),
    at(X, dwater),
    respond(['You died in some strange deep waters']),
    assert(dead),
    !.

check_dead.

whereami :-
    % coord(Pos),
    % respond(['Your position is ', Pos]),
    local(player, Place),
    namesq(Place, Name),
    respond(['You are at the ', Name]).

whatishere :-
    local(player, Place),
    respond(['Here you can see: ']),
    whatlist(Place).

whatlist(Place) :-
    listhouses(Place),
    listpeople(Place),
    listthings(Place),
    nl.

listhouses(Place) :-
    sq_house(Place, House),
    namesq(House, Name),
    write(Name),
    write(', '),
    fail.

listhouses(_).

listpeople(Place) :-
    local(Person, Place),
    namesq(Person, Name),
    write(Name),
    write(', '),
    fail.

listpeople(_).

listthings(Place) :-
    pl_thing(Place, Thing),
    namesq(Thing, Name),
    write(Name),
    write(', '),
    fail.

listthings(_).

/* ---------------------------------------------------------------------- */
exec_command :-
    respond(['What do you want to do?']),
    get_command(Comm),
    nl,
    % respond([Comm]),
    do(Comm).

% moves around the map
do(go(X)) :-
    go(X), !.
% talks to people
do(talk(X)) :-
    talk(X), !.
% takes objects to inventory
do(take(X)) :-
    take(X), !.
% drops objects from inventory
do(drop(X)) :-
    drop(X), !.
% eats things
do(eat(X)) :-
    eat(X), !.
% fights against things
do(fight(X)) :-
    fight(X), !.
% take a closer look at something
do(inspect(X)) :-
    inspect(X), !.
% looks around the place
do(explore(X)) :-
    explore(X), !.
% enters houses or strange places
do(enter(X)) :-
    enter(X), !.
% run away for your life
do(run(X)) :-
    run(X), !.
% recovers some energy
do(rest(X)) :-
    rest(X), !.
% plays a musical instrument
do(play(X)) :-
    play(X), !.
% lists the inventory
do(list(X)) :-
    list(X), !.
% opens things (may need keys)
do(opens(X)) :-
    opens(X), !.
% closes things
do(closes(X)) :-
    closes(X), !.
% give up the game and exit
do(quit(X)) :-
    quit(X), !.
% prints a help message
do(help(X)) :-
    help(X), !.
% default: do not understand
do(_) :-
    respond(['Sorry, I didn\'t understand that.']), !.

/* ---------------------------------------------------------------------- */
% the real actions go here
go([H|_]) :-
    % respond(['You are trying to go ', H]),
    trymove(H),
    !.

go(_).

trymove(out) :- 
    local(player, Place),
    not(mapsq(Place)), % can only go out if inside something
    coord(Pos),
    getat(Pos, Outside),
    retractall(local(player, _)),
    assert(local(player, Outside)),
    !.

trymove(out) :- 
    respond(['You are not inside anything']),
    !.

trymove(north) :-
    local(player, Place),
    mapsq(Place), % can only walk coordinates if outside
    coord([X0, Y0]),
    Y1 is Y0 + 1,
    Npos = [X0, Y1],
    accessible([X0, Y0], Npos),
    respond(['You moved north']),
    retractall(coord(_)),
    assert(coord(Npos)),
    getat(Npos, Place2),
    retractall(local(player, _)),
    assert(local(player, Place2)),
    !.

trymove(south) :-
    local(player, Place),
    mapsq(Place), % can only walk coordinates if outside
    coord([X0, Y0]),
    Y1 is Y0 - 1,
    Npos = [X0, Y1],
    accessible([X0, Y0], Npos),
    respond(['You moved south']),
    retractall(coord(_)),
    assert(coord(Npos)),
    getat(Npos, Place2),
    retractall(local(player, _)),
    assert(local(player, Place2)),
    !.

trymove(east) :-
    local(player, Place),
    mapsq(Place), % can only walk coordinates if outside
    coord([X0, Y0]),
    X1 is X0 + 1,
    Npos = [X1, Y0],
    accessible([X0, Y0], Npos),
    respond(['You moved east']),
    retractall(coord(_)),
    assert(coord(Npos)),
    getat(Npos, Place2),
    retractall(local(player, _)),
    assert(local(player, Place2)),
    !.

trymove(west) :-
    local(player, Place),
    mapsq(Place), % can only walk coordinates if outside
    coord([X0, Y0]),
    X1 is X0 - 1,
    Npos = [X1, Y0],
    accessible([X0, Y0], Npos),
    respond(['You moved west']),
    retractall(coord(_)),
    assert(coord(Npos)),
    getat(Npos, Place2),
    retractall(local(player, _)),
    assert(local(player, Place2)),
    !.

trymove(H) :-
    (H = north ; H = south ; H = east ; H = west),
    local(player, Place),
    not(mapsq(Place)),
    namesq(Place, Name),
    respond(['You can\'t move ', H, ' while inside ', Name]),
    !.

trymove(H) :-
    (H = north ; H = south ; H = east ; H = west),
    respond(['The passage is blocked by a wall']),
    !.

trymove(H) :-
    mapsq(H),
    respond(['Try using "go" followed by "north", "south", "east" or "west"']),
    !.

trymove(H) :-
    respond(['I don\'t know how to get to ', H]),
    !.

/* ---------------------------------------------------------------------- */
talk([H|_]) :-
    respond(['You tried to talk to ', H]),
    !.

/* ---------------------------------------------------------------------- */
take([H|_]) :-
    respond(['You tried to take a ', H]),
    !.

/* ---------------------------------------------------------------------- */
drop([H|_]) :-
    respond(['You tried to drop a ', H]),
    !.

/* ---------------------------------------------------------------------- */
eat([H|_]) :-
    respond(['You tried to eat a ', H]),
    !.

/* ---------------------------------------------------------------------- */
fight([H|_]) :-
    respond(['You tried to fight the ', H]),
    !.

/* ---------------------------------------------------------------------- */
inspect([H|_]) :-
    respond(['You tried to look better at ', H]),
    !.

/* ---------------------------------------------------------------------- */
explore(_) :-
    respond(['You tried to explore your surrounds']),
    !.

/* ---------------------------------------------------------------------- */
% enter houses or strange places
enter([H|_]) :-
    % respond(['You tried to enter ', H]),
    local(player, Place),
    sq_house(Place, In), % is there a house here to enter?
    house_syn(In, Lsyn),
    member(H, Lsyn),   % is H member of the list of synonymous for this place?
    retractall(local(player, _)),
    assert(local(player, In)),
    % namesq(In, Name),
    % respond(['You are now inside the ', Name]),
    !.

enter([H|_]) :-
    respond(['There is no entrance to ', H]),
    !.

 
/* ---------------------------------------------------------------------- */
run(_) :-
    respond(['You tried to run away']),
    !.

/* ---------------------------------------------------------------------- */
rest(_) :-
    respond(['You tried to rest for a while']),
    !.

/* ---------------------------------------------------------------------- */
play([H|_]) :-
    respond(['You tried to play some music with ', H]),
    !.

/* ---------------------------------------------------------------------- */
list(_) :-
    respond(['You tried to list your inventory']),
    !.

/* ---------------------------------------------------------------------- */
opens([H|_]) :-
    respond(['You tried to open a ', H]),
    !.

/* ---------------------------------------------------------------------- */
closes([H|_]) :-
    respond(['You tried to close a ', H]),
    !.

/* ---------------------------------------------------------------------- */
quit(_) :-
    respond(['Good bye! Thanks for playing...']),
    assert(done),
    !.

/* ---------------------------------------------------------------------- */
help(_) :-
    respond(['Your objective is...']),
    !.

/* ---------------------------------------------------------------------- */
% facts listing all things

% generic square names (can't hold objects)
mapsq(forest). % all forest squares
mapsq(water). % all shallow waters near both the margins of the river
mapsq(dwater). % all middle of the river, plus a trap at 9,1
mapsq(margin). % all margin squares of the river
mapsq(lroad). % goes left/west
mapsq(road). % middle road, before the bridge
mapsq(rroad). % right road, after the bridge
mapsq(mroad). % mountain road, goes south to the cave
mapsq(froad). % forest road, goes north to the castle
mapsq(mountain). % all mountains around

% unique squares on the map
mapsq(witch). % witch's yard
mapsq(smoke). % smoked forest
mapsq(church). % city church
mapsq(cemitery). % city cemitery
mapsq(plaza). % city plaza square
mapsq(gate). % city gates
mapsq(bridge). % bridge over Ekofiume river
mapsq(intersection). % road intersection between rroad, mroad and froad
mapsq(neighbor4). % neighborhood of the Mosquitos
mapsq(knights3). % neighborhood of the Knights
mapsq(palace). % city palace
mapsq(fountain). % city fountain
mapsq(fishermans2). % neighborhood the the friendly fishermans
mapsq(fishermans3). % neighborhood of the old creepy fishermans
mapsq(fishermans1). % Caexis the Brother house square
mapsq(castle). % the Red Castle
mapsq(cave). % Caesirus' cave
mapsq(sstreet). % Saudades street, goes north
mapsq(wstreet). % Whale street, goes east
mapsq(woods). % light woods near the city plaza and church
mapsq(dwoods). % city deep woods near the river and the north wall
mapsq(dforest). % at 9,6 a deep forest
mapsq(dmountain). % a strange mountain place at 9,0
mapsq(loach). % Caesirus' hidden place in the river margin
mapsq(tavern). % Great Whale Tavern

% people on the world 
people(madaleine). % the witch
people(eklasius). % the saint
people(domdoom). % the cursed
people(lustre). % Councilor
people(guard). % gate's guard
people(collector). % bridge's tax collector
people(liljohn). % 1st mosquito
people(lilly). % 2nd mosquito
people(lilbil). % 3rd mosquito
people(lilove). % 4th mosquito
people(greminlot). % 1st knight
people(robinlot). % 2nd knight
people(lancelot). % 3rd knight
people(klavareko). % ghost of the king
people(ekofype). % ghost of the city founder
people(saudadesmirage). % mirage of the princess
people(titus). % friendly fisherman
people(pitus). % friendly fisherman
people(claudius). % old grumpy fisherman
people(otavius). % old grumpy fisherman
people(julius). % old grumpy fisherman
people(caexis). % Caesirus' brother
people(tukernook). % red dragon
people(saudades). % the princess
people(caesirus). % ermit fisherman
people(wghost). % deep woods ghost
people(mghost). % deep mountains ghost
people(whale). % Whomobyl the White Whale
people(bartender). % Thomorn Armorsmith the Barbarian
people(waitress). % Lady Quierris Seaskipper
people(drunk). % Tumblebelly the Bard

% houses and places one can enter
house(hwitch).
house(hchurch).
house(htomb).
house(hmosquito1).
house(hmosquito2).
house(hmosquito3).
house(hmosquito4).
house(hknight1).
house(hknight2).
house(hknight3).
house(hpalace).
house(hfisherfriend1).
house(hfisherfriend2).
house(hfisherold1).
house(hfisherold2).
house(hfisherold3).
house(hfisher1).
house(hcastle).
house(hcave).
house(hloach).
house(htavern).
house(hwell). % water well in the fountain
house(hpool). % water pool in the deep mountains 

% vocabulary(saia).
% vocabulary(va).
% vocabulary(fale).
% vocabulary(pegue).
% vocabulary(solte).
% vocabulary(coma).
% vocabulary(lute).
% vocabulary(inspecione).
% vocabulary(explore).
% vocabulary(entre).
% vocabulary(fuja).
% vocabulary(descanse).
% vocabulary(toque).
% vocabulary(liste).
% vocabulary(abra).
% vocabulary(feche).
% vocabulary(desista).
% vocabulary(ajuda).

vocabulary(go). % synonym get(out)
vocabulary(talk).
vocabulary(take). % synonym get(something)
vocabulary(drop). % synonym release(something)
vocabulary(eat).
vocabulary(fight).
vocabulary(inspect). % synonyms look(something), search(something)
vocabulary(explore). % synonyms look(around), search(around)
vocabulary(enter).
vocabulary(run).
vocabulary(rest).
vocabulary(play).
vocabulary(list). % synonym inventory()
vocabulary(opens).
vocabulary(closes).
vocabulary(quit). % synonym give(up)
vocabulary(help).

% indoors
% squares that have houses or things you can enter
sq_house(witch, hwitch).
sq_house(church, hchurch).
sq_house(cemitery, htomb).
sq_house(neighbor4, hmosquito1).
sq_house(neighbor4, hmosquito2).
sq_house(neighbor4, hmosquito3).
sq_house(neighbor4, hmosquito4).
sq_house(knights3, hknight1).
sq_house(knights3, hknight2).
sq_house(knights3, hknight3).
sq_house(palace, hpalace).
sq_house(fishermans2, hfisherfriend1).
sq_house(fishermans2, hfisherfriend2).
sq_house(fishermans3, hfisherold1).
sq_house(fishermans3, hfisherold2).
sq_house(fishermans3, hfisherold3).
sq_house(fishermans1, hfisher1).
sq_house(castle, hcastle).
sq_house(cave, hcave).
sq_house(loach, hloach).
sq_house(tavern, htavern).
sq_house(fountain, hwell). % water well in the fountain
sq_house(dmountain, hpool). % water pool in the deep mountains 

% house synonymous for typing
house_syn(hwitch, [madaleine, witchs, witch, living, room]).
house_syn(hchurch, [eklasius, church, chapel]).
house_syn(htomb, [tomb, dom, doom, domdoom]).
house_syn(hmosquito1, [liljohn, first, mosquitos, bedroom, mosquito]).
house_syn(hmosquito2, [lilly, second, bathroom, ladybug]).
house_syn(hmosquito3, [lilbil, third, inglenook]).
house_syn(hmosquito4, [lillove, fourth, guestroom]).
house_syn(hknight1, [greminlots, greminlot, gremin, cellar]).
house_syn(hknight2, [robinlots, robinlot, robin, garage, saudades]).
house_syn(hknight3, [lancelots, lancelot, lance, library]).
house_syn(hpalace, [great, palace, ekofype, throne]).
house_syn(hfisherfriend1, [first, fisherman, fishermans, one, green, titus]).
house_syn(hfisherfriend2, [second, two, gym, pitus]).
house_syn(hfisherold1, [claudius, old, basement]).
house_syn(hfisherold2, [otavius, chamber]).
house_syn(hfisherold3, [julius, dungeon]).
house_syn(hfisher1, [ermit, caexis, caesirus, great, darkroom, fisherman, fishermans]).
house_syn(hcastle, [red, castle, firearms, lobby]).
house_syn(hcave, [cave, hollow, ermit, caesirus, great, conservatory, fishermans, fisherman]).
house_syn(hloach, [caesirus, hidden, hole, sand, wormhole, secret, passage]).
house_syn(htavern, [great, whale, tavern, salon]).
house_syn(hwell, [limpid, water, well]).
house_syn(hpool, [crystalline, water, pool]).

% people on starting places
put_people :-
    % retractall(local(_, _)),
    assert(local(madaleine, hwitch)),
    assert(local(eklasius, hchurch)),
    assert(local(domdoom, tomb)),
    assert(local(lustre, plaza)),
    assert(local(guard, gate)),
    assert(local(collector, bridge)),
    assert(local(liljohn, htavern)),
    assert(local(lilly, mosquito2)),
    assert(local(lilbil, mosquito3)),
    assert(local(lilove, mosquito4)),
    assert(local(greminlot, knight1)),
    assert(local(robinlot, knight2)),
    assert(local(lancelot, knight3)),
    assert(local(klavareko, hpalace)),
    assert(local(ekofype, hpalace)),
    assert(local(saudadesmirage, fountain)),
    assert(local(titus, fisherfriend1)),
    assert(local(pitus, fisherfriend2)),
    assert(local(claudius, htavern)),
    assert(local(otavius, fisherold2)),
    assert(local(julius, fisherold3)),
    assert(local(caexis, htavern)),
    assert(local(tukernook, hcastle)),
    assert(local(saudades, hcastle)),
    assert(local(caesirus, hcave)),
    assert(local(wghost, dwoods)),
    assert(local(mghost, dmountain)),
    assert(local(whale, hpool)),
    assert(local(bartender, htavern)),
    assert(local(waitress, htavern)),
    assert(local(drunk, htavern)).

% movable things in place
put_things :-
    % retractall(pl_thing(_, _)),
    assert(pl_thing(smoke, sword)), % place, thing
    assert(pl_thing(fishermans1, boat)),
    assert(pl_thing(mosquito1, coin)),
    assert(pl_thing(fisherold1, coin)),
    assert(pl_thing(hloach, coin)),
    assert(pl_thing(tomb, coin)),
    assert(pl_thing(woods, coin)),
    assert(pl_thing(hcastle, coin)).

/* ---------------------------------------------------------------------- */
% atoms and the respective string descriptions
% names of the important squares, plazas, objects
namesq(forest, "enchanted forest").
namesq(water, "shallow waters of the Ekofiume river").
namesq(dwater, "deep waters of the Ekofiume river").
namesq(margin, "margin of Ekofiume river").
namesq(lroad, "EK101 regional road").
namesq(road, "EK042 city road").
namesq(rroad, "EK666 cursed road").
namesq(mroad, "King's Mountain Road").
namesq(froad, "Queen's Forest Road").
namesq(mountain, "Great Mountains of King Klavareko").
namesq(witch, "famine witch's yard").
namesq(smoke, "smoked forest").
namesq(church, "St. Eklasius Church entrance").
namesq(cemitery, "Dom Doom Cemitery").
namesq(plaza, "Councilor Lustre Plaza").
namesq(gate, "Entrance gates of Ekofype City").
namesq(bridge, "Ekofy Fireater Bridge").
namesq(intersection, "EMF03 Three-way Intersection Eko-Mountain-Forest").
namesq(neighbor4, "Four Mosquitoes neighborhood").
namesq(knights3, "Three Knights neighborhood").
namesq(palace, "Ekofype Great Palace main door").
namesq(fountain, "Saudades Fountain").
namesq(fishermans2, "Fisherman's Friends neighborhood").
namesq(fishermans3, "Fisherman's Old Village").
namesq(fishermans1, "Ermit Fisherman's yard").
namesq(castle, "Red Castle of Firearms").
namesq(cave, "Ermit Caesirus cave entrance").
namesq(sstreet, "Saudades Fountain street").
namesq(wstreet, "Whale street").
namesq(woods, "city woods").
namesq(dwoods, "creepy deep woods").
namesq(dforest, "deep forest").
namesq(dmountain, "Deep Cursed  Mountains").
namesq(loach, "Caesirus hidden place at the river margin").
namesq(tavern, "Great Whale Tavern").

% names of houses
namesq(hwitch, "Madaleine witch's living room").
namesq(hchurch, "chapel of St. Eklasius Church").
namesq(htomb, "tomb of Dom Doom").
namesq(hmosquito1, "Liljohn bedroom").
namesq(hmosquito2, "Lilly's bathroom").
namesq(hmosquito3, "Lilbil's inglenook").
namesq(hmosquito4, "Lillove's guestroom").
namesq(hknight1, "Greminlot's cellar").
namesq(hknight2, "Robinlot's garage").
namesq(hknight3, "Lancelot's library").
namesq(hpalace, "throne room of the Ekofype Great Palace").
namesq(hfisherfriend1, "Titu's green room").
namesq(hfisherfriend2, "Pitu's gym room").
namesq(hfisherold1, "Old Claudius' basement").
namesq(hfisherold2, "Old Otavius' chamber").
namesq(hfisherold3, "Old Julius's dungeon").
namesq(hfisher1, "Ermit fisherman Caesirus' darkroom").
namesq(hcastle, "lobby of the Red Castle of Firearms").
namesq(hcave, "conservatory of the Ermit Caesirus cave").
namesq(hloach, "Caesirus' hidden hole in the sand").
namesq(htavern, "salon of the Great Whale Tavern").
namesq(hwell, "limpid water well"). % water well in the fountain
namesq(hpool, "crystalline water pool"). % water pool in the deep mountains 

% names of people
namesq(madaleine, "Madaleine the Witch").
namesq(eklasius, "Eklasius the Saint").
namesq(domdoom, "Dom Doom the Cursed").
namesq(lustre, "Councilor Leonel Lustre").
namesq(guard, "Justus the Guard of the Gate").
namesq(collector, "Jurus the Coin Collector").
namesq(liljohn, "Liljohn the First Mosquito").
namesq(lilly, "Lilly the Second Ladybug").
namesq(lilbil, "Lilbil the Third Mosquito").
namesq(lilove, "Lillove the Fourth Ladybug").
namesq(greminlot, "Sir Greminlot the First Knight").
namesq(robinlot, "Sir Robinlot the Second Knight").
namesq(lancelot, "Sir Lancelot the Third Knight").
namesq(klavareko, "Ghost of the dead king Klavareko").
namesq(ekofype, "Ghost of the city founder Ekofype"). 
namesq(saudadesmirage, "mirage of a beautiful princess").
namesq(titus, "Titus Seaskipper the friendly fisherman").
namesq(pitus, "Pitus Seaskipper the friendly fisherman").
namesq(claudius, "Claudius the old fisherman").
namesq(otavius, "Otavius the old fisherman").
namesq(julius, "Julius the old fisherman").
namesq(caexis, "Caexis the Brother"). % Caesirus' brother
namesq(tukernook, "Tukernook the Red Dragon").
namesq(saudades, "Saudades the Princess").
namesq(caesirus, "Caesirus the Great Ermit Fisherman").
namesq(wghost, "Ghost of the deep woods").
namesq(mghost, "Ghost of the deep mountains").
namesq(whale, "Whomobyl the White Whale").
namesq(bartender, "Thomorn Armorsmith the bartender").
namesq(waitress, "Lady Quierris Seaskipper the waitress").
namesq(drunk, "Tumblebelly the drunk").

% names of things
namesq(wall, "Great Wall of Eklotan").
namesq(sword, "Kalista the dragon killer sword").
namesq(boat, "WWW boat").
namesq(coin, "Klavareko golden coin").

/* ---------------------------------------------------------------------- */
% get only the first
getat(X, P) :-
    at(X, P),
    !.

% outdoors
% map from [X,Y]=[0,0] to [X,Y]=[10,6]
at([0, 0], witch).
at([0, 1], smoke).
at([0, 2], forest). % generic
at([0, 3], lroad). % generic
at([0, 4], woods).
at([0, 5], church).
at([0, 6], cemitery).
at([1, 0], forest). % generic
at([1, 1], forest). % generic
at([1, 2], forest). % generic
at([1, 3], gate).
at([1, 4], plaza).
at([1, 5], sstreet).
at([1, 6], fountain).
at([2, 0], forest). % generic
at([2, 1], forest). % generic
at([2, 2], forest). % generic
at([2, 3], road). % generic
at([2, 4], wstreet).
at([2, 5], neighbor4).
at([2, 6], palace).
at([3, 0], forest). % generic
at([3, 1], forest). % generic
at([3, 2], forest). % generic
at([3, 3], road). % generic
at([3, 4], tavern).
at([3, 5], knights3).
at([3, 6], dwoods).
at([4, 0], margin). % generic
at([4, 1], margin). % generic
at([4, 2], margin). % generic
at([4, 3], road). % generic
at([4, 4], fishermans2).
at([4, 5], fishermans3).
at([4, 6], fishermans1).
at([5, 0], water). % generic
at([5, 1], water). % generic
at([5, 2], water). % generic
at([5, 3], road). % generic
at([5, 4], water). % generic
at([5, 5], water). % generic
at([5, 6], water). % generic
at([6, 0], dwater). % generic
at([6, 1], dwater). % generic
at([6, 2], dwater). % generic
at([6, 3], bridge).
at([6, 4], dwater). % generic
at([6, 5], dwater). % generic
at([6, 6], dwater). % generic
at([7, 0], water). % generic
at([7, 1], water). % generic
at([7, 2], water). % generic
at([7, 3], rroad). % generic
at([7, 4], water). % generic
at([7, 5], water). % generic
at([7, 6], water). % generic
at([8, 0], margin). % generic
at([8, 1], margin). % generic
at([8, 2], margin). % generic
at([8, 3], rroad). % generic
at([8, 4], margin). % generic
at([8, 5], margin). % generic
at([8, 6], loach).
at([9, 0], dmountain).
at([9, 1], mountain). % generic
at([9, 2], mountain). % generic
at([9, 3], rroad). % generic
at([9, 4], forest). % generic
at([9, 5], forest). % generic
at([9, 6], dforest).
at([10, 0], cave).
at([10, 1], mroad). % generic
at([10, 2], mroad). % generic
at([10, 3], intersection).
at([10, 4], froad). % generic
at([10, 5], froad). % generic
at([10, 6], castle).

% forest at north, west and south-west
at([X, Y], forest) :-
    (X < 4, Y < 0);
    (X < 0, Y < 3);
    (X < 0, Y > 3);
    (X < 4, Y > 6);
    (X > 8, Y > 6);
    (X > 10, Y > 3).

% mountains at south-east
at([X, Y], mountain) :-
    (X > 10, Y =< 3);
    (X > 8, Y < 0 ).

% left road goes on to west infinitely
at([X, 3], lroad) :-
    X < 0.

% the river goes north/south infinitely
at([4, Y], margin) :-
    Y < 0 ; Y > 6.

at([8, Y], margin) :-
    Y < 0 ; Y > 6.

at([5, Y], water) :-
    Y < 0 ; Y > 6.

at([7, Y], water) :-
    Y < 0 ; Y > 6.

at([6, Y], dwater) :-
    Y < 0 ; Y > 6.

/* ---------------------------------------------------------------------- */
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

/* ---------------------------------------------------------------------- */
% get user input and convert to "command([arguments])"
get_command(C) :-
    read_command(RC),
    vocab_syn(RC, C),
    % vocabulary(C), % erro tratado no do(_)
    retractall(last_command(_)),
    assert(last_command(C)).

vocab_syn(get([out|T]), go([out|T])) :- !.
% synonymous(get([H|T]), take([H|T])) :- not(H == out), !.
vocab_syn(get(L), take(L)) :- !.
vocab_syn(release(L), drop(L)) :- !.
vocab_syn(look([around|T]), explore(T)) :- !.
% synonymous(look([H|T]), inspect([H|T])) :- not(H == around), !.
vocab_syn(look(L), inspect(L)) :- !.
vocab_syn(search([around|T]), explore(T)) :- !.
% synonymous(search([H|T]), inspect([H|T])) :- not(H == around), !.
vocab_syn(search(L), inspect(L)) :- !.
vocab_syn(enter([inside|T]), enter(T)).
vocab_syn(inventory(L), list(L)).
vocab_syn(give([up|_]), quit([])).
vocab_syn(last([]), C) :- last_command(C), !.
vocab_syn(C, C).

read_command(C) :-
    readlist(L),
    remdet(L, Ld),
    Ld = [Co | Tail],
    Cr = [Co, Tail],
    assembly_command(Cr, C),
    !.

assembly_command(['',[]], C) :-
    C =.. [last,[]], !.
assembly_command(Cr, C) :-
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

% read user input and return a list of atoms
readlist(L) :-
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

/* ---------------------------------------------------------------------- */
% respond simplifies writing a mixture of literals and variables
respond([]) :-
    nl.
respond([H|T]) :-
    write(H),
    respond(T).

/* ---------------------------------------------------------------------- */
version('20190405.211933').

/* ----------------------------------------------------------------------- */
/* vi: set ai et ts=4 sw=4 tw=0 wm=0 fo=croql : PL config for Vim modeline */
/* Template by Dr. Beco <rcb at beco dot cc>       Version 20150620.224740 */


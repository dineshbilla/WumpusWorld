%my_agent.pl

%   this procedure requires the external definition of two procedures:
%
%     init_agent: called after new world is initialized.  should perform
%                 any needed agent initialization.
%
%     run_agent(percept,action): given the current percept, this procedure
%                 should return an appropriate action, which is then
%                 executed.
%
% This is what should be fleshed out

:- dynamic agentpos/2,ispit/3,iswumpus/3,visited/3,parent/2,numerofmoves/1,foundgold/1,wumpusalive/1,foundwumpuslocation/2.
:- dynamic agent_orientation/1,pendingaction/1,secondpass/1.

adj(1,2).
adj(2,1).
adj(2,3).
adj(3,2).
adj(3,4).
adj(4,3).

%actionList = [].

init_agent:-
  format('\n=====================================================\n'),
  format('This is init_agent:\n\tIt gets called once, use it for your initialization\n\n'),
  format('=====================================================\n\n'),
  retractall(agentpos(_,_)),
  retractall(ispit(_,_,_)),
  retractall(visited(_,_,_)),
  retractall(iswumpus(_,_,_)),
  retractall(parent(_,_)),
  retractall(agent_orientation(_)),
  retractall(foundwumpuslocation(_,_)),
  (
    foundgold(yes) -> retract(foundgold(yes))
  ; true  
  ),
  (
    wumpusalive(no) -> retract(wumpusalive(no))
  ; true
  ),
  assert(agentpos(1,1)),
  assert(agent_orientation(0)),
  assert(world_size(4)),
  %findall([VX,VY],validpoints(VX,VY),VList),
  %foreach(member([VX,VY],VList),assert(visited(no,VX,VY))),
  assert(visited(yes,1,1)),
  assert(parent([1,1],[1,1])),
  assert(numberofmoves(0)),      % parent([PX,PY],[CX,CY])
  assert(iswumpus(no,1,1)),
  assert(ispit(no,1,1)),
  assert(foundgold(no)),
  assert(wumpusalive(yes)),
  assert(secondpass(yes)),
  assert(secondpass(no)).  

run_agent([_,_,no,yes,no],Action):-
  write('We bumped into wall when trying to goto '),
  agentpos(CX,CY),
  write(CX),write(','),write(CY),nl,
  parent([PX,PY],[CX,CY]),
  retract(agentpos(CX,CY)),
  assert(agentpos(PX,PY)),
  assert(visited(yes,CX,CY)),
  (
    (PX is 1, PY is 1) -> halt
  ; true
  ),
  (
    get_safe_nbr(NX,NY)
    -> 
    get_action(NX,NY,Action)  % goto safe nbr
    ;
    get_action(PX,PY,Action)   % goback to parent
  ),
  updateAgentPosition(Action).


run_agent([Stench,Breeze,no,no,yes],Action):-
  %foundwumpuslocation(WX,WY),
  %read(_),
  write('['),write(Stench),write(','),write(Breeze),write(',no,no,yes]'),nl,
  assert(wumpusalive(no)),
  retract(wumpusalive(yes)),
  retractall(iswumpus(yes,_,_)),
  format('Heard Scream, Wumpus is dead\n'),
  agentpos(X,Y),
  parent([PX,PY],[X,Y]),
  update_KB([Stench,Breeze]),
  (
    get_safe_nbr(NX,NY)
    -> 
    get_action(NX,NY,Action)  % goto safe nbr
    ;
    get_action(PX,PY,Action)   % goback to parent
  ),
  updateAgentPosition(Action).
%  findall([VX,VY],validpoints(VX,VY),VList),
% foreach(member([VX,VY],VList),assert(iswumpus(no,VX,VY))).

run_agent([Stench,Breeze,yes,Bump,Scream],grab):-
  %read(_),
  write('['),write(Stench),write(','),write(Breeze),write(',yes,'),write(Bump),write(','),write(Scream),write(']'),nl,
  format('Found Gold\n'),
  update_KB([Stench,Breeze]),
  write('Action is grab\n'),
  assert(foundgold(yes)),
  retract(foundgold(no)).%display_world. 

run_agent([_,_,_,_,_],climb):-
  foundgold(yes),
  secondpass(yes),
  agentpos(1,1),  
  %read(_),
  format('Found Gold and back at starting location\nAction is Climb\n').%display_world.

run_agent([Stench,Breeze,no,no,no],Action):-
  (foundgold(no);foundwumpuslocation(WX,WY)),
  agentpos(1,1),
  %wumpusalive(yes),
  % Check if all neighbours are visited
  visited(yes,1,2),
  visited(yes,2,1),
  format('Back at starting position without Gold, But I found Wumpus location\n  restart exploration'),
  retractall(visited(_,_,_)),
  retractall(parent(_,_)),
  assert(parent([1,1],[1,1])),
  assert(visited(yes,1,1)),
  assert(secondpass(yes)),
  retract(secondpass(no)),
  %read(_),
  write('['),write(Stench),write(','),write(Breeze),write(','),write('no'),write(','),write(Bump),write(',no,'),write(']'),nl,
  update_KB([Stench,Breeze]),
  (
    get_safe_nbr(NX,NY) -> get_action(NX,NY,Action)  
  ; Action = climb   % goback to parent
  ),
  updateAgentPosition(Action).


% wumpus is present in the path of agent
run_agent([Stench,Breeze,no,Bump,Scream],shoot):-
  Bump == no,
  can_i_kill_wumpus(yes),
  %read(_),
  write('['),write(Stench),write(','),write(Breeze),write(','),write('no'),write(','),write(Bump),write(','),write(Scream),write(']'),nl,
  format('I can kill Wumpus\n'),
  write('Action is Shoot\n'),
  update_KB([Stench,Breeze]).%display_world.

% case where wumpus is nbr but not facing the agent
run_agent([Stench,Breeze,no,Bump,Scream],Action):-
  %is_nbr_wumpus(NX,NY),
  Bump == no,
  can_i_turn_to_kill_wumpus(yes),
  not(can_i_kill_wumpus(yes)),
  %read(_),
  write('['),write(Stench),write(','),write(Breeze),write(','),write('no'),write(','),write(Bump),write(','),write(Scream),write(']'),nl,
  write('Turn to kill wumpus\n'),
  update_KB([Stench,Breeze]),
  Action = turnright,
  updateAgentPosition(Action),
  updatemoves.%display_world.  

%General Case
%run_agent(Percept,Action):-
run_agent([Stench,Breeze,no,Bump,no],Action):-
 % Glitter == no,
  Bump == no,
  not(is_nbr_wumpus(NX,NY)),
  not(can_i_kill_wumpus(yes)),
  %read(_),  
  write('['),write(Stench),write(','),write(Breeze),write(','),write('no'),write(','),write(Bump),write(',no,'),write(']'),nl,
  agentpos(X,Y),
  parent([PX,PY],[X,Y]),
  update_KB([Stench,Breeze]),
  (
    get_safe_nbr(NX,NY) -> get_action(NX,NY,Action)  % goto safe nbr
  %; get_unvisited_nbr(UX,UY) -> (get_action(UX,UY,Action),assert(pendingaction(goforward)))
  ; get_action(PX,PY,Action)   % goback to parent
  ),
  updateAgentPosition(Action),
  updatemoves.%display_world. 

get_safe_nbr(X,Y):-
  agentpos(CX,CY),
  adjacent([CX,CY],[X,Y]),
  permitted([X,Y]),
  not(visited(yes,X,Y)),
  not(parent([X,Y],[CX,CY])),
  not(ispit(yes,X,Y)),
  not(iswumpus(yes,X,Y)),
  write('Agent is at '),write(CX),write(','),write(CY),write(', Found Safe Neighbour '),write(X),write(' '),write(Y),nl.

get_unvisited_nbr(X,Y):-
  write('Check for unsafe neighbors with 10% probablity, random number = '),
  random(1,10,R),
  write(R),nl,
  R =:= 9,
  agentpos(CX,CY),
  adjacent([CX,CY],[X,Y]),
  permitted([X,Y]),
  not(visited(yes,X,Y)),
  not(parent([X,Y],[CX,CY])),
  write('Agent is at '),write(CX),write(','),write(CY),write(', Unvisited unsafe neighbour '),write(X),write(' '),write(Y),nl.

get_action(NX,NY,Action):-
  agentpos(CX,CY),
  write('Moving from ['),write(CX),write(','),write(CY),write('] to ['),write(NX),write(','),write(NY),write(']'),
  agent_orientation(Angle),
  write('\nAngle: '),write(Angle),write('\nAction is '),
  RightAngle is (Angle + 270) mod 360,
  LeftAngle is (Angle + 90) mod 360,
  %write(LeftAngle),nl,
  (
     goforwardmove([CX,CY],[NX,NY]) -> Action = goforward
  ;  goforwardmove(RightAngle,[CX,CY],[NX,NY]) -> Action = turnright
  ;  goforwardmove(LeftAngle,[CX,CY],[NX,NY]) -> Action = turnleft
  ;  Action = turnright
  ),write(Action),nl.



update_KB([Stench,Breeze]) :-
  %write('Stench is '),write(Stench), write('  Breeze is '),write(Breeze),nl,
  add_pit_KB(Breeze),
  (
    wumpusalive(yes) -> add_wumpus_KB(Stench)
  ; wumpusalive(no)  -> write('Wumpus is dead, no further updates to KB\n')
  ),
  % Make sure the current position doesnt say PIT/WUMPUS
/*  agentpos(X,Y),
  (iswumpus(yes,X,Y) -> retract(iswumpus(yes,X,Y)) ; true),
  (ispit(yes,X,Y)    -> retract(ispit(yes,X,Y)) ; true),
  (not(iswumpus(no,X,Y)) -> assert(iswumpus(no,X,Y)) ; true),
  (not(ispit(no,X,Y))    -> assert(ispit(no,X,Y)) ; true),
*/
%  write('PIT KB\n'),
  printYPITLIST,
 % printNPITLIST,
%  write('WUMPUS KB\n'),
  printYWUMPLIST.
%  printNWUMPLIST.

add_wumpus_KB(no):-
  agentpos(X,Y),
  findall([NX,NY],find_wumpus_nbrs_to_set_no(NX,NY),NList),
  foreach(member([NX,NY],NList),assert(iswumpus(no,NX,NY))),
  findall([MX,MY],update_wumpus_nbr([X,Y],[MX,MY]),MList),
  foreach(member([MX,MY],MList),retract(iswumpus(yes,MX,MY))),
  aggregate_all(count,iswumpus(yes,_,_),Count),
  check_if_wumpus_is_found.
  %format('DONE').

add_wumpus_KB(yes) :-
  agentpos(X,Y),
  (
   foundwumpuslocation(_,_) -> true% write('WUMPUS is found, no further updates\n')
   ;  check_if_wumpus_is_found,
      findall([NX,NY],unknown_wumpus_nbr([X,Y],[NX,NY]),NList),
      foreach(member([NX,NY],NList),assert(iswumpus(yes,NX,NY)))
  ).

find_wumpus_nbrs_to_set_no(NX,NY):-
  agentpos(X,Y),
  adjacent([X,Y],[NX,NY]),
  not(iswumpus(no,NX,NY)).% removing duplicates
%  write(NX),write(' '),write(NY). 

unknown_wumpus_nbr([X,Y],[NX,NY]):-
  adjacent([X,Y],[NX,NY]),
  not(iswumpus(no,NX,NY)),
  not(iswumpus(yes,NX,NY)). % to not allow duplicates

update_wumpus_nbr([X,Y],[NX,NY]):-
  adjacent([X,Y],[NX,NY]),
  iswumpus(no,NX,NY),
  iswumpus(yes,NX,NY).

foundWumpus:-
  write('Found Wumpus Location\n'),
  iswumpus(yes,X,Y),
  assert(foundwumpuslocation(X,Y)).

check_if_wumpus_is_found:-
  aggregate_all(count,iswumpus(yes,_,_),Count),
  (
    Count == 1 -> foundWumpus
  ; true
  ).

add_pit_KB(no) :-
  agentpos(X,Y),
  findall([NX,NY],find_pit_nbrs_to_set_no(NX,NY),NList),
  foreach(member([NX,NY],NList),assert(ispit(no,NX,NY))),
  findall([MX,MY],update_pit_nbr([X,Y],[MX,MY]),MList),
  foreach(member([MX,MY],MList),retract(ispit(yes,MX,MY))).

add_pit_KB(yes) :-
  agentpos(X,Y),
  findall([NX,NY],unknown_pit_nbr([X,Y],[NX,NY]),NList),
  foreach(member([NX,NY],NList),assert(ispit(yes,NX,NY))).

find_pit_nbrs_to_set_no(NX,NY):-
  agentpos(X,Y),
  adjacent([X,Y],[NX,NY]),
  not(ispit(no,NX,NY)).

unknown_pit_nbr([X,Y],[NX,NY]):-
  adjacent([X,Y],[NX,NY]),
  not(ispit(no,NX,NY)),
  not(ispit(yes,NX,NY)). % to remove duplicates

update_pit_nbr([X,Y],[NX,NY]):-
  adjacent([X,Y],[NX,NY]),
  ispit(no,NX,NY),
  ispit(yes,NX,NY).

  
is_goforward_safe:-
  agentpos(CX,CY),
  goforwardmove([CX,CY],[NX,NY]),
  ispit(no,NX,NY),
  iswumpus(no,NX,NY).


updateAgentPosition(goforward):-
  agentpos(CX,CY),
  goforwardmove([CX,CY],[NX,NY]),
  retract(agentpos(CX,CY)),
  assert(agentpos(NX,NY)),
  assert(visited(yes,CX,CY)),
  (
    not(parent([NX,NY],[CX,CY])) -> assert(parent([CX,CY],[NX,NY]))
  ).

updateAgentPosition(Action):-
  %not(Action =/= goforward),
  agent_orientation(Angle),
  (
    Action == turnleft ->  NewAngle is (Angle + 90) mod 360
  ; Action == turnright ->  NewAngle is (Angle + 270) mod 360
  ; NewAngle is Angle
  ),
  retract(agent_orientation(Angle)),
  assert(agent_orientation(NewAngle)).

/*
adjacent( [X1, Y1], [X2, Y2] ) :-
( X1 = X2, adj( Y1, Y2 )
; Y1 = Y2, adj( X1, X2 )
).
*/
adjacent( [X1, Y1], [X2, Y2] ) :-
(  
  ( X1 = X2, Y2 is Y1 + 1);
  ( Y1 = Y2, X2 is X1 + 1);
  ( X1 = X2, Y2 is Y1 - 1);
  ( Y1 = Y2, X2 is X1 - 1)
),X2 >0,Y2>0.


permitted([X,Y]) :-
  %world_size(WS),
  0 < X,% X < WS+1,
  0 < Y.%, Y < WS+1.

goforwardmove(NewAngle,[CX,CY],[NX,NY]) :-
  agentpos(CX,CY),
  (
    NewAngle == 0,NX is CX+1,NY is CY;
    NewAngle == 90,NX is CX,NY is CY+1;
    NewAngle == 180,NX is CX-1,NY is CY;
    NewAngle == 270,NX is CX,NY is CY-1
  ),
  permitted([NX,NY]).
goforwardmove([CX,CY],[NX,NY]):-
  agentpos(CX,CY),
  (
    agent_orientation(0),NX is CX+1,NY is CY;
    agent_orientation(90),NX is CX,NY is CY+1;
    agent_orientation(180),NX is CX-1,NY is CY;
    agent_orientation(270),NX is CX,NY is CY-1
  ),
  permitted([NX,NY]).

printYPITLIST:-
  findall(ispit(yes,X,Y),ispit(yes,X,Y),YPitList),
  foreach(member(X,YPitList),write(X)),
  (
    ispit(yes,_,_) -> nl
  ; true
  ).

printNPITLIST:-
  findall(ispit(no,X,Y),ispit(no,X,Y),NPitList),
  foreach(member(X,NPitList),write(X)),
  nl.
printYWUMPLIST:-
  findall(iswumpus(yes,A,B),iswumpus(yes,A,B),WumpusList),
  foreach(member(Y,WumpusList),write(Y)),
  (
    iswumpus(yes,_,_) -> nl
  ; true
  ).
printNWUMPLIST:-
  findall(iswumpus(no,A,B),iswumpus(no,A,B),WumpusList),
  foreach(member(Y,WumpusList),write(Y)),
  nl.

validpoints(VX,VY):-
  (VX = 1; VX = 2; VX = 3; VX = 4),
  (VY = 1; VY = 2; VY = 3; VY = 4).

updatemoves.
/*:-
  numberofmoves(X),
  N is X + 1,
  (N < 10).
*/
shoot_path(NX,NY):-
  agentpos(CX,CY),
  member(IX,[1,2,3,4,5,6,7,8,9,10]),
  (
    agent_orientation(0),NX is CX+IX,NY is CY;
    agent_orientation(90),NX is CX,NY is CY+IX;
    agent_orientation(180),NX is CX-IX,NY is CY;
    agent_orientation(270),NX is CX,NY is CY-IX
  ).
  
can_i_turn_to_kill_wumpus(yes):-
  wumpusalive(yes),
  foundwumpuslocation(WX,WY),
  agent_orientation(Angle),agentpos(CX,CY),
  member(Rotation,[90,180,270]),NewAngle is (Angle+Rotation) mod 360,
  member(IX,[1,2,3,4,5,6,7,8,9,10]),
  (
    NewAngle is 90,WX is CX,WY is CY+IX;
    NewAngle is 180,WX is CX-IX,WY is CY;
    NewAngle is 270,WX is CX,WY is CY-IX
  ).
can_i_turn_to_kill_wumpus(no).

can_i_kill_wumpus(yes):-
  wumpusalive(yes),
  foundwumpuslocation(WX,WY),
  findall([NX,NY],shoot_path(NX,NY),ShootList),
  member([WX,WY],ShootList).
  
can_i_kill_wumpus(no).


is_nbr_wumpus(NX,NY):-
  wumpusalive(yes),
  agentpos(CX,CY),
  adjacent([CX,CY],[NX,NY]),
  foundwumpuslocation(NX,NY).


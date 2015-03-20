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

:- dynamic agentpos/2,ispit/2,iswumpus/3,visited/3,forward/1,parent/2,numerofmoves/1,foundgold/1,wumpusalive/1,foundwumpuslocation/2.
:- dynamic actionList/1.

adj(1,2).
adj(2,1).
adj(2,3).
adj(3,2).
adj(3,4).
adj(4,3).



init_agent:-
  format('\n=====================================================\n'),
  format('This is init_agent:\n\tIt gets called once, use it for your initialization\n\n'),
  format('=====================================================\n\n'),
  assert(actionList([])).  

run_agent(_,[turnleft,goforward]).
run_agent(_,Action):-
  actionList(L),
  retract(actionList(L)),
  append(turnleft,goforward,L),
  assert(actionList(L)). 


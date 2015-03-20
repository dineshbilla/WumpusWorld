WumpusWorld
===========

Wumpus world in Prolog

http://robotics.cs.tamu.edu/dshell/cs625/asgn4.pdf

1. Open terminal and start the prolog interpreter
2. Load the simulator and agent
3. Type 'evaluate_agent(1, Score, Time).'

Properties of Agent:

1. Does a depth first style exploration of the world
2. Does not take a risky move, i.e will move to an adjacent cube only if it knows its safe
3. Finds all the gold pieces in the world, works for multiple gold pieces as well
4. If wumpus's location is found, it will be killed as it may contain another gold piece



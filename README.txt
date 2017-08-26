###################################################################################
##GREEDY GOLD GAME 
#algorithm written by Nick Valstar.
#
##Explanation
#A game of greedy-gold works as follows:
#n>0 stacks of gold laying next to each other in a row, each stack containing an amount of gold (>=0).
#the first player choses the far left or far right stack, takes the gold laying there and removes the stack from the game. 
#then the next player does the same, etc. until there are no stacks left.
#your goal is to gather as much gold as possible.

#This game becomes exponentially harder when playing with more stacks, because you must consider what your opponent will do.
#This is an algorithm that will give you the optimal choice (left or right).
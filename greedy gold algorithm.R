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

##Examples:
#board [1 2 3]. player one plays 'right' and takes the 3. 
best.choice(c(1,2,3))
#board remaining: [1 2]. player two plays 'right' and takes the 2. 
best.choice(c(1,2))
#board remaining [1]. player one takes the 1.
best.choice(c(1))
#outcome: player one has gathered 4 gold. this was the best he/she could do.
exp.gold(c(1,2,3))

#What would you do given the following board?
best.choice(c(4,3,6,7,2,4,0,8,3,7,2,8,3,6,2,7)) #takes 1 minute.
###################################################################################

#Given the current board, returns the best choice (take left or right) you can make right now, 
#..taking into account your and your opponent's future optimal move(s).
#note that even when your opponent does not choose optimally, your choice will be still optimal.
best.choice <- function(g){
      optimal.choice <- 'right'
      if(exp.gold(g,'left') >= exp.gold(g,'right'))
            optimal.choice <- 'left'
      #print.round(g,optimal.choice)
      return(optimal.choice)
}

#Given the current board, returns the sum of the gold you will gain from the game when playing optimally.
exp.gold <- function(g,my.choice){
  exp_sum <- take.gold(g,my.choice)
  if (length(g)<=2)
    return (exp_sum)
  his.choice <- best.choice(remove.gold(g,my.choice))
  new.board <- remove.gold(remove.gold(g,my.choice),his.choice) #we both play
  exp_sum <- exp_sum + exp.gold(new.board,best.choice(new.board))   
  return(exp_sum)
}

#takes a stack of gold from the current board, either the most left or the most right one, 
#and returns the amount of gold laying there.
take.gold <- function(g,choice){
      if (choice=='left')
            return(g[1])
      return(g[length(g)])
}

#removes a stack of gold from the current board, either the most left or the most right one, 
#and returns the remaining board.
remove.gold <- function(g,choice){
      n=length(g)
      if(choice=='left')
            return(g[2:n]) #remove left tile
      return(g[1:n-1]) #remove right tile
}

print.round <- function(g,optimal.choice='nothing yet.'){
      text <- 'board: '
      for (i in 1:length(g))
            text <- paste(text,g[i])
      text <- paste(text,', so player chooses ',optimal.choice)
      print(text)
}

a <- best.choice(c(2,10,8,1)) 
a #right
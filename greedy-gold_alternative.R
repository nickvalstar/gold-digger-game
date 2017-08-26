###################################################################################
##GREEDY_GOLD game explanation
#A game of greedy-gold works as follows:
#n>0 stacks of gold laying next to each other in a row, each stack containing an amount of gold (>=0).
#the first player choses the far left or far right stack, takes the gold laying there and removes the stack from the game. 
#then the next player does the same, etc. until there are no stacks left.
#your goal is to gather as much gold as possible.

#This game becomes exponentially harder when playing with more stacks, because you must consider what your opponent will do.
#This is an algorithm that will give you the optimal choice (left or right).

##Examples:
#board [1 2 3]. player one plays 'right' and takes the 3. 
get_best_choice(c(1,2,3))
#board remaining: [1 2]. player two plays 'right' and takes the 2. 
get_best_choice(c(1,2))
#board remaining [1]. player one takes the 1.
get_best_choice(c(1))
#outcome: player one has gathered 4 gold. this was the best he/she could do.
waarde(c(1,2,3))

#What would you do given the following board?
get_best_choice(c(4,3,6,7,2,4,0,8,3,7,2,8,3,6,2,7)) #takes 1 minute.
###################################################################################

#Given the current board, returns the best choice (take left or right) you can make right now, 
#..taking into account your and your opponent's future optimal move(s).
#note that even when your opponent does not choose optimally, your choice will be still optimal.
get_best_choice <- function(bord){
  n=length(bord) #hoeveel stapels liggen er nog
  if(n<=1){return('left')} #nog maar 1 keuze
  #if(n==2){return(max(bord))} #als bord 1 of 2 groot is, pak gewoon de hoogste
  #check left
  best_choice_next <- get_best_choice(remove(bord,'left')) #wat doet tegenstander als ik left doe
  left_choice_waarde <- take(bord,'left') + waarde(remove(bord,'left',best_choice_next)) #waarde van het bord na ik left doe, en hij zn beste keus daarna
  #check right
  best_choice_next <- get_best_choice(remove(bord,'right')) #wat doet tegenstander als ik right doe
  right_choice_waarde <- take(bord,'right') + waarde(remove(bord,'right',best_choice_next)) #waarde van het bord na ik right doe, en hij zn beste keus daarna
  #beste keus?
  if(right_choice_waarde > left_choice_waarde) {return('right')}
  return('left')
}

#Given the current board, returns the sum of the gold you will gain from the game when playing optimally.
waarde <- function(bord){
  n=length(bord) #hoeveel stapels liggen er nog
  if(n<=0){return(0)} #bord is leeg
  if(n<=2){return(max(bord))} #als bord 1 of 2 groot is, pak gewoon de hoogste
  #moet ik links of rechts kiezen?
  my_best_choice <- get_best_choice(bord)
  #wat zal hij daarna kiezen? dus nadat ik mijn beste keuze heb gemaakt.
  his_best_choice <- get_best_choice(remove(bord,my_best_choice))
  #mijn waarde nu als ik optimaal kies, en wat er dan overblijft nadat hij kiest
  my_waarde <- take(bord,my_best_choice)+waarde(remove(bord,my_best_choice,his_best_choice))
  return(my_waarde)
}

#takes a stack of gold from the current board, either the most left or the most right one, 
#and returns the amount of gold laying there.
take <- function(bord, #current board
                 this_move #your choice: left or right
                 ){
  n=length(bord)
  if(n<=0){return(0)} #return 0 gold
  if(this_move=='left'){
    return(bord[1])
  } else { #rechts
    return(bord[n])
  }
}

#removes a stack of gold from the current board, either the most left or the most right one, 
#and returns the remaining board.
#note: you may optionally remove two stacks consequtively
remove <- function(bord,#current board
                   this_move,#your choice: left or right
                   next_move=NA #optional: your opponents choice after that: left or right
                   ){
  n=length(bord)
  if(n<=1){return(c(0))} #return dan een leeg bord (1 stapel met 0 gold)
  #eerste keus:
  if(this_move=='left'){
    bord <- bord[2:n]
  } else { #rechts
    bord <- bord[1:(n-1)]
  }
  if(!is.na(next_move)){ #dan nog een move (met een lege next_move)
    return(remove(bord,next_move))
  }
  return(bord)
}


########################################### tests
stopifnot(get_best_choice(c(1))=='left')
stopifnot(get_best_choice(c(4,8))=='right')
stopifnot(get_best_choice(c(14,8))=='left')
stopifnot(get_best_choice(c(14,14))=='left')
stopifnot(get_best_choice(c(0))=='left')
stopifnot(get_best_choice(c(1,2,3))=='right')
stopifnot(get_best_choice(c(1,2,3,4))=='right')
stopifnot(get_best_choice(c(4,3,2,1))=='left')
stopifnot(get_best_choice(c(1,3,2,1))=='right')

stopifnot(waarde(c(1))==1)
stopifnot(waarde(c(4,8))==8)
stopifnot(waarde(c(14,8))==14)
stopifnot(waarde(c(14,14))==14)
stopifnot(waarde(c(0))==0)
stopifnot(waarde(c(1,2,3))==4)
stopifnot(waarde(c(1,2,3,4))==6)
stopifnot(waarde(c(4,3,2,1))==6)
stopifnot(waarde(c(1,3,2,1))==4)

stopifnot(remove(c(1,2),'left')==c(2))
stopifnot(remove(c(1,2,3),'right')==c(1,2))
stopifnot(remove(c(1),'left')==c(0))
stopifnot(remove(c(1,2,3),'right','left')==c(2))
stopifnot(remove(c(4,3,5,6),'left','left')==c(5,6))
stopifnot(remove(c(1,2),'right','left')==c(0))

stopifnot(take(c(1,2),'left')==1)
stopifnot(take(c(1,2,3),'right')==3)


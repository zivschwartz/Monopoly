gameboard <- data.frame(space = 1:40, title = c("Go" , "Mediterranean Avenue" , "Community Chest" , "Baltic Avenue" , "Income Tax" , "Reading Railroad" , "Oriental Avenue" , "Chance" , "Vermont Avenue" , "Connecticut Avenue" , "Jail" , "St. Charles Place" , "Electric Company" , "States Avenue" , "Virginia Avenue" , "Pennsylvania Railroad" , "St. James Place" , "Community Chest" , "Tennessee Avenue" , "New York Avenue" , "Free Parking" , "Kentucky Avenue" , "Chance" , "Indiana Avenue" , "Illinois Avenue" , "B & O Railroad" , "Atlantic Avenue" , "Ventnor Avenue" , "Water Works" , "Marvin Gardens" , "Go to jail" , "Pacific Avenue" , "North Carolina Avenue" , "Community Chest" , "Pennsylvania Avenue" , "Short Line Railroad" , "Chance" , "Park Place" , "Luxury Tax" , "Boardwalk"))

chancedeck <- data.frame(index = 1:15, card = c("Advance to Go" , "Advance to Illinois Ave." , "Advance to St. Charles Place" , "Advance token to nearest Utility" , "Advance token to the nearest Railroad" , "Take a ride on the Reading Railroad" , "Take a walk on the Boardwalk" , "Go to Jail" , "Go Back 3 Spaces" , "Bank pays you dividend of $50" , "Get out of Jail Free" , "Make general repairs on all your property" , "Pay poor tax of $15" , "You have been elected Chairman of the Board" , "Your building loan matures"))

communitydeck <- data.frame(index = 1:16, card = c("Advance to Go" , "Go to Jail" , "Bank error in your favor. Collect $200" , "Doctor's fees Pay $50" , "From sale of stock you get $45" , "Get Out of Jail Free" , "Grand Opera Night Opening" , "Xmas Fund matures" , "Income tax refund" , "Life insurance matures. Collect $100" , "Pay hospital fees of $100" , "Pay school tax of $150" , "Receive for services $25" , "You are assessed for street repairs" , "You have won second prize in a beauty contest" , "You inherit $100"))

# Dice --------------------------------------------------------------------

dice <- function(verbose=FALSE){
  faces <- sample(1:6, 2, replace=TRUE)
  if(faces[1] == faces[2]) doubles = TRUE
  else doubles = FALSE
  movement = sum(faces)
  if(verbose) cat("Rolled:", faces[1], faces[2], "\n")
  return(list(faces=faces, doubles=doubles, movement=movement))
}

# Manual Dice -------------------------------------------------------------

Dice = setRefClass("Dice", 
                   fields = list(
                     rolls = "numeric",
                     pos = "numeric",
                     verbose = "logical"
                   ), 
                   methods = list(
                     roll = function() {
                       faces = rolls[pos + seq_len(2)]
                       pos <<- pos + 2
                       if(faces[1] == faces[2]) doubles = TRUE
                       else doubles = FALSE
                       movement = sum(faces)
                       if(verbose) cat("Rolled:", faces[1], faces[2], "\n")
                       return(list(faces=faces, doubles=doubles, movement=movement))
                     }
                   )
)

utility <- gameboard$space == 13|gameboard$space == 29 #two utility spaces
railroads <- gameboard$space == 6|gameboard$space == 16|gameboard$space == 26|gameboard$space == 36 #four railroads

# a **very basic** reference class for our players
player <- setRefClass("player", 
  fields = list(
    pos = "numeric",      # position on the board
    is_jailed = "logical",
    ndoubles = "numeric",
    verbose = "logical"
  ), 
  methods = list(
    move_n = function(n) {
      if(verbose) 
        cat("Player at:", pos)
      if(verbose) 
        cat(" Player moves:", n)
      pos <<- pos + n
      if(pos > 40) 
        pos <<- pos - 40
      if(verbose) 
        cat(" Player now at:", pos, "\n")
    },
    go_2_space_n = function(n){
      if(verbose) 
        cat("Player at:", pos, ".")
      pos <<- n
      if(verbose) 
        cat(" Player now at:", pos, ".\n")
    },
    
    ##A. Chance Card
    #There are nine cards (of 15) in the Chance deck that move the player's token.
    chance_card = function(){
      ncard <- sample(chancedeck$index, 1)
      if(verbose) 
        cat("Player Picked a Chance Card! \n")
      if(ncard == 1) 
        pos <<- 1 #Advance to Go
      if(ncard == 2) 
        pos <<- 25 #Advance to Illionis Ave
      if(ncard == 3) 
        pos <<- 12 #Advance to St. Charles Place
      if(ncard == 4){ #Advance token to nearest Utility
        for(i in pos:length(utility)){ #Move to nearest utility between current position and utility
          if(utility[i]){ 
            pos <<- i
            break
          }
          else if(i == 40) #After second utility and at final position, move to first utility at 13
            pos <<- 13
        }
      }
      if(ncard == 5){ #Advance token to the nearest Railroad
        for(i in pos:length(railroads)){ #Move to nearest railroad between current position and utility
          if(railroads[i]){
            pos <<- i
            break
          }
          else if(i == 40) #After fourth railroad and at final position, move to first railroad at 6
              pos <<- 6
        }
      }
      if(ncard == 6) 
        pos <<- 6 #Take a ride on the Reading Railroad
      if(ncard == 7) 
        pos <<- 40 #Take a walk on the Boardwalk
      if(ncard == 8){ #Go to Jail
        pos <<- 11
        is_jailed <<- TRUE
        if(verbose)
          cat("8th Chance Card selected: 'Go to Jail' ")
      }
      if(ncard == 9) 
        pos <<- pos - 3 #Go Back 3 Spaces
    },

    ##B. Community Chest Card
    #There are two cards (of 16) in the Community Chest deck that move the player's token
    community_chest_card = function(){
      ncard <- sample(communitydeck$index, 1)
      if(verbose) 
        cat("Player Picked a Community Chest Card! \n")
      if(ncard == 1) 
        pos <<- 1 #Advance to Go
      if(ncard == 2){ #Go to Jail
        pos <<- 11
        is_jailed <<- TRUE
      }
    },
    
    ##C. Landing on 'Go to Jail'
    go_to_jail = function(player, n){
      if(verbose) 
        cat("Going to Jail! \n")
      pos <<- 11
      is_jailed <<- TRUE
    },
    
    ##D. Rolling Doubles
    rolling_doubles = function(move){
      if(move$doubles){
        ndoubles <<- ndoubles + 1
        if(verbose) 
          cat("The count of doubles is currently:", ndoubles,"\n")
        
    ##E. Going to jail for rolling three doubles    
        if(ndoubles == 3){
          if(verbose) 
            cat("Going to Jail! \n")
          pos <<- 11
          ndoubles <<- 0
          is_jailed <<- TRUE
          return(FALSE)
        }
        else 
          return(TRUE)
      }
      else{
        ndoubles <<- 0
        return(TRUE)
      }
    },
    
    #Re-Rolling after a player has rolled a double
    re_rolling = function(player, tracking, move){
      if(move$doubles){
        if(verbose) 
          cat("Player just rolled a double, please take another turn. \n")
        taketurn(player, tracking)
      }
    },
    
    ##F. Jail Functionality
    jail_functionality = function(move){
      if(move$doubles){
        is_jailed <<- FALSE
        ndoubles <<- 0
        if(verbose)
          cat("In Jail: Player rolled doubles \nGets out of jail. \n")
        return(TRUE)
      }
      else if(ndoubles == 2){
        is_jailed <<- FALSE
        ndoubles <<- 0
        if(verbose)
          cat("In Jail: Players third turn in jail \nGets out of jail. \n")
        return(TRUE)
      }
      else{
        if(verbose)
          cat("Staying in Jail. \n")
        ndoubles <<- ndoubles + 1
        return(FALSE)
      }
    }
  )
)

player1 <- player$new(pos = 1, is_jailed = FALSE, ndoubles = 0, verbose = TRUE)  # create new players
player2 <- player$new(pos = 1, is_jailed = FALSE, ndoubles = 0, verbose = TRUE)


# Space Tracking Reference Class ------------------------------------------

# a *basic* reference class to keep track of where people landed
tracking <- setRefClass("tracking",
  fields = list(
    tally = "numeric"
  ),
  methods = list(
    increase_count = function(n){
      tally[n] <<- tally[n] + 1
    }
  )
)

space_tracking <- tracking$new(tally = rep(0,40))


# Taking a turn -----------------------------------------------------------

taketurn <- function(player, tracking){
  roll <- dice()
  #roll <- dice(verbose = TRUE)  # this will only work if you are not using the manual dice
  #player$move_n(roll$movement)
  #tracking$increase_count(player$pos)
  
  jailed <- FALSE
  
  #When player is jailed
  if(player$is_jailed)
    jailed <- player$jail_functionality(roll)
  double_roll <- TRUE
  if(jailed){
    double_roll <- TRUE
    roll$doubles <- FALSE
    jailed <- FALSE
  }
  else
    double_roll <- TRUE
  
  #When player is NOT jailed
  if(player$is_jailed == FALSE)
    double_roll <- player$rolling_doubles(roll)
  
  #When a double is rolled and player not in jail
  if(double_roll && !player$is_jailed){
    player$move_n(roll$movement)
    if(player$pos %in% gameboard[gameboard$title == "Chance",1])
      player$chance_card()
    if(player$pos %in% gameboard[gameboard$title == "Community Chest",1])
      player$community_chest_card()
    if(player$pos %in% gameboard[gameboard$title == "Go to jail",1])
      player$go_to_jail(player, player$pos)
  }
  
  tracking$increase_count(player$pos)
  
  #After rolling a double, checking to see if another double was rerolled
  if(double_roll && !player$is_jailed)
    player$re_rolling(player, tracking, roll)
}


# testing one turn --------------------------------------------------------

set.seed(1)

taketurn(player1, space_tracking)  # roll a 2,3 the player is now on space 6
taketurn(player2, space_tracking)  # roll a 4,6 the player is now on space 11

# check to verify
space_tracking$tally  ## tallys have been updated to show that spot 6 and spot 11 have been landed on 1 time each
player1  # shows that player 1 is on spot 6


#####1000 Games Simulation
set.seed(1)

space_tracking <- tracking$new(tally = rep(0,40))
for(i in 1:1000){ # simulate 100 games
  #cat("#### NEW GAME",i,"##### \n")
  player1 <- player$new(pos = 1, is_jailed = FALSE, ndoubles = 0, verbose = FALSE)  # new players for each game
  player2 <- player$new(pos = 1, is_jailed = FALSE, ndoubles = 0, verbose = FALSE)
  for(i in 1:150){ # 150 turns for each game
    if(player1$verbose) cat("Player 1 turn\n")
    taketurn(player1, space_tracking)
    if(player2$verbose) cat("Player 2 turn\n")
    taketurn(player2, space_tracking)
  }
}

# the results after 100 turns. No rules have been implemented
results <- cbind(gameboard, tally = space_tracking$tally)
results <- cbind(results, rel = results$tally/sum(results$tally)) #table in the order of the spaces on the board.
print(results) 
sum(results$tally)


library(dplyr)
library(tidyr)
library(ggplot2)
frequency_table <- results %>%
  arrange(desc(rel))
print(frequency_table) #table in descending order of frequency of landing
print(results) #table in the order of the spaces on the board.
results_hist <- results %>%
  ggplot(aes(x = rel)) + geom_histogram(breaks=seq(0, .35, by = 0.01))
results_hist #extra histogram too look at distribution of relative landing frequency


#####20 Rolls Simulation
# a *basic* reference class to keep track of where people landed
#Implementing the tracking reference class again, adding a tally for verbose during the 20 rolls
tracking <- setRefClass("tracking",
  fields = list(
    tally = "numeric"
  ),
  methods = list(
    increase_count = function(n){
      tally[n] <<- tally[n] + 1
      cat("Landed on ", n, " times.", sep = "") #Counts the number of times landed on this position
    }
  )
)

set.seed(10)
setdice <- Dice$new(rolls = c(6, 4, 5, 3, 3, 5, 6, 2, 5, 4, 4, 1, 2, 6, 4, 4, 4, 4, 2, 2, 
                              4, 3, 4, 4, 1, 4, 3, 4, 1, 2, 3, 6, 5, 4, 5, 5, 1, 2, 5, 4, 
                              3, 3, 1, 1, 2, 1, 1, 3),
                    pos = 0, verbose = TRUE)
dice <- function() setdice$roll()
space_tracking <- tracking$new(tally = rep(0,40))
player1 <- player$new(pos = 1, is_jailed = FALSE, ndoubles = 0, verbose = TRUE)  # new players for each game
for(i in 1:20){ # 100 turns for each game
  cat("\n## Turn", i,"\n")
  taketurn(player1, space_tracking) 
}

space_tracking$tally


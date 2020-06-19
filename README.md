# Monopoly

Create a simulation of the classic board game, Monopoly. The goal is to find out which spaces on the board get landed on the most.
Run 1,000 simulations of a two-player game that lasts 150 turns. This is a total of over 6 hundred thousand dice rolls - 1000 games x 150 turns x 2 players x 2 dice + additional rolls if the player gets doubles.
Keep track of where the players land. We ultimately want to build a distribution showing which spaces are most likely to be landed upon. Advance the tokens around the board according to the rules. Keep in mind the special situations involving the cards, jail, and rolling doubles. After 150 turns, reset the game and start over. Simulate 1000 games.

Code includes seperate reference classes for the player and gameboard. Player implements the following methods:
  a. drawing chance card
  b. drawing community chest card
  c. landing on "go to jail"
  d. roll again for rolling doubles
  e. going to jail for rolling three doubles
  f. jail functionality

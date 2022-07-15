TicTacToe
=========

A (questionably) playable game of tic tac toe. The rules are simple:
1. Two players (Circle and Cross) play in turns. Circle starts first
2. A prompt will ask for you to move. Enter TL, T, TR, CL, C, CR, BL, B, BR to control. Rows are split into T, C, B for top, center and bottom, whilst the columns are split into L, // , R for left, (neutral), and right. Overlapping moves will cause the turn to restart
3. Try to connect three of your own spaces to win (Horizontal, Vertical and Diagonal.) If there is a winner, the game will stop. If all 9 spaces are filled and there are no winner, the game draws and ends.

Overview of the program
==========================

The idea behind the logic is to create a stack of moves. I found that it was unnecessary to store the data for the current board state if I can just recreate the board state between moves using the current stack of moves. This made the state a lot simpler because I originally intend to create a tuple of both the moves and the board state. The stack also allowed easier implementation of functions like revert moves because popping off a stack is very simple.

For each move, the program has to check whether a move is valid. This includes checking if the move made will overlap, or if the same player is called twice. In the main program I made sure that players will be called in an alternating fashion as to not trigger the "Bad input" from the moving.

I wish to improve on the controls of the game a bit more, as well as the graphics, which I think are two problems that are bundled together. I cannot directly interact with the board because of the poor GUI leading to the need of using strings to convey a command. It will be nice to be able to just hit a square and play the move...
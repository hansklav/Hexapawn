# Hexapawn
A self learning chess-like game: the simplest form of Artificial Intelligence.

It was Ken Thomsom, the programmer of the early versions of UNIX, who made me curious how to program a self-learning version of a simple game. In the [1st Edition UNIX Programmer's Manual (1971)](https://web.archive.org/web/20060314022603/http://cm.bell-labs.com/cm/cs/who/dmr/1stEdman.html) in section VI (User-maintained software)
a "learning" Tic-Tac-Toe program by Ken is mentioned "that never makes the same mistake twice". He programmed it in DEC PDP-7 assembler and I had no idea how that could be done. Some time later I found a description and source code by Jeff Dalton of the game Hexapawn in the first (1973) edition of "[101 BASIC Computer Games](https://archive.org/details/bitsavers_decBooks10Mar75_26006648/page/n121/mode/2up?view=theater)" (editied by [David Ahl](https://en.wikipedia.org/wiki/David_H._Ahl), and published by DEC), stating that "The program learns by elimination of bad moves". This sparked my interest.

Hexapawn was unknown to me but I was led to [a 1962 article by Martin Gardner](http://cs.williams.edu/~freund/cs136-073/GardnerHexapawn.pdf) in the Mathematical Games columns of Scientific American. It describes a board game played by human against a hypothetical "computer" with six chess pawns on a 3×3 board. Before a series of games the "computer", called by Martin Gartner HER (Hexapawn Educatable Robot) or HIM (Hexapawn Instructable Robot), has zero knowledge of playing-strategy, at first makes random moves and loses several times before learning how to play well. After ten to twenty games (of at most eight turns (= four moves) each) s(h)e invariably gets invincible. HER/HIM consists of 24 matchboxes, one for each possible state (position) of the game, filled with coloured beads, one for every legal move that can be made from this state. When a move indicated by a box immediately leads to the "computer's" defeat the bead with the colour that represents that move is taken away from that box, so that particular move will not be played again by the machine. When a box for a particular position is empty (all moves from it led to defeat), s(h)e resigns, and the bead from the pre-last box (that got it there) is removed. If this explanation sounds a bit too abstract for you then watching <a href="https://www.youtube.com/watch?v=sw7UAZNgGg8" target="_blank" rel="noopener noreferrer">this YouTube video</a> will clear things up. 

Later editions of the above mentioned book, now titled "BASIC Computer Games", for instance used [here](https://github.com/coding-horror/basic-computer-games/tree/main/46_Hexapawn), have a Hexapawn BASIC implementation by R.A. Kaapke from 1976 with the whole game tree already "hard wired" into the source code (just like Martin Gardner's matchboxes machine). The original 1974 version of the BASIC program by Jeff Dalton is a bit more interesting in that it builds from scratch the in-memory database of all possible game states and legal moves from them. So I decided to use that code as a basis for my own implementation in Oberon-07.

Because I had no way to run the DEC BASIC program and I could not make sense of the program text I translated it to BASICA, which I could run using the DOSBox and Boxer MS/PC&nbsp;DOS emulators. Figuring out line by line what it did I gradually gained insight into its logic. Then I translated it to Oberon. The Oberon code stays fairly close to the BASIC code and is cross-referenced with the BASIC line numbers in order to help you deciphering the BASIC source.

To me it is quite amazing that Jeff Dalton as a high school student managed to make this program using the (imho) primitive tool that was BASIC in those days. I am not surprised to learn (from the Internet) that he ended up being a computer scientist specializing in Artificial Intelligence.

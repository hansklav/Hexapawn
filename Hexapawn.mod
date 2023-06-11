MODULE Hexapawn;
(*
  This program plays the game 'Hexapawn' by a method outlined by Martin Gardner in
  'Mathematical Games', Scientific American, March 1962.
  http://cs.williams.edu/~freund/cs136-073/GardnerHexapawn.pdf

  It is programmed in Oberon-07 using the OBNC compiler ( https://miasap.se/obnc/ ).
  Oberon-07 is the latest descendent of Pascal, designed by Niklaus Wirth himself.
  
  After a Digital Equipment Corp. BASIC-PLUS program for the PDP-11 by Jeff Dalton (1974)
  that I found in an early DEC edition of the book '101 BASIC Computer Games', p. 122.
  http://www.bitsavers.org/pdf/dec/_Books/101_BASIC_Computer_Games_Mar75.pdf
  
  You can find the original BASIC-PLUS source here: https://github.com/hansklav/Hexapawn/tree/main/BASIC
  There you can also find my rewrite to BASICA (IBM/Microsoft BASIC). In order to decipher 
  the BASIC source (which for me was quite impenetrable) I made some cross-references
  in the Oberon-07 comments to the BASIC lines and variables. To stay close to the BASIC
  source the Oberon-07 code is not optimally structured; especially the extensive use
  of global variables is not to be recommended.
  
  The program implements a simple self-learning machine which Martin Gardner
  called HER (Hexapawn Educatable Robot) or HIM (Hexapawn Instructable Robot). 
  The robot learns by the elimination of bad moves. 
  All board positions encountered by the program that have acceptable moves from them 
  are stored in a global database (array) cDB. When the program encounters an unfamiliar
  position, that position and all legal moves from it are added to cDB. If HER loses 
  a game, it erases the moves that led to defeat from cDB. If it hits a position from 
  which all moves have been deleted (they all led to defeat), it erases the move that 
  got it there and resigns.
  Eventually, HER/HIM learns to play extremely well and, indeed, is unbeatable.
  The learning strategy could be adopted to other simple games with a finite number
  of moves (tic-tac-toe, small board checkers, or other chess-based games).
*)

  IMPORT Input := Input0, In, Out, Random;
  
  CONST 
    TAB = 9X;  CR = 0DX;
    maxDB = 25;               (* max. index of the cDB array *)
    lenMov = 8;               (* length of an array of moves *)
    lenStr = 64;              (* length of a String *)
    nSquares = 9;             (* the 9 squares of the board *)
  
  TYPE
    Board = ARRAY nSquares + 2 OF CHAR;  (* 9 squares, [0] unused, [LEN(Board)] = 0X  *)
                                         (* Indices: 1 2 3   Startposition: * * *
                                                     4 5 6                  - - -
                                                     7 8 9                  O O O  
                                                             O = player  * = computer *)
    Move = RECORD 
      from, to: BYTE
    END;

    MoveArray = ARRAY lenMov OF Move;

    PositionData = RECORD
      turnNr: INTEGER;        (* <- (NR) OF MOVE IN GAME> (better: half-move or turn) *)
      board: Board;           (* <POSITION> *)
      N: BYTE;                (* current number of elements in array moves *)
      moves: MoveArray        (* <LIST OF MOVES> M$ *)
    END;

    String = ARRAY lenStr OF CHAR;
    

  VAR  
    (* All global variables start with c ('current'; resembles a G, 'Global')           *)
    cDB: ARRAY maxDB+1 OF PositionData; (* P$(I%): database for turnnumbers and 
                                     board positions with matching moves,
                                     actively managed by HER/HIM; index 0 is not used   *)
    cPos: PositionData;       (* C$ (310): copy of one element of cDB                   *)
    cDbIndex,                 (* C% (310-320): index in cDB of current known position   *)
    cOldIndex: INTEGER;       (*               index in cDB of previous known position  *)
    cDbN: INTEGER;            (* Q%: current number of elements in array cDB            *)
    cMoveC: Move;             (* K$: current move of computer                           *)
    cLegal: MoveArray;        (* M$: holds the legal moves for a board position         *)
    cLegalN: INTEGER;         (*     current number of elements in array cLegal         *)
    cBoard: Board;            (* P$: current board position of the pawns                *)
    cDir: INTEGER;            (* D%: current direction: -1 = player, 1 = computer       *)
    cTurnCh: CHAR;            (* Q$: current turn character: "O" or "*"                 *)
    cTurnNr,                  (* P%: nr of current turn; Computer does turns 2, 4 and 6 *)       
    cWcomp, cWuser: INTEGER;  (* W1%, W2%: number of games won by Computer or User      *)

  
  PROCEDURE WriteLn(s: ARRAY OF CHAR);
  BEGIN
    Out.String(s); Out.Ln
  END WriteLn;

  
  PROCEDURE InputMove (s: String; VAR mv: Move);
    VAR a, b: INTEGER; (* A$, B$ *)  c: CHAR;  (* comma *)
      i: INTEGER;
      
    PROCEDURE ReadSeparator(VAR ch: CHAR);
    BEGIN
      REPEAT In.Char(ch) UNTIL (ch # " ") & (ch # TAB)
    END ReadSeparator;
    
  BEGIN 
    i := 0;
    REPEAT INC(i);
      Out.String(s);  Out.String(" ");
      In.Int(a);  ReadSeparator(c);  In.Int(b);
      IF i = 5 THEN  (* to prevent an infinite loop after ^D *)
        Out.Ln; Out.Ln;
        WriteLn("Input error again!");
        WriteLn("Or you pressed Ctrl+D at this prompt, which you shouldn't.");
        WriteLn("To stop the program in the middle of a game press Ctrl+Z or Ctrl+C.");       
        ASSERT(FALSE)
      ELSIF ~ In.Done OR (c # ",") THEN
        WriteLn("Input error: type two digits separated by a comma, then press <return>")
      END
    UNTIL In.Done & (c = ",");
    mv.from := a;  mv.to := b
  END InputMove;

  
  PROCEDURE InputChar (s: String): CHAR;
    VAR ch: CHAR;

    PROCEDURE FlushBuffer;  (* Empty the keyboard buffer *)
      VAR c: CHAR;
    BEGIN
      IF Input.Available() > 0 THEN
        REPEAT Input.Read(c) UNTIL Input.Available() = 0
      END
    END FlushBuffer;

  BEGIN
    FlushBuffer;
    Out.String(s);
    REPEAT
      Input.Read(ch)
    UNTIL (ch = "y") OR (ch = "Y") OR (ch = CR) OR 
          (ch = "n") OR (ch = "N") OR 
          (ch = "l") OR (ch = "L");
    Out.Ln
  RETURN ch
  END InputChar;
  
  
  PROCEDURE PrintBoard;                                        (* Lines 200 and 520 *)
    VAR i: INTEGER;
  BEGIN
    Out.Ln;
    Out.String("Board:"); Out.Ln;
    FOR i := 1 TO 9 DO 
      Out.Char(cBoard[i]); Out.Char(" ");
      IF i MOD 3 = 0 THEN Out.Ln END
    END;
    Out.Ln
  END PrintBoard;
  

  PROCEDURE ShowDB;
    VAR i, j: INTEGER;
  BEGIN
    Out.Ln;
    FOR i := 1 TO cDbN DO
      Out.Int(i, 2); Out.String(". ");
      Out.Int(cDB[i].turnNr, 0); Out.String(cDB[i].board); Out.String(" ");
      FOR j := 0 TO lenMov - 1 DO 
        Out.Int(cDB[i].moves[j].from, 0);
        Out.Int(cDB[i].moves[j].to, 0)
      END;
      Out.Ln
    END
  END ShowDB;

    
  PROCEDURE ComputerLost (index: INTEGER);
  (* Called when the Computer is defeated or resigns.
     The parameter is the index (K%) into cDB for the element that contains 
     the position and move that led to defeat or resignation. 
     Uses cMoveC to search cDB.
     Updates globals cWuser and cDB.
  *)
    VAR i, j: INTEGER;
  BEGIN                                                        (* Line 500 *)
    INC(cWuser);                                               (* W2%=W2%+1% *)
    (* Find the current Move of Computer cMoveC in the database *)
    i := 0;
    WHILE   (i < cDB[index].N)  & 
          ~ ( (cDB[index].moves[i].from = cMoveC.from) &
              (cDB[index].moves[i].to = cMoveC.to) )
    DO
      INC(i);
    END;
    
    IF (cDB[index].moves[i].from = cMoveC.from) & (cDB[index].moves[i].to = cMoveC.to) THEN
      (* Delete this move from the list *)
      FOR j := i TO cDB[index].N - 1 DO
        IF j = cDB[index].N - 1 THEN
          cDB[index].moves[j].from := 0;
          cDB[index].moves[j].to := 0
        ELSE
          cDB[index].moves[j] := cDB[index].moves[j+1];
          cDB[index].moves[j+1].from := 0;
          cDB[index].moves[j+1].to := 0
        END     
      END;
      DEC(cDB[index].N)
    ELSE
      Out.String("cMoveC not found in the DB. Actually this should not happen."); Out.Ln
    END
  END ComputerLost;
  
  
  PROCEDURE ListLegalMoves;
  (* Lines 2000-2060
     Lists (in the global array cLegal) all the valid moves for a given 
     turn direction (cDir) and board position (cBoard). 
     Updates cLegalN, the current number of elements of cLegal.
     Also updates the database cDB if the turn is for the computer and there
     are legal moves found.
  *)
    VAR i, j, k, t: INTEGER;                                   (* J%, T% *)
      
    PROCEDURE UpdateLegalMoves (i, f, t: BYTE);
    (* Lines 2020, 2030 and 2040:
       M$ := M$+FNN$(J%) + FNN$(T%) *)
    BEGIN
      cLegal[i].from := f;
      cLegal[i].to := t;
      cLegalN := i + 1
    END UpdateLegalMoves;
    
  BEGIN (*ListLegalMoves*)                                     
    (* Initialize cLegal *)                                    (* Line 2000 *)
    cLegalN := 0;                                                  
    FOR j := 0 TO LEN(cLegal) - 1 DO                           (* M$="" *)
      cLegal[j].from := 0;
      cLegal[j].to := 0
    END;
    (* Fill cLegal[] with the currently legal moves *)
    i := 0;
    FOR j := 1 TO 9 DO                                         (* Line 2010 *)
      IF cBoard[j] = cTurnCh THEN
        t := j + cDir * 3;  
        IF (1 <= t) & (t <= 9) THEN  (* forward moves *)
          IF cBoard[t] = "-" THEN                              (* Line 2020 *)
            UpdateLegalMoves(i, j, t);  INC(i)
          END
        END;
        t := j + cDir * 2;                                     (* Line 2025 *)
        IF (1 <= t) & (t <= 9) &
           ( (cDir =  1) & (j # 1) & (j # 4) & (j # 7) OR 
             (cDir = -1) & (j # 3) & (j # 6) & (j # 9)    )
        THEN  (* right diagonal moves *)
          IF (cBoard[t] # cTurnCh) & (cBoard[t] # "-") THEN    (* Line 2030 *)
            UpdateLegalMoves(i, j, t);  INC(i)
          END
        END;
        t := j + cDir * 4;                                     (* Line 2035 *)
        IF (1 <= t) & (t <= 9) & (j # 3) & (j # 7)             
        THEN  (* left diagonal moves *)
          IF (cBoard[t] # cTurnCh) & (cBoard[t] # "-") THEN    (* Line 2040 *)
            UpdateLegalMoves(i, j, t);  INC(i)
          END
        END
      END  (*IF cBoard[j]*)
    END; (*FOR*)                                               (* NEXT J% *) 
    IF (cDir = 1) & (cLegalN # 0) THEN                         (* Line 2050 *)
      (* Add this board position and its legal moves to cDB.
         Obviously it is a new position because ComputerMove first searches cDB
         before calling ListLegalMoves. *)
      INC(cDbN);                                               (* Q%=Q%+1 *)
      (* P$(Q%)=FNN$(P%)+P$+M$ *)
      cDB[cDbN].turnNr := cTurnNr;
      cDB[cDbN].board := cBoard;
      cDB[cDbN].moves := cLegal;
      (* Update the number of moves cDB[].N *)
      k := 0; 
      WHILE (cDB[cDbN].moves[k].from # 0) & (cDB[cDbN].moves[k].to # 0) DO
        INC(k)
      END;
      cDB[cDbN].N := k
    END
  END ListLegalMoves;
  

  PROCEDURE NoneLeftOf (ch: CHAR): BOOLEAN;
  (* Reports if no "*" or "O" is left over or not.
     Uses global variable cBoard, which should be updated before a call *)
    VAR i: INTEGER; res: BOOLEAN;
  BEGIN res := TRUE;                      (* Line 430 *)
    FOR i := 1 TO 9 DO
      IF cBoard[i] = ch THEN res := FALSE END
    END
  RETURN res
  END NoneLeftOf;
  
    
  PROCEDURE OtherSideReached (ch: CHAR; mv: Move): BOOLEAN;
  (* Uses global VAR cBoard, which should be updated before a call *)
    VAR i: INTEGER; res: BOOLEAN;
  BEGIN res := FALSE;                     (* Line 430 *)
    IF ch = "*" THEN
      FOR i := 7 TO 9 DO
        IF i = mv.to THEN res := TRUE END
      END
    ELSIF ch = "O" THEN
      FOR i := 1 TO 3 DO
        IF i = mv.to THEN res := TRUE END
      END
    END   
  RETURN res
  END OtherSideReached; 

      
  PROCEDURE PlayGame;
    VAR ch: CHAR;
      legal, gameEnded: BOOLEAN;
      userMove: Move;
      
    PROCEDURE PrintNumbering;             (* Line 120 *)
    BEGIN
      Out.Ln;
      WriteLn("Numbering:");
      WriteLn("1 2 3");
      WriteLn("4 5 6");
      WriteLn("7 8 9")
    END PrintNumbering;

    PROCEDURE NextPlayer;                 (* Lines 190 and 320 *)
    BEGIN
      INC(cTurnNr);                       (* P%; is only incremented (by 2) in line 300 *)
      IF ODD(cTurnNr) THEN
        cDir := -1;  cTurnCh := "O"       (* D%=-1%: Q$="O" *)
      ELSE
        cDir := 1;   cTurnCh := "*"       (* D%=1%: Q$="*" *)
      END
    END NextPlayer;
  
    PROCEDURE ReportGame;
    BEGIN 
      Out.String("I have won ");         Out.Int(cWcomp, 0);
      Out.String(" and you have won ");  Out.Int(cWuser, 0); 
      Out.String(" of ");                Out.Int(cWcomp + cWuser, 0); 
      Out.String(" games."); 
      Out.Ln;
    END ReportGame;
   
    PROCEDURE ComputerMove (): BOOLEAN;
      VAR i, n: INTEGER;
        tMove: Move;                                 (* K$: tentative move for computer *)
        finishGame, found: BOOLEAN; 
    BEGIN (* ComputerMove *)
      finishGame := FALSE;  found := FALSE;
      cOldIndex := cDbIndex;                         (* remember previous move *)
      cDbIndex := 0; 
      REPEAT                                         (* Line 310 *)
        INC(cDbIndex);  (* cDB[0] is not used *)
        cPos := cDB[cDbIndex];                       (* C$=P$(C%) *)
        IF (cPos.turnNr = cTurnNr) & (cPos.board = cBoard) THEN
          found := TRUE;  (* known position found in cDB *)
          cLegal := cPos.moves;
          cLegalN := cPos.N;
          IF cLegalN = 0 THEN                        (* Line 400 IF M$="" 'no moves left *)
            WriteLn("I resign.");
            ComputerLost(cOldIndex);                 (* GOTO 500 *)
            finishGame := TRUE
          END           
        END
      UNTIL (cDbIndex >= cDbN) OR found;  
                                                     (* Line 320 *)   
      IF ~found THEN
        ListLegalMoves;                              (* GOSUB 2000 *)
        cDbIndex := cDbN                             (* C%=Q% *)
      END;
                            
      IF (cLegalN = 0) & ~finishGame THEN            (* Line 320 IF M$="" 'no legal moves *)
        WriteLn("I can't move, you win.");
        ComputerLost(cOldIndex);                     (* GOTO 500 *)
        finishGame := TRUE
      ELSIF (cLegalN # 0) & ~finishGame THEN
        (* randomly choose one of the legal moves *)
        tMove := cLegal[0];  n := cLegalN;
        i := -1;
        WHILE (Random.Uniform() > 0.33333333) & (n # 0) DO
          INC(i);  tMove := cLegal[i];  DEC(n);      (* Line 410 *)
        END;
        cBoard[tMove.from] := "-";                   (* Line 420 *)
        cBoard[tMove.to] := "*";
        IF NoneLeftOf("O") OR OtherSideReached("*", tMove) THEN
          Out.String("I move from "); Out.Int(tMove.from, 1);
          Out.String(" to ");         Out.Int(tMove.to, 1);   
          WriteLn(". I win!");                       (* Line 430 *)
          INC(cWcomp);                               (* Line 510 *)
          finishGame := TRUE
        ELSE                                         (* Line 440 *)
          cMoveC := tMove;
          Out.String("I move from "); Out.Int(tMove.from, 1);
          Out.String(" to ");         Out.Int(tMove.to, 1);   Out.Ln
        END                                  
      END
    RETURN finishGame
    END ComputerMove;   

    PROCEDURE IsLegalMove(mv: Move): BOOLEAN;
      VAR res: BOOLEAN; i: INTEGER;
    BEGIN res := FALSE;
      FOR i := 0 TO cLegalN - 1 DO
        IF (mv.from = cLegal[i].from) & (mv.to = cLegal[i].to) THEN res := TRUE END
      END
    RETURN res
    END IsLegalMove;
      
  BEGIN (*PlayGame*)                                 (* Line 120 *)
    cTurnNr := 0;                                    (* P%=0% *)
    cBoard := " ***---OOO";                          (* P$="***---OOO" *)
    PrintNumbering;   
    gameEnded := FALSE;
    REPEAT                                           (* Line 190 *)
      (* User's move *)
      NextPlayer;
      ListLegalMoves;                                (* GOSUB 2000 *)
      IF cLegalN = 0 THEN                            (* IF M$="" THEN *)
        WriteLn("You can't move. I win.");
        INC(cWcomp);                                 (* Line 510 *)
(**)    gameEnded := TRUE  (* bug in BASIC program (cDB not updated)? *)
      ELSE
        PrintBoard;
        REPEAT                                       (* Line 200 *)
          InputMove("What is your move?", userMove); (* Line 210 *)
          legal := IsLegalMove(userMove);
          IF ~legal THEN WriteLn("Illegal move.") END
        UNTIL legal;
        cBoard[userMove.from] := "-";                (* Line 230 *)
        cBoard[userMove.to] := "O";
        IF NoneLeftOf("*") OR OtherSideReached("O", userMove) THEN
          WriteLn("You win!");
          ComputerLost(cDbIndex);                    (* GOTO 500 *)               
          gameEnded := TRUE
        ELSE
          (* Computer's move *)
          NextPlayer;                                (* Line 300 *)
          gameEnded := ComputerMove()
        END
      END
    UNTIL gameEnded;
    PrintBoard;  ReportGame;                         (* Line 520 *)
    ch := InputChar("Another game (y/n)?  --  L lists the database  ");
    IF (ch = "y") OR (ch = "Y") OR (ch = CR) THEN PlayGame
    ELSIF (ch = "l") OR (ch = "L") THEN ShowDB; PlayGame
    ELSIF (ch # "n") & (ch # "N") THEN PlayGame
    ELSIF (ch = "n") OR (ch = "N") THEN (* exit PlayGame *)
    END
  END PlayGame;


  PROCEDURE StartGame;                            
  BEGIN
    Random.Randomize;                                (* Line 110 *) 
    cDbN := 0;                                       (* Q%: number of elements in cDB *)
    cDbIndex := 0;
    cWcomp := 0;  cWuser := 0;                       (* W1%, W2%: numbers of won games *)
    Out.Ln; WriteLn("Since I'm a good sport, you'll always go first.");
    PlayGame
  END StartGame;


  PROCEDURE PrintInstructions;
  BEGIN 
    Out.Ln; Out.Char(TAB); 
    WriteLn("This program plays the game of HEXAPAWN.");
    WriteLn("HexaPawn is played with chess pawns on a 3 by 3 board.");
    WriteLn("The pawns are moved like in the game of chess, as follows:");
    WriteLn("- one space forward to an empty space or");
    WriteLn("- one space forward and diagonally to capture an opposing pawn.");
    Out.Ln; Out.Char(TAB); 
    WriteLn("On the board your pawns are 'O', the computer's pawns are");
    WriteLn("'*' and empty squares are '-'. To enter a move, type the number");
    WriteLn("of the square you will move FROM followed by the number of the");
    WriteLn("square you will move TO (separate the numbers by a comma), then");
    WriteLn("press the <return> or <enter> key.  E.g.: 8,5<return>");
    Out.Ln; Out.Char(TAB);
    WriteLn("The program starts a series of games knowing only when the game");
    WriteLn("is won and how to move (a draw is impossible). So at first the");
    WriteLn("computer has no strategy and just moves randomly. However, it learns");
    WriteLn("from each game. Thus, defeating it becomes more and more difficult.");
    WriteLn("Also, to help offset your initial advantage, you will not be told");
    WriteLn("how to win the game but must learn this by playing."); 
    Out.Ln;
    IF InputChar("Start first game (y/n)? ") = "y" THEN
      StartGame
    END
  END PrintInstructions;
  

BEGIN
  IF InputChar("Instructions (y/n)? ") = "y" THEN
    PrintInstructions 
  ELSE
    StartGame
  END
END Hexapawn.

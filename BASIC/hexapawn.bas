1 ' BY JEFF DALTON. CLASS OF '74, NORTHFlELD MOUNT HERMON SCHOOL.
2 ' THIS PROGRAM PLAYS THE GAME 'HEXAPAWN' BY A METHOD OUTLINED IN
3 '     'MATHEMATICAL GAMES' IN MARCH 1962 SCIENTIFIC AMERICAN.
4 ' THE PROGRAM LEARNS BY ELIMINATION OF BAD MOVES. ALL POSITIONS
5 '     ENCOUNTERED BY THE PROGRAM AND THE ACCEPTABLE MOVES FROM THEM
6 '     ARE STORED IN P$(I%).
7 ' WHEN THE PROGRAM ENCOUNTERS AN UNFAMILIAR POSITION, THE POSITION
8 '     AND ALL LEGAL MOVES FROM IT ARE ADDED TO THE LIST.
9 ' IF THE PROGRAM LOSES A GAME, IT ERASES THE MOVE THAT LED TO DEFEAT.
10'     IF IT HITS A POSITION FROM WHICH ALL MOVES HAVE BEEN DELETED 
11'     ( THEY ALL LED TO DEFEAT ), IT ERASES THE MOVE THAT GOT
12'     IT HERE AND RESIGNS.
13' From the book '101 BASIC Computer Games', Digital Equipment Corp., 1973
14' The original version HEX.BAS was written in DEC PDP-11 BASIC-PLUS.
15' This IBM-PC BASIC (BASICA, GW-BASIC) version by Hans Klaver, 2021.
16'
17 REM Put the user-defined functions before all other lines (to prevent a jump over them)
18 DEF FNR$(A$,N%)=MID$(A$,N%,LEN(A$)-N%+1%): 'Emulates DEC BASIC-PLUS function RIGHT(A$,N%)
19 REM DEF FNC$(X$,X%,Y$)=LEFT(X$,X%-1%)+Y$+RIGHT(X$,X%+LEN(Y$))    ' Original BASIC-PLUS version
20 DEF FNC$(X$,X%,Y$)=LEFT$(X$,X%-1%)+Y$+FNR$(X$,X%+LEN(Y$))
22 REM Copies Y$ over X$ starting at position X%; it is an error to try and make X$ longer
23 REM DEF FNN$(X%): X$=NUM$(X%): FNN$=MID(X$,2%,LEN(X$)-2%): FNEND ' Original BASIC-PLUS version
24 REM This function definition is impossible in IBM BASIC: NUM$ does not exist, and a user 
25 REM defined function cannot contain more than one statement. The patch is as follows:
26 DEF FNNUM$(X%)=STR$(X%)+" ": 'Gives the $tring of X%, with a leading space or minus sign, and with a trailing space; like NUM$.
27 DEF FNN$(X$)=MID$(X$,2%,LEN(X$)-2%)
28 REM FNN$(X$) removes the first and last characters of X$ (leading and trailing spaces).
29 REM Call like this: X$=FNNUM$(X%) ... FNN$(X$) ' But beware, see line 2051 !
30 INPUT "INSTRUCTIONS"; C$: IF LEFT$(C$,1%)<>"Y" THEN 110
31 PRINT:PRINT "THIS PROGRAM PLAYS THE GAME OF -*O HEXAPAWN O*-.":PRINT
32 PRINT "HEXAPAWN IS PLAYED WITH CHESS PAWNS ON A 3 BY 3 BOARD. THE PAWNS ARE"
33 PRINT "MOVED AS IN CHESS - ONE SPACE FORWARD TO AN EMPTY SPACE, OR ONE SPACE" 
34 PRINT "FORWARD AND DIAGONALLY TO CAPTURE AN OPPOSING MAN.":PRINT
35 PRINT "	ON THE BOARD YOUR PAWNS ARE '0', THE COMPUTER'S PAWNS ARE '*'"
36 PRINT "AND EMPTY SQUARES ARE '-'. TO ENTER A MOVE, TYPE THE NUMBER OF THE"
37 PRINT "SQUARE YOU WILL MOVE FROM FOLLOWED BY THE NUMBER OF THE SQUARE"
38 PRINT "YOU WILL MOVE TO (THE NUMBERS ARE SEPARATED BY A COMMA)."
39 PRINT "	THE PROGRAM STARTS A SERIES OF GAMES KNOWING ONLY WHEN THE"
40 PRINT "GAME IS WON (A DRAW IS IMPOSSIBLE) AND HOW TO MOVE. IT HAS NO"
41 PRINT "STRATEGY AT FIRST AND JUST MOVES RANDOMLY. HOWEVER, IT LEARNS"
42 PRINT "FROM EACH GAME. THUS, DEFEATING IT BECOMES MORE AND MORE"
43 PRINT "DIFFICULT. ALSO, TO HELP OFFSET YOUR INITIAL ADVANTAGE, YOU WILL"
44 PRINT "NOT BE TOLD HOW TO WIN THE GAME BUT MUST LEARN THIS BY PLAYING.":PRINT
45 INPUT "WOULD YOU PLEASE PRESS THE 'RETURN' KEY TO CONTINUE"; R$
110 DIM P$(50%): RANDOMIZE TIMER
111     Q%=0%: PRINT: PRINT "SINCE I'M A GOOD SPORT, YOU'LL ALWAYS GO FIRST
120 P%=0%: P$="***---OOO": PRINT: PRINT"NUMBERING:"
121     PRINT"123": PRINT"456": PRINT"789": PRINT
190 D%=-1%: Q$="O": GOSUB 2000
191     IF M$="" THEN PRINT "YOU CAN'T MOVE. I WIN.": GOTO 510
200 PRINT:PRINT"BOARD:": PRINT LEFT$(P$,3%): PRINT MID$(P$,4%,3%): PRINT FNR$(P$,7%): PRINT
210 INPUT "WHAT IS YOUR MOVE"; A%,B%: A$=FNNUM$(A%): B$=FNNUM$(B%)
220     IF INSTR(1%,M$,FNN$(A$)+FNN$(B$))=0% THEN PRINT "ILLEGAL MOVE.": GOTO 210  
230 P$=FNC$(FNC$(P$,A%,"-"),B%,"O")
231     IF INSTR(1%,P$,"*")=0% OR INSTR(1%,P$,"O")<4% THEN PRINT "YOU WIN.": GOTO 500
300 P%=P%+2%:       REM  COMPUTERS'S MOVE
310 FOR C%=1% TO Q%
311     C$=P$(C%)
312     IF VAL(LEFT$(C$,1%))=P% AND MID$(C$,2%,9%)=P$ THEN M$=FNR$(C$,11%): GOTO 400
320 NEXT C%: Q$="*": D%=1%: GOSUB 2000: C%=Q%
321     IF M$="" THEN PRINT "I CAN'T MOVE. YOU WIN.": GOTO 500
400 IF M$="" THEN PRINT "I RESIGN.": GOTO 500
410 K$=LEFT$(M$,2%): M$=FNR$(M$,3%)
411     IF RND>.33333333 AND M$<>"" THEN 410
420 K%=C%: A%=VAL(LEFT$(K$,1%)): B%=VAL(FNR$(K$,2%))
421     P$=FNC$(FNC$(P$,A%,"-"),B%,"*")
430 IF INSTR(1%,P$,"O")=0% OR INSTR(7%,P$,"*") THEN PRINT "I WIN!": GOTO 510
440 PRINT "I MOVE FROM" A% "TO" B%: GOTO 190
500 W2%=W2%+1%: I%=INSTR(11%,P$(K%),K$)
501     P$(K%)=LEFT$(P$(K%),I%-1%)+FNR$(P$(K%),I%+2%): IF I% GOTO 520
510 W1%=W1%+1%
520 PRINT:PRINT "BOARD:": PRINT LEFT$(P$,3%): PRINT MID$(P$,4%,3%): PRINT FNR$(P$,7%):PRINT
521     PRINT "I HAVE WON" W1% "AND YOU HAVE WON" W2% "OF" W1%+W2% "GAMES"
523 GOSUB 3000                                                                           '***
530 INPUT "ANOTHER GAME"; C$: IF C$="NO" OR C$="N" OR C$="no" OR C$="n" THEN 9999 ELSE 120
900 ' DATA**= <- OF MOVE IN GAME><POSITION><LIST OF MOVES>
901 '   IN <POSITION>, -=BLANK, *=COMPUTER'S PAWN, O=PLAYER'S PAWN
902 '   <LIST OF MOVES> IS <- MOVE FROM><- MOVE TO><LIST OF MOVES>
2000 M$=""
2010 FOR J%=1% TO 9%
2011    IF MID$(P$,J%,1%)<>Q$ THEN 2050
2015    T%=J%+D%*3%: IF T%<1% OR T%>9% THEN 2025
2020    IF MID$(P$,T%,1%)="-" THEN J$=FNNUM$(J%): T$=FNNUM$(T%): M$=M$+FNN$(J$)+FNN$(T$)
2025    T%=J%+D%*2%
2026    IF T%<1% OR T%>9% OR (D%=1% AND (J%=1% OR J%=4% OR J%=7%)) OR (D%=-1% AND (J%=3% OR J%=6% OR J%=9%)) THEN 2035
2030    IF INSTR(1%,Q$+"-",MID$(P$,T%,1%))=0% THEN J$=FNNUM$(J%):T$=FNNUM$(T%):M$=M$+FNN$(J$)+FNN$(T$)
2035    T%=J%+D%*4%: IF T%<1% OR T%>9% OR J%=3% OR J%=7% THEN 2050
2040    IF INSTR(1%,Q$+"-",MID$(P$,T%,1%))=0% THEN J$=FNNUM$(J%):T$=FNNUM$(T%):M$=M$+FNN$(J$)+FNN$(T$)
2050 NEXT J%
2051 IF D%=1% AND M$<>"" THEN Q%=Q%+1%: PRINT "Q%=" Q%: E$=P$: G$=FNNUM$(P%): P$(Q%)=FNN$(G$)+E$+M$: REM temp. variables E$ and G$ are crucial!
2060 RETURN
3000 'Show P$(Q%)                                                                        '***
3010 FOR L%=1% TO Q%                                                                     '***
3020    PRINT P$(L%)                                                                     '***
3030 NEXT                                                                                '***
3040 RETURN                                                                              '***
9999 END

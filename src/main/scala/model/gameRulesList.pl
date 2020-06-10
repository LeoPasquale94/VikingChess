% Definizione di Players: [player(+Type, +Faction)] (NE BASTANO DUE)
player('Human', white).
player('Human', black).
player('Machine', white).
player('Machine', black).

% Definizione avversari: [opponent(+Player, +Player)]
opponent(player('Human', black), player('Machine', white)).
opponent(player('Human', black), player('Human', white)).
opponent(player('Human', white), player('Machine', black)).
opponent(player('Human', white), player('Human', black)).

% Definizione indici di board: [emptyBoard([[+IndexRow, [ListColumnIndexes]], .........., [+IndexRow, [ListColumnIndexes]]])]
emptyBoard([ [1, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]],
             [2, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]],
             [3, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]],
             [4, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]],
             [5, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]],
             [6, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]],
             [7, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]],
             [8, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]],
             [9, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]],
             [10, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]],
             [11, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]]
]).

% Definizione indici pedine: [initPawn(+cell(+pawn(+colorPawn, +typePawn), +IndexRow, +IndexColumn))]
initPawn(cell(pawn(black, pedestrian),1,4)).
initPawn(cell(pawn(black, pedestrian),1,5)).
initPawn(cell(pawn(black, pedestrian),1,6)).
initPawn(cell(pawn(black, pedestrian),1,7)).
initPawn(cell(pawn(black, pedestrian),1,8)).
initPawn(cell(pawn(black, pedestrian),2,6)).
initPawn(cell(pawn(black, pedestrian),4,1)).
initPawn(cell(pawn(white, pedestrian),4,6)).
initPawn(cell(pawn(black, pedestrian),4,11)).
initPawn(cell(pawn(black, pedestrian),5,1)).
initPawn(cell(pawn(white, pedestrian),5,5)).
initPawn(cell(pawn(white, pedestrian),5,6)).
initPawn(cell(pawn(white, pedestrian),5,7)).
initPawn(cell(pawn(black, pedestrian),5,11)).
initPawn(cell(pawn(black, pedestrian),6,1)).
initPawn(cell(pawn(black, pedestrian),6,2)).
initPawn(cell(pawn(white, pedestrian),6, 4)).
initPawn(cell(pawn(white, pedestrian),6,5)).
initPawn(cell(pawn(white, king),6,6)).
initPawn(cell(pawn(white, pedestrian),6,7)).
initPawn(cell(pawn(white, pedestrian),6,8)).
initPawn(cell(pawn(black, pedestrian),6,10)).
initPawn(cell(pawn(black, pedestrian),6,11)).
initPawn(cell(pawn(black, pedestrian),7,1)).
initPawn(cell(pawn(white, pedestrian),7,5)).
initPawn(cell(pawn(white, pedestrian),7,6)).
initPawn(cell(pawn(white, pedestrian),7,7)).
initPawn(cell(pawn(black, pedestrian),7,11)).
initPawn(cell(pawn(black, pedestrian),8,1)).
initPawn(cell(pawn(white, pedestrian),8,6)).
initPawn(cell(pawn(black, pedestrian),8,11)).
initPawn(cell(pawn(black, pedestrian),10,6)).
initPawn(cell(pawn(black, pedestrian),11,4)).
initPawn(cell(pawn(black, pedestrian),11,5)).
initPawn(cell(pawn(black, pedestrian),11,6)).
initPawn(cell(pawn(black, pedestrian),11,7)).
initPawn(cell(pawn(black, pedestrian),11,8)).

% Creazione completa della board: [initBoard(+IndexedBoard, +ListCellPawns, -CompleteBoard)]
initBoard([[_,[]]],[],[]).
initBoard([[X,[Y]]], [], [[X,[cell(pawn(void),Y)]]]) :- !.
initBoard([[X,[Y]]], [cell(P,X,Y)], [[X,[cell(P,Y)]]]) :- !.
initBoard([[X,[Y]]|T], [cell(P,X,Y)|T2], [[X,[cell(P,Y)]]|T3]) :- initBoard(T,T2,T3),!.
initBoard([[X,[Y]]|T], L, [[X,[cell(pawn(void),Y)]]| T3]) :- initBoard(T, L, T3), !.
initBoard([[X,[Y|Ys]]|T], [cell(P,X,Y)|T2], [[X,[cell(P,Y)|T3]]|T4]) :- initBoard([[X,Ys]|T],T2,[[X,T3]|T4]), !.
initBoard([[X,[Y|Ys]]|T], L, [[X,[cell(pawn(void),Y)|T3]]|T4]) :- initBoard([[X,Ys]|T], L, [[X,T3]|T4]), !.

showBoard(Board) :-  emptyBoard(L1), findall(Cell, initPawn(Cell), L), initBoard(L1,L,Board).

% Check cella vuota: [isFreeCell(+Board, +IndexRow, +IndexColumn)]
isFreeCell([[X, [cell(pawn(void), Y) | Ys]] | T], X, Y).
isFreeCell([[X, [cell(P, Y1) | Ys]]|T], X, Y) :- isFreeCell([[X, Ys] | T], X, Y).
isFreeCell([[X1, [Y1 | Ys]] | T], X, Y) :- isFreeCell(T, X, Y).

% Check cella centrale e angolo: [isCenterCell(+IndexRow, +IndexColumn) / isCornerCell(+IndexRow, +IndexColumn)]
isCenterCell(X,Y) :- X == 6, Y == 6.
isCornerCell(X,Y) :- (X == 1, Y == 1 ; X == 1, Y == 11 ; X == 11, Y == 1; X == 11, Y == 11).

% Rimuove le celle speciali: [removeSpecialCell(+ListCell, -FilteredList)]
removeSpecialCell([], []).
removeSpecialCell([(X,Y)|L], [(X,Y)|L1]) :- not(isCenterCell(X,Y)), not(isCornerCell(X,Y)), removeSpecialCell(L, L1).
removeSpecialCell([(X,Y)|L], L1) :- (isCenterCell(X,Y) ; isCornerCell(X,Y)), removeSpecialCell(L, L1).

% Cattura pedone/i: [capturePawn(+Board, +IndexRow, +IndexColumn, +Player, -ListCellsCaptured)]
capturePawn(Board,X,Y,BN,List) :- findall((MR, MC),capturePawnInRow(Board, X,Y,BN, MR, MC), L1),findall((MR, MC),capturePawnInCol(Board, X,Y,BN, MR, MC), L2), append(L1,L2,List).

capturePawnInRow([[R, [cell(pawn(BN,_), C), cell(pawn(NB, pedestrian), C2), cell(pawn(BN,_), C3) |T]]| T1], R, C, BN, R, C2).
capturePawnInRow([[R, [cell(pawn(BN,_), C3), cell(pawn(NB, pedestrian), C2), cell(pawn(BN,_), C) |T]]| T1], R, C, BN, R, C2).
capturePawnInRow([[R, [cell(pawn(void), C3), cell(pawn(NB, pedestrian), C2), cell(pawn(BN,_), C) |T]]| T1], R, C, BN, R, C2) :- isCornerCell(R,C3) ; isCenterCell(R,C3).
capturePawnInRow([[R, [cell(pawn(BN,_), C), cell(pawn(NB, pedestrian), C2), cell(pawn(void), C3) |T]]| T1], R, C, BN, R, C2) :- isCornerCell(R,C3) ; isCenterCell(R,C3).
capturePawnInRow([[NR, L]|T], R, C, BN, MR, MC):- capturePawnInRow(T, R, C, BN, MR, MC).
capturePawnInRow([[R, [cell(_, NC)|T]]|T1], R, C, BN, MR, MC):- !, capturePawnInRow([[R, T]|T1], R, C, BN, MR, MC).

capturePawnInCol([[R1, [cell(pawn(BN,_), C)|T1]], [R2, [cell(pawn(NB, pedestrian), C)|T2]], [R3, [cell(pawn(BN,_), C)|T3]]|T], R1, C, BN, R2, C).
capturePawnInCol([[R1, [cell(pawn(BN,_), C)|T1]], [R2, [cell(pawn(NB, pedestrian), C)|T2]], [R3, [cell(pawn(BN,_), C)|T3]]|T], R3, C, BN, R2, C).
capturePawnInCol([[R1, [cell(pawn(BN,_), C)|T1]], [R2, [cell(pawn(NB, pedestrian), C)|T2]], [R3, [cell(pawn(void), C)|T3]]|T], R1, C, BN, R2, C) :- isCenterCell(R3,C) ; isCornerCell(R3,C).
capturePawnInCol([[R1, [cell(pawn(void), C)|T1]], [R2, [cell(pawn(NB, pedestrian), C)|T2]], [R3, [cell(pawn(BN,_), C)|T3]]|T], R3, C, BN, R2, C) :- isCenterCell(R1,C) ; isCornerCell(R1,C).
capturePawnInCol([[R1, [cell(_, NC)|TD1]], [R2,[cell(_, NC)|TD2]], [R3,[cell(_, NC)|TD3]]|TF], R1, C, BN, MR, MC):- capturePawnInCol([[R1, TD1],[R2, TD2],[R3, TD3]|TF], R1, C, BN, MR, MC).
capturePawnInCol([[R1, [cell(_, NC)|TD1]], [R2,[cell(_, NC)|TD2]], [R3,[cell(_, NC)|TD3]]|TF], R3, C, BN, MR, MC):- capturePawnInCol([[R1, TD1],[R2, TD2],[R3, TD3]|TF], R3, C, BN, MR, MC).
capturePawnInCol([[R1,L1], [R2,L2], [R3,L3]|TF], R, C, BN, MR, MC):- !, capturePawnInCol([[R2,L2],[R3,L3]|TF], R, C, BN, MR, MC).
capturePawnInCol([[R1,L1], [R2,L2], [R3,L3]|TF], R2, C, BN, MR, MC):- !, capturePawnInCol([[R2,L2], [R3,L3]|TF], R2, C, BN, MR, MC).

% Cattura Re: [captureKing(+Board, +IndexRow, +IndexColumn, +PlayerBlack, -CaptureIndexRow, -CaptureIndexColumn)]
%newBoard(B8) :- showBoard(B), updateBoard(B,cell(pawn(white,pedestrian),4,6), cell(pawn(void),4,2), B1), updateBoard(B1,cell(pawn(white,pedestrian),5,5), cell(pawn(void),5,2), B2), updateBoard(B2,cell(pawn(white,pedestrian),5,6), cell(pawn(void),5,3), B3), updateBoard(B3,cell(pawn(white,king),6,6), cell(pawn(void),4,6), B4), updateBoard(B4,cell(pawn(black,pedestrian),2,6), cell(pawn(void),3,6), B5), updateBoard(B5,cell(pawn(black,pedestrian),4,1), cell(pawn(void),4,5), B6), updateBoard(B6,cell(pawn(black,pedestrian),4,11), cell(pawn(void),4,7), B7), updateBoard(B7,cell(pawn(black,pedestrian),5,1), cell(pawn(void),5,6), B8).
captureKing(Board, XR, YR, black, XK, YK) :- captureKingInRow(Board, XR, YR, black, R1, C1), (R2 is R1+1;R2 is R1-1), !, captureKingInCol(Board, R2, C1, black, XK, YK).
captureKing(Board, XC, YC, black, XK, YK) :- captureKingInCol(Board, XC, YC, black, R1, C1), (C2 is C1+1;C2 is C1-1), !, captureKingInRow(Board, R1, C2, black, XK, YK).

captureKingInRow([[R, [cell(pawn(black,_), C), cell(pawn(white, king), C2), cell(pawn(black,_), C3) |T]]| T1], R, C, BN, R, C2).
captureKingInRow([[R, [cell(pawn(black,_), C3), cell(pawn(white, king), C2), cell(pawn(black,_), C) |T]]| T1], R, C, BN, R, C2).
captureKingInRow([[R, [cell(pawn(void), C3), cell(pawn(white, king), C2), cell(pawn(black,_), C) |T]]| T1], R, C, BN, R, C2) :- isCenterCell(R,C3).
captureKingInRow([[R, [cell(pawn(black,_), C), cell(pawn(white, king), C2), cell(pawn(void), C3) |T]]| T1], R, C, BN, R, C2) :- isCenterCell(R,C3).
captureKingInRow([[NR, L]|T], R, C, BN, MR, MC):- captureKingInRow(T, R, C, BN, MR, MC).
captureKingInRow([[R, [cell(_, NC)|T]]|T1], R, C, BN, MR, MC):- !, captureKingInRow([[R, T]|T1], R, C, BN, MR, MC).

captureKingInCol([[R1, [cell(pawn(black,_), C)|T1]], [R2, [cell(pawn(white, king), C)|T2]], [R3, [cell(pawn(black,_), C)|T3]]|T], R1, C, BN, R2, C).
captureKingInCol([[R1, [cell(pawn(black,_), C)|T1]], [R2, [cell(pawn(white, king), C)|T2]], [R3, [cell(pawn(black,_), C)|T3]]|T], R3, C, BN, R2, C).
captureKingInCol([[R1, [cell(pawn(black,_), C)|T1]], [R2, [cell(pawn(white, king), C)|T2]], [R3, [cell(pawn(void), C)|T3]]|T], R1, C, BN, R2, C) :- isCenterCell(R3,C).
captureKingInCol([[R1, [cell(pawn(void), C)|T1]], [R2, [cell(pawn(white, king), C)|T2]], [R3, [cell(pawn(black,_), C)|T3]]|T], R3, C, BN, R2, C) :- isCenterCell(R1,C).
captureKingInCol([[R1, [cell(_, NC)|TD1]], [R2,[cell(_, NC)|TD2]], [R3,[cell(_, NC)|TD3]]|TF], R1, C, BN, MR, MC):- captureKingInCol([[R1, TD1],[R2, TD2],[R3, TD3]|TF], R1, C, BN, MR, MC).
captureKingInCol([[R1, [cell(_, NC)|TD1]], [R2,[cell(_, NC)|TD2]], [R3,[cell(_, NC)|TD3]]|TF], R3, C, BN, MR, MC):- captureKingInCol([[R1, TD1],[R2, TD2],[R3, TD3]|TF], R3, C, BN, MR, MC).
captureKingInCol([[R1,L1], [R2,L2], [R3,L3]|TF], R, C, BN, MR, MC):- !, captureKingInCol([[R2,L2],[R3,L3]|TF], R, C, BN, MR, MC).
captureKingInCol([[R1,L1], [R2,L2], [R3,L3]|TF], R2, C, BN, MR, MC):- !, captureKingInCol([[R2,L2], [R3,L3]|TF], R2, C, BN, MR, MC).

% Match vittoria: [checkVictory(+Board, +CellLastMove)]
%newBoard(B7) :- showBoard(B), updateBoard(B,cell(pawn(white,pedestrian),4,6), cell(pawn(void),4,2), B1), updateBoard(B1,cell(pawn(white,pedestrian),5,5), cell(pawn(void),5,2), B2), updateBoard(B2,cell(pawn(white,pedestrian),5,6), cell(pawn(void),5,3), B3), updateBoard(B3,cell(pawn(white,king),6,6), cell(pawn(void),4,6), B4), updateBoard(B4,cell(pawn(white,king),4,6), cell(pawn(void),4,3), B5), updateBoard(B5,cell(pawn(white,king),4,3), cell(pawn(void),1,3), B6), updateBoard(B6,cell(pawn(white,king),1,3), cell(pawn(void),1,1), B7).
checkVictory(Board, cell(pawn(P,TP),X,Y)) :- whiteWin(cell(pawn(P,TP),X,Y)) ; captureKing(Board, X, Y, P, XK, YK).
whiteWin(cell(pawn(white,king),X,Y)) :- isCornerCell(X,Y), !.

% Match tra player e pedina (Player bianco non può muovere il nero e viceversa): [controlPawn(+Board, +CellToBeChecked, +Player)]
controlPawn([[X,[cell(pawn(WB,_),Y)|Ys]]|T],cell(pawn(WB,_),X,Y), player(HM,WB)) :- !. 
controlPawn([[X,[cell(Any,Y1)|Ys]]|T] ,cell(P,X,Y), player(HM,WB)) :- controlPawn([[X,Ys]|T] ,cell(P,X,Y),player(HM,WB)). 
controlPawn([[X1,L]|T] ,cell(P,X,Y),player(HM,WB)) :- controlPawn(T,cell(P,X,Y),player(HM,WB)).

% Possibili celle raggiungibili dalla pedina selezionata [possibleMoves(+Board, +StartCell, -ListCellsVoid)]
possibleMoves(IB, cell(pawn(_,king),X,Y), OList) :- checkRowMove(IB, cell(pawn(_,king),X,Y), OList1) , checkColumnMove(IB, cell(pawn(_,king),X,Y), OList2), append(OList1, OList2, OList).
possibleMoves(IB, cell(pawn(_,pedestrian),X,Y), OList) :- checkRowMove(IB, cell(pawn(_,pedestrian),X,Y), OList1) , checkColumnMove(IB, cell(pawn(_,pedestrian),X,Y), OList2), append(OList1, OList2, OList3), removeSpecialCell(OList3, OList).

% Celle vuote vicine in riga prima della cella selezionata: [emptyAdjacentCellsInRow(+Row, +StartIndexColumn, +EndIndexColumn)]
emptyAdjacentCellsInRow([cell(pawn(void), Y) | T], Y1, Y2) :- emptyAdjacentCellsInRow(T, Y1, Y2).
emptyAdjacentCellsInRow([cell(P, Y2) | T], Y1, Y2).

% Possibili celle raggiungibili, sulla stessa riga, dalla pedina selezionata: [checkRowMove(+Board, +StartCell, -ListVoid)]
checkRowMove([[X, L] | T], cell(P1, X1, Y1), LO) :- checkRowMove(T, cell(P1, X1, Y1), LO), !.
checkRowMove([[X1, [cell(pawn(void), Y) | T1]] | T], cell(P1, X1, Y1), [(X1, Y) | T2]) :- emptyAdjacentCellsInRow([cell(pawn(void), Y) | T1], Y, Y1), checkRowMove([[X1, T1] |T], cell(P1, X1, Y1), T2), !.   
checkRowMove([[X1, [cell(pawn(void), Y) | T1]] | T], cell(P1, X1, Y1), LO) :- Y < Y1, not(emptyAdjacentCellsInRow([cell(P, Y) | T1], Y, Y1)) , checkRowMove([[X1, T1] | T], cell(P1, X1, Y1), LO), !.
checkRowMove([[X1, [cell(pawn(_,_), Y) | T1]] | T], cell(P1, X1, Y1), LO) :- Y < Y1, checkRowMove([[X1, T1] | T], cell(P1, X1, Y1), LO), !.
checkRowMove([[X1, [cell(P, Y) | T1]] | T], cell(P, X1, Y), LO) :- checkRowMove([[X1, T1] | T], cell(P, X1, Y), LO), !.
checkRowMove([[X1, [cell(pawn(void), Y) | T1]] | T], cell(P1, X1, Y1) , [(X1,Y) | T2]) :- Y > Y1, checkRowMove([[X1, T1] | T], cell(P1, X1, Y1), T2), !.
checkRowMove([[X1, [cell(pawn(_,_), Y) | T1]] | T], cell(P1, X1, Y1) , []) :- Y > Y1, !.
checkRowMove([[X1, [cell(pawn(void), Y)]] | T], cell(P1, X1, Y1) , [(X1, Y)]).

% Celle vuote vicine in colonna prima della cella selezionata: [emptyAdjacentCellsInCol(+Board, +StartIndexRow, +EndIndexRow, +IndexColumn)]
emptyAdjacentCellsInCol([[XF,[cell(_,Y)|Ys]]|T], XS, XF, Y).
emptyAdjacentCellsInCol([[XS,[cell(pawn(void),Y)|Ys]]|T], XS, XF, Y):- emptyAdjacentCellsInCol(T, XS, XF, Y).
emptyAdjacentCellsInCol([[X,[cell(pawn(void),Y)|Ys]]|T], XS, XF, Y) :- emptyAdjacentCellsInCol(T, XS, XF, Y).
emptyAdjacentCellsInCol([[X,[cell(P,Y)|Ys]]|T], XS, XF, Y1) :- emptyAdjacentCellsInCol([[X,Ys]|T], XS, XF, Y1).

% Possibili celle raggiungibili, sulla stessa colonna, dalla pedina selezionata: [checkColumnMove(+Board, +StartCell, -ListVoid)]
checkColumnMove([], C, []) :- !.
checkColumnMove([[X,[cell(pawn(WB,_),Y)|Ys]]|T], cell(P,X1,Y), []) :- X > X1, !.
checkColumnMove([[X,[cell(pawn(void),Y)|Ys]]|T], cell(P,X1,Y), [(X,Y)|L]) :- X > X1, checkColumnMove(T, cell(P,X1,Y), L), !.
checkColumnMove([[X,[cell(_,Y)|Ys]]|T], cell(P,X1,Y1), L) :- checkColumnMove([[X,Ys]|T], cell(P,X1,Y1), L), !.
checkColumnMove([[X,[cell(pawn(void),Y)|Ys]]|T], cell(P,X1,Y), [(X,Y)|L]) :- emptyAdjacentCellsInCol([[X,[cell(pawn(void),Y)|Ys]]|T], X, X1, Y), checkColumnMove(T, cell(P,X1,Y), L), !.
checkColumnMove([[X,[cell(P1,Y)|Ys]]|T], cell(P,X1,Y), L) :- X < X1, not(emptyAdjacentCellsInCol([[X,[cell(P1,Y)|Ys]]|T], X, X1, Y)), checkColumnMove(T, cell(P,X1,Y), L), !.
checkColumnMove([[X,[cell(P,Y)|Ys]]|T], cell(P,X,Y), L) :- checkColumnMove(T, cell(P,X,Y), L), !.

% Modifica della board: [updateBoard(+Board, +StartCell, +EndCell, -NewBoard)]
updateBoard([], C1, C2, []) :- !.
updateBoard([[X2, [cell(P2,Y) | T1]] | T2], cell(P1, X1, Y), cell(P2, X2, Y), [[X2, [cell(P1, Y) | T1]] | T3]) :-  updateBoard(T2, cell(P1, X1, Y), cell(P2, X2, Y), T3), !.            
updateBoard([[X1, [cell(P1, Y) | T1]] | T2], cell(P1, X1, Y), cell(P2, X2, Y), [[X1, [cell(pawn(void), Y) | T1]] | T3]) :- updateBoard(T2, cell(P1, X1, Y), cell(P2, X2, Y), T3), !. 
updateBoard([[X1, [cell(P1, Y1) | T1]] | T2], cell(P1, X1, Y1), cell(P2, X1, Y2), [[X1, [cell(pawn(void), Y1) | T3]] | T4]) :- Y1 < Y2, updateBoard([[X1, T1] | T2], cell(P1, X1, Y1), cell(P2, X1, Y2), [[X1, T3] | T4]), !. 
updateBoard([[X1, [cell(P2, Y2) | T1]] | T2], cell(P1, X1, Y1), cell(P2, X1, Y2), [[X1, [cell(P1, Y2) | T3]] | T4]) :- Y2 < Y1, updateBoard([[X1, T1] | T2], cell(P1, X1, Y1), cell(P2, X1, Y2), [[X1, T3] | T4]), !. 
updateBoard([[X1, [cell(P2, Y2) | T1]] | T2], cell(P1, X1, Y1), cell(P2, X1, Y2), [[X1, [cell(P1, Y2) | T1]] | T3] ) :- Y1 < Y2, updateBoard(T2, cell(P1, X1, Y1), cell(P2, X1, Y2), T3), !.
updateBoard([[X1, [cell(P1, Y1) | T1]] | T2], cell(P1, X1, Y1), cell(P2, X1, Y2), [[X1, [cell(pawn(void), Y1) | T1]] | T3] ) :- Y2 < Y1, updateBoard(T2, cell(P1, X1, Y1), cell(P2, X1, Y2), T3), !.
updateBoard([[X, [cell(Any, Y) | T1]] | T2], cell(P1, X1, Y1), cell(P2, X2, Y2), [[X, [cell(Any, Y) | T3]] | T4]) :- ((X == X1, X == X2); (X == X1, X \= X2, Y < Y1); (X == X2, X \= X1, Y < Y1)), updateBoard([[X, T1] | T2], cell(P1, X1, Y1), cell(P2, X2, Y2), [[X, T3] | T4]), !. 
updateBoard([[X, L] | T], cell(P1, X1, Y1), cell(P2, X2, Y2), [[X, L] | T1]) :- (X \= X1 ; X \= X2), updateBoard(T, cell(P1, X1, Y1), cell(P2, X2, Y2), T1), !.


%%%%%%%%%%%%%%%%%%%%%%%%   NEW STRUCT   %%%%%%%%%%%%%%%%%%%
% *********************************************** VARIANTS ******************************************************

% Defines board size of specified game variant: [boardSize(+Variant, -BoardSize)]
boardSize(classic, 11).

% Defines initial position of pieces of specified game variant: [initPieces(+Variant, [+cell(piece(+Player, +PieceType), +X, +Y), ...])]
initPieces(classic,
    [
        cell(piece(black, pawn), 1, 4),
        cell(piece(black, pawn), 1, 5),
        cell(piece(black, pawn), 1, 6),
        cell(piece(black, pawn), 1, 7),
        cell(piece(black, pawn), 1, 8),
        cell(piece(black, pawn), 2, 6),
        cell(piece(black, pawn), 4, 1),
        cell(piece(white, pawn), 4, 6),
        cell(piece(black, pawn), 4, 11),
        cell(piece(black, pawn), 5, 1),
        cell(piece(white, pawn), 5, 5),
        cell(piece(white, pawn), 5, 6),
        cell(piece(white, pawn), 5, 7),
        cell(piece(black, pawn), 5, 11),
        cell(piece(black, pawn), 6, 1),
        cell(piece(black, pawn), 6, 2),
        cell(piece(white, pawn), 6, 4),
        cell(piece(white, pawn), 6, 5),
        cell(piece(white, king), 6, 6),
        cell(piece(white, pawn), 6, 7),
        cell(piece(white, pawn), 6, 8),
        cell(piece(black, pawn), 6, 10),
        cell(piece(black, pawn), 6, 11),
        cell(piece(black, pawn), 7, 1),
        cell(piece(white, pawn), 7, 5),
        cell(piece(white, pawn), 7, 6),
        cell(piece(white, pawn), 7, 7),
        cell(piece(black, pawn), 7, 11),
        cell(piece(black, pawn), 8, 1),
        cell(piece(white, pawn), 8, 6),
        cell(piece(black, pawn), 8, 11),
        cell(piece(black, pawn), 10, 6),
        cell(piece(black, pawn), 11, 4),
        cell(piece(black, pawn), 11, 5),
        cell(piece(black, pawn), 11, 6),
        cell(piece(black, pawn), 11, 7),
        cell(piece(black, pawn), 11, 8)
    ]
).


% *********************************************** LISTS UTILS ***********************************************

% Puts +Element in last position of +List: addLast(+List, +Element, +OutputList)
addLast([], X, [X]).
addLast([X|Xs], Y, [X|L]) :- addLast(Xs, Y, L).

% Returns size of a list: [size(+List, -SizeList)]
size([], 0).
size([_|T], M) :- size(T, N), M is N + 1.

% Returns a sequence of integers from 1 to +N: [sequence(+N, -Sequence)]
sequence(1, [1]).
sequence(N, O) :- N > 1, N2 is N - 1, sequence(N2, T), addLast(T, N, O), !.

% Returns if specified lists are equal: [equalLists(+List1, +List2)]
equalLists([], []).
equalLists([H|T], [H|T]) :- equalLists(T, T).

% Returns a list of the first +FirstNElems elements of a +List: [take(+FirstNElems, +List, -OutputList)]
take(_, [], []).
take(0, _, []).
take(N, [X|Xs], [X|Ys]) :- M is N-1, take(M, Xs, Ys).

% Appends four lists in one: [append4(+ListOne, +ListTwo, +ListThree, +ListFour, -OutputList)]
append4(L, L1, L2, L3, O) :- append(L, L1, O1), append(L2, L3, O2), append(O1, O2, O).


% *********************************************** BOARD ******************************************************

% Creates row's indexes list of specified size (integer sequence from 1 to +Size): [boardRow(+Size, +Row)]
boardRow(S, R) :- sequence(S, R).

% Creates an empty board of specified size: [emptyBoard(+Size, -OutputBoard)] (ex: [cell(piece(void),1,1),cell(piece(void),1,2), ...])
emptyBoard(S, B) :- boardRow(S, R), findall(cell(piece(void),X,Y), (member(X, R), member(Y, R)), B).

% Sorts a list of coordinates: [sortCoordinates(+CoordList, -SortedList)]
sortCoordinates(L, OL) :- quicksort(L, coordPredicate, OL).
coordPredicate((X, Y), (X1, Y1)) :- X < X1 ; X == X1, Y =< Y1.

% Sorts a list of cells: [sortCells(+CellList, -SortedList)]
sortCells(L, O) :- quicksort(L, cellPredicate, O).
cellPredicate(cell(_, X, Y), cell(_, X1, Y1)) :- coordPredicate((X, Y), (X1, Y1)).

% Gets the cell in specified coordinate: [getCell(+Board, +X, +Y, -CellInXY)]
getCell([cell(P, X, Y)|_], X, Y, cell(P, X, Y)).
getCell([cell(_, X, Y)|T], X1, Y1, C) :- getCell(T, X1, Y1, C).

% Sets a cell in a specified board: [setCell(+Board, +Cell, -NewBoard)]
% +Cell ex: cell(+Piece, +X, +Y)
setCell([cell(_, X, Y)|T], cell(P, X, Y), [cell(P, X, Y)|T]).
setCell([cell(P, X, Y)|T], cell(P1, X1, Y1), [cell(P, X, Y)|O]) :- setCell(T, cell(P1, X1, Y1), O).

% Sets cells in a specified board: [setCell(+Board, +ListOfCells, -NewBoard)]
setAllCells(B, L, O) :- sortCells(L, SC), setSortedCells(B, SC, O).
setSortedCells(B, [], B).
setSortedCells([cell(_, X, Y)|T], [cell(P, X, Y)|T1], [cell(P, X, Y)|T2]) :- setSortedCells(T, T1, T2), !.
% Other coordinates
setSortedCells([C|T], SC, [C|T2]) :- setSortedCells(T, SC, T2).

% Defines corner cells in board of specified +BoardSize: [cornerCell(+BoardSize, +(X, Y))]
cornerCell(_, (1, 1)).
cornerCell(S, (1, S)).
cornerCell(S, (S, 1)).
cornerCell(S, (S, S)).

% Defines center cell in board of specified +BoardSize: [centerCell(+BoardSize, +(X, Y))]
centralCell(S, (X, X)) :- X is (S // 2) + 1.

% Returns all corner cells of spiecified +BoardSize: [centerCell(+BoardSize, +ListCornerCells)]
allCornerCells(S, L) :- findall(C, cornerCell(S, C), L).

% Returns corner cells and center cell of specified board size: [specialCells(+BoardSize, -ListSpecialCells)]
specialCells(S, O) :- allCornerCells(S, C), centralCell(S, C1), append(C, [C1], O).

% Returns if cell of coordinate X, Y is a special cell of specified board size:
% [isSpecialCell(+BoardSize, +X, +Y)]
isSpecialCell(S, X, Y) :- specialCells(S, L), member((X, Y), L).

% Returns -PlayerOwner of specified +Piece: [playerOwner(+Piece, -PlayerOwner)]
playerOwner(piece(P, _), P).

% Creates complete board of specified game variant: [initBoard(+GameVariant, -Board)]
initBoard(V, B) :- boardSize(V, S), emptyBoard(S, EB), initPieces(V, P), setAllCells(EB, P, B), !.

% Returns all cells located Up, Right, Down and Left of specified FromX, FromY coordinate:
% [orthogonalCells(+Board, +Fromx, +FromY, -UpCells, -RightCells, -DownCells, -LeftCells)]
orthogonalCells([], FromX, FromY, [], [], [], []).
% Same row left
orthogonalCells([cell(P, FromX, ToY)|T], FromX, FromY, Up, Right, Down, [cell(P, FromX, ToY)|Left]) :-
		ToY < FromY, orthogonalCells(T, FromX, FromY, Up, Right, Down, Left), !.
% Same row right
orthogonalCells([cell(P, FromX, ToY)|T], FromX, FromY, Up, [cell(P, FromX, ToY)|Right], Down, Left) :-
		ToY > FromY, orthogonalCells(T, FromX, FromY, Up, Right, Down, Left), !.
% Same column up
orthogonalCells([cell(P, ToX, FromY)|T], FromX, FromY, [cell(P, ToX, FromY)|Up], Right, Down, Left) :-
		ToX < FromX, orthogonalCells(T, FromX, FromY, Up, Right, Down, Left), !.
% Same column down
orthogonalCells([cell(P, ToX, FromY)|T], FromX, FromY, Up, Right, [cell(P, ToX, FromY)|Down], Left) :-
		ToX > FromX, orthogonalCells(T, FromX, FromY, Up, Right, Down, Left), !.
% Other cases: Different row and different column / current cell
orthogonalCells([cell(P, ToX, ToY)|T], FromX, FromY, Up, Right, Down, Left) :-
		orthogonalCells(T, FromX, FromY, Up, Right, Down, Left), !.

% Returns the first +N adjacent cells to specified +X, +Y cell, divided in all four directions and ordered
% from closest to farthest.
% [getNAdjacentCells(+Board, +N, +X, +Y, -Up, -Right, -Down, -Left)]
getNAdjacentCells(B, N, X, Y, U, R, D, L) :-
		orthogonalCells(B, X, Y, U1, R1, D1, L1),
		reverse(U1, U2), take(N, U2, U),
		take(N, R1, R),
		take(N, D1, D),
		reverse(L1, L2), take(N, L2, L), !.

% Checks if each cell of a list contains a black pawn: [allBlackPawns(+CellsList)]
allBlackPawns([cell(piece(black, _), _, _)]).
allBlackPawns([cell(piece(black, _), _, _)|T]) :- allBlackPawns(T).


% *********************************************** GAME ******************************************************

% Defines opponent in the game: [opponent(+Player, -Opponent)]
opponent(white, black).
opponent(black, white).

% Init a new game of specified game variant: [newGame(+Variant, -Game)]
% -Game ex: (Variant, PlayerToMove, Winner, Board)
newGame(V, (V, black, none, B)) :- initBoard(V, B).

% Returns -Board of specified +Game: [gameBoard(+G, -B)]
gameBoard((_, _, _, B), B).


% ****************************************** POSSIBLE MOVES *************************************************

% Returns all possible moves in a specified Game (NB: only PlayerToMove pieces, see Game example above): [gamePossibleMoves(+Game, -ListOfPossibleMoves)]
% -ListPossibleMoves ex.: [(FromX, FromY, ToY, ToY), ...]
gamePossibleMoves((V, Player, W, B), O) :- computeMoves((V, Player, W, B), B, O).

% Used in gamePossibleMoves, browses +CellsToInspect and if a Cell contains a PlayerToMove Piece computes possible moves of that Piece:
% [computeMoves(+Game, +CellsToInspect, -ListPossibleMoves)
computeMoves((_, _, _, _), [], []).
computeMoves((V, Player, W, B), [cell(piece(Player, Type), X, Y)|T], O) :-
		possibleMoves((V, Player, W, B), cell(piece(Player, Type), X, Y), M),
		computeMoves((V, Player, W, B), T, M1),
		append(M, M1, O), !.
% Other cases: opponent Player or piece(void)
computeMoves(G, [H|T], O) :- computeMoves(G, T, O).

% Returns all possible moves of specified coordinate FromX, FromY, in a Game (NB: only PlayerToMove pieces, see Game example above):
% [getCoordPossibleMoves(+Game, +FromX, +FromY, -ListOfPossibleMoves)]
% VIEW
getCoordPossibleMoves((V, Player, W, B), FromX, FromY, O) :-
		getCell(B, FromX, FromY, cell(piece(Player, T), FromX, FromY)),
		possibleMoves((V, Player, W, B), cell(piece(Player, T), FromX, FromY), L),
		only_ToMove(L,O), !.
% Other cases: opponent Player or piece(void)
getCoordPossibleMoves((_, _, _), _, _, []).

% Returns all possible moves of the piece in the specified cell
% NB: assumes that +Cell contains a PlayerToMove piece, if you need to check piece player you may want to use getCoordPossibleMoves:
% [possibleMoves(+Game, +Cell, -ListOfPossibleMoves)]
possibleMoves((V, _, _, B), cell(piece(_, T), FromX, FromY), O) :-
		orthogonalCells(B, FromX, FromY, U, R, D, L),
		cutAfterPieces(U, R, D, L, FromX, FromY, M),
		boardSize(V, S),
		filterIfPawn(T, S, M, O).

% Cuts all cells in a specified sequence of cells as soon as it founds a cell containing a piece
% mapping them as moves like (FromX, FromY, ToX, ToY):
% [cut(+SequenceOfCells, +FromX, +FromY, -LineMoves)]
cut([], FromX, FromY, []).
cut([cell(piece(void), ToX, ToY)|T], FromX, FromY, [(FromX, FromY, ToX, ToY)|O]) :- cut(T, FromX, FromY, O).
% Cuts as soon as it founds a piece
cut([cell(piece(_, _), _, _)|_], _, _, []) :- !.

% Cuts all direction sequences of cells as soons as it founds cells containing pieces
% and returns possible moves from the cell at coordinate FromX, FromY
% moves are mapped like (FromX, FromY, ToX, ToY):
% [cutAfterPieces(+UpCells, +RightCells, +DownCells, +LeftCells, +FromX, +FromY, -MovesList)]
cutAfterPieces(U, R, D, L, FromX, FromY, O) :-
		cut(R, FromX, FromY, O1),
		reverse(L, LR), cut(LR, FromX, FromY, O2),
		cut(D, FromX, FromY, O3),
		reverse(U, UR), cut(UR, FromX, FromY, O4),
		append4(O1, O2, O3, O4, O).

% Filters a list of moves removing special cells if pawn piece: [filterIfPawn(+PieceType, +BoardSize, +ListOfMoves, -FilteredMovesList)]
filterIfPawn(pawn, S, L, O) :- filterSpecialCells(S, L, O).
filterIfPawn(king, _, L, L).

% Filters a list of moves removing special cells: [filterSpecialCells(+BoardSize, +MovesList, -FilteredMovesList)]
filterSpecialCells(S, L, O) :- specialCells(S, SpCells), removeSpecialCells(L, SpCells, O).
removeSpecialCells([], SpCells, []).
removeSpecialCells([(_, _, ToX, ToY)|T], SpCells, O) :-
		member((ToX, ToY), SpCells),
		removeSpecialCells(T, SpCells, O), !.
removeSpecialCells([(FromX, FromY, ToX, ToY)|T], SpCells, [(FromX, FromY, ToX, ToY)|T1]) :-
		not(member((ToX, ToY), SpCells)), removeSpecialCells(T, SpCells, T1).

% map move predicates
only_ToMove([], []).
only_ToMove([(_, _, ToX, ToY)|T], [coord(ToX, ToY)|T1]) :- only_ToMove(T, T1).
% ****************************************** MAKING MOVES *************************************************

% Makes a move from (+FromX, +FromY) to (+ToX, +ToY) in a specified +Game, move must be legit:
% [makeLegitMove(+Game, +FromX, +FromY, +ToX, +ToY, -NewGame)]
% VIEW
makeLegitMove(G, FromX, FromY, ToX, ToY, O) :-
		checkLegitMove(G, FromX, FromY, ToX, ToY),
		makeMove(G, FromX, FromY, ToX, ToY, O), !.

% Checks is move from (+FromX, +FromY) to (+ToX, +ToY) in a specified +Game is legit:
% [checkLegitMove(+Game, +FromX, +FromY, +ToX, +ToY)]
checkLegitMove(G, FromX, FromY, ToX, ToY) :-
		getCoordPossibleMoves(G, FromX, FromY, M),
		member(coord(ToX, ToY), M).

% Makes a move from (+FromX, +FromY) to (+ToX, +ToY) in a specified +Game:
% NB: assumes that the move is legit!
% [makeMove(+Game, +FromX, +FromY, +ToX, +ToY, -NewGame)]
makeMove((V, Player, W, B), FromX, FromY, ToX, ToY, (V, Opponent, Player, O)) :-
		opponent(Player, Opponent),
		boardSize(V, S),
		move(S, B, FromX, FromY, ToX, ToY, O),
		checkVictory(S, O, Player), !.
% If player who moved didn't win
makeMove((V, Player, W, B), FromX, FromY, ToX, ToY, (V, Opponent, none, O)) :-
		opponent(Player, Opponent),
		boardSize(V, S),
		move(S, B, FromX, FromY, ToX, ToY, O), !.

% Moves piece from (+FromX, +FromY) to (+ToX, +ToY) and checks for captures:
% [move(+BoardSize, +Board, +FromX, +FromY, +ToX, +ToY, -OutputBoard)]
move(S, B, FromX, FromY, ToX, ToY, O) :-
		getCell(B, FromX, FromY, cell(P, FromX, FromY)),
		writeMove(B, P, FromX, FromY, ToX, ToY, NB),
		playerOwner(P, Player),
		checkAllCaptures(S, NB, Player, ToX, ToY, LC),
		updateBoardCaptures(NB, LC, O), !.

% Moves +Piece in (+ToX, +ToY) and sets piece(void) in (+FromX, +FromY) in specified +Board:
% [writeMove(+Board, +Piece, +FromX, +FromY, +ToX, +ToY, +OutputBoard)]
writeMove([], _, _, _, _, _, []).
writeMove([cell(_, ToX, ToY)|T], P, FromX, FromY, ToX, ToY, [cell(P, ToX, ToY)|T1]) :-
		writeMove(T, P, FromX, FromY, ToX, ToY, T1), !.
writeMove([cell(_, FromX, FromY)|T], P, FromX, FromY, ToX, ToY, [cell(piece(void), FromX, FromY)|T1]) :-
		writeMove(T, P, FromX, FromY, ToX, ToY, T1), !.
writeMove([C|T], P, FromX, FromY, ToX, ToY, [C|T1]) :- writeMove(T, P, FromX, FromY, ToX, ToY, T1).

% Returns a list of coordinates of captured pawns on a side:
% [checkCapture(+Size, +PlayerWhoMoved, +AdjacentCells, -CapturesList)]
checkCapture(S, P, [], []).
checkCapture(S, P, [X], []).
checkCapture(S, P, [cell(piece(void), _, _)|T], []).
checkCapture(S, P, [cell(piece(P, _), _, _)|T], []).
checkCapture(S, P, [cell(piece(P1, pawn), _, _),cell(piece(P1, _), _, _)], []).
checkCapture(S, P, [cell(piece(P1, pawn), X, Y),cell(piece(P, pawn), _, _)], [(X, Y)]).
checkCapture(S, P, [cell(piece(P1, pawn), X, Y),cell(piece(void), X1, Y1)], [(X, Y)]) :-
		isSpecialCell(S, X1, Y1).
checkCapture(S, P, [cell(piece(P1, pawn), _, _),cell(piece(void), X, Y)], []) :-
		not(isSpecialCell(S, X, Y)).
% King captured triggers the end of the game so it is evaluated in checkVictory,
% moreover, the rule for capturing the king is different
checkCapture(S, P, [cell(piece(P1, king), _, _)|T], []).

% Returns captured pieces coordinates after +PlayerWhoMoved moved a piece to (+MovedX, +MovedY):
% [checkAllCaptures(+BoardSize, +Board, +PlayerWhoMoved, +MovedX, +MovedY, -CapturesList)]
checkAllCaptures(S, B, P, MovedX, MovedY, OC) :-
		getNAdjacentCells(B, 2, MovedX, MovedY, U, R, D, L),
		checkCapture(S, P, U, OC1),
		checkCapture(S, P, R, OC2),
		checkCapture(S, P, D, OC3),
		checkCapture(S, P, L, OC4),
		append4(OC1, OC2, OC3, OC4, OC).

% Writes an unsorted list of captures in a specified board: [updateBoardCaptures(+Board, +UnsortedCapturesList, -NewBoard)]
updateBoardCaptures(B, L, O) :- sortCoordinates(L, OL), writeCaptures(B, OL, O).

% Writes a sorted list of captures in a specified board: [writeCaptures(+Board, +SortedCapturesList, -NewBoard)]
% NB: if you need to write an unsorted list of captures you may want to use updateBoardCaptures.
writeCaptures(B, [], B).
writeCaptures([cell(_, X, Y)|T], [(X, Y)|T1], [cell(piece(void), X, Y)|T2]) :- !, writeCaptures(T, T1, T2).
writeCaptures([C|T], [Coord|T1], [C|T2]) :- writeCaptures(T, [Coord|T1], T2).

% Finds the king's coordinate in the specified +Board: [findKing(+Board, -KingX, -KingY)]
findKing([cell(piece(_, king), XK, YK)|T], XK, YK) :- !.
findKing([cell(_, X, Y)|T], XK, YK) :- findKing(T, XK, YK).

% Checks if +Player has won: [checkVictory(+BoardSize, +Board, +Winner)]
checkVictory(S, B, white) :- findKing(B, XK, YK), allCornerCells(S, L), member((XK,YK), L).
checkVictory(S, B, black) :- findKing(B, XK, YK), kingCaptured(S, B, XK, YK).

% Checks if the king has been captured: [kingCaptured(+BoardSize, +Board, +KingX, +KingY)]
kingCaptured(_, B, XK, YK) :-
		getNAdjacentCells(B, 1, XK, YK, U, R, D, L),
		append4(U, R, D, L, All),
		allBlackPawns(All), !.
% Central cell is hostile!
kingCaptured(S, B, XK, YK) :-
		getNAdjacentCells(B, 1, XK, YK, U, R, D, L),
		centralCell(S, (X, Y)),
		append4(U, R, D, L, All),
		delete(cell(_, X, Y), All, All1),
		allBlackPawns(All1), !.
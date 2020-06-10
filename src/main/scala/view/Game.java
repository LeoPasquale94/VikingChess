package view;

import model.PieceEnum;
import scala.Enumeration;
import scala.Int;
import scala.Tuple3;
import utils.Board.Board;
import utils.Board.BoardCell;
import utils.Pair;
import utils.Pair.PairImpl;

import javax.swing.*;
import java.awt.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

public class Game {

    GameViewImpl gameViewImpl;

    public JPanel gamePanel,northPanel,southPanel,boardPanel,boardPlusColumns, leftPanel, rightPanel;
    private JButton menuButton;
    private HashMap<Pair<Int>, JButton> cells;
    private ScalaViewFactory viewFactory;
    private List<Pair<Int>> possibleMoves;
    private Optional<Pair> selectedCell = Optional.empty();
    private Board board;



    public Game(ScalaViewFactory scalaViewFactory, GameViewImpl GVImpl){
        this.viewFactory = scalaViewFactory;
        gameViewImpl = GVImpl;
        cells = new HashMap<>();
        possibleMoves = new ArrayList<>();

    }

    public JPanel initGamePanel(Board board){
        this.board = board;
        gamePanel = viewFactory.createGamePanel();
        initNorthPanel();
        initSouthPanel();
        initLeftRightPanel();
        initBoard();
        setPawns(board.cells());

        gamePanel.add(northPanel);

        boardPlusColumns = viewFactory.createBoardPlusColumnsPanel();

        boardPlusColumns.add(leftPanel);

        boardPlusColumns.add(boardPanel);

        boardPlusColumns.add(rightPanel);

        gamePanel.add(boardPlusColumns);

        gamePanel.add(southPanel);

        return gamePanel;
    }

    private void initBoard(){
        boardPanel = viewFactory.createBoardPanel();
        GridBagLayout layout = new GridBagLayout();
        boardPanel.setLayout(layout);
        GridBagConstraints lim = new GridBagConstraints();
        for (int i = 1; i <= gameViewImpl.getDimension(); i++) {
            for (int j = 1; j<= gameViewImpl.getDimension(); j++) {
                Pair<Int> coordinate = new PairImpl(i,j);
                JButton cell= cellChoice(coordinate);
                cell.addActionListener(e -> actionCell(cell));
                lim.gridx=j;
                lim.gridy=i;
                layout.setConstraints(cell,lim);
                cells.put(coordinate,cell);
                boardPanel.add(cell);
            }
        }
    };

    public void actionCell(JButton cell) {
        if(cell.getComponents().length > 0 && possibleMoves.isEmpty()){
            selectedCell = Optional.of(getCoordinate(cell));
            moveRequest(getCoordinate(cell));
        } else if(!possibleMoves.isEmpty() &&  !possibleMoves.contains(getCoordinate(cell))) {
            setColorBackground( new Color(83, 143, 159));
            deselectCell();
        }else if(possibleMoves.contains(getCoordinate(cell)) && selectedCell.isPresent()){
            Pair<Int> coordinateStart = selectedCell.get();
            Pair<Int> coordinateArrival = getCoordinate(cell);
            Tuple3 tuple = gameViewImpl.setMove(coordinateStart, coordinateArrival);
            Board board = (Board) tuple._1();
            new Thread(() -> {
                    setPawns(board.cells());
                    setColorBackground(new Color(83, 143, 159));
                    deselectCell();
                    boardPanel.validate();
                    rightPanel.add(viewFactory.createLostWhitePawn());
                    rightPanel.validate();
            }).start();
        }
    }

    private Pair getCoordinate(JButton cell) {
        Pair<Int> cord= null;
        for(Map.Entry<Pair<Int>,JButton> entry : cells.entrySet()){
            if(entry.getValue().equals(cell)){
                cord= entry.getKey();
            }
        }
        return cord;
    }

    public void moveRequest(Pair<Int> coord) {
        possibleMoves = gameViewImpl.getPossibleMoves(coord);
        setColorBackground(new Color(41,71,79));
    }

    public void setColorBackground(Color color){
        possibleMoves.forEach((c -> {
            cells.get(c).setBackground(color);
        }));
    }

    public void deselectCell(){
        selectedCell = Optional.empty();
        possibleMoves.clear();
    }


    private void setPawns(List<BoardCell> positions) {
        positions.forEach(p -> {
            cells.get(p.getCoordinate()).removeAll();
            pawnChoice(p);
            cells.get(p.getCoordinate()).repaint();
        });
    }


    private void pawnChoice(BoardCell c) {
        Enumeration.Value piece = c.getPiece();
        if(piece.equals(PieceEnum.WhitePawn())){
            cells.get(c.getCoordinate()).add(viewFactory.createWhitePawn());
        }
        else if(piece.equals(PieceEnum.BlackPawn())) {
            cells.get(c.getCoordinate()).add(viewFactory.createBlackPawn());
        }
        else if(piece.equals(PieceEnum.WhiteKing())) {
            cells.get(c.getCoordinate()).add(viewFactory.createKingPawn());
        }
    }


    private void initNorthPanel(){
        northPanel=viewFactory.createTopBottomPanel();
        GridBagLayout layout = new GridBagLayout();
        northPanel.setLayout(layout);
        GridBagConstraints lim = new GridBagConstraints();

        menuButton=viewFactory.createGameButton("");
        menuButton.addActionListener(e-> gameViewImpl.showMenu());

        lim.gridx=0;
        lim.gridy=0;
        lim.weightx=1;
        lim.fill = GridBagConstraints.NONE;
        lim.anchor=GridBagConstraints.LINE_END;

        northPanel.add(menuButton, lim);
    }
    private void initSouthPanel(){
        southPanel=viewFactory.createTopBottomPanel();
    }

    private JButton cellChoice(Pair c){
        if(isCornerCell(c)) {
            return viewFactory.createCornerCell(gameViewImpl.dimension);
        }
        if(isCenterCell(c)){
            return viewFactory.createCenterCell(gameViewImpl.dimension);
        }
        return viewFactory.createNormalCell(gameViewImpl.dimension);

    }

    private void initLeftRightPanel() {
        leftPanel=viewFactory.createLeftRightPanel(1, gameViewImpl.dimension);
        rightPanel=viewFactory.createLeftRightPanel(1, gameViewImpl.dimension);

    }

    public void restoreGame() {
        cells.clear();
        gamePanel.removeAll();
    }

    private boolean isCornerCell(Pair<Integer> c) {
        return c.getX() == 1 && c.getY() == 1 ||
                c.getX() == 1 && c.getY() == gameViewImpl.dimension ||
                c.getX() == gameViewImpl.dimension && c.getY() == 1 ||
                c.getX() == gameViewImpl.dimension && c.getY() == gameViewImpl.dimension;
    }

    private boolean isCenterCell(Pair<Integer> c) {
        return c.getX() == gameViewImpl.dimension/2 + 1  && c.getY() == gameViewImpl.dimension/2 + 1;
    }
}

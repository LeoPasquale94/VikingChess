package view;

import utils.Pair;
import utils.Pair.PairImpl;

import javax.swing.*;
import java.awt.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

public class Game {

    GameViewImpl gameViewImpl;

    public JPanel gamePanel,northPanel,southPanel,boardPanel,boardPlusColumns, leftPanel, rightPanel;
    private JButton menuButton;
    private HashMap<Pair<Integer>, JButton> cells;
    private ScalaViewFactory viewFactory;
    private ArrayList<Pair> possibleMoves;
    private Optional<Pair> selectedCell = Optional.empty();



    public Game(ScalaViewFactory scalaViewFactory, GameViewImpl GVImpl){
        this.viewFactory = scalaViewFactory;
        gameViewImpl = GVImpl;
        cells = new HashMap<>();
        possibleMoves = new ArrayList<>();

    }

    public JPanel initGamePanel(){
        gamePanel = viewFactory.createGamePanel();
        initNorthPanel();
        initSouthPanel();
        initLeftRightPanel();
        initBoard();
        initPawns();

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
        for (int i = 1; i <= gameViewImpl.dimension; i++) {
            for (int j = 1; j<= gameViewImpl.dimension; j++) {
                Pair<Integer> coordinate = new PairImpl(i,j);
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
            cells.forEach((k, v) -> {
                if(v.equals(cell))
                    selectedCell=Optional.of(getCoordinate(cell));
                moveRequest(k);
            });
        } else if(!possibleMoves.isEmpty() &&  !possibleMoves.contains(getCoordinate(cell))) {
            setColorBackground( new Color(83, 143, 159));
            deselectCell();

        }else if(possibleMoves.contains(getCoordinate(cell)) && selectedCell.isPresent()){
            JButton destinationCell = cells.get(selectedCell.get());
            cell.add(destinationCell.getComponent(0));
            destinationCell.removeAll();
            setColorBackground(new Color(83, 143, 159));
            destinationCell.repaint();
            deselectCell();
            rightPanel.add(viewFactory.createLostWhitePawn());
            rightPanel.validate();

        }
    }

    private Pair getCoordinate(JButton cell) {
        Pair<Integer> cord= null;
        for(Map.Entry<Pair<Integer>,JButton> entry : cells.entrySet()){
            if(entry.getValue().equals(cell)){
                cord= entry.getKey();
            }
        }
        return cord;
    }

    public void moveRequest(Pair<Integer> coord) {
        possibleMoves = gameViewImpl.getPossibleMoves(coord);
        setColorBackground(new Color(41,71,79));
    }

    public void setColorBackground(Color color){
        possibleMoves.forEach((c -> {
            cells.get(c).setBackground(color);
        }));
    }

    public void deselectCell(){
        selectedCell =Optional.empty();
        possibleMoves.clear();
    }


    private void initPawns(){
        ArrayList<Pair<Integer>> initPositions = null; //gameViewImpl.controller.initPositions();


        // DA RIMUOVERE
        System.out.println(cells);


        initPositions.forEach(p ->{
            cells.get(p).add(viewFactory.createBlackPawn());
        });
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

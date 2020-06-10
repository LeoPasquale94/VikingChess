package view;

import controller.ControllerHnefatafl;
import model.GameVariant;
import scala.Int;
import scala.Tuple3;
import utils.Board.Board;
import utils.Pair;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Optional;

public class GameViewImpl implements GameView, ActionListener {
    public int dimension;
    private JFrame frame;
    private JPanel overlayPanel,gamePanel, menuPanel;
    private HashMap<Pair, JButton> cells;
    private ScalaViewFactory viewFactory;
    public ControllerHnefatafl controller;
    public List<Pair<Int>> possibleMoves;
    private Optional<Pair> selectedCell = Optional.empty();
    private Menu menuUtils;
    private Game gameUtils;
    private Board board;

    public GameViewImpl(ControllerHnefatafl controller){
        this.controller = controller;
        dimension = 11;
        cells = new HashMap<>();
        possibleMoves = new ArrayList<>();
        viewFactory = new ScalaView.ScalaViewImpl();
        menuUtils = new Menu(viewFactory, this);
        gameUtils = new Game(viewFactory, this);
        frame = viewFactory.createFrame();
        overlayPanel = viewFactory.createOverlayLayoutPanel();
        gamePanel = viewFactory.createGamePanel();
        initMenu();
        overlayPanel.add(menuPanel);
        frame.add(overlayPanel);
        frame.setVisible(true);
    }

    private void initMenu() {
        menuPanel = menuUtils.initMenu();
    }

    private void initGamePanel(Board board) {
        gamePanel = gameUtils.initGamePanel(board);
    }

    public void initOrRestoreGUI(){
        if(gamePanel.getComponents().length>0) {
            gameUtils.restoreGame();
            overlayPanel.remove(gamePanel);
        }
        board = controller.newGame(GameVariant.Hnefatafl());
        dimension = (int) Math.sqrt(board.size());
        initGamePanel(board);
        overlayPanel.add(gamePanel);
        showGame();
    }

    private void showGame(){
        menuPanel.setVisible(false);
        gamePanel.setVisible(true);
    }

    public void closeMenu(){
        if(gamePanel.getComponents().length== 0 )
            System.exit(0);
        else showGame();
    }

    public void showMenu(){
        menuPanel.setVisible(true);
        gamePanel.setVisible(false);
    }

    public List<Pair<Int>> getPossibleMoves(Pair coord) {
        return controller.getPossibleMoves(coord);
    }

    public Tuple3 setMove(Pair coordinateStart, Pair coordinateArrival){
        return controller.setMove(coordinateStart, coordinateArrival);
    }

    public int getDimension() {
        return dimension;
    }


    @Override
    public void update(ArrayList<Pair> list) {
        list.forEach(coord -> {
            cells.get(coord).add(viewFactory.createBlackPawn());
        });
    }

    @Override
    public void showPossibleMoves() {

    }

    @Override
    public void actionPerformed(ActionEvent e) {

    }
}

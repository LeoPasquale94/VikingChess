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
    public JPanel overlayPanel,gamePanel, menuPanel, variantsPanel, diffPanel, playerChoicePanel, inGameMenuPanel;
    private HashMap<Pair, JButton> cells;
    private ScalaViewFactory viewFactory;
    public ControllerHnefatafl controller;
    public List<Pair<Int>> possibleMoves;
    private Optional<Pair> selectedCell = Optional.empty();
    public Menu menuUtils;
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
        initMainMenu();
        overlayPanel.add(menuPanel);
        initVariantsMenu();
        overlayPanel.add(variantsPanel);
        initDiffMenu();
        overlayPanel.add(diffPanel);
        initPlayerChoiceMenu();
        overlayPanel.add(playerChoicePanel);
        initInGameMenu();
        overlayPanel.add(inGameMenuPanel);
        frame.add(overlayPanel);
        frame.setVisible(true);
    }


    //Menu principale
    private void initMainMenu() {
        menuPanel = menuUtils.initMenu();
    }


    private void initVariantsMenu() {
        variantsPanel = menuUtils.initVariantsMenu();
    }

    private void initDiffMenu() {
        diffPanel = menuUtils.initDiffMenu();
    }

    private void initPlayerChoiceMenu() {
        playerChoicePanel = menuUtils.initPlayerChoiceMenu();
    }

    private void initInGameMenu() {
        inGameMenuPanel = menuUtils.initInGameMenu();
    }

    private void initGamePanel(Board board) {
        gamePanel = gameUtils.initGamePanel(board);
    }

    public void initOrRestoreGUI(){
        if(gamePanel.getComponents().length>0) {
            gameUtils.restoreGame();
            overlayPanel.remove(gamePanel);
        }
        board = controller.newGame((GameVariant.Val) menuUtils.boardVariant);
        dimension = (int) Math.sqrt(board.size());
        initGamePanel(board);
        overlayPanel.add(gamePanel);
        showGame();
    }

    private void showGame(){
        playerChoicePanel.setVisible(false);
        gamePanel.setVisible(true);
    }

    public void closeMenu(){
        if(gamePanel.getComponents().length== 0 )
            System.exit(0);
        else showGame();
    }

    public void showOverlay(JPanel actualPanel, JPanel panelToShow) {
        actualPanel.setVisible(false);
        panelToShow.setVisible(true);
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

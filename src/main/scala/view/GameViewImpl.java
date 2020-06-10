package view;

import controller.ControllerHnefatafl;
import utils.Pair;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Optional;

public class GameViewImpl implements GameView, ActionListener {
    public int dimension;
    private JFrame frame;
    private JPanel overlayPanel,gamePanel, menuPanel;
    private HashMap<Pair, JButton> cells;
    private ScalaViewFactory viewFactory;
    public ControllerHnefatafl controller;
    public ArrayList<Pair> possibleMoves;
    private Optional<Pair> selectedCell = Optional.empty();
    private Menu menuUtils;
    private Game gameUtils;

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

    private void initGamePanel() {
        gamePanel = gameUtils.initGamePanel();
    }

    public void initOrRestoreGUI(){
        if(gamePanel.getComponents().length>0) {
            gameUtils.restoreGame();
            overlayPanel.remove(gamePanel);
        }
        initGamePanel();
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

    public ArrayList<Pair> getPossibleMoves(Pair coord) {
        return null;
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

package view;

import javax.swing.*;

public class Menu {

    ScalaViewFactory viewFactory;
    private JPanel menuPanel;
    private JButton pvpButton,pveButton, exitButtonMenu ;
    private JPopupMenu dimPopUp, levelPopUp, playerPopUp, exitPopUp;
    private JMenuItem seven, eleven, levelPVE1, levelPVE2, levelPVE3, levelPVE4, white, black, exitMenu, quitGame;
    public GameViewImpl gameViewImpl;


    public Menu(ScalaViewFactory viewFactory, GameViewImpl GVImpl){
        this.viewFactory = viewFactory;
        gameViewImpl = GVImpl;
    }

    public JPanel initMenu() {
        menuPanel = viewFactory.createMenuPanel();
        pveButton = viewFactory.createMenuButton(" Player Vs Computer");
        pvpButton = viewFactory.createMenuButton("Player Vs Player");
        exitButtonMenu = viewFactory.createMenuButton("Exit");

        initDimPopUp();
        initLevelPopUp();
        initPlayerPopUp();
        initExitPopUp();

        pveButton.addActionListener(e -> dimPopUp.show(pveButton, pveButton.getWidth() * 6/7, pveButton.getHeight() * 1/7));
        pveButton.setComponentPopupMenu(dimPopUp);

        pvpButton.addActionListener(e-> playerPopUp.show(pvpButton , pvpButton.getWidth() * 6/7, pvpButton.getHeight() * 1/8));
        pvpButton.setComponentPopupMenu(playerPopUp);

        exitButtonMenu.addActionListener(e-> exitPopUp.show(exitButtonMenu, exitButtonMenu.getWidth() * 5/8, exitButtonMenu.getHeight() * 1/7));
        exitButtonMenu.setComponentPopupMenu(exitPopUp);

        menuPanel.add(Box.createHorizontalGlue());
        menuPanel.add(pveButton);
        menuPanel.add(pvpButton);
        menuPanel.add(exitButtonMenu);
        menuPanel.add(Box.createVerticalGlue());

        return menuPanel;
    }

    private void initDimPopUp() {
        dimPopUp = viewFactory.createPopUpButton();
        JLabel chooseBetween = new JLabel();
        chooseBetween.setText("Choose Between: ");
        seven = viewFactory.createJMenuItem("7 x 7");
        seven.addActionListener(e -> diffChoice(seven));
        eleven = viewFactory.createJMenuItem("11 x 11");
        eleven.addActionListener(e -> diffChoice(eleven));

        dimPopUp.add(chooseBetween);
        dimPopUp.add(seven);
        dimPopUp.add(eleven);

    }

    private void diffChoice(JMenuItem menuItem) {
        if(menuItem.equals(seven))
            gameViewImpl.dimension = 7;
        else
            gameViewImpl.dimension = 11;

        levelPopUp.show(pveButton, pveButton.getWidth() * 6/7, pveButton.getHeight() * 1/7);

        menuItem.setComponentPopupMenu(levelPopUp);
    }

    private void initLevelPopUp() {
        levelPopUp = viewFactory.createPopUpButton();
        JLabel chooseBetween = new JLabel();
        chooseBetween.setText("Choose Between: ");
        levelPVE1 = viewFactory.createJMenuItem("1 - Newcomer");
        levelPVE1.addActionListener(e -> colorChoice(levelPVE1));
        levelPVE2 = viewFactory.createJMenuItem("2 - Amateur");
        levelPVE2.addActionListener(e -> colorChoice(levelPVE2));
        levelPVE3 = viewFactory.createJMenuItem("3 - Standard");
        levelPVE3.addActionListener(e -> colorChoice(levelPVE3));
        levelPVE4 = viewFactory.createJMenuItem("4 - Advanced");
        levelPVE4.addActionListener(e -> colorChoice(levelPVE4));

        levelPopUp.add(chooseBetween);
        levelPopUp.add(levelPVE1);
        levelPopUp.add(levelPVE2);
        levelPopUp.add(levelPVE3);
        levelPopUp.add(levelPVE4);

    }

    private void initPlayerPopUp(){
        playerPopUp = viewFactory.createPopUpButton();
        JLabel choosePlayer = new JLabel("Choose Player: ");
        white = viewFactory.createJMenuItem("White Pawns");
        white.addActionListener(e -> gameViewImpl.initOrRestoreGUI());
        black = viewFactory.createJMenuItem("Black Pawns");
        black.addActionListener(e -> gameViewImpl.initOrRestoreGUI());

        playerPopUp.add(choosePlayer);
        playerPopUp.add(white);
        playerPopUp.add(black);
    }


    private void initExitPopUp(){
        exitPopUp = viewFactory.createPopUpButton();
        JLabel chooseBetween = new JLabel("Choose Between: ");
        exitMenu = viewFactory.createJMenuItem("Exit Menu");
        exitMenu.addActionListener(e -> gameViewImpl.closeMenu());
        quitGame = viewFactory.createJMenuItem("Quit Game");
        quitGame.addActionListener(e -> System.exit(0));

        exitPopUp.add(chooseBetween);
        exitPopUp.add(exitMenu);
        exitPopUp.add(quitGame);

    }

    private void colorChoice(JMenuItem menuItem) {
        playerPopUp.show(pveButton, pveButton.getWidth() * 6/7, pveButton.getHeight() * 1/7);
        menuItem.setComponentPopupMenu(playerPopUp);
    }
}

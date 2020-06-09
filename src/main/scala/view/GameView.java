package view;

import utils.Coordinate;

import java.util.ArrayList;

public interface GameView {

    void update(ArrayList<Coordinate> list);

    void showPossibleMoves();

}

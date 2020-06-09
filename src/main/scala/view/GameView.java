package view;

import features.Coordinate;

import java.util.ArrayList;

public interface GameView {

    void update(ArrayList<Coordinate> list);

    void showPossibleMoves();

}

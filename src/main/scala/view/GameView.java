package view;

import utils.Coordinate;
import utils.Pair;

import java.util.ArrayList;

public interface GameView {

    void update(ArrayList<Pair> list);

    void showPossibleMoves();

}

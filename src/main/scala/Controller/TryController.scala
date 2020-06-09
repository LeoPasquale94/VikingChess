package Controller

import java.util
import java.util.ArrayList

import utils.Coordinate
import view.GameViewImpl


class TryController() {

  var gameViewImpl = new GameViewImpl(this)

  def initPositions: ArrayList[Coordinate] = {
    val list = new ArrayList[Coordinate]
    list.add(new Coordinate(1, 6))
    list.add(new Coordinate(2, 6))
    list.add(new Coordinate(1, 7))
    list
  }

  def getPossibleMoves(c: Coordinate): ArrayList[Coordinate] = {
    val list = new util.ArrayList[Coordinate]
    list.add(new Coordinate(1, 3))
    list.add(new Coordinate(1, 4))
    list.add(new Coordinate(1, 5))
    list
  }
}

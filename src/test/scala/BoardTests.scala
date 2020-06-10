import alice.tuprolog.{Prolog, SolveInfo, Theory}
import org.scalatest.{FunSuite, Matchers}

abstract class BoardTests extends FunSuite with Matchers {

  val prolog: Prolog = new Prolog()
  val theory: Theory = new Theory("src/main/scala/model/gameRulesListOfList.pl")
  var goal: SolveInfo = _
  prolog.setTheory(theory)

  test("Gets a cell with a piece from the board game.") {
    goal = prolog.solve("testGetCell.")
    assert(goal.isSuccess)
  }

  test("Gets a cell empty from the board game.") {
    goal = prolog.solve("testGetCell1.")
    assert(goal.isSuccess)
  }

  test("Gets a cell not present in the board game.") {
    goal = prolog.solve("testGetOutCell.")
    assert(!goal.isSuccess)
  }

  test("Gets a cell list from the board game.") {
    goal = prolog.solve("testGetCellList.")
    assert(goal.isSuccess)
  }

  test("Sets a cell in a board game.") {
    goal = prolog.solve("testSetCell.")
    assert(goal.isSuccess)
  }

  test("Sets a cell not present in the board game.") {
    goal = prolog.solve("testSetOutCell.")
    assert(!goal.isSuccess)
  }

  test("Sets a list of cells in a board game.") {
    goal = prolog.solve("testSetCellList.")
    assert(goal.isSuccess)
  }

  test("Computes the coordinates of the special cells based on board size = 7x7.") {
    goal = prolog.solve("testSpecialCellsBoard7.")
    assert(goal.isSuccess)
  }

  test("Computes the coordinates of the special cells based on board size = 11x11.") {
    goal = prolog.solve("testSpecialCellsBoard11.")
    assert(goal.isSuccess)
  }

  test("Compares if is a special cell.") {
    goal = prolog.solve("testIsSpecialCell.")
    assert(goal.isSuccess)
  }

  test("Tests if owner is white pawn.") {
    goal = prolog.solve("testCellOwner.")
    assert(goal.isSuccess)
  }

  test("Tests if owner is white king.") {
    goal = prolog.solve("testCellOwner1.")
    assert(goal.isSuccess)
  }

  test("Tests if owner is black pawn.") {
    goal = prolog.solve("testCellOwner2.")
    assert(!goal.isSuccess)
  }

  test("Tests if owner is black pawn.") {
    goal = prolog.solve("testCellOwner3.")
    assert(goal.isSuccess)
  }

  test("Returns orthogonal cells from a specific coordinate.") {
    goal = prolog.solve("testOrthogonalCells.")
    assert(goal.isSuccess)
  }

  test("Returns n adjacent cells from a specific cell.") {
    goal = prolog.solve("testNAdjacentCells.")
    assert(goal.isSuccess)
  }

  test("Tests if there are only black pawns in cells list.") {
    goal = prolog.solve("testAllBlackPawns.")
    assert(goal.isSuccess)
  }

  test("Tests if there are only black pawns in cells list; one is white.") {
    goal = prolog.solve("testAllBlackPawns1.")
    assert(!goal.isSuccess)
  }

  test("Tests if there are only black pawns in cells list; one is empty.") {
    goal = prolog.solve("testAllBlackPawns2.")
    assert(!goal.isSuccess)
  }
}

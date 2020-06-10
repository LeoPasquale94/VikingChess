import alice.tuprolog.{Prolog, SolveInfo, Theory}
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
abstract class TestAddLast extends FunSuite with Matchers {
  val prolog: Prolog = new Prolog()
  val theory: Theory = new Theory("src/main/scala/model/gameRulesListOfList.pl")
  var goal: SolveInfo = _

  test("Adds 10 in the tail of the reference list.") {
    prolog.setTheory(theory)
    goal = prolog.solve("testAddLast(O).")
    val string: String = goal.getTerm("O").toString
    assert(string.equals("[1,2,3,4,5,6,7,8,9,10]"))
  }
}

@RunWith(classOf[JUnitRunner])
abstract class TestSize extends FunSuite with Matchers {
  val prolog: Prolog = new Prolog()
  val theory: Theory = new Theory("src/main/scala/model/gameRulesListOfList.pl")
  var goal: SolveInfo = _

  test("Returns the size of the reference list.") {
    prolog.setTheory(theory)
    goal = prolog.solve("testSize(SR).")
    val string: String = goal.getTerm("SR").toString
    assert(string.equals("9"))
  }
}

@RunWith(classOf[JUnitRunner])
abstract class TestSequence extends FunSuite with Matchers {
  val prolog: Prolog = new Prolog()
  val theory: Theory = new Theory("src/main/scala/model/gameRulesListOfList.pl")
  var goal: SolveInfo = _

  test("Returns the sequence from 4 to 7 of the reference list.") {
    prolog.setTheory(theory)
    goal = prolog.solve("testSequence(L).")
    val string: String = goal.getTerm("L").toString
    assert(string.equals("[4,5,6,7]"))
  }
}

@RunWith(classOf[JUnitRunner])
abstract class TestSequenceBigger extends FunSuite with Matchers {
  val prolog: Prolog = new Prolog()
  val theory: Theory = new Theory("src/main/scala/model/gameRulesListOfList.pl")
  var goal: SolveInfo = _

  test("Returns the bigger element from 7 to 4 of the reference list.") {
    prolog.setTheory(theory)
    goal = prolog.solve("testSequenceBiggerM(E).")
    val string: String = goal.getTerm("E").toString
    assert(string.equals("[]"))
  }
}

@RunWith(classOf[JUnitRunner])
abstract class TestEqualLists extends FunSuite with Matchers {
  val prolog: Prolog = new Prolog()
  val theory: Theory = new Theory("src/main/scala/model/gameRulesListOfList.pl")
  var goal: SolveInfo = _

  test("Compare if two lists are equal.") {
    prolog.setTheory(theory)
    goal = prolog.solve("testEqualLists(B).")
    assert(goal.isSuccess)
  }
}

@RunWith(classOf[JUnitRunner])
abstract class TestDifferentLists extends FunSuite with Matchers {
  val prolog: Prolog = new Prolog()
  val theory: Theory = new Theory("src/main/scala/model/gameRulesListOfList.pl")
  var goal: SolveInfo = _

  test("Compare if two lists are different.") {
    prolog.setTheory(theory)
    goal = prolog.solve("testDifferentLists(B).")
    assert(goal.isSuccess)
  }
}

@RunWith(classOf[JUnitRunner])
abstract class TestTake extends FunSuite with Matchers {
  val prolog: Prolog = new Prolog()
  val theory: Theory = new Theory("src/main/scala/model/gameRulesListOfList.pl")
  var goal: SolveInfo = _

  test("Takes the first three elements of the reference list.") {
    prolog.setTheory(theory)
    goal = prolog.solve("testTake(O).")
    val string: String = goal.getTerm("O").toString
    assert(string.equals("[1,2,3]"))
  }
}

@RunWith(classOf[JUnitRunner])
abstract class TestTakeBiggerN extends FunSuite with Matchers {
  val prolog: Prolog = new Prolog()
  val theory: Theory = new Theory("src/main/scala/model/gameRulesListOfList.pl")
  var goal: SolveInfo = _

  test("Takes the bigger n elements of the reference list.") {
    prolog.setTheory(theory)
    goal = prolog.solve("testTakeBiggerN(O).")
    val string: String = goal.getTerm("O").toString
    assert(string.equals("[1,2,3,4,5,6,7,8,9]"))
  }
}

@RunWith(classOf[JUnitRunner])
abstract class TestAppend4 extends FunSuite with Matchers {
  val prolog: Prolog = new Prolog()
  val theory: Theory = new Theory("src/main/scala/model/gameRulesListOfList.pl")
  var goal: SolveInfo = _

  test("Appends 4 lists: [1,2,3],[4,5,6],[7,8,9],[10,11,12].") {
    prolog.setTheory(theory)
    goal = prolog.solve("testAppend4(O).")
    val string: String = goal.getTerm("O").toString
    assert(string.equals("[1,2,3,4,5,6,7,8,9,10,11,12]"))
  }
}

@RunWith(classOf[JUnitRunner])
abstract class TestAppend4Empty extends FunSuite with Matchers {
  val prolog: Prolog = new Prolog()
  val theory: Theory = new Theory("src/main/scala/model/gameRulesListOfList.pl")
  var goal: SolveInfo = _

  test("Appends 4 empty lists.") {
    prolog.setTheory(theory)
    goal = prolog.solve("testAppend4Empty(O).")
    val string: String = goal.getTerm("O").toString
    assert(string.equals("[]"))
  }
}

@RunWith(classOf[JUnitRunner])
abstract class TestAppend4SomeEmpty extends FunSuite with Matchers {
  val prolog: Prolog = new Prolog()
  val theory: Theory = new Theory("src/main/scala/model/gameRulesListOfList.pl")
  var goal: SolveInfo = _

  test("Appends 4 lists: [],[4,5,6],[],[10,11,12].") {
    prolog.setTheory(theory)
    goal = prolog.solve("testAppend4SomeEmpty(O).")
    val string: String = goal.getTerm("O").toString
    assert(string.equals("[4,5,6,10,11,12]"))
  }
}

@RunWith(classOf[JUnitRunner])
abstract class TestIthElem extends FunSuite with Matchers {
  val prolog: Prolog = new Prolog()
  val theory: Theory = new Theory("src/main/scala/model/gameRulesListOfList.pl")
  var goal: SolveInfo = _

  test("Returns the 4th element from reference list") {
    prolog.setTheory(theory)
    goal = prolog.solve("testIthElem(E).")
    val string: String = goal.getTerm("E").toString
    assert(string.equals("4"))
  }
}

@RunWith(classOf[JUnitRunner])
abstract class TestOutIthElem extends FunSuite with Matchers {
  val prolog: Prolog = new Prolog()
  val theory: Theory = new Theory("src/main/scala/model/gameRulesListOfList.pl")
  var goal: SolveInfo = _

  test("An element not present in the reference list cannot be returned.") {
    prolog.setTheory(theory)
    goal = prolog.solve("testOutIthElem(E).")
    assert(!goal.isSuccess)
  }
}

@RunWith(classOf[JUnitRunner])
abstract class TestSetIthElem extends FunSuite with Matchers {
  val prolog: Prolog = new Prolog()
  val theory: Theory = new Theory("src/main/scala/model/gameRulesListOfList.pl")
  var goal: SolveInfo = _

  test("Sets elem in 4-th position.") {
    prolog.setTheory(theory)
    goal = prolog.solve("testSetIthElem(O).")
    val string: String = goal.getTerm("O").toString
    assert(string.equals("[1,2,3,elem,5,6,7,8,9]"))
  }
}

@RunWith(classOf[JUnitRunner])
abstract class TestSetOutIthElem extends FunSuite with Matchers {
  val prolog: Prolog = new Prolog()
  val theory: Theory = new Theory("src/main/scala/model/gameRulesListOfList.pl")
  var goal: SolveInfo = _

  test("Cannot set an element to a position greater than the reference list's size.") {
    prolog.setTheory(theory)
    goal = prolog.solve("testSetOutIthElem(O).")
    assert(!goal.isSuccess)
  }
}

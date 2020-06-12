package model

/**
  * Defines Enumeration for the game mode.
  */
object ModeGame extends Enumeration {
  val PVP: ModeGame.Value = Value("PVP")
  val PVE: ModeGame.Value = Value("PVE")
}

/**
  * Defines Enumeration for the player type.
  */
object Player extends Enumeration {
  val White: Player.Value = Value("White")
  val Black: Player.Value = Value("Black")
  val None: Player.Value = Value("None")
  val Draw: Player.Value = Value("Draw")
}

/**
  * Defines Enumeration for the IA.
  */
object Level extends Enumeration {
  val Newcomer: Level.Value = Value("Newcomer")
  val Amateur: Level.Value = Value("Amateur")
  val Standard: Level.Value = Value("Standard")
  val Advanced: Level.Value = Value("Advanced")
}

/**
  * Defines Enumeration for the game variant.
  */
object GameVariant extends Enumeration {
  case class Val(nameVariant: String, size: Int) extends super.Val
  val Hnefatafl = Val("Hnefatafl", 11)
  val Tawlbwrdd = Val("Tawlbwrdd", 11)
  val Tablut = Val("Tablut", 9)
  val Brandubh = Val("Brandubh", 7)
}

/**
  * Defines Enumeration for the piece in each cell.
  */
object PieceEnum extends Enumeration {
  type PieceType = Value
  val WhitePawn, BlackPawn, WhiteKing, Void = Value
}


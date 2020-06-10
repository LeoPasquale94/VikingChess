package view

import java.awt.event.{MouseAdapter, MouseEvent}
import java.awt.{Color, Component, Cursor, Dimension, Font, Graphics, GraphicsEnvironment, GridLayout, Image}
import java.io.File

import utils.ScreenSize
import javax.imageio.ImageIO
import javax.swing.border.LineBorder
import javax.swing.{Box, BoxLayout, ImageIcon, JButton, JFrame, JLabel, JMenuItem, JPanel, JPopupMenu, OverlayLayout, SwingConstants}
import view.ColorProvider.ColorProviderImpl

trait ScalaViewFactory {

  /**
   * Creates a new Center Cell.
   *
   * @param dimension
   *
   * @return a new Center Cell.
   */

  def createCenterCell(dimension: Int): JButton

  /**
   * Creates a new Corner Cell.
   *
   * @return a new Corner Cell.
   */
  def createCornerCell(dimension: Int): JButton


  /**
   * Creates a new Pawn Cell.
   *
   * @return a new Pawn Cell.
   */
  def createPawnCell(dimension: Int): JButton

  /**
   * Creates a new Normal Cell.
   *
   * @return a new Normal Cell.
   */
  def createNormalCell(dimension: Int): JButton

  /**
   * Creates a new GamePanel.
   *
   * column of GridLayout.
   * row of GridLayout.
   * @return a new GamePanel.
   */
  def createBoardPanel(): JPanel

  /**
   * Creates a new MenuPanel.
   *
   * @return a new MenuPanel.
   */
  def createMenuPanel: JPanel


  /**
   * Creates a panel with flow layout.
   *
   * @return a panel with flow layout.
   *
   */
  def createTopBottomPanel: JPanel

  def createLeftRightPanel(columns: Int, rows: Int): JPanel

  /**
   * Creates a panel with overlay layout.
   *
   * @return a panel with overlay layout.
   *
   */
  def createOverlayLayoutPanel: JPanel

  /**
   * Creates a panel with border layout.
   *
   * @return a panel with border layout.
   *
   */
  def createGamePanel: JPanel

  def createBoardPlusColumnsPanel: JPanel

  /**
   * Creates a button with black background.
   * * @param s
   * name of button.
   *
   * @return a panel with border layout.
   *
   */
  def createMenuButton(s: String): JButton

  def createPopUpButton: JPopupMenu

  def createJMenuItem(text: String): JMenuItem

  /**
   * Creates a button.
   * *@param s
   * name of button.
   *
   * @return a panel with border layout.
   *
   */
  def createGameButton(s: String): JButton

  /**
   * Creates a black pawn.
   *
   * @return a label.
   *
   */
  def createBlackPawn: JLabel

  /**
   * Creates a white pawn.
   *
   * @return a label.
   *
   */
  def createWhitePawn: JLabel

  /**
   * Creates a king pawn.
   *
   * @return a label.
   *
   */
  def createKingPawn: JLabel


  def createLostWhitePawn : JLabel

  /**
   * Creates a Frame.
   *
   * @return a JFrame.
   *
   */
  def createFrame: JFrame

}

object ScalaView {

  case class ScalaViewImpl() extends ScalaViewFactory {

    private var colorProvider = new ColorProviderImpl
    private var cellDimension = 0;
    private var smallerSide = ScreenSize.getSmallerSide * 9 / 10
    val f: Font = Font.createFont(Font.TRUETYPE_FONT, new File("src/main/resources/font/NorseBold-2Kge.otf"))
    val ge: GraphicsEnvironment = GraphicsEnvironment.getLocalGraphicsEnvironment
    ge.registerFont(f)


    override def createCenterCell(dimension: Int): JButton = new CenterCell(dimension)

    override def createCornerCell(dimension: Int): JButton = new CornerCell(dimension)

    override def createPawnCell(dimension: Int): JButton = new PawnCell(dimension)

    override def createNormalCell(dimension: Int): JButton = new NormalCell(dimension)

    override def createBoardPanel(): JPanel = new BoardPanel()

    override def createMenuPanel: JPanel = new MenuPanel

    override def createTopBottomPanel: JPanel = new TopBottomPanel

    override def createLeftRightPanel(columns: Int, rows: Int): JPanel = new LeftRightPanel(columns, rows)

    override def createOverlayLayoutPanel: JPanel = new OverlayLayoutPanel

    override def createGamePanel: JPanel = new GamePanel

    override def createBoardPlusColumnsPanel: JPanel = new BoardPlusColumns

    override def createMenuButton(s: String): JButton = new MenuButton(s)

    override def createPopUpButton: JPopupMenu = new JPopupMenu

    override def createJMenuItem(text: String): JMenuItem = new MenuItem(text)

    override def createGameButton(s: String): JButton = new GameButton(s)

    override def createWhitePawn: JLabel = new WhitePawn

    override def createBlackPawn: JLabel = new BlackPawn

    override def createKingPawn: JLabel = new KingPawn

    override def createLostWhitePawn : JLabel = new LostWhitePawn

    override def createFrame: JFrame = new Frame

    abstract private class Cell(dimension: Int) extends JButton {
      cellDimension = smallerSide / dimension * 80 / 100

      var colorCell: Color = null

      var color: Color = null
      setPreferredSize(new Dimension(cellDimension, cellDimension))
      setAlignmentX(Component.CENTER_ALIGNMENT)
      setAlignmentY(Component.CENTER_ALIGNMENT)
      setLayout(new GridLayout())
      setBorder(new LineBorder(Color.BLACK))
      setCursor(new Cursor(Cursor.HAND_CURSOR))
      setOpaque(true)
      addMouseListener(new MouseAdapter() {
        override def mouseEntered(e: MouseEvent): Unit = {
          color = getBackground
          setBackground(Color.LIGHT_GRAY)
        }
        override def mouseExited(e: MouseEvent): Unit = {
          if (getComponents().length > 0) {
            setBackground(colorCell)
          }
          else {
            setBackground(color)
          }
        }
      })
    }

    private class SpecialCell(dimension: Int) extends Cell(dimension) {

      private val colorCellWin = colorProvider.getSpecialCellColor
      colorCell = colorCellWin
      setBackground(colorCellWin)

    }

    private class CenterCell(dimension: Int) extends SpecialCell(dimension){

      private val iconCell = new ImageIcon("src/main/resources/images/iconThrone.png")
      setIcon(iconCell)

    }

    private class CornerCell(dimension: Int) extends SpecialCell(dimension){

      private val iconCell = new ImageIcon("src/main/resources/images/iconCellWin.png")
      setIcon(iconCell)

    }

    private class PawnCell(dimension: Int) extends Cell(dimension) {
      private val colorCellPawn = colorProvider.getPawnCellColor
      colorCell = colorCellPawn
      setBackground(colorCellPawn)
    }

    private class NormalCell(dimension: Int) extends Cell(dimension) {
      private val colorNormalCell = colorProvider.getNormalCellColor
      colorCell = colorNormalCell
      setBackground(colorNormalCell)
    }

    private class BoardPanel() extends JPanel {
      this.setBackground(colorProvider.getLightBrownColor)
    }

    private class MenuPanel extends JPanel {

      private val menuLabel = new JLabel()

      private val image = ImageIO.read(new File("src/main/resources/images/Cornice.png"))

      private val imageScaled = image.getScaledInstance(smallerSide, smallerSide * 98/100, Image.SCALE_DEFAULT)

      private val img = new ImageIcon("src/main/resources/images/logo.png")

      setLayout(new BoxLayout(this, BoxLayout.Y_AXIS))
      menuLabel.setPreferredSize(new Dimension(smallerSide, smallerSide * 25/ 100))
      menuLabel.setAlignmentX(Component.CENTER_ALIGNMENT)
      menuLabel.setIcon(img)
      add(Box.createRigidArea(new Dimension(smallerSide, smallerSide * 5/100)))
      add(menuLabel)
      add(Box.createRigidArea(new Dimension(smallerSide, smallerSide * 8/100)))

      override protected def paintComponent(g: Graphics): Unit = {
        super.paintComponent(g)
        g.drawImage(imageScaled, 0, 0, null)
      }
    }

    private class TopBottomPanel extends JPanel {
      private val WIDTH_DIMENSION = smallerSide
      private val HEIGHT_DIMENSION = smallerSide * 8 / 100

      setBackground(colorProvider.getLightBrownColor)
      setPreferredSize(new Dimension(WIDTH_DIMENSION, HEIGHT_DIMENSION))
      setAlignmentX(Component.RIGHT_ALIGNMENT)

    }

    private class LeftRightPanel(val columns: Int, val rows: Int) extends JPanel {
      private val WIDTH_DIMENSION = smallerSide * 10 / 100
      private val HEIGHT_DIMENSION = smallerSide * 80 / 100
      private val colorSide = colorProvider.getLightBrownColor
      setLayout(new GridLayout(rows, columns))
      setBackground(colorSide)
      setPreferredSize(new Dimension(WIDTH_DIMENSION, HEIGHT_DIMENSION))
    }

    private class OverlayLayoutPanel extends JPanel {
      setLayout(new OverlayLayout(this))
      setVisible(true)
    }

    private class GamePanel extends JPanel {
      setBackground(colorProvider.getLightBrownColor)
    }

    private class BoardPlusColumns extends JPanel {
      setLayout(new BoxLayout(this, BoxLayout.X_AXIS))
    }

    abstract private class EmptyButton(val s: String) extends JButton {
      setCursor(new Cursor(Cursor.HAND_CURSOR))
      setHorizontalAlignment(SwingConstants.CENTER)
      setText(s)
    }

    private class MenuButton(s: String) extends EmptyButton(s) {

      private val FONT_DIMENSION = smallerSide * 6 / 100
      private val BUTTON_DIMENSION = new Dimension(smallerSide * 60/100, smallerSide * 12/100)

      setPreferredSize(BUTTON_DIMENSION)
      setMaximumSize(getPreferredSize)
      setAlignmentX(Component.CENTER_ALIGNMENT)

      setOpaque(false);
      setContentAreaFilled(false);
      setBorderPainted(false);

      setFont(new Font(f.getFontName, Font.BOLD, FONT_DIMENSION))
      setForeground(colorProvider.getWhiteColor)
      setBorderPainted(false)
    }

    private class GameButton(s: String) extends EmptyButton(s) {
      private var imageIcon = new ImageIcon("src/main/resources/images/hamburgerMenu.png")
      private var image = imageIcon.getImage
      image = image.getScaledInstance(smallerSide * 7/ 100, smallerSide * 7/100, Image.SCALE_SMOOTH)
      imageIcon = new ImageIcon(image)
      setIcon(imageIcon)
      setBorderPainted(false)
      setOpaque(false);
      setContentAreaFilled(false);

    }

    private class PopUpMenu extends JPopupMenu {

      private val FONT_DIMENSION = smallerSide * 3 / 100
      setCursor(new Cursor(Cursor.HAND_CURSOR))
      setFont(new Font(f.getFontName, Font.PLAIN, FONT_DIMENSION))

    }

    private class MenuItem(text: String) extends JMenuItem {

      private val colorSide = colorProvider.getBrownColor
      private val FONT_DIMENSION = smallerSide * 3 / 100

      setText(text)
      setFont(new Font(f.getFontName, Font.PLAIN, FONT_DIMENSION))
      setForeground(Color.WHITE)
      setBackground(colorSide)
      setOpaque(true)

    }

    abstract private class Pawn extends JLabel {

      protected var externalColor: Color = null
      protected var internalColor: Color = null
      protected var namePawn: String = null

      protected var EXTERNAL_ROUNDRECT_DIMENSION = cellDimension * 8 / 10
      protected var INTERNAL_ROUNDRECT_DIMENSION = cellDimension * 7 / 10

      setOpaque(false)
      setVisible(true)


      override def paintComponent(g: Graphics): Unit = {
        super.paintComponent(g)
        val X_CENTRE = getWidth / 2
        val Y_CENTRE = getHeight / 2
        val radius1 = EXTERNAL_ROUNDRECT_DIMENSION * Math.sqrt(2).toInt / 2
        val radius2 = INTERNAL_ROUNDRECT_DIMENSION * Math.sqrt(2).toInt / 2
        g.setColor(externalColor)
        g.fillRoundRect(X_CENTRE - radius1, Y_CENTRE - radius1, EXTERNAL_ROUNDRECT_DIMENSION, EXTERNAL_ROUNDRECT_DIMENSION, 10, 10)
        g.setColor(internalColor)
        g.fillRoundRect(X_CENTRE - radius2, Y_CENTRE - radius2, INTERNAL_ROUNDRECT_DIMENSION, INTERNAL_ROUNDRECT_DIMENSION, 10, 10)
      }

    }

    private class WhitePawn extends Pawn {
      private val white = colorProvider.getWhiteColor
      private val black = colorProvider.getBlackColor
      namePawn = "white"
      externalColor = black
      internalColor = white
    }

    private class BlackPawn extends Pawn {
      private val white = colorProvider.getWhiteColor
      private val black = colorProvider.getBlackColor
      namePawn = "black"
      externalColor = white
      internalColor = black
    }

    private class KingPawn extends Pawn {
      private val gold = colorProvider.getGoldColor
      private val white = colorProvider.getWhiteColor
      namePawn = "king"
      externalColor = gold
      internalColor = white

    }

    private class LostWhitePawn extends WhitePawn {
      EXTERNAL_ROUNDRECT_DIMENSION = EXTERNAL_ROUNDRECT_DIMENSION / 2
      INTERNAL_ROUNDRECT_DIMENSION = INTERNAL_ROUNDRECT_DIMENSION / 2
    }

    private class LostBlackPawn extends BlackPawn {
      EXTERNAL_ROUNDRECT_DIMENSION = EXTERNAL_ROUNDRECT_DIMENSION / 2
      INTERNAL_ROUNDRECT_DIMENSION = INTERNAL_ROUNDRECT_DIMENSION / 2
    }

    private class Frame extends JFrame {
      private val FRAME_TITLE = "Viking Chess - Hnefatafl"
      private val iconApp = new ImageIcon("src/main/resources/images/iconApp.png")

      setTitle(FRAME_TITLE)
      setIconImage(iconApp.getImage)
      setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
      setResizable(false)
      setSize(smallerSide, smallerSide)
      setLocationRelativeTo(null)
    }
  }

}

package view

import java.awt.Color

trait ColorProvider {


  def getBlackColor:Color

  def getHighlightColor:Color

  def getSpecialCellColor:Color

  def getPawnCellColor:Color

  def getNormalCellColor:Color

  def getLightBrown:Color

  def getMenuButtonColor:Color

  def getWhiteColor:Color

  def getGoldColor:Color

}

object ColorProvider{

  class ColorProviderImpl extends ColorProvider{

    override def getBlackColor: Color = new Color(47, 53, 59)

    override def getHighlightColor: Color = Color.LIGHT_GRAY

    override def getSpecialCellColor: Color = new Color(46, 50, 100)

    override def getPawnCellColor: Color = new Color(153, 203, 205)

    override def getNormalCellColor: Color = new Color(83, 143, 159)

    override def getLightBrown: Color = new Color(200, 170, 109)

    override def getMenuButtonColor: Color = new Color(255, 250, 240)

    override def getWhiteColor: Color = new Color(255, 250, 240)

    override def getGoldColor: Color = new Color(212, 175, 55)
  }
}

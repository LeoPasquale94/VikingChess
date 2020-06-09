package features

import java.awt.Dimension
import java.awt.Toolkit


/**
 * A class for providing informations about screen size.
 *
 */
object ScreenSize {

    private var smallerSide = 0
    /**
     * Returns the size of the smaller side of the screen.
     *
     * @return the size of the smaller side of the screen.
     */
    def getSmallerSide: Int = smallerSide

    val screen: Dimension = Toolkit.getDefaultToolkit.getScreenSize

    smallerSide = (if (screen.getHeight < screen.getWidth) screen.getHeight
    else screen.getWidth).toInt

}



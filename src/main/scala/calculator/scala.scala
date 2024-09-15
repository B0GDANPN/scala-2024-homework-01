package calculator

import scala.util.boundary
import scala.util.boundary.break

/** Software implementation of PROC (PROstoy Calculator) mk. 1 (or mk. 2).
 *
 * You should finish this procedure according to
 * the reference described in `README.md` to complete
 * the assignment.
 */
@main def calculator(commands: String*): Unit = {
  /** Converts given string `s` to integer.
   *
   * Throws [[NumberFormatException]] if `s` can't be converted to integer,
   * but you shouldn't worry about it at this moment.
   */
  def parseInt(s: String): Int = s.toInt

  /** Representation of `acc` register. */
  var acc: Int = 0
  var A: Int = 0
  var B: Int = 0
  var blink: Boolean = false
  // define additional registers here
  boundary {
    for (c <- commands) {
      c match {
        case "break" =>
          break()
        case "+" =>
          acc = A + B
          blink = false
        case "-" =>
          acc = A - B
          blink = false
        case "*" =>
          acc = A * B
          blink = false
        case "/" =>
          acc = A / B
          blink = false
        case "swap" =>
          val tmp = B
          B = A
          A = tmp
        case "blink" =>
          acc = A * B
          blink = !blink
        case "acc" =>
          if (blink) {
            B = acc
          }
          else {
            A = acc
          }
          blink = !blink
        case x =>
          if (blink) {
            B = parseInt(x)
          }
          else {
            A = parseInt(x)
          }
          blink = !blink
      }
    }
    // implement your calculator's logic here
  }

  println(acc)
}
package opera.common

import chisel3.Data
import dsptools.numbers.Convergent
import fixedpoint.FixedPoint
import java.io.{BufferedWriter, File, FileWriter}
import scala.math.BigDecimal.double2bigDecimal

object FileUtils {
  def writeWindowFunction2File(fileName: String, dataType: Data, window: Seq[Double], dataPerWord: Int = 1, dataBytes: Int): Unit = {
    val binPointPosition = dataType match {
      case fp: FixedPoint => fp.binaryPoint.get
      case _ => 0
    }

    val file = new File(fileName)

    // Create parent directories if they don't exist
    file.getParentFile.mkdirs()

    val w = new BufferedWriter(new FileWriter(file))
    val windowShifted = window.map(
      c => StringUtils.formatString(ArithmeticUtils.roundWithMode(c * (1 << binPointPosition), Convergent).toBigInt, dataBytes)
    )

    windowShifted.grouped(dataPerWord).foreach { m => w.write(m.mkString + "\n") }
    w.close()
  }
}

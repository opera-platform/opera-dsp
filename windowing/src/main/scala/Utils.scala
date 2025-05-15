package windowing

import chisel3.Data
import fixedpoint.FixedPoint

import java.io.{BufferedWriter, File, FileWriter}

object Utils {
  def writeWindowFunction2File(fileName: String, dataType: Data, window: Seq[Double]): Unit = {
    val binPointPosition = dataType match {
      case fp: FixedPoint => fp.binaryPoint.get
      case _ => 0
    }

    val file = new File(fileName)

    // Create parent directories if they don't exist
    file.getParentFile.mkdirs()

    val w = new BufferedWriter(new FileWriter(file))
    val windowShifted = window.map(c => c * scala.math.pow(2, binPointPosition).toInt)

    windowShifted.foreach { m => w.write(f"${m.toInt}%02x" + "\n")}
    w.close()
  }
}

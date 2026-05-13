package opera.fft

import breeze.linalg.DenseVector
import breeze.math.Complex
import breeze.plot._

import java.awt.{Color, Font}
import java.io.File
import javax.swing.SwingUtilities

object PlotUtils {
  private val PlotDpi = 144
  private val ComparisonPlotWidth = 1800
  private val ComparisonPlotHeight = 1200
  private val TitleFont = new Font("SansSerif", Font.BOLD, 20)
  private val LabelFont = new Font("SansSerif", Font.PLAIN, 18)
  private val TickFont = new Font("SansSerif", Font.PLAIN, 16)
  private val GridColor = new Color(220, 220, 220)

  def writePlotIfEnabled(
      name       : String,
      model      : Seq[Complex],
      breeze     : Seq[Complex],
      modelLabel : String = "model",
      breezeLabel: String = "floating-point",
  ): Option[File] =
    if (TestConfig.plot && model.nonEmpty) {
      Some(writePlot(
        output      = new File(TestConfig.plotDirectory, s"${TestUtils.safeFileStem(name)}.png"),
        title       = name,
        model       = model,
        breeze      = breeze,
        modelLabel  = modelLabel,
        breezeLabel = breezeLabel
      ))
    } else {
      None
    }

  def writePlot(
      output     : File,
      title      : String,
      model      : Seq[Complex],
      breeze     : Seq[Complex],
      modelLabel : String = "model",
      breezeLabel: String = "floating-point",
  ): File = {
    require(model.length == breeze.length, "comparison sequences must have equal length")
    require(model.nonEmpty, "plot requires at least one FFT output sample")

    val bins      = DenseVector((0 until model.length).map(i => i - model.length / 2.0).toArray)
    val modelMag  = DenseVector(fftShift(model.map(abs).toVector).toArray)
    val breezeMag = DenseVector(fftShift(breeze.map(abs).toVector).toArray)
    val error     = DenseVector(fftShift(model.zip(breeze).map { case (a, b) => math.hypot(a.real - b.real, a.imag - b.imag) }.toVector).toArray)

    Option(output.getParentFile).foreach(_.mkdirs())
    val figure = Figure()
    SwingUtilities.invokeAndWait { () =>
      figure.visible   = false
      figure.width     = ComparisonPlotWidth
      figure.height    = ComparisonPlotHeight
      val magnitude    = figure.subplot(2, 1, 0)
      magnitude       += plot(bins, modelMag, name = modelLabel)
      magnitude       += plot(bins, breezeMag, name = breezeLabel)
      magnitude.title  = s"$title - magnitude"
      magnitude.xlabel = "shifted FFT bin"
      magnitude.ylabel = "magnitude"
      magnitude.legend = true
      formatPlot(magnitude)

      val errorPlot = figure.subplot(2, 1, 1)
      errorPlot       += plot(bins, error, name = "absolute error")
      errorPlot.title  = "absolute error"
      errorPlot.xlabel = "shifted FFT bin"
      errorPlot.ylabel = "error"
      formatPlot(errorPlot)
    }

    saveHighResolution(figure, output)
    output
  }

  private def formatPlot(panel: Plot): Unit = {
    panel.setXAxisIntegerTickUnits()
    panel.setYAxisDecimalTickUnits()
    panel.xaxis.setTickLabelsVisible(true)
    panel.yaxis.setTickLabelsVisible(true)
    panel.xaxis.setTickMarksVisible(true)
    panel.yaxis.setTickMarksVisible(true)
    panel.xaxis.setLabelFont(LabelFont)
    panel.yaxis.setLabelFont(LabelFont)
    panel.xaxis.setTickLabelFont(TickFont)
    panel.yaxis.setTickLabelFont(TickFont)
    Option(panel.chart.getTitle).foreach(_.setFont(TitleFont))
    panel.chart.setBackgroundPaint(Color.WHITE)
    panel.plot.setBackgroundPaint(Color.WHITE)
    panel.plot.setDomainGridlinesVisible(true)
    panel.plot.setRangeGridlinesVisible(true)
    panel.plot.setDomainGridlinePaint(GridColor)
    panel.plot.setRangeGridlinePaint(GridColor)
    panel.yaxis.setAutoRangeIncludesZero(true)
  }

  private def saveHighResolution(figure: Figure, output: File): Unit = {
    SwingUtilities.invokeAndWait { () => () }
    figure.saveas(output.getAbsolutePath, dpi = PlotDpi)
  }

  private def abs(x: Complex): Double =
    math.hypot(x.real, x.imag)

  private def fftShift(values: Vector[Double]): Vector[Double] = {
    val half = values.length / 2
    values.drop(half) ++ values.take(half)
  }
}

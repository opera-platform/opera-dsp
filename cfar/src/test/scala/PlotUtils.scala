package opera.cfar

import breeze.linalg.DenseVector
import breeze.plot._
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer

import java.awt.{BasicStroke, Color, Font}
import java.awt.geom.Ellipse2D
import java.io.File
import javax.swing.SwingUtilities

private[cfar] final case class PlotSample(
    fftBin   : Int,
    cut      : Option[Double],
    threshold: Double,
    peak     : Boolean
)

private[cfar] object PlotUtils {
  private val PlotDpi    = 144
  private val PlotWidth  = 1800
  private val PlotHeight = 900
  private val TitleFont  = new Font("SansSerif", Font.BOLD, 20)
  private val LabelFont  = new Font("SansSerif", Font.PLAIN, 18)
  private val TickFont   = new Font("SansSerif", Font.PLAIN, 16)
  private val GridColor  = new Color(220, 220, 220)

  def writePlotIfEnabled(name: String, samples: Seq[PlotSample]): Option[File] =
    if (TestConfig.plot && samples.nonEmpty) {
      Some(writePlot(
        output  = new File(TestConfig.plotDirectory, s"${safeFileStem(name)}.png"),
        title   = name,
        samples = samples
      ))
    } else {
      None
    }

  def writePlot(output: File, title: String, samples: Seq[PlotSample]): File = {
    require(samples.nonEmpty, "CFAR plot requires at least one sample")

    val ordered    = samples.sortBy(_.fftBin)
    val bins       = DenseVector(ordered.map(_.fftBin.toDouble).toArray)
    val thresholds = DenseVector(ordered.map(_.threshold).toArray)
    val cuts       = ordered.map(_.cut)

    Option(output.getParentFile).foreach(_.mkdirs())
    val figure = Figure()
    SwingUtilities.invokeAndWait { () =>
      figure.visible = false
      figure.width   = PlotWidth
      figure.height  = PlotHeight
      val panel      = figure.subplot(1, 1, 0)
      if (cuts.exists(_.isDefined)) {
        panel += plot(bins, DenseVector(cuts.map(_.getOrElse(Double.NaN)).toArray), name = "cut")
      }
      panel += plot(bins, thresholds, name = "threshold")
      peakStems(ordered).foreach { case (peakBins, peakValues) =>
        panel += plot(peakBins, peakValues, name = "peak stem")
        val datasetIndex = panel.plot.getDatasetCount - 1
        val renderer = panel.plot.getRenderer(datasetIndex)
        renderer.setSeriesPaint(0, Color.RED)
        renderer.setSeriesStroke(0, new BasicStroke(1.4f))
      }
      peakDots(ordered).foreach { case (peakBins, peakValues) =>
        panel += plot(peakBins, peakValues, name = "peak")
        val datasetIndex = panel.plot.getDatasetCount - 1
        val renderer = panel.plot.getRenderer(datasetIndex)
        renderer.setSeriesPaint(0, Color.RED)
        renderer match {
          case lineRenderer: XYLineAndShapeRenderer =>
            lineRenderer.setSeriesLinesVisible(0, false)
            lineRenderer.setSeriesShapesVisible(0, true)
            lineRenderer.setSeriesShape(0, new Ellipse2D.Double(-2.5, -2.5, 5.0, 5.0))
          case _ =>
        }
      }
      panel.title = s"$title - CFAR outputs"
      panel.xlabel = "FFT bin"
      panel.ylabel = "magnitude"
      panel.legend = true
      formatPlot(panel)
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

  private def peakStems(samples: Seq[PlotSample]): Option[(DenseVector[Double], DenseVector[Double])] = {
    val points   = samples.filter(_.peak).flatMap { sample =>
      val height = sample.cut.getOrElse(sample.threshold)
      Seq(
        sample.fftBin.toDouble -> 0.0,
        sample.fftBin.toDouble -> height,
        Double.NaN -> Double.NaN
      )
    }
    if (points.isEmpty) None
    else Some((
      DenseVector(points.map(_._1).toArray),
      DenseVector(points.map(_._2).toArray)
    ))
  }

  private def peakDots(samples: Seq[PlotSample]): Option[(DenseVector[Double], DenseVector[Double])] = {
    val points = samples.filter(_.peak).map { sample =>
      sample.fftBin.toDouble -> sample.cut.getOrElse(sample.threshold)
    }
    if (points.isEmpty) None
    else Some((
      DenseVector(points.map(_._1).toArray),
      DenseVector(points.map(_._2).toArray)
    ))
  }

  private def safeFileStem(name: String): String =
    name.replace("^", "x").replaceAll("[^A-Za-z0-9_.-]", "-")
}

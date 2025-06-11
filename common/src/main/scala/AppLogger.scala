package opera.common

object AppLogger {
  private sealed abstract class Color
  private object Red extends Color
  private object Orange extends Color
  private object Green extends Color
  private object Magenta extends Color
  private val escape = "\u001b[0m"

  private val colorToAnsi = Map(
    Red     -> "\u001b[31m",
    Orange  -> "\u001b[38;5;208m",
    Green   -> "\u001b[32m",
    Magenta -> "\u001b[35m"
  )

  private val colorToAnsiBackground = Map(
    Red     -> "\u001b[41;1m",
    Orange  -> "\u001b[48;5;208m",
    Green   -> "\u001b[42;1m",
    Magenta -> "\u001b[45;1m"
  )

  private sealed trait LogLevel
  private case object Error extends LogLevel
  private case object Warn extends LogLevel
  private case object Info extends LogLevel
  private case object Debug extends LogLevel

  private val levelEnv = System.getenv("LOG_LEVEL")
  private val logLevel = levelEnv match {
    case "Error" => Error
    case "Warn"  => Warn
    case "Debug" => Debug
    case _       => Info
  }

  private def isDebugEnabled: Boolean = logLevel == Debug
  private def isInfoEnabled: Boolean = isDebugEnabled | logLevel == Info
  private def isWarnEnabled: Boolean = isInfoEnabled | logLevel == Warn
  private def isErrorEnabled: Boolean = isWarnEnabled | logLevel == Error

  def debug(msg: String): Unit =
    if (isDebugEnabled) println(s"[DEBUG] $msg")
  def info(msg: String): Unit =
    if (isInfoEnabled)
      println(s"${colorToAnsi(Green)}[INFO] $msg$escape")
  def warn(msg: String): Unit =
    if (isWarnEnabled)
      println(s"${colorToAnsi(Orange)}[WARN] $msg$escape")
  def error(msg: String): Unit =
    if (isErrorEnabled) println(s"${colorToAnsi(Red)}[ERROR] $msg$escape")
}
package opera.fft

object TestUtils {
  def passWhen(fields: (String, Any)*): String =
    "pass when:\n" + fields.map { case (key, value) => s"\t\t$key = $value" }.mkString(",\n") + "\n"
}

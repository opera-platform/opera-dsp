package opera.fft

object TestUtils {
  def passWhen(fields: (String, Any)*): String =
    "pass when:\n" + fields.map { case (key, value) => s"\t\t$key = $value" }.mkString(",\n") + "\n"

  def titleFields(required: Seq[(String, Any)], optional: (Boolean, (String, Any))*): Seq[(String, Any)] =
    required ++ optional.collect { case (true, field) => field }

  def safeFileStem(name: String): String =
    name.replace("^", "x").replaceAll("[^A-Za-z0-9_.-]", "-")
}

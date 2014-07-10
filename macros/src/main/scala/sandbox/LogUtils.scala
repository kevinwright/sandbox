package sandbox

/**
 * Shamelessly liberated from the async macro
 */
object LogUtils {
  private def enabled(level: String) = sys.props.getOrElse(s"autoproxy.$level", "false").equalsIgnoreCase("true")

  private def verbose = enabled("debug")
  private def trace   = enabled("trace")

  private[sandbox] def vprintln(s: => Any): Unit = if (verbose) println(s"[async] $s")

  private[sandbox] def trace(s: => Any): Unit = if (trace) println(s"[async] $s")
}

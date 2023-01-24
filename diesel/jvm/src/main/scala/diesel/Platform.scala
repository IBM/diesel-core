package diesel

private[diesel] object Platform {

  def consoleWithFile(path: String)(thunk: => Unit): Unit = {
    import java.io.PrintStream
    Console.withOut(new PrintStream(path)) {
      thunk
    }
    println(s"Written to $path")
  }

}

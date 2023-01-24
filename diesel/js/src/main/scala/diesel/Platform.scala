package diesel

private[diesel] object Platform {

  def consoleWithFile(path: String)(thunk: => Unit): Unit = {
    thunk
    println(s"written to stdout. instead of $path")
  }

}

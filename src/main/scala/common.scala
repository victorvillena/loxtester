package org.willena.loxtester

import java.io.{ByteArrayOutputStream, PrintWriter}

val testsBasePath = os.pwd / "lox-tests"

case class LoxOutput(out: Seq[String], err: Seq[String], code: Int)

def executeFile(cmd: Seq[String]) =
  import scala.sys.process.*

  val out = ByteArrayOutputStream()
  val err = ByteArrayOutputStream()
  val outWriter = PrintWriter(out)
  val errWriter = PrintWriter(err)

  val exitCode = cmd
    .run(ProcessLogger(outWriter.println, errWriter.println))
    .exitValue() // blocks until command finishes, ensuring we get the output

  outWriter.close()
  errWriter.close()

  LoxOutput(out.toString.linesIterator.toSeq, err.toString.linesIterator.toSeq, exitCode)


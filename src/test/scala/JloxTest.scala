package eu.willena.loxtester

import java.io.{ByteArrayOutputStream, PrintWriter}
import scala.sys.process

case class LoxTestFile(path: String, expected: Seq[String])
case class LoxTestResult(out: Seq[String], err: Seq[String], code: Int)

// Fortunately, each test file focuses on either checking for correct outputs, or aiming for a
// specific error. This allows us to classify each file into a category, and test it accordingly.
// The comments are styled differently, so we'll be able to look for that.
enum TestCategory:
  case ExpectedOutputs
  case Error
  case RuntimeError

class JloxTest extends munit.FunSuite:

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

    LoxTestResult(out.toString.linesIterator.toSeq, err.toString.linesIterator.toSeq, exitCode)

  val loxCommand = "java -cp ../jlox/target/classes eu.willena.lox.Lox"

  val loxTestFiles = os.walk(os.pwd / "lox-tests" / "assignment").filter(os.isFile)

  val loxTests = loxTestFiles.map: file =>
    val contents = os.read.lines(file)
    val expects = contents
      .filter(_.contains(" // expect: "))
      .map(_.split(" // expect: ").toSeq(1))

    LoxTestFile(file.toString, expects)


  loxTests.filter(_._2.nonEmpty).foreach: loxTest =>
    test(loxTest.path) {

      val command = s"$loxCommand ${loxTest.path}".split(' ').toSeq
      val result = executeFile(command)

    }


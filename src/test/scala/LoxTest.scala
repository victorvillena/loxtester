package org.willena.loxtester

import scala.sys.{error, process}

import java.io.{ByteArrayOutputStream, PrintWriter}

case class LoxTestFile(path: os.Path, category: TestCategory)
case class LoxTestResult(out: Seq[String], err: Seq[String], code: Int)

// Fortunately, almost every test file focuses on either checking for correct outputs, or aiming for
// a specific error. This allows us to classify each file into a category, and test it accordingly.
// The comments are styled differently, so we'll be able to look for that.
enum TestCategory:
  // The most common case is one or more comments of the form "// expect: value", where the code
  // is expected to output those "value".
  case ExpectedOutputs(outputs: Seq[String])

  // Runtime errors are marked with comments "// expect runtime error: message", and we expect the
  // message to match the one in the comment. Due to the nature of runtime errors, there's just one
  // at a time.
  case RuntimeError(error: String)

  // Static errors are commented as-is, and we expect the message to match the one reported.
  case Errors(errors: Seq[String])

  // Finally, some files do not contain any expected output or errors, the only challenge is they
  // should work, without errors of any kind.
  case ShouldWork

  // And this is where any files we don't want to test now will go.
  case Skip

abstract class LoxTest(
    loxCommand: String, // TODO probably doesn't work with spaces
    testImplMarker: String,
) extends munit.FunSuite:

  val errorCode        = 65
  val runtimeErrorCode = 70

  def executeFile(cmd: Seq[String]) =
    import scala.sys.process.*

    val out       = ByteArrayOutputStream()
    val err       = ByteArrayOutputStream()
    val outWriter = PrintWriter(out)
    val errWriter = PrintWriter(err)

    val exitCode = cmd
      .run(ProcessLogger(outWriter.println, errWriter.println))
      .exitValue() // blocks until command finishes, ensuring we get the output

    outWriter.close()
    errWriter.close()

    LoxTestResult(out.toString.linesIterator.toSeq, err.toString.linesIterator.toSeq, exitCode)

  def categorize(test: os.Path) =
    val expectedMarker        = "// expect: "
    val runtimeErrorMarker    = "// expect runtime error: "
    val staticErrorRegex      = "^.*// \\[line.*"
    val implStaticErrorRegex  = s"^.*// \\[$testImplMarker line.*"
    val staticErrorMarker     = "// [line"
    val implStaticErrorMarker = s"// [$testImplMarker line"

    val contents = os.read.lines(test)
    if contents.exists(_.matches(s"^.*$runtimeErrorMarker.*")) then
      val error = contents
        .filter(_.contains(runtimeErrorMarker))
        .map(_.split(runtimeErrorMarker).toSeq(1))
      TestCategory.RuntimeError(error.head)
    else if contents.exists(_.matches(s"^.*$expectedMarker.*")) then
      val expected = contents
        .filter(_.contains(expectedMarker))
        .map(_.split(expectedMarker).toSeq(1))
      TestCategory.ExpectedOutputs(expected)
    else if contents.exists(s => s.matches(staticErrorRegex) || s.matches(implStaticErrorRegex)) then
      val expected = contents
        .filter(s => s.contains(staticErrorMarker) || s.contains(implStaticErrorMarker))
        .map(_.split("// ").toSeq(1).replace(s"[$testImplMarker ", "["))
      TestCategory.Errors(expected)
    else TestCategory.ShouldWork

  val testsBasePath = os.pwd / "lox-tests"

  def skippedPaths(p: os.Path) =
    // These files and folders are not part of the final test suite
    val skipList = Seq("benchmark", "limit", "nan_equality.lox", "scanning", "expressions", "comments")

    val segments = p.relativeTo(testsBasePath).segments
    skipList.exists(segments.contains) || segments.exists(_.startsWith("."))

  val loxTestFiles = os.walk(testsBasePath, skip = skippedPaths).filter(os.isFile)

  val loxTests = loxTestFiles.map: file =>
    LoxTestFile(file, categorize(file))

  loxTests.foreach: loxTest =>
    val fileName = loxTest.path.relativeTo(testsBasePath).toString
    test(s"${loxTest.category.productPrefix} - $fileName") {

      val command = s"$loxCommand ${loxTest.path}".split(' ').toSeq
      val result  = executeFile(command)

      loxTest.category match
        case TestCategory.ExpectedOutputs(outputs) =>
          assertEquals(result.code, 0)
          assertEquals(result.out, outputs, result)

        case TestCategory.RuntimeError(error) =>
          assertEquals(result.code, runtimeErrorCode)

          // It's possible for some test files to print things before reaching the error, and stack
          // information will be after. Check all lines for the expected error message.
          assert(result.err.contains(error))
        // assertEquals(result.err(result.err.length - 2), error)

        case TestCategory.Errors(errors) =>
          assertEquals(result.code, errorCode)
          assertEquals(result.err, errors)

        case TestCategory.ShouldWork =>
          assertEquals(result.code, 0)

        case TestCategory.Skip =>
          // Do nothing
          ()
    }

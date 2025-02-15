package org.willena.loxtester

object LoxBenchmark:

  @main def benchmark(loxCommand: String) =
    val benchmarks = os.walk(testsBasePath / "benchmark").filter(os.isFile).filter(_.last.endsWith(".lox"))
      //.filterNot(_.last.contains("string_equality.lox")) // fails on clox
      .sortBy(_.last)

    benchmarks.foreach: file =>
      val name = file.last

      val command = s"$loxCommand $file".split(' ').toSeq
      val result  = executeFile(command)

      val time = name.stripSuffix(".lox") match
        case "binary_trees"    => result.out.last
        case "equality"        => result.out(3)
        case "fib"             => result.out.last
        case "instantiation"   => result.out.last
        case "invocation"      => result.out.last
        case "method_call"     => result.out.last
        case "properties"      => result.out.last
        case "string_equality" => result.out(3)
        case "trees"           => result.out.last
        case "zoo"             => result.out.last
        case "zoo_batch"         => result.out(1)

      println(f"$name%-20s: ${time.toDouble}%2.3f")

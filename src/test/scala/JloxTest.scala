package eu.willena.loxtester

class JloxTest
    extends LoxTest(
      loxCommand = "java -cp ../jlox/target/classes eu.willena.lox.Lox",
      testImplMarker = "java",
    )

package eu.willena.loxtester

class CloxTest
  extends LoxTest(
    loxCommand = "../clox/cmake-build-debug/clox",
    testImplMarker = "c",
  )

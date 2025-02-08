package eu.willena.loxtester

class CustomTest extends LoxTest(
  loxCommand = sys.props.get("test.command").get,
  testImplMarker = sys.props.get("test.impl").get,
)

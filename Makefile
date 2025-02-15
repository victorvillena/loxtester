impl ?= none

test-jlox:
	sbt "testOnly *JloxTest"

test-clox:
	sbt "testOnly *CloxTest"

test-slox:
	sbt "testOnly *SloxTest"

test-custom:
	@# args:
	@# - command - Path to custom Lox executable. The executable will be passed a .lox file as its
	@#             first argument.
	@# - impl - Marker for the implementation, eg. `java` or `c`. This allows tests to check for
	@#          specific errors present only in those implementations. If set to a value not present
	@#          in the tests, it will have no effect. If not set, defaults to a value that will
	@#          bypass all impl-specific checks.
	sbt "testOnly *CustomTest -- \"-Dtest.command=$(command)\" -Dtest.impl=$(impl)"

benchmark:
	sbt "run \"$(command)\""

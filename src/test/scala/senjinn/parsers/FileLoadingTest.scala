package senjinn.parsers

import senjinn.base.{ResourceLocator, loadResource}

/**
 * Abstraction of a test in which test cases are parsed from external
 * resource files.
 */
trait FileLoadingTest {
  protected val testpkg = getClass.getPackage
  
  protected type TestCaseArgs
  protected def resourceNameSequence: Seq[String]
  protected def parseTestFile(lines: Seq[String]): TestCaseArgs
  protected def performTest(args: TestCaseArgs): Unit

  protected final def executeAllTestCases(): Unit = {
    testCaseIterator foreach { performTest(_) }
  }
  
  private final def testCaseIterator: Iterator[TestCaseArgs] = {
    resourceNameSequence.iterator
    .map(loadResource(testpkg, _))
    .map(parseTestFile(_))
  }
}
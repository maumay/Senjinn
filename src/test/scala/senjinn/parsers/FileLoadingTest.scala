package senjinn.parsers

import senjinn.base.ResourceLocator

/**
 * Abstraction of a test in which test cases are parsed from external
 * resource files.
 */
trait FileLoadingTest 
{
  protected val testpkg = getClass.getPackage
  protected type TestCaseArgs
  protected def testCaseIterator: Iterator[TestCaseArgs]
  protected def performTest(args: TestCaseArgs): Unit
  
  protected final def executeAllTestCases(): Unit = { testCaseIterator foreach {performTest(_)} }
}
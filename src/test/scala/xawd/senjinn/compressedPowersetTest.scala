package xawd.senjinn

import org.scalatest.FlatSpec

class CompressedPowersetTest extends FlatSpec
{
  val inputset = Vector(1L, 2L, 4L)
  val expectedresult = Set(0L, 1L, 2L, 4L, 1L | 2L, 1L | 4L, 2L | 4L, 1L | 2L | 4L)
  s"The compressed powerset of $inputset" must "be correct" in {
    assert(compressedPowerset(inputset).toSet == expectedresult)
  }
}

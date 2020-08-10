package iohk

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

class BlockSpec extends AnyWordSpec with should.Matchers{

  val blk = Block(index = 0, parentHash = Sha256.Zero_Hash, transactions = Vector(Transaction("This is a transaction")), Miner.StdMiningTargetNumber,  Miner.targetByLeadingZeros(3).toLong)

  "A Block" when {
    ".cryptoHash" should {
      "return a Hash object of the hash value of  the entire block" in {
        blk.cryptoHash shouldBe a[Hash]
      }

      ".verifyThisHasBeenMinedProperly" should {
        "Throw assertion error if the block's computed hash is greater than the mining target" in {
          assertThrows[java.lang.AssertionError]{
            blk.verifyThisHasBeenMinedProperly()
          }
        }
      }

    }

  }
}

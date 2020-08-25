package iohk

import org.scalatest.OptionValues
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

class BlockchainSpec extends AnyWordSpec with should.Matchers with OptionValues {

  val chain: FastBlockchain = FastBlockchain()

  "FastBlockchain" when {
    ".apply" should {
      "Initialize the chain with a Genesis block" in {
        val genesis = chain.findByIndex(0)
        genesis.value.parentHash.toNumber shouldBe Sha256.Zero_Hash.toNumber
      }
      }

    ".findByIndex" should {
      "Find an element by its index" in {
        val initialBlk = chain.findByIndex(0)
        initialBlk shouldBe defined
        initialBlk.value.index shouldBe 0
        initialBlk.value.cryptoHash shouldBe a[Hash]
      }
    }

    ".append" should {
      "add anew block to the chain" in {
        val initialBlk = chain.lastBlock
        val blk = Block(index = 1, parentHash = initialBlk.cryptoHash, transactions = Vector(Transaction("This is a transaction")), Miner.StdMiningTargetNumber,  Miner.targetByLeadingZeros(3).toLong)
        val app = chain.append(blk)
        val second = app.findByIndex(1)
        val first = chain.findByIndex(0).value
        second.value.parentHash.toNumber shouldBe first.cryptoHash.toNumber
      }
      "Reject appending a block with invalid index or hash" in {
        assertThrows[InvalidBlock]{
          val initialBlk = chain.lastBlock
          val blk = Block(index = 5, parentHash = initialBlk.cryptoHash, transactions = Vector(Transaction("This is a transaction")), Miner.StdMiningTargetNumber,  Miner.targetByLeadingZeros(3).toLong)
          chain.append(blk)
        }
      }
    }

    ".findByHash" should {
      "Find an element by its Hash" in {
        val last = chain.lastBlock
        val blk = Block(index = last.index + 1, parentHash = last.cryptoHash, transactions = Vector(Transaction("This is a second transaction")), Miner.StdMiningTargetNumber,  Miner.targetByLeadingZeros(3).toLong)
        val app = chain.append(blk)
        val res = app.findByHash(blk.cryptoHash)
        res shouldBe defined
        res.value.index shouldBe 2
      }
    }

    }
}

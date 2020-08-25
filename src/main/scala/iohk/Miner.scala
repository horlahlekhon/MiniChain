package iohk

import java.lang.management.ManagementFactory
import java.util.concurrent.{ExecutorService, Executors, ThreadPoolExecutor}

import iohk.Base._

import scala.collection.mutable
import scala.util.Random

object Miner {

  // NOTE: A Hash is also a Number, we use the two interchangeably.

  //

  // Mining is about computing hashes until we get something that is less

  // than a given target number.

  // This target serves, in a way, as the maximum possible number that a

  // proof of work computation should produce.

  final val StdMiningTargetNumber = targetByLeadingZeros(1)



  // Whoa! We actually mine the Genesis block.

  // Normally, this is done by the system during bootstrapping

  // and every other block is mined by a miner.

  final val Genesis = Miner.mineNextBlock(

    index = 0, // The very first block

    parentHash = Sha256.Zero_Hash, // Let's assume this is by definition for the Genesis block.

    transactions = Seq(Transaction("Hello Blockchain, this is Genesis :)")),

    StdMiningTargetNumber,

  )



  // We basically create a target number with the requirement of having

  // some leading zeros. More leading zeros means smaller number target number.

  //

  // NOTE: To actually solve the current coding challenge, would you choose a

  // small or a big number of leading zeros?

  def targetByLeadingZeros(zeros: Int): BigInt = {

    require(zeros < Sha256.NumberOfBytes)



    val bytes: Bytes =

      Array.tabulate[Byte](32) { n =>

        if (n < zeros) {

          0

        }

        else {

          0xff.toByte

        }

      }


    BigInt(1, bytes)

  }


  // And now let's implement the actual "proof-of-work"-style computation.

  // Compare the parameters of this method with the fields of a Block and

  // you'll see that the only thing missing here is the nonce. Here is why.

  //

  // Initially we have all the fixed elements a block:

  //

  //  - index,

  //  - parentHash,

  //  - transactions,

  //  - miningTargetNumber

  //

  // and by varying the nonce we try to have a block hash that is below the

  // given miningTargetNumber.

  //

  // NOTE Remember that the block hash can be transformed to an actual number,

  //      so we can talk about hash and number interchangeably.


  def mineNextBlock(index: Int, parentHash: Hash, transactions: Seq[Transaction], miningTargetNumber: BigInt): Block = {
    val noncesBuffer: mutable.Queue[(Hash, Long)] = new mutable.Queue[(Hash, Long)]()
    val bufferSize = 20
    var nonceGotten = false
    var validNonce: Option[Long] = None

    val pool = Executors.newFixedThreadPool(4)
    def miner(): Unit =  {
      while (!nonceGotten) noncesBuffer.synchronized{
        if(bufferSize == noncesBuffer.size){
          println("[Miner]... waiting buffer filled")
          noncesBuffer.wait()
        }
        // some verifier should have dequeue at this point
        val randome = new Random()
        var nonce = randome.nextLong
        val blk = Block(index, parentHash, transactions, miningTargetNumber, nonce)
        println(s"[Miner]..producing: hash = ${blk.cryptoHash}\tNonce = ${nonce}")
        noncesBuffer.enqueue((blk.cryptoHash, nonce))
        nonce += 1
        noncesBuffer.notify() // notify the verifier if it is waiting for us to compute
      }
    }

    def verifier(): Unit = {
      while(!nonceGotten){
        noncesBuffer.synchronized{
          if (noncesBuffer.isEmpty){
            println("[Verifier].. waiting for a value to be produced")
            noncesBuffer.wait()
          }
          val (blockHash, nonce) = noncesBuffer.dequeue()
          println(s"[Verifier]... Consumed : hash = ${blockHash} with \tNonce = ${nonce} ")
          println(s"[Verifier].... verifying consumed hash ${blockHash.toNumber} less than $miningTargetNumber : ${blockHash.toNumber < miningTargetNumber} ")
          if(blockHash.toNumber < miningTargetNumber){
            println(s"[Verifier].. we Got our nonce... valid nonce: ${nonce}")
            nonceGotten = true
            validNonce = Some(nonce)
          }

          println(s"[verifier]...number of currently running threads: ${ManagementFactory.getThreadMXBean.getThreadCount}")
          noncesBuffer.notify()
        }
      }
    }

    pool.execute(() => verifier())
    pool.execute(() => miner())
    pool.execute(() => verifier())
    pool.execute(() => miner())
    Block(index, parentHash, transactions, miningTargetNumber, validNonce.getOrElse(0))
  }

  def numberOfLeadingZeros(amount: Int, hash: Hash): Boolean = hash.bytes.count(_.toChar == '0') == amount


}



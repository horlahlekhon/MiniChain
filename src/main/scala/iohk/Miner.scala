package iohk

import iohk.Base._

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

  def mineNextBlock(

                     index: Int,

                     parentHash: Hash,

                     transactions: Seq[Transaction],

                     miningTargetNumber: BigInt,

                   ): Block = {
    // Solve this informal inequality for nonce:

    //

    //   Hash(block; nonce).toNumber < miningTargetNumber

    //

    // where Hash(block; nonce) is a function of nonce only, all the other block
    import scala.util.control.Breaks._
    // field values are just the given method arguments.
    var nonce = targetByLeadingZeros(Random.nextInt(2))
    var blk = Block(index, parentHash, transactions, miningTargetNumber, targetByLeadingZeros(1).toLong)
////    var pi =  1
//    while(blk.cryptoHash.toNumber > miningTargetNumber){
//       nonce = targetByLeadingZeros(31)
//      println(s"hash : ${blk.cryptoHash.toNumber}..... is there a hit: ${blk.cryptoHash.toNumber < miningTargetNumber}.... pow : ${miningTargetNumber}")
//       blk = blk.copy(nonce = nonce.toLong)
//    }
//    BigInt

    blk

  }

}



package iohk

import iohk.Base.Unknown

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Try
import scala.util.control.NoStackTrace
import Miner._
// A Blockchain is a sequence of blocks, each one having an index.

// The index of a block is the index of its parent plus one.

// A Blockchain always has a genesis block at index 0, which is the lowest index.

sealed trait Blockchain {



  // Add a block to the chain.

  // The return type is up to you, as explained in the definition of Unknown.

  def append(block: Block): FastBlockchain



  // Find a block by index.

  def findByIndex(index: Int): Option[Block]



  // Find a block by hash.

  def findByHash(hash: Hash): Option[Block]


  // Find a common ancestor between this blockchain and that blockchain.

  def common_ancestor(that: Blockchain): Unknown

}



// Implement an in-memory blockchain that internally has an indexing data structure.

// The purpose of this internal data structure is to avoid traversing the linked list

// of blocks when answering queries like findByIndex.
case class InvalidBlock(msg: String) extends Exception(msg)

case class FastBlockchain(private val chain: ArrayBuffer[Block] = ArrayBuffer(Miner.Genesis)) extends  Blockchain {
   var hashIndexMapping: mutable.Map[Number, Int] = mutable.Map.empty[Number, Int]
  override def append(block: Block): FastBlockchain = {
    if(block.index != chain.last.index + 1 || block.parentHash.toNumber != chain.last.cryptoHash.toNumber){
      throw  InvalidBlock(s"Please check the block's hash and index to be properly aligned." +
        s"""
           |Last block in the chain hash: ${chain.last.cryptoHash.toNumber}
           |newBlock parenthash: ${block.parentHash.toNumber}
           |""".stripMargin)
    }
    else{
      val nChain  = new FastBlockchain(chain.append(block))
      nChain.hashIndexMapping.put(block.cryptoHash.toNumber, block.index)
      nChain
    }
  }


  override def findByIndex(index: Int): Option[Block] = Try(chain(index)).fold(ex => None, value => Some(value))

  override def findByHash(hash: Hash): Option[Block] = {
    hashIndexMapping.get(hash.toNumber) match {
      case Some(value) => Some(chain(value))
      case None => None
    }
  }

  /// I didnt implement because i dont understand if two blockchains can
  // be related such as having ancestors, please i will appreciate an insight into this
  override def common_ancestor(that: Blockchain): Unknown = ???

  def length: Int = chain.length

  def lastBlock: Block = chain.last

  override def toString: String = s"FastBlockchain(chain = ${chain})"

}

object FastBlockchain {
  def apply(): FastBlockchain = {
    val initBlock = Miner.Genesis
    val chain  = FastBlockchain(ArrayBuffer(initBlock))
    chain.hashIndexMapping.put(initBlock.cryptoHash.toNumber, initBlock.index)
    chain
  }
}


// Finally, please write some tests to validate some of the properties

// you have encountered in the above description.



// Fun idea just for the discussion: How is this "mini-chain" similar (or not) to Git?



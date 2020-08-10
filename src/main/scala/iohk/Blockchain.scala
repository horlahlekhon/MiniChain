package iohk

import iohk.Base.Unknown

import scala.collection.mutable.ArrayBuffer
import scala.util.Try

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

case class FastBlockchain(private val chain: Vector[Block] = Vector(Miner.Genesis)) extends  Blockchain {

  override def append(block: Block): FastBlockchain =  FastBlockchain(block +: chain)


  override def findByIndex(index: Int): Option[Block] = chain.find(_.index == index)

  override def findByHash(hash: Hash): Option[Block] = chain.find(_.cryptoHash.toNumber == hash.toNumber)

  override def common_ancestor(that: Blockchain): Unknown = ???

  override def toString: String = s"FastBlockchain(chain = ${chain})"

}

object FastBlockchain {
  def apply(): FastBlockchain =  FastBlockchain(Vector(Miner.Genesis))
}


// Finally, please write some tests to validate some of the properties

// you have encountered in the above description.



// Fun idea just for the discussion: How is this "mini-chain" similar (or not) to Git?



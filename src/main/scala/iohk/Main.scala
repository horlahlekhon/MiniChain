package iohk

object Main extends App {

  val link: FastBlockchain =  FastBlockchain()
//  println(s"Targe leading hy seros: ${Miner.targetByLeadingZeros(1)}")
//  val initBlock: Block = Miner.Genesis
  val gen = link.findByIndex(0).get
  val more = Miner.mineNextBlock(gen.index + 1, gen.cryptoHash, Seq(Transaction("Olake")), Miner.StdMiningTargetNumber )

//  val nChain = FastBlockchain()
  println(more)
  val app = link.append(more)
//  println(app)

  println(app)
  val gen3 = app.findByIndex(0).get
  val Olak = app.findByIndex(1).get
//  println(app.findByHash(more.cryptoHash))
  println(s"Block 1: ${gen.cryptoHash.toNumber}")
  println(s"block 2: ${Olak.parentHash.toNumber}")
  println(Olak.parentHash.toNumber == gen3.cryptoHash.toNumber)




  //  more.cryptoHash
//  initBlock.verifyThisHasBeenMinedProperly()
//  link.append(initBlock)
//  link.append(more)
////  link.print()
//  println(link.findByIndex(1))

}

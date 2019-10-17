import scala.io.Source

class TestBinParser(filePath: String) {
  val source = Source.fromFile(filePath)
  val lines = source.getLines

  var memAData = Map[BigInt, BigInt]()
  var memBData = Map[BigInt, BigInt]()
  var romData = Map[BigInt, BigInt]()
  var res = 0
  var cycle = 0


  lines.foreach(s => parseLine(s))
  val romSeq = (romData map (x => x._2)).toSeq

  def parseLine(line:String){
    val tokens = line.split(" ", 0)
    if(tokens.length == 3){
      val addr = BigInt(java.lang.Long.parseUnsignedLong(tokens(1), 16))
      val data = BigInt(java.lang.Long.parseUnsignedLong(tokens(2), 16))
      if(tokens(0).contains("ROM")){
        romData += (addr->data)
      }
      else if(tokens(0).contains("RAM")){
        if((addr&0x1) == 1){
          memAData += ((addr>>1)->data)
        }else{
          memBData += ((addr>>1)->data)
        }
      }
    }
    else if(tokens.length == 2){
      if(tokens(0).contains("FIN")){
        res = Integer.parseUnsignedInt(tokens(1), 16)
      }
      else if(tokens(0).contains("CYCLE")){
        cycle = Integer.parseUnsignedInt(tokens(1), 16)
      }
    }
  }
}

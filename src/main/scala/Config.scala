case class RV16KConfig() {
  var debugIf = true
  var debugId = true
  var debugEx = true
  var debugMem = true
  var debugWb = true

  var test = false

  val romAddrWidth = 9
}

case class CAHPConfig() {
  var debugIf = true
  var debugId = true
  var debugEx = true
  var debugMem = true
  var debugWb = true

  var test = false

  //IF Unit
  val romAddrWidth = 9
  val romCacheWidth = 32
  val longInstWidth = 24
}

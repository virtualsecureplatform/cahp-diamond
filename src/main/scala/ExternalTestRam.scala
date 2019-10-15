/*
Copyright 2019 Naoki Matsumoto

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

class ExternalTestRam(ram:Map[BigInt,BigInt]) {
  var write = false
  var addr:BigInt = 0
  var data:BigInt = 0
  var ramData:Map[BigInt, BigInt] = ram

  def step(writeIn:Boolean, addrIn:BigInt, dataIn:BigInt): Unit ={
    if(write){
      memWrite()
    }else{
      memRead()
    }
    fetch(writeIn, addrIn, dataIn)
  }
  def fetch(writeIn:Boolean, addrIn:BigInt, dataIn:BigInt): Unit ={
    write = writeIn
    addr = addrIn
    data = dataIn
  }

  def memRead():BigInt = {
    val data = ramData.get(addr)

    if(data.isDefined){
      data.get&0xFF
    }else{
      0
    }
  }

  def memWrite() = {
    ramData = ramData.updated(addr, data)
  }

}

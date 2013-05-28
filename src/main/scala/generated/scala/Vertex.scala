package generated.scala


import collection.mutable.ArrayBuffer

class Vertex[VD:Manifest,ED:Manifest](
  val _graph: Graph[VD,ED], 
  var _data: VD) {
  val _tasks = new ArrayBuffer[Vertex[VD,ED]]
}

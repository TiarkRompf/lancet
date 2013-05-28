package generated.scala



class Edge[VD:Manifest,ED:Manifest](
  val _graph: Graph[VD,ED],
  var _inData: ED, 
  var _outData: ED, 
  val _v1: Vertex[VD,ED], 
  val _v2: Vertex[VD,ED]
)

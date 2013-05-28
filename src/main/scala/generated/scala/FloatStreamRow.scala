package generated.scala



/* StreamRowImpl wraps a DenseVectorView that represents a Stream row.
 *
 * author:  Arvind Sujeeth (asujeeth@stanford.edu)
 * created: 3/15/11
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 */

class FloatStreamRow(chunkRow: Int, offset: Int, stream: FloatStream, x: Array[Float])
  extends FloatDenseVectorView(x, chunkRow*stream.numCols, 1, stream.numCols, true) {

  // absolute row index in the stream
  val index = offset*stream.chunkSize + chunkRow
}

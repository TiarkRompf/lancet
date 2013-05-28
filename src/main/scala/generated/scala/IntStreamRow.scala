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

class IntStreamRow(chunkRow: Int, offset: Int, stream: IntStream, x: Array[Int])
  extends IntDenseVectorView(x, chunkRow*stream.numCols, 1, stream.numCols, true) {

  // absolute row index in the stream
  val index = offset*stream.chunkSize + chunkRow
}

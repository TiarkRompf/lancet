package generated.scala.application
import generated.scala._


class BinarizedGradientTemplate (
  // In the reduced image. The side of the template square is then 2*r+1.
  val radius: Int,

  // Holds a tighter bounding box of the object in the original image scale
  val rect: Rect,
  val mask_list: IntDenseVector,

  // Pyramid level of the template (reduction_factor = 2^level)
  val level: Int,

  // The list of gradients in the template
  val binary_gradients: DoubleDenseVector,

  // indices to use for matching (skips zeros inside binary_gradients)
  val match_list: IndexVectorDenseC,

  // This is a match list of list of sub-parts. Currently unused.
  val occlusions: DenseVector[IntDenseVector],

  val templates: DenseVector[BinarizedGradientTemplate],

  val hist: FloatDenseVector
)
 

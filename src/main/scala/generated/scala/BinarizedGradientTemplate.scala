/*
 * Copyright (c) 2013 Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.

 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 * 
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see http://www.gnu.org/licenses/agpl.html.
 * 
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 */
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
 

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
package generated.scala


import collection.mutable.{HashMap}

class Graph[VD:Manifest,ED:Manifest] {
  // TODO: we are storing a lot of data here. investigate reducing the footprint vs. performance.
  var _edgeToVertices = HashMap[Edge[VD,ED], (Vertex[VD,ED], Vertex[VD,ED])]()
  // var verticesToEdges = HashMap[(V, V), E]()

  // this is used only during construction (before frozen), for fast sorting
  var _adjacencies = HashMap[Vertex[VD,ED], List[(Edge[VD,ED], Vertex[VD,ED])]]()

  // this is used only after construction (after frozen), for fast access
  // Map from vertex to id
  val _vertexIds = HashMap[Vertex[VD,ED], Int]()

  var _vertices : Array[Vertex[VD,ED]] = null
  var _edges : Array[Edge[VD,ED]] = null

  var _vertexEdges : Array[DenseVector[Edge[VD,ED]]] = null
  var _neighbors : Array[DenseVector[Vertex[VD,ED]]] = null
  var _neighborsSelf : Array[DenseVector[Vertex[VD,ED]]] = null

  var _frozen = false
}

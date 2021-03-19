// Copyright (C) 2011-2012 the original author or authors.
// See the LICENCE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package org.scalastyle.scalariform

import org.scalastyle.scalariform.VisitorHelper.visit
import org.scalastyle.PositionError
import org.scalastyle.ScalariformChecker
import org.scalastyle.ScalastyleError
import _root_.scalariform.parser.{CompilationUnit, Param, ParamClauses}

class ColonSpaceChecker extends ScalariformChecker {
  val errorKey = "spaces.after.colon"

  def verify(ast: CompilationUnit): List[ScalastyleError] = {
    val paramClauses = localVisit(ast.immediateChildren.head)
    val paramSpacingInvalidChecks = paramClauses.map(hasInvalidSpacingInFuncDef)

    paramSpacingInvalidChecks.flatMap { paramsCheck =>
      paramsCheck.flatMap { paramCheck =>
        val paramIsInvalid = paramCheck._1

        if (paramIsInvalid) {
          Option(PositionError(paramCheck._2))
        } else {
          None
        }
      }
    }
  }

  private def hasInvalidSpacingInFuncDef(expr: ParamClauses): List[(Boolean, Int)] = {
    val functionDefParamClause = expr.paramClausesAndNewlines.head._1
    val firstParam = functionDefParamClause.firstParamOption

    firstParam match {
      case None =>
        List() // No first parameter means nothing to check
      case _ =>
        val remainingParams = functionDefParamClause.otherParams
        val allParams = List(firstParam.get) ++ remainingParams.map(_._2)

        allParams.map({ param =>
          if (paramHasInvalidSpacing(param)){
            val errorOffset = param.paramTypeOpt.get._2.tokens.head.offset
            (true, errorOffset)
          } else {
            (false, -1)
          }
        })
    }
  }

  private def paramHasInvalidSpacing(param: Param): Boolean = {
    val colonOffset = param.paramTypeOpt.get._1.offset
    val actualParamTypeOffset = param.paramTypeOpt.get._2.tokens.head.offset
    val expectedParamTypeOffset = colonOffset + 2 // One for the space and once for the first char of the type

    actualParamTypeOffset != expectedParamTypeOffset
  }

  private def localVisit(ast: Any): List[ParamClauses] = {
    ast match {
    case expr: ParamClauses => List(expr)
    case other: Any => visit(other, localVisit)
  }
  }
}

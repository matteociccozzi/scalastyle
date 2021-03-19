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

import org.junit.Test
import org.scalastyle.file.CheckerTest
import org.scalatest.junit.AssertionsForJUnit

class ColonSpaceCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "spaces.after.colon"
  val classUnderTest = classOf[ColonSpaceChecker]

  @Test def testOneFunctionNoParamsOk(): Unit = {
    val source =
    """
      def foobar(): Unit = {
        println("hello")
      }
    """
    assertErrors(List(), source)
  }

  @Test def testOneFunctionOneParamNoSpace(): Unit = {
    val source =
    """
      def foobar(param1:String): Unit = {
        println("hello")
      }
    """

    assertErrors(List(columnError(2, 18)), source)
  }

  @Test def testOneFunctionOneParamTwoSpaces(): Unit = {
    val source =
      """
      def foobar(param1:  String): Unit = {
        println("hello")
      }
    """

    assertErrors(List(columnError(2, 20)), source)
  }

  @Test def testOneFunctionOneParamOK(): Unit = {
    val source =
    """
    def foobar(param1: String): Unit = {
      println("hello")
    }
    """
    assertErrors(List(), source)
  }

  @Test def testTwoFunctionsOneParam(): Unit = {
    val source =
    """
    def foo1(param1:String): Unit = {
      println("hello")
    }

    def foo2(param2:String): Unit = {
      println("hello")
    }
    """
    assertErrors(List(columnError(2, 16), columnError(6, 16)), source)
  }

  @Test def testOneFunctionMultipleParams(): Unit = {
    val source =
    """
    def foo1(param1:String, param2:Int, param3:  String): Unit = {
      println("hello")
    }
    """
    assertErrors(List(
      columnError(2, 16),
      columnError(2, 31),
      columnError(2,45)
    ), source)
  }
}

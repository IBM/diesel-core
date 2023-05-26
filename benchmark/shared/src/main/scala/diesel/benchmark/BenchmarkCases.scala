/*
 * Copyright 2018 The Diesel Authors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package diesel.benchmark

import diesel.samples.glsl.Glsl
import diesel.samples.jsmodeldsl.BmdDsl
import diesel.{AstHelpers, Dsl}

object BenchmarkCases {

  def parseSimpleBmd: Unit = {
    val text =
      """|start with a Foo.
         |a Foo is a concept.
         |a Gnu is a xx.
         |""".stripMargin
    parseSome(BmdDsl, Some(BmdDsl.aCompileUnit))(text)
  }

  def parseSomeGlsl: Unit = {
    val text =
      """void mainImage( out vec4 fragColor, in vec2 fragCoord )
        |{
        |    vec2 uv = fragCoord/iResolution.xy;
        |
        |    if (true) {
        |    	return 12;
        |    }
        |
        |    vec3 col = 0.5 + 0.5*cos(iTime+uv.xyx+vec3(0,2,4));
        |
        |    fragColor = vec4(col,1.0);
        |}
        |void mainImage( out vec4 fragColor, in vec2 fragCoord )
        |{
        |    vec2 uv = fragCoord/iResolution.xy;
        |
        |    if (true) {
        |    	return 12;
        |    }
        |
        |    vec3 col = 0.5 + 0.5*cos(iTime+uv.xyx+vec3(0,2,4));
        |
        |    fragColor = vec4(col,1.0);
        |}
        |void mainImage( out vec4 fragColor, in vec2 fragCoord )
        |{
        |    vec2 uv = fragCoord/iResolution.xy;
        |
        |    if (true) {
        |    	return 12;
        |    }
        |
        |    vec3 col = 0.5 + 0.5*cos(iTime+uv.xyx+vec3(0,2,4));
        |
        |    fragColor = vec4(col,1.0);
        |}
        |void mainImage( out vec4 fragColor, in vec2 fragCoord )
        |{
        |    vec2 uv = fragCoord/iResolution.xy;
        |
        |    if (true) {
        |    	return 12;
        |    }
        |
        |    vec3 col = 0.5 + 0.5*cos(iTime+uv.xyx+vec3(0,2,4));
        |
        |    fragColor = vec4(col,1.0);
        |}
        |void mainImage( out vec4 fragColor, in vec2 fragCoord )
        |{
        |    vec2 uv = fragCoord/iResolution.xy;
        |
        |    if (true) {
        |    	return 12;
        |    }
        |
        |    vec3 col = 0.5 + 0.5*cos(iTime+uv.xyx+vec3(0,2,4));
        |
        |    fragColor = vec4(col,1.0);
        |}
        |void mainImage( out vec4 fragColor, in vec2 fragCoord )
        |{
        |    vec2 uv = fragCoord/iResolution.xy;
        |
        |    if (true) {
        |    	return 12;
        |    }
        |
        |    vec3 col = 0.5 + 0.5*cos(iTime+uv.xyx+vec3(0,2,4));
        |
        |    fragColor = vec4(col,1.0);
        |}
        |
        |float x = 12;
        |
        |void mainImage( out vec4 fragColor, in vec2 fragCoord )
        |{
        |    vec2 uv = fragCoord/iResolution.xy;
        |
        |    if (true) {
        |    	return 12;
        |    }
        |
        |    vec3 col = 0.5 + 0.5*cos(iTime+uv.xyx+vec3(0,2,4));
        |
        |    fragColor = vec4(col,1.0);
        |}
        |
        |void mainImage( out vec4 fragColor, in vec2 fragCoord )
        |{
        |    vec2 uv = fragCoord/iResolution.xy;
        |
        |    if (true) {
        |    	return 12;
        |    }
        |
        |    vec3 col = 0.5 + 0.5*cos(iTime+uv.xyx+vec3(0,2,4));
        |
        |    fragColor = vec4(col,1.0);
        |}
        |""".stripMargin
    parseSome(Glsl, Some(Glsl.a))(text)
  }

  private def parseSome(dsl: Dsl, axiom: Option[Dsl.Axiom[_]] = None)(text: String): Unit = {
    AstHelpers.parse(dsl, text, axiom)
  }

  // use main with IntelliJ profiler
  def main(args: Array[String]): Unit = {
    parseSimpleBmd
    parseSimpleBmd
  }
}

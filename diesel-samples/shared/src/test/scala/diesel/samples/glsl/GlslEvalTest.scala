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

package diesel.samples.glsl

import diesel.samples.glsl.Ast.TranslationUnit
import diesel.samples.glsl.eval.Expr._
import diesel.samples.glsl.eval.{EValue, GlslEval, Scope}
import diesel.{DslTestFunSuite, GenericTree}

class GlslEvalTest extends DslTestFunSuite {

  override def dsl = Glsl
  type Ast = Scope

  override def ast(tree: GenericTree): Scope = {
    val scope = Scope.initial()
    new GlslEval(scope).eval(tree.value.asInstanceOf[TranslationUnit])
    scope
  }

  private def doCheck(text: String)(f: Scope => Unit) = {
    withAst(text)(f)
  }

  private def assertInScope(s: Scope, varName: String, value: EValue): Unit = {
    val e = s.find(varName).get
    assert(e == value)
  }

  test("one float") {
    doCheck(
      "float x = 12;"
    )(s => {
      assert(s.getVariables.size == 1)
      assertInScope(s, "x", num(12.0))
    })
  }

  test("two floats") {
    doCheck(
      "float x = 12, y = 13;"
    )(s => {
      assert(s.getVariables.size == 2)
      assertInScope(s, "x", num(12.0))
      assertInScope(s, "y", num(13.0))
    })
  }

  test("add") {
    doCheck(
      "float x = 12 + 1;"
    )(s => {
      assert(s.getVariables.size == 1)
      assertInScope(s, "x", num(13.0))
    })
  }

  test("external call") {
    doCheck(
      """
        |void myInc(float x) {
        |  return x + 1;
        |}
        |""".stripMargin
    )(s => {
      val myInc = s.findFunc("myInc").get
      val res   = myInc.call(s, Seq(num(10)))
      assert(res == num(11.0))
    })
  }

  test("internal call") {
    doCheck(
      """
        |float myFun(float x, float y, float z) {
        | return x + y + z;
        |}
        |float my = myFun(1,2,3);
        |""".stripMargin
    )(s => {
      assertInScope(s, "my", num(6.0))
    })
  }

  test("call funct in funct") {
    doCheck(
      """
        |float f1(float x) {
        | return x + 1;
        |}
        |float f2(float x) {
        | return f1(x) + 1;
        |}
        |float foo = f2(0);
        |""".stripMargin
    )(s => {
      assertInScope(s, "foo", num(2.0))
    })
  }

  test("out params") {
    doCheck(
      """
        |void myFun( out float y ) {
        |  y = 2;
        |}
        |""".stripMargin
    )(s => {
      val f = s.findFunc("myFun").get
      s.declare("foo", num(1))
      assert(f.call(s, Seq(ref("foo"))) == voidExpr)
      assertInScope(s, "foo", num(2))
    })
  }

  test("shadertoy row 1") {
    doCheck(
      """
        |void mainImage( out vec4 fragColor, in vec2 fragCoord )
        |{
        |    vec2 uv = fragCoord/iResolution.xy;
        |}
        |""".stripMargin
    )(s => {
      assert(s.findFunc("mainImage").isDefined)
    })
  }

  test("shadertoy row 2") {
    doCheck(
      """
        |void mainImage( out vec4 fragColor, in vec2 fragCoord )
        |{
        |    vec3 col = 0.1 + 0.2 * cos(iTime + uv.xyx + vec3(0,2,4));
        |}
        |""".stripMargin
    )(s => {
      assert(s.findFunc("mainImage").isDefined)
    })
  }

  test("cos") {
    doCheck(
      """
        |float y = cos(0);
        |""".stripMargin
    )(s => {
      assertInScope(s, "y", num(1))
    })
  }

  test("func precedence") {
    doCheck(
      """
        |float f(float x) {
        | return cos(x - 1);
        |}
        |float y = f(1);
        |""".stripMargin
    )(s => {
      assertInScope(s, "y", num(1))
    })

  }

  test("shadertoy call") {
    doCheck(
      """
        |void mainImage( out vec4 fragColor, in vec2 fragCoord )
        |{
        |    vec2 uv = fragCoord/iResolution.xy;
        |
        |    vec3 col = 0.5 + 0.5*cos(iTime+uv.xyx+vec3(0,2,4));
        |
        |    fragColor = vec4(col,1.0);
        |}
        |""".stripMargin
    )(s => {
      val f = s.findFunc("mainImage").get
      s.declare("iResolution", vec2(10, 20))
      s.declare("iTime", num(111))
      s.declare("fc", voidExpr)
      assert(f.call(s, Seq(ref("fc"), vec2(1, 2))) == voidExpr)
      assertInScope(
        s,
        "fc",
        vec4(0.29313871709764433, 0.9999982251499324, 0.29071592355169523, 1.0)
      )
    })
  }

  test("vecs from ctors") {
    Map(
      "vec2 v = vec2(1);"                -> vec2(1, 1),
      "vec2 v = vec2(1, 2);"             -> vec2(1, 2),
      "vec3 v = vec3(1);"                -> vec3(1, 1, 1),
      "vec3 v = vec3(1, 2, 3);"          -> vec3(1, 2, 3),
      "vec3 v = vec3(vec2(1, 2), 3);"    -> vec3(1, 2, 3),
      "vec3 v = vec3(1, vec2(2, 3));"    -> vec3(1, 2, 3),
      "vec4 v = vec4(1);"                -> vec4(1, 1, 1, 1),
      "vec4 v = vec4(1, 2, 3);"          -> vec4(1, 2, 3, 4),
      "vec4 v = vec4(vec2(1, 2), 3, 4);" -> vec4(1, 2, 3, 4),
      "vec4 v = vec4(1, vec2(2, 3), 4);" -> vec4(1, 2, 3, 4),
      "vec4 v = vec4(1, 2, vec2(3, 4));" -> vec4(1, 2, 3, 4),
      "vec4 v = vec4(vec3(1, 2, 3), 4);" -> vec4(1, 2, 3, 4),
      "vec4 v = vec4(1, vec3(2, 3, 4));" -> vec4(1, 2, 3, 4)
    )

    doCheck(
      """
        |vec2 v = vec2(1, 2);
        |""".stripMargin
    )(s => {
      assertInScope(s, "v", vec2(1, 2))
    })

  }

  test("swizzle") {
    doCheck(
      """
        |vec3 v = vec2(1,2).xyx;
        |""".stripMargin
    )(s => {
      assertInScope(s, "v", vec3(1, 2, 1))
    })

  }

  test("add vec3s") {
    doCheck(
      """
        |vec3 x = vec3(0);
        |vec3 foo = x + vec3(0, 2, 4);
        |""".stripMargin
    )(s => {
      assertInScope(s, "x", vec3(0, 0, 0))
      assertInScope(s, "foo", vec3(0, 2, 4))
    })
  }

  test("simple for".ignore) {
    doCheck(
      """
        |int f(in int x) {
        | int r = 0;
        | for (int i = 0 ; i < x ; i++) {
        |   r = r + 1;
        | }
        | return r;
        |}
        |
        |int foo = f(10);
        |""".stripMargin
    )(s => {
      assertInScope(s, "foo", num(10))
    })
  }

  test("mandelbrot".ignore) {
    doCheck(
      """
        |const int MAXITER = 180;
        |const vec2 CENTER = vec2(-0.74364388703715,0.13182590420533);
        |const float MAXTIME = 25.0;
        |const float PI = 3.1415;
        |
        |vec2 nextMandel(vec2 z, vec2 c)
        |{
        |    return vec2(dot(vec3(z,1.0),vec3(z.x,-z.y,c.x)),dot(vec3(z.xx,1.0),vec3(z.yy,c.y)));
        |}
        |
        |float mandel(vec2 c)
        |{
        |    vec2 z = vec2(0,0);
        |    int n=0;
        |    for(int i=0;i<MAXITER;i++){
        |        z=nextMandel(z,c);
        |        if(dot(z,z)>10000){
        |            break;
        |        }
        |        n++;
        |    }
        |    return float(n) - log2(log2(dot(z,z)));
        |}
        |
        |float x = mandel(vec2(1,1));
        |""".stripMargin
    )(s => {
      assertInScope(s, "CENTER", vec2(-0.74364388703715, 0.13182590420533))
    })

  }

//  private val x = """
//            |const int MAXITER = 180;
//            |const vec2 CENTER = vec2(-0.74364388703715,0.13182590420533);
//            |const float MAXTIME = 25.0;
//            |const float PI = 3.1415;
//            |
//            |
//            |
//            |vec2 nextMandel(vec2 z, vec2 c)
//            |{
//            |    return vec2(dot(vec3(z,1.0),vec3(z.x,-z.y,c.x)),dot(vec3(z.xx,1.0),vec3(z.yy,c.y)));
//            |}
//            |
//            |float mandel(vec2 c)
//            |{
//            |    vec2 z = vec2(0,0);
//            |    int n=0;
//            |    for(int i=0;i<MAXITER;i++){
//            |        z=nextMandel(z,c);
//            |        if(dot(z,z)>1e4){ // test divergence
//            |            break;
//            |        }
//            |        n++;
//            |    }
//            |    return float(n) - log2(log2(dot(z,z)));
//            |}
//            |
//            |void mainImage( out vec4 fragColor, in vec2 fragCoord )
//            |{
//            |	vec2 uv = fragCoord.xy / iResolution.xy;
//            |    float ratio = iResolution.x/iResolution.y;
//            |    uv = uv*2.0 -vec2(1.0,1.0);
//            |
//            |    float t = (sin(0.3*iTime-PI*0.5)*0.5+0.5)*MAXTIME;
//            |    float size=1.0/(pow(1.6,t));
//            |    vec2 center = CENTER;
//            |    uv*= size;
//            |
//            |    uv.x*= ratio;
//            |    uv+= center;
//            |
//            |    float value = float(mandel(uv));
//            |	fragColor = vec4(sin(value*0.2),sin(value*0.7),cos(value*0.3),1.0);
//            |}
//            |
//            |
//            |""".stripMargin

  test("shadertoy eval inside") {
    doCheck(
      """
        |float mainImage( out vec4 fragColor, in vec2 fragCoord )
        |{
        |    // Normalized pixel coordinates (from 0 to 1)
        |    vec2 uv = fragCoord/iResolution.xy;
        |
        |    // Time varying pixel color
        |    vec3 col = 0.5 + 0.5*cos(iTime+uv.xyx+vec3(0,2,4));
        |
        |    // Output to screen
        |    fragColor = vec4(col,1.0);
        |
        |return 1;
        |}
        |
        |vec4 c = vec4(0);
        |vec2 fc = vec2(0);
        |float iTime = 0;
        |float iResolution = vec2(320,200);
        |float x = mainImage(c, fc);
        |""".stripMargin
    )(s => {
      assertInScope(s, "fc", vec2(0, 0))
      assertInScope(s, "x", num(1))
      assertInScope(s, "c", vec4(1.0, 0.2919265817264288, 0.17317818956819403, 1.0))
    })

  }

}

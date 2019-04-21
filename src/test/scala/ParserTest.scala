class ParserTest extends org.scalatest.FunSuite {
  import fastparse._

  test("PositiveParse") {
    val program =
      """class MiniJavaTest1 {
             public static void main(String[] args) {
                 int a;
                 boolean b;
                 if (a < 2)
                     System.out.println(1);
                 else
                     System.out.println(3);
             }
         }
         class MiniJavaTest2 {
             public int doStuff(int a, int b) {
                 int[] x;
                 Foo b;
                 x = b.doOtherStuff(b, a);
                 return x;
             }

             public int calculate() {
                 MiniJavaTest2 c;
                 int i;
                 boolean b;
                 /*
                 Test d;
                 */
                 //int[] banan;


                 if (a < true) {} else {}

                 if (a > 3 && g <= 78) {
                     while (a >= 5) {

                     }
                 } else {
                     c = a == g;
                     if (a != b && foo) {

                     } else {
                         b = g.foo(a != c > d);
                     }
                 }

                 if (a == 5) {
                     System.out.println(56);
                 }

                 while(true) {
                     if (c.isGood()) {
                       i = c.getGoodness();
                     } else {
                       i = c.getBadness(1337, 1338);
                     }
                     System.out.println(i);
                     c = this;
                     System.out.println(c.length);
                     System.out.println(i.length);
                 }
                 b = !true;
                 b = (!true && false && c.partay());
/*TMP
                 a = d.foo().bar() + array[24].length;
                 a = array.length.length;
                 a = bar[24][27];
*/
                 a = !!true;
                 b = !!!!!!true;

                 x = !(!false && (true && !false)) + 5 * 6 * 3 - 4 + 5 - 3 + (5 - 3)*7;
/*TMP                 z = !cat.car().bar()[47].length; */

/*TMP                 a = !5 + !(2 && 3 - hej.i().crow()); */


                 b = 5 + true * foo.bar(hejsan.length);

                 d = new Test();
                 {
                     banan = new int[5];
                     banan[4] = 18;
                 }

                 return 5*3+2-(5+2)-7;
             }
         }

         class Empty {}
      """

    assertSuccess(program)
  }

  test("Precedence") {
    val program =
      """class MiniJavaTest1 {
        public static void main(String[] args) {
          a = 5 + 3 || 7 * 3 && 57 == 63 != 84 < 34 > 39 >= 938;
        }
      }
      """

    assertSuccess(program)
  }

  private def assertSuccess(s: String) = {
    val Parsed.Success(_, index) = Parser.parse(s, debug = true)
    assert(index == s.length)
  }

  private def assertFail(s: String) =
    Parser.parse(s, debug = true) match {
      case Parsed.Success(_, index) => assert(index != s.length)
      case Parsed.Failure(_, _, _) => assert(true)
    }

  private def assertSuccess(s: String, parseFn: P[_] => P[Any]) = {
    val Parsed.Success(_, index) = parse(s, parseFn)
    assert(index == s.length)
  }

  private def assertFail(s: String, parseFn: P[_] => P[Any]) = {
    parse(s, parseFn) match {
      case Parsed.Success(_, index) => assert(index != s.length)
      case Parsed.Failure(_, _, _) => assert(true)
    }
  }
}

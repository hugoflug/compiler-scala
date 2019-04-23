import org.scalatest.Matchers

class TypeCheckerTest extends org.scalatest.FunSuite with Matchers {
  private def typeCheck(program: String) =
    for {
      p <- Parser.parse(program)
      symTable <- SymbolTableCreator.create(p)
      _ <- TypeChecker.typeCheck(p, symTable)
    } yield ()

  test("ValidProgram") {
    val program = """
    class Foo {
      public static void main(String[] a){
        Foo foo;
        foo = new Foo();
      }
    }

    class Zoo {
      int[] x;
      boolean main;

      public int[] getX() {
        main = true;
        return x;
      }

      public int[] setX(int[] y) {
        x = y;
        return x;
      }
    }

    class Boo {
      int e;

      public int foo(int[] bar, int[] zoo) {
        int asdf;
        Foo foo;
        int a;
        a = 3;
        bar[5] = 7;
        System.out.println(bar.length);
        System.out.println(a < 3);
        a = bar[3];
        foo = new Foo();
        return a + 2;
      }

      public int croo(Foo foo, Boo boo) {
        int e;
        int[] f;
        e = 1;
        if (e < 2 && false) {
          while (true) {
            e = boo.croo(new Foo(), new Boo());
            f = new int[e];
            f[0] = 56;
            f[1] = 2;
          }
        } else {
          {
            /*
                boolean a = 5;
                comments should work!
            */
            /* also on only one line */
            //boolean class = 7; and this kind too
          }
        }
        return 0;
      }

      public int tzar(int[] arrayzish) {
        int a;
        int[] i;
        Boo boo;
        boo = new Boo();
        arrayzish[5] = this.foo(arrayzish, arrayzish);
        a = boo.foo(arrayzish, arrayzish);
        a = boo.foo(arrayzish, arrayzish);
        a = boo.croo(new Foo(), this);
        return arrayzish.length;
      }
    }"""

    typeCheck(program) should matchPattern { case Right(_) => }
  }

  test("UndefinedTypeInFormal") {
    val program = """
    class UndefinedTypeInFormal {
      public static void main(String[] args) {
      }
    }

    class Foo {
      int foo;

      public int bar(Double foo) {
        return 1;
      }
    }"""

    typeCheck(program) should matchPattern { case Left(_) => }
  }
}

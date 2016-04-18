import com.jatescher.SuperTinyCompiler
import com.jatescher.SuperTinyCompiler._
import org.scalatest._

class SuperTinyCompilerSpec extends WordSpec with Matchers {

  "tokenizer" should {
    "turn input strings into arrays of tokens" in {
      val input = "(add 2 3)"
      SuperTinyCompiler.tokenizer(input) shouldBe Vector(
        Paren(value = "("),
        Name(value = "add"),
        Number(value = "2"),
        Number(value = "3"),
        Paren(value = ")")
      )
    }

    "tokenize nested expressions" in {
      val input = "(add 1 (add 23 3))"
      SuperTinyCompiler.tokenizer(input) shouldBe Vector(
        Paren("("),
        Name("add"),
        Number("1"),
        Paren("("),
        Name("add"),
        Number("23"),
        Number("3"),
        Paren(")"),
        Paren(")")
      )
    }

    "turn empty strings into empty arrays" in {
      val input = ""
      SuperTinyCompiler.tokenizer(input) shouldBe Vector()
    }

    "error on unknown characters" in {
      val input = "!@#$%^&*()"
      intercept[IllegalArgumentException] {
        SuperTinyCompiler.tokenizer(input)
      }
    }
  }

  "parser" should {
    "turn vectors of tokens into an AST" in {
      val tokens = Vector(Paren("("), Name("add"), Number("2"), Number("3"), Paren(")"))
      SuperTinyCompiler.parser(tokens) shouldBe Program(body = Vector(
        LispCallExpression(name = "add", params = Vector(
          NumberLiteral(2),
          NumberLiteral(3)
        ))
      ))
    }

    "turn nested vectors of tokens into an AST" in {
      val tokens = Vector(
        Paren("("),
        Name("add"),
        Number("1"),
        Paren("("),
        Name("add"),
        Number("23"),
        Number("3"),
        Paren(")"),
        Paren(")")
      )
      SuperTinyCompiler.parser(tokens) shouldBe Program(body = Vector(
        LispCallExpression(name = "add", params = Vector(
          NumberLiteral(1),
          LispCallExpression(name = "add", params = Vector(
            NumberLiteral(23),
            NumberLiteral(3)
          ))
        ))
      ))
    }

    "turn empty vectors into an empty program" in {
      SuperTinyCompiler.parser(Vector()) shouldBe Program(body = Vector())
    }

    "error on malformed token" in {
      val tokens = Vector(Paren(value = "!@*&^%$"))
      intercept[IllegalArgumentException] {
        SuperTinyCompiler.parser(tokens)
      }
    }
  }

  "transformer" should {
    "transform one AST into another" in {
      val ast = Program(body = Vector(
        LispCallExpression(name = "add", params = Vector(
          NumberLiteral(2),
          NumberLiteral(3)
        ))
      ))

      SuperTinyCompiler.transformer(ast) shouldBe Program(body = Vector(
        ExpressionStatement(expression = CCallExpression(callee = Identifier(name = "add"), arguments = Vector(
          NumberLiteral(2),
          NumberLiteral(3)
        )))
      ))
    }

    "transform nested ASTs into another nested AST" in {
      val ast = Program(body = Vector(
        LispCallExpression(name = "add", params = Vector(
          NumberLiteral(1),
          LispCallExpression(name = "add", params = Vector(
            NumberLiteral(23),
            NumberLiteral(3)
          ))
        ))
      ))

      SuperTinyCompiler.transformer(ast) shouldBe Program(body = Vector(
        ExpressionStatement(expression = CCallExpression(callee = Identifier(name = "add"), arguments = Vector(
          NumberLiteral(1),
          ExpressionStatement(expression = CCallExpression(callee = Identifier(name = "add"), arguments = Vector(
            NumberLiteral(23),
            NumberLiteral(3)
          )))
        )))
      ))
    }

    "error if the AST is not valid" in {
      val ast = CCallExpression(callee = Identifier(name = "bug"), arguments = Vector())
      intercept[IllegalArgumentException] {
        SuperTinyCompiler.transformer(ast)
      }
    }
  }

  "codeGenerator" should {
    "generate a c style code string from our AST" in {
      val ast = Program(body = Vector(
        ExpressionStatement(expression = CCallExpression(callee = Identifier(name = "add"), arguments = Vector(
          NumberLiteral(2),
          NumberLiteral(3)
        )))
      ))

      SuperTinyCompiler.codeGenerator(ast) shouldBe "add(2, 3);"
    }

    "generate a c style code string from nested ASTs" in {
      val ast = Program(body = Vector(
        ExpressionStatement(expression = CCallExpression(callee = Identifier(name = "add"), arguments = Vector(
          NumberLiteral(1),
          ExpressionStatement(expression = CCallExpression(callee = Identifier(name = "add"), arguments = Vector(
            NumberLiteral(23),
            NumberLiteral(3)
          )))
        )))
      ))

      SuperTinyCompiler.codeGenerator(ast) shouldBe "add(1, add(23, 3););"
    }

    "error if the AST is not valid" in {
      val ast = LispCallExpression(name = "bug", params = Vector())
      intercept[IllegalArgumentException] {
        SuperTinyCompiler.codeGenerator(ast)
      }
    }
  }

  "compiler" should {
    "turn simple lisp style code into simple c style code" in {
      SuperTinyCompiler.compiler("(subtract 2 3)") shouldBe "subtract(2, 3);"
    }
    "turn nested lisp style code into nested c style code" in {
      SuperTinyCompiler.compiler("(subtract 2 (add 13 21))") shouldBe "subtract(2, add(13, 21););"
    }
  }

}

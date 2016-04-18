package com.jatescher

import scala.collection.mutable.ArrayBuffer

/**
  * Today we're going to create a super tiny compiler but instead of JavaScript, this one will be in scala!
  *
  * If you haven't checked out James Kyle's compiler yet then I suggest you do that first.
  *
  * We're going to compile the same lisp-like function calls into some C-like function calls.
  *
  * If we had two functions `add` and `subtract` they would be written like this:
  *
  *                  LISP                      C
  *
  *   2 + 2          (add 2 2)                 add(2, 2)
  *   4 - 2          (subtract 4 2)            subtract(4, 2)
  *   2 + (4 - 2)    (add 2 (subtract 4 2))    add(2, subtract(4, 2))
  *
  * Should be easy enough. Let's get started.
  */

/**
  * Most compilers break down into three primary stages: Parsing, Transformation, and Code Generation.
  *
  * 1. *Parsing* is taking raw code and turning it into a more abstract representation of the code.
  *
  * 2. *Transformation* takes this abstract representation and manipulates to do whatever the compiler wants it to.
  *
  * 3. *Code Generation* takes the transformed representation of the code and turns it into new code.
  */

/**
  * Parsing
  * -------
  *
  * Parsing typically gets broken down into two phases: Lexical Analysis and Syntactic Analysis.
  *
  * 1. *Lexical Analysis* takes the raw code and splits it apart into these things called tokens by a thing called a
  *    tokenizer (or lexer).
  *
  * 2. *Syntactic Analysis* takes the tokens and reformats them into a representation that describes each part of the
  *    syntax and their relation to one another. This is known as an intermediate representation or Abstract Syntax
  *    Tree.
  *
  * For the following syntax:
  *
  *   (add 2 (subtract 4 2))
  *
  * Tokens might look something like this:
  *
  *   Vector(
  *     Paren(value = "("),
  *     Name(value = "add"),
  *     Number(value = "2"),
  *     Paren(value = "("),
  *     Name(value = "subtract"),
  *     Number(value = "4"),
  *     Number(value = "2"),
  *     Paren(value = ")"),
  *     Paren(value = ")")
  *   )
  *
  * And an Abstract Syntax Tree (AST) might look like this:
  *
  *   Program(body = Vector(
  *     CallExpression(name = 'add', params = Vector(
  *       NumberLiteral(2),
  *       CallExpression(name = 'subtract', params = Vector(
  *         NumberLiteral(4),
  *         NumberLiteral(2)
  *       ))
  *     ))
  *   )
  */

/**
  * Transformation
  * --------------
  *
  * The next type of stage for a compiler is transformation. Again, this just takes the AST from the last step and
  * makes changes to it. It can manipulate the AST in the same language or it can translate it into an entirely new
  * language.
  *
  * Let’s look at how we would transform an AST.
  *
  * You might notice that our AST has elements within it that look very similar. Each of these are known as an AST Node.
  * These nodes have defined properties on them that describe one isolated part of the tree.
  *
  * We can have a node for a "NumberLiteral":
  *
  *   NumberLiteral(4)
  *
  * Or maybe a node for a "CallExpression":
  *
  *   CallExpression(name = "subtract, params = Vector(... nested nodes go here ...))
  *
  * When transforming the AST we can manipulate nodes by adding/removing/replacing properties, we can add new nodes,
  * remove nodes, or we could leave the existing AST alone and create an entirely new one based on it.
  *
  * Since we’re targeting a new language, we’re going to focus on creating an entirely new AST that is specific to the
  * target language.
  */

/** Traversal
  * ---------
  *
  * In order to navigate through all of these nodes, we need to be able to traverse through them. This traversal process
  * goes to each node in the AST depth-first.
  *
  *   Program(body = Vector(
  *     CallExpression(name = "add", params = Vector(
  *       NumberLiteral(2),
  *       CallExpression(name = "subtract", params = Vector(
  *         NumberLiteral(4),
  *         NumberLiteral(2)
  *       ))
  *     ))
  *   ))
  *
  * So for the above AST we would go:
  *
  *   1. Program - Starting at the top level of the AST
  *   2. CallExpression (add) - Moving to the first element of the Program's body
  *   3. NumberLiteral (2) - Moving to the first element of CallExpression's params
  *   4. CallExpression (subtract) - Moving to the second element of CallExpression's params
  *   5. NumberLiteral (4) - Moving to the first element of CallExpression's params
  *   6. NumberLiteral (2) - Moving to the second element of CallExpression's params
  *
  * If we were manipulating this AST directly, instead of creating a separate AST, we would likely introduce all sorts
  * of abstractions here. But we will use recursive pattern matching to cleanly generate a new AST.
  */

/**
  * Code Generation
  * ---------------
  *
  * The final phase of a compiler is code generation. Sometimes compilers will do things that overlap with
  * transformation, but for the most part code generation just means take our AST and string-ify code back out.
  *
  * Code generators work several different ways, some compilers will reuse the tokens from earlier, others will have
  * created a separate representation of the code so that they can print node linearly, but we're following James'
  * example so we will reuse the existing AST.
  *
  * Effectively our code generator will know how to “print” all of the different node types of the AST, and it will
  * recursively call itself to print nested nodes until everything is printed into one long string of code.
  */

/**
  * Ok. Now that we've got all the building blocks for the compiler ready, let's build it.
  */
object SuperTinyCompiler extends {

  /**
    * ============================================================================
    *                                   (/^▽^)/
    *                                THE TOKENIZER!
    * ============================================================================
    */

  // These are the currently supported token types.
  sealed trait Token { val value: String }
  case class Paren(value: String) extends Token
  case class Name(value: String) extends Token
  case class Number(value: String) extends Token

  /**
    * We're gonna start off with our first phase of parsing, lexical analysis, with
    * the tokenizer.
    *
    * We're just going to take our string of code and break it down into an array
    * of tokens.
    *
    *   (add 2 (subtract 4 2))   =>   [Paren(value = "("), ...]
    */
  def tokenizer(input: String): Vector[Token] = {

    // A `current` variable for tracking our position in the code like a cursor.
    var current = 0

    // And a `tokens` array buffer for pushing our tokens to.
    val tokens = scala.collection.mutable.ArrayBuffer[Token]()

    // We start by creating a `while` loop where we are setting up our `current` variable to be incremented as much as
    // we want inside the loop.
    //
    // We do this because we may want to increment `current` many times within a single loop because our tokens can be
    // any length. It is not idiomatic scala to use while loops, but we'll stick with this for now for clarity.
    while (current < input.length) {

      // Here we pattern match against the current position of the cursor, push found tokens to the tokens buffer, and
      // move the cursor forward equal to the length of the found token.
      input.charAt(current) match {

        // These will later be used for `CallExpressions`, but for now we just care about the character.
        case char if List('(', ')').contains(char) => {
          tokens += Paren(value = char.toString)
          current += 1
        }

        // We don't care about whitespace besides separating tokens, so we simply increment past this position.
        case char if char.isWhitespace => {
          current += 1
        }

        // The next type of token is a number. This is different than what we have seen before because a number could
        // be any number of characters and we want to capture the entire sequence of characters as one token.
        //
        //   (add 123 456)
        //        ^^^ ^^^
        //        Only two separate tokens
        //
        // So we start this off when we encounter the first number in a sequence.
        case char if char.isDigit => {
          val digits = input.substring(current).takeWhile(_.isDigit) // Grab the longest string of digits after cursor
          tokens += Number(value = digits)                           // Then create a token from the digits
          current += digits.length                                   // And make sure to move the cursor past the digits
        }

        // Name tokens are similar to digits, except we check for letters.
        case char if char.isLetter => {
          val value = input.substring(current).takeWhile(_.isLetter) // Grab the longest string of letters after cursor
          tokens += Name(value = value)                              // Then create a token from the letters
          current += value.length                                    // And make sure to move the cursor past the letters
        }

        // If we do not recognize the token we will throw an exception and exit.
        case char => throw new IllegalArgumentException(s"I don't know what this character is: $char")
      }

    }

    // We relied on a mutable data structure for simplicity in the implementation, but we can convert the buffer to an
    // immutable `Vector` of tokens now for use by the rest of the system.
    tokens.toVector
  }

  /**
    * ============================================================================
    *                                 ヽ/❀o ل͜ o\ﾉ
    *                                THE PARSER!!!
    * ============================================================================
    */

  /**
    * This is the list of the supported AST Node types we currently support. To keep the call expression types simple we
    * have separated out the `LispCallExpression` structure from the `CCallExpression`. It will become clearer when we
    * transform the AST from lisp-style to c-style.
    */
  trait ASTNode
  case class Program(body: Vector[ASTNode])                                  extends ASTNode
  case class LispCallExpression(name: String, params: Vector[ASTNode])       extends ASTNode
  case class CCallExpression(callee: Identifier, arguments: Vector[ASTNode]) extends ASTNode
  case class Identifier(name: String)                                        extends ASTNode
  case class ExpressionStatement(expression: ASTNode)                        extends ASTNode
  case class NumberLiteral(value: Long)                                      extends ASTNode

  /**
    * For our parser we're going to take our array of tokens and turn it into an AST.
    *
    *   Vector(Paren(value = "("), ...)   =>   Program(body = Vector(...))
    */
  def parser(tokens: Vector[Token]): ASTNode = {

    // Again we keep a `current` variable that we will use as a cursor.
    var current = 0

    // Body is buffer we will use to build program body
    val body = ArrayBuffer[ASTNode]()

    // But this time we're going to use recursion instead of a `while` loop. So we define a `walk` function.
    def walk: ASTNode = {

      // Inside we pattern match on the current token.
      tokens(current) match {

        // We will convert number tokens into a number literal AST node.
        case token: Number => {
          current += 1
          NumberLiteral(value = token.value.toLong)
        }

        // Next we wil convert open `Paren`s into `CallExpression`s
        case Paren("(") => {
          current += 1
          var token = tokens(current)           // The first token we care about is after the "(" character
          val callExpressionName = token.value  // The base node will be a `CallExpression` with the name of the token

          current += 1                          // We increment *again* to skip the name token

          val params = ArrayBuffer[ASTNode]()   //

          // And now we want to loop through each token that will be the `params` of our `CallExpression` until we
          // encounter a closing parenthesis.
          //
          // Now this is where recursion comes in. Instead of trying to parse a potentially infinitely nested set of
          // nodes we're going to rely on recursion to resolve things.
          //
          // To explain this, let's take our Lisp code. You can see that the parameters of the `add` are a number and a
          // nested `CallExpression` that includes its own numbers.
          //
          //   (add 2 (subtract 4 2))
          //
          // You'll also notice that in our tokens array we have multiple closing parenthesis.
          //
          //   Vector(
          //     Paren(value = "("),
          //     Name(value = "add"),
          //     Number(value: "2"),
          //     Paren(value: "("),
          //     Name(value = "subtract"),
          //     Number(value = "4"),
          //     Number(value = "2"),
          //     Paren(value = ")"), <<< Closing parenthesis
          //     Paren(value = ")")  <<< Closing parenthesis
          //   ]
          //
          // We're going to rely on the nested `walk` function to increment our `current` variable past any nested
          // `CallExpressions`. So we create a `while` loop that will continue until it encounters a token with a
          // type of `Paren` and a `value` of a closing parenthesis.
          while (!token.isInstanceOf[Paren] || (token.isInstanceOf[Paren] && token.value != ")")) {
            params += walk
            token = tokens(current)
          }

          current += 1 // Finally we will increment `current` one last time to skip the closing parenthesis.

          LispCallExpression(name = callExpressionName, params = params.toVector) // And return the expression
        }

        // If it is another type of token, we will error and exit.
        case token => throw new IllegalArgumentException(s"I don't know what this token is: $token")
      }
    }

    // And we're going to kickstart our `walk` function, pushing nodes to our `body` array.
    //
    // The reason we are doing this inside a loop is because our program can have `CallExpressions` after one another instead of being nested.
    //
    //   (add 2 2)
    //   (subtract 4 2)
    //
    while (current < tokens.length) {
      body += walk
    }

    // At the end of our parser we'll return the `Program` AST.
    Program(body = body.toVector)
  }


  /**
    * ============================================================================
    *                                 ⌒(❀>◞౪◟<❀)⌒
    *                               THE TRANSFORMER!!!
    * ============================================================================
    */

  /**
    * Next up, the transformer. Our transformer is going to take the AST that we have built and pass it to our traverser
    * function with a visitor and will create a new ast.
    *
    * ---------------------------------------------------------|--------------------------------------------------------
    *   Original AST                                           | Transformed AST
    * ---------------------------------------------------------|--------------------------------------------------------
    *   Program(body = Vector(                                 | Program(body = Vector(
    *     CallExpression(name = "add", params = Vector(        |   ExpressionStatement(
    *       NumberLiteral(2),                                  |     expression = CallExpression(
    *       CallExpression(name = "subtract", params = Vector( |       callee = Identifier(name = "add")
    *         NumberLiteral(4),                                |       arguments = Vector(
    *         NumberLiteral(2)                                 |         NumberLiteral(2),
    *       ))                                                 |         CallExpression(
    *     ))                                                   |           callee = Identifier(name = "subtract")
    *   )                                                      |           arguments = Vector(
    *                                                          |             NumberLiteral(4),
    * ---------------------------------------------------------|             NumberLiteral(2),
    *                                                          |           )
    *                                                          |         )
    *                                                          |       )
    * (sorry the other one is longer.)                         |     )
    *                                                          |   )
    *                                                          | )
    */
  def transformer(node: ASTNode): ASTNode = node match {

    // Our lisp AST `Program` will convert directly to a c AST `Program` with the params properly mapped.
    case Program(body) => Program(body = body.map(transformer))

    // The `LispCallExpression` maps to wrapped `ExpressionStatement` with the arguments mapped
    case LispCallExpression(name, params) => ExpressionStatement(expression = CCallExpression(
      callee = Identifier(name = name),
      arguments = params.map(transformer)
    ))

    // Number literals will be expressed the same way. Simply keep them as they are.
    case numberLiteral: NumberLiteral => numberLiteral

    // Any unexpected AST nodes should raise an exception.
    case _ => throw new IllegalArgumentException(s"I don't know what this node is $node")
  }

  /**
    * ============================================================================
    *                               ヾ（〃＾∇＾）ﾉ♪
    *                            THE CODE GENERATOR!!!!
    * ============================================================================
    */

  /**
    * Now let's move onto our last phase: The Code Generator.
    *
    * Our code generator is going to recursively call itself to print each node in the tree into one giant string.
    */
  def codeGenerator(node: ASTNode): String = node match {
    // If we have a `Program` node. We will map through each node in the `body` and run them through the code generator.
    case Program(body) => body.map(codeGenerator).mkString("\n")

    // For `ExpressionStatements` we'll call the code generator on the nested expression and we'll add a semicolon...
    case ExpressionStatement(expression) => codeGenerator(expression) + ";"

    // For `CallExpressions` we will print the `callee`, add an open parenthesis, we'll map through each node in the
    // `arguments` array and run them through the code generator, joining them with a comma, and then we'll add a
    // closing parenthesis.
    case CCallExpression(callee, arguments) => s"${codeGenerator(callee)}(${arguments.map(codeGenerator).mkString(", ")})"

    // For `Identifiers` we'll just return the `node`'s name.
    case Identifier(name) => name

    // For `NumberLiterals` we'll just return the `node`'s value.
    case NumberLiteral(value) => value.toString

    // And if we haven't recognized the node, we'll throw an error.
    case _ => throw new IllegalArgumentException(s"I don't know what this node is $node")
  }

  /**
    * ============================================================================
    *                                  (۶* ‘ヮ’)۶”
    *                         !!!!!!!!THE COMPILER!!!!!!!!
    * ============================================================================
    */

  /**
    * FINALLY! We'll create our `compiler` function. Here we will link together every part of the pipeline.
    *
    *   1. input  => tokenizer   => tokens
    *   2. tokens => parser      => ast
    *   3. ast    => transformer => newAst
    *   4. newAst => generator   => output
    */
  def compiler(input: String): String = {
    val tokens = tokenizer(input)
    val ast = parser(tokens)
    val newAST = transformer(ast)
    codeGenerator(newAST)
  }

  // And here we go. Fully compiled glory.
  def main(args: Array[String]): Unit = {
    val input = "(add 2 3)"
    val output = compiler(input)
    println(s"In lisp style it would be $input, and in c style it would be $output")
  }

}

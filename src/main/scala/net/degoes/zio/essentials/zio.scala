// Copyright(C) 2019 - John A. De Goes. All rights reserved.

package net.degoes.zio
package essentials

import java.io.{ File, IOException }
import java.text.SimpleDateFormat
import java.util.concurrent.{ Executors, TimeUnit }

import zio._
import zio.internal.PlatformLive

import scala.io.Source
import java.time.Clock

import zio.console.{ putStrLn, Console }
import zio.random.Random

import scala.util.{ Success, Try }

/**
 * `ZIO[R, E, A]` is an immutable data structure that models an effect, which
 * could be synchronous, asynchronous, concurrent, resourceful, errorful, or
 * environmental.
 *
 *  - The effect requires an environment `R`
 *  - The effect may fail with an error `E`
 *  - The effect may succeed with a value `A`
 *
 * Mental model: An effectful version of: R => Either[E, A]
 */
object zio_types {

  /**
   * Write the following types in terms of the `ZIO` type.
   */
  /**
   * EXERCISE 1
   *
   * An effect that might fail with an error of type `E` or succeed with a
   * value of type `A`.
   */
  type FailOrSuccess[E, A] = ZIO[Any, E, A]

  /**
   * EXERCISE 2
   *
   * An effect that never fails and might succeed with a value of type `A`
   */
  type Success[A] = ZIO[Any, Nothing, A]

  /**
   * EXERCISE 3
   *
   * An effect that runs forever but might fail with `E`.
   */
  type Forever[E] = ZIO[Any, E, Nothing]

  /**
   * EXERCISE 4
   *
   * An effect that cannot fail or succeed with a value.
   */
  type NeverStops = ZIO[Any, Nothing, Nothing]

  /**
   * EXERCISE 5
   *
   * An effect that may fail with a value of type `E` or succeed with a value
   * of type `A`, and doesn't require any specific environment.
   */
  type IO[E, A] = ZIO[Any, E, A]

  /**
   * EXERCISE 6
   *
   * An effect that may fail with `Throwable` or succeed with a value of
   * type `A`, and doesn't require any specific environment.
   */
  type Task[A] = ZIO[Any, Throwable, A]

  /**
   * EXERCISE 7
   *
   * An effect that cannot fail but may succeed with a value of type `A`,
   * and doesn't require any specific environment.
   */
  type UIO[A] = ZIO[Any, Nothing, A]

}

object zio_values {

  /**
   * EXERCISE 1
   *
   * Using the `ZIO.succeed` method. Construct an effect that succeeds with the
   * integer `42`, and ascribe the correct type.
   */
  val ioInt: UIO[Int] = ZIO.succeed(42)

  /**
   * EXERCISE 2
   *
   * Using the `ZIO.succeedLazy` method, construct an effect that succeeds with
   * the (lazily evaluated) specified value and ascribe the correct type.
   */
  lazy val bigList       = (1L to 100000000L).toList
  lazy val bigListString = bigList.mkString("\n")
  val ioString: ???      = ???

  /**
   * EXERCISE 3
   *
   * Using the `ZIO.fail` method, construct an effect that fails with the string
   * "Incorrect value", and ascribe the correct type.
   */
  val incorrectVal: IO[String, Nothing] = ZIO.fail("Incorrect value")

  /**
   * EXERCISE 4
   *
   * Using the `ZIO.effectTotal` method, construct an effect that wraps Scala
   * `println` method, so you have a pure functional version of `println`, and
   * ascribe the correct type.
   */
  def putStrLn(line: String): UIO[Unit] = ZIO.effectTotal(println(line))

  /**
   * EXERCISE 5
   *
   * Using the `ZIO.effect` method, wrap Scala's `readLine` method to make it
   * purely functional with the correct ZIO error type.
   *
   * Note: You will have to use the `.refineOrDie` method to refine the
   * `Throwable` type into something more specific.
   */
  val getStrLn: IO[IOException, String] = Task(scala.io.StdIn.readLine()).refineToOrDie[IOException]

  /**
   * EXERCISE 6
   *
   * Using the `ZIO.effect` method, wrap Scala's `getLines` to make it
   * purely functional with the correct ZIO error type.
   *
   * Note: You will have to use the `.refineOrDie` method to refine the
   * `Throwable` type into something more specific.
   */
  def readFile(file: File): Task[List[String]] =
    ZIO.effect {
      Source.fromFile(file).getLines.toList
    }

  /**
   * EXERCISE 7
   *
   * Using the `ZIO.effect` method, wrap Scala's `Array#update` method to make
   * it purely functional with the correct ZIO error type.
   *
   * Note: You will have to use the `.refineOrDie` method to refine the
   * `Throwable` type into something more specific.
   */
  def arrayUpdate[A](a: Array[A], i: Int, f: A => A): ??? =
    a.update(i, f(a(i))) ?

  /**
   * EXERCISE 8
   *
   * Using the `ZIO#refineOrDie` method, catch the `NoSuchElementException` and
   * return -1.
   */
  def firstOrNegative1(as: List[Int]): UIO[Int] = Task.effect(as.head) ?

  /**
   * EXERCISE 9
   *
   * Using the `ZIO.effectAsync` method, translate the `ScheduledExecutor` callback-
   * based API into a ZIO effect.
   */
  val scheduledExecutor = Executors.newScheduledThreadPool(1)
  def sleep(l: Long, u: TimeUnit): UIO[Unit] =
    UIO.effectAsync[Unit] { callback =>
      scheduledExecutor
        .schedule(new Runnable {
          def run(): Unit = callback(ZIO.unit)
        }, l, u)
    }

  /**
   * EXERCISE 10
   *
   * Using the `ZIO.effectAsync` method, translate the following callback-based API
   * into a ZIO API that does not use any callbacks.
   */
  def readChunkCB(success: Array[Byte] => Unit, failure: Throwable => Unit): Unit = ???
  val readChunkIO: Task[Array[Byte]] = Task.effectAsync[Array[Byte]] { callback =>
    readChunkCB(bytes => callback(Task.succeed(bytes)), failure => callback(ZIO.fail(failure)))
  }

  /**
   * EXERCISE 11
   *
   * Using the `ZIO.effectAsyncInterrupt` method, wrap the following Java
   * callback API into a ZIO effect. When the ZIO effect is interrupted,
   * call the canceller in the returned `HttpGetToken` of the Java API.
   */
  case class HttpGetToken(canceller: () => Unit)

  def httpGetCB(url: String)(success: Array[Byte] => Unit, error: Throwable => Unit): HttpGetToken = ???
  def httpGetIO(url: String): Task[Array[Byte]]                                                    = ???

  /**
   * EXERCISE 12
   *
   * In order to run effects, you need to interpret them using a `Runtime` in
   *  ZIO (such as `DefaultRuntime`) and call `unsafeRun`, or write your
   * pure main function inside `App`.
   */
  object Example extends DefaultRuntime {
    val sayHelloIO: UIO[Unit] = putStrLn("Hello ZIO!")

    //run sayHelloIO using `unsafeRun`
    val sayHello: Unit = unsafeRun(sayHelloIO)
  }
}

/**
 * Basic operations in ZIO.
 */
object zio_operations {

  /**
   * EXERCISE 1
   *
   * Using `ZIO#map`, map an effect that succeeds with an `Int` into one that
   * succeeds with a string.
   */
  val toStr: UIO[String] = IO.succeed(42).map(_.toString)

  /**
   * EXERCISE 2
   *
   * Using `ZIO#map`, map an effect that succeeds with an `Int` into one that
   * succeeds with one plus that integer.
   */
  def addOne(i: Int): UIO[Int] = IO.succeed(i).map(_ + 1)

  /**
   * EXERCISE 3
   *
   * Using the `ZIO#mapError` method, map an effect that fails with an `Int`
   * into one that fails with a string.
   */
  val toFailedStr: IO[String, Nothing] =
    IO.fail(42).mapError(_ => "asd")

  /**
   * EXERCISE 3
   *
   * Using `ZIO#flatMap`, check the integer produced by an effect, and if it
   * is even, return `attack`, but if it is odd, return `retreat`.
   */
  val attack: UIO[Boolean]  = UIO.effectTotal(println("Attacking!")).const(true)
  val retreat: UIO[Boolean] = UIO.effectTotal(println("Retreating!")).const(false)
  val action: UIO[Boolean]  = UIO(42).flatMap(i => if (i % 2 == 0) attack else retreat)

  /**
   * EXERCISE 4
   *
   * Using `ZIO#flatMap` and `ZIO#map` compute the sum of the values produced
   * by the `int1` and `int2` effects.
   */
  val int1: UIO[Int] = IO.succeed(14)
  val int2: UIO[Int] = IO.succeed(16)
  val sum: UIO[Int]  = ???

  /**
   * EXERCISE 5
   *
   * Translate this procedure (which repeats an action `n` times) into a function
   * that returns an effect that repeats the input effect the specified number of
   * times (hint: use `ZIO#flatMap` or `ZIO#zipRight`).
   */
  def repeatN1(n: Int, action: () => Unit): Unit =
    if (n <= 0) ()
    else {
      action()
      repeatN1(n - 1, action)
    }

  def repeatN2[E](n: Int, action: IO[E, Unit]): IO[E, Unit] =
    if (n <= 1) action
    else action.flatMap(_ => repeatN2(n - 1, action))

  /**
   * EXERCISE 6
   *
   * Translate this factorial function into its ZIO equivalent.
   */
  def factorial(n: Int): Int =
    if (n <= 1) 1
    else n * factorial(n - 1)
  def factorialIO(n: Int): UIO[Int] =
    ???

  /**
   * EXERCISE 7
   *
   * Write a new version of the factorial function, this one tail recursive.
   */
  def factorialTailIO(n: Int, acc: Int = 1): UIO[Int] = ???

  /**
   * EXERCISE 8
   *
   * Using `ZIO#zip`, combine the result of two effects into a tuple.
   */
  def toTuple[A, B](io1: UIO[A], io2: UIO[B]): UIO[(A, B)] = ???

  /**
   * EXERCISE 9
   *
   * Using `ZIO#zipWith`, add the two values produced by the two effects.
   */
  val combine: UIO[Int] = UIO.succeed(2).zipWith(UIO.succeed(40))(???)

  /**
   * EXERCISE 10
   *
   * Using `ZIO.foreach`, convert a list of integers into a List of String
   */
  def convert(l: List[Int]): UIO[List[String]] = UIO.succeed(l.map(_.toString))

  /**
   * EXERCISE 11
   *
   * Using `ZIO.collectAll`
   * evaluate a list of effects and collect the result into an IO of a list with their result
   */
  def collect(effects: List[UIO[Int]]): UIO[List[Int]] = effects ?

  /**
   * EXERCISE 12
   *
   * Rewrite the following series of `flatMap`/`map` into a `for` comprehension.
   */
  import zio.console.{ getStrLn, putStrLn }
  val nameAsk: ZIO[Console, Throwable, String] =
    for {
      _    <- Task.effect("what is your name?")
      name <- getStrLn
      _    <- putStrLn(s"hello $name")
    } yield name

  /**
   * EXERCISE 13
   *
   * Rewrite the following `for` comprehension into a series of `flatMap`/`map`.
   */
  val ageAsk: ZIO[Console, IOException, Task[Int]] =
    putStrLn("what is your age?").flatMap(_ => getStrLn.map(name => Task.fromTry(Try(name.toInt))))

  /**
   * EXERCISE 14
   *
   * Translate the following procedural program into its ZIO equivalent.
   */
  def playGame1(): Unit = {
    val number = scala.util.Random.nextInt(5)
    println("Enter a number between 0 - 5: ")
    scala.util.Try(scala.io.StdIn.readLine().toInt).toOption match {
      case None =>
        println("You didn't enter an integer!")
        playGame1()
      case Some(guess) if guess == number =>
        println("You guessed right! The number was " + number)
      case _ =>
        println("You guessed wrong! The number was " + number)
    }
  }
  def randomNumber: UIO[Int] = UIO(scala.util.Random.nextInt(5))
  def retryParseInputToInt: ZIO[Console, Nothing, Int] = getStrLn.map(_.toInt).catchAll { _ =>
    putStrLn("please enter a valid number") *> retryParseInputToInt
  }

  lazy val playGame2 = {
    for {
      num <- randomNumber
      _   <- putStrLn("Enter a number between 0 - 5: ")
      res <- getStrLn
              .flatMap(_ => retryParseInputToInt)
              .flatMap { x =>
                if (x == num)
                  putStrLn("you guessed right! the number was " + num)
                else putStrLn("you guessed wrong! the number was " + num)
              }
    } yield res
  }
}

object zio_failure {

  /**
   * EXERCISE 1
   *
   * Using `ZIO.fail` method, create an `IO[String, Int]` value that
   * represents a failure with a string error message, containing
   * a user-readable description of the failure.
   */
  val stringFailure: IO[String, Int] = ???

  /**
   * EXERCISE 2
   *
   * Translate the following exception-throwing program into its ZIO equivalent.
   */
  def accessArr1[A](i: Int, a: Array[A]): A =
    if (i < 0 || i >= a.length)
      throw new IndexOutOfBoundsException(s"The index $i is out of bounds [0, ${a.length} )")
    else a(i)

  def accessArr2[A](i: Int, a: Array[A]): IO[IndexOutOfBoundsException, A] =
    ???

  /**
   * EXERCISE 3
   *
   * Using `ZIO#fold`, recover from a division by zero error by supplying a recovery
   * value.
   */
  def divide(n: Int, d: Int): IO[ArithmeticException, Int] =
    if (d == 0) IO.fail(new ArithmeticException("Cannot divide by 0")) else IO.succeed(n / d)
  val recovered1: UIO[Option[Int]] = divide(100, 0).fold(_ => None, Some(_))

  /**
   * EXERCISE 4
   *
   * Using `ZIO#foldM`, print out either an error message or the division.
   */
  def printError(err: String): UIO[Unit] = UIO(println(err))
  def printDivision(int: Int): UIO[Unit] = UIO(println("Division is: " + int))
  val recovered2: UIO[Unit] = divide(100, 0).foldM(
    err => printError(err.getMessage),
    succ => printDivision(succ)
  )

  /**
   * EXERCISE 5
   *
   * Using `ZIO#either`, recover from division by zero error by returning -1.
   */
  val recovered3: UIO[Int] = divide(100, 0) ?

  /**
   * EXERCISE 6
   *
   * Using `ZIO#option`, recover from division by zero by returning -1.
   */
  val recovered4: UIO[Int] = divide(100, 0) ?

  /**
   * EXERCISE 7
   *
   * Using `ZIO#orElse`, attempt `firstChoice`, and fallback to `secondChoice` only
   * if `firstChoice` fails.
   */
  val firstChoice: IO[ArithmeticException, Int] = divide(100, 0)
  val secondChoice: UIO[Int]                    = IO.succeed(-1)
  val combined: UIO[Int]                        = firstChoice orElse secondChoice

  /**
   * EXERCISE 8
   *
   * Using `ZIO#catchAll`, recover from an error.
   */
  val caughtAll: UIO[Int] = divide(100, 0) ?

  /**
   * EXERCISE 9
   *
   * Using `ZIO#catchSome`, recover from only `EmptyStringError` error.
   */
  case object EmptyStringError extends Throwable
  val readNumber: Task[Int] = UIO(scala.io.StdIn.readLine()).flatMap { input =>
    if (input == "") IO.fail(EmptyStringError)
    else IO.effect(input.toInt)
  }
  val caughtSome = readNumber ?

  /**
   * EXERCISE 10
   *
   * Using `IO.effectTotal`, import code that is really not total.
   */
  val defect1: UIO[Int] = "this is a short text".charAt(30) ?

  /**
   * EXERCISE 11
   *
   * Using `ZIO#sandbox`, recover from the defect `defect1`.
   *
   */
  val caught1: UIO[Int] = defect1 ?

  /**
   * EXERCISE 12
   *
   * Using the `ZIO#catchAll` method, convert any exceptions in reading the
   * specified file into an empty list.
   */
  def readFile(file: File): UIO[List[String]] =
    Task(Source.fromFile(file).getLines.toList) ?

}

object sss extends App {

//  override def run(args: List[String]) = foundations.numberAdder.fold(_ => 1, _ => 0)
  override def run(args: List[String]) = impure_to_pure.ageExplainer2.fold(_ => 1, _ => 0)
}

object foundations {
  import zio.console._
  import zio.clock._
  import zio.random._

  def repeatUntil[R, E, A, B](effect: ZIO[R, E, A])(f: A => Task[B]): ZIO[R, Nothing, B] =
    effect.foldM(
      _ => repeatUntil(effect)(f),
      x => f(x).orElse(repeatUntil(effect)(f))
    )

  def repeatUntil3[R, E, A, B](effect: ZIO[R, E, A])(f: A => Task[B], onFailure: UIO[Any]): ZIO[R, E, B] =
    effect.flatMap(a => f(a).catchAll(_ => onFailure *> repeatUntil3(effect)(f, onFailure)))

  def repeatUntil2[R, E, A, B](effect: ZIO[R, E, A])(f: PartialFunction[A, B]): ZIO[R, Nothing, B] =
    effect.eventually.flatMap { x =>
      if (f.isDefinedAt(x)) ZIO.succeed(f(x))
      else repeatUntil2(effect)(f)
    }

  def parseHHmm(text: String): Task[(Int, Int)] = Task.effect {
    val parsed = new SimpleDateFormat("HH:mm").parse(text)

    (parsed.getHours, parsed.getMinutes)
  }

  /**
   * Ex 1
   * Ask the user for a time in 24 hour format and convert it to AM/PM format.
   */
  val timeConversion = for {
    _                <- putStrLn("please enter a time in 24 hour format(hh:mm): ")
    time             <- repeatUntil(getStrLn.orDie)(parseHHmm)
    (hours, minutes) = time
    conversion       = if (hours > 12) s"${hours - 12}:${minutes}pm" else s"${hours}:${minutes}am"
    _                <- putStrLn(s"The conversion is: $conversion")
  } yield conversion

  /**
   * EX 2
   * Ask the user fo a time to wait, then sleepm, then finally print our a wakeup msg
   */
  import zio.duration._
  val timerApp =
    for {
      _ <- putStrLn("how long sohuld I sleep, in seconds?")
      v <- repeatUntil(getStrLn)(x => Task(x.toInt))
      _ <- sleep(v seconds)
      _ <- putStrLn("time to wakup!")
    } yield ()

  /**
   * Ex 3
   * Choose a number between 0 and 10, and ask the user to guess it.
   * Give them 4 tries to get it right.
   */
  val guessGame = {
    def loop(remaining: Int, num: Int): ZIO[Random with Console, IOException, Unit] =
      if (remaining <= 0) putStrLn(s"trouble. you are out of guesses., and the number was $num")
      else {
        for {
          _     <- putStrLn("enter a guess plz")
          guess <- getStrLn
          _ <- if (Try(guess).map(_.toInt) == Success(num)) putStrLn(s"done! good guess ${num}")
              else putStrLn("sorry your guess was false ") *> loop(remaining - 1, num)
        } yield ()
      }
    zio.random.nextInt.flatMap(num => loop(4, num))
  }

  /*
   * ~ex 4,. A number Adder*/
  val numberAdder: ZIO[Console, Nothing, Int] = {
    val askForOneNumber: ZIO[Console, Nothing, Int] =
      putStrLn("can you please provide a number") *>
        repeatUntil(getStrLn.orDie)(str => Task(str.toInt))

    for {
      _    <- putStrLn("please give me anumber !")
      nums <- ZIO.collectAll(List(askForOneNumber, askForOneNumber))
      _    <- putStrLn(s"this is the res ${nums.sum}")
    } yield nums.sum
  }

}

object impure_to_pure {

  /**
   * EXERCISE 1
   *
   * Translate the following procedural program into ZIO.
   */
  def getName1(print: String => Unit, read: () => String): Option[String] = {
    print("Do you want to enter your name?")
    read().toLowerCase.take(1) match {
      case "y" => Some(read())
      case _   => None
    }
  }
  def getName2[E](print: String => IO[E, Unit], read: IO[E, String]): IO[E, Option[String]] =
    ???

  /**
   * EXERCISE 2
   *
   * Translate the following procedural program into ZIO.
   */
  def ageExplainer1(): Unit = {
    println("What is your age?")
    scala.util.Try(scala.io.StdIn.readLine().toInt).toOption match {
      case Some(age) =>
        if (age < 12) println("You are a kid")
        else if (age < 20) println("You are a teenager")
        else if (age < 30) println("You are a grownup")
        else if (age < 50) println("You are an adult")
        else if (age < 80) println("You are a mature adult")
        else if (age < 100) println("You are elderly")
        else println("You are probably lying.")
      case None =>
        println("That's not an age, try again")

        ageExplainer1()
    }
  }
  import zio.console._
  def ageExplainer2: ZIO[Console, Nothing, Unit] =
    for {
      _ <- putStrLn("what is your age?")
      age <- getStrLn
              .flatMap(input => Task(input.toInt))
              .flatMapError(_ => putStrLn("invalid int"))
              .eventually
      _ <- if (age < 12) putStrLn("You are a kid")
          else if (age < 20) putStrLn("You are a teenager")
          else if (age < 30) putStrLn("You are a grownup")
          else if (age < 50) putStrLn("You are an adult")
          else if (age < 80) putStrLn("You are a mature adult")
          else if (age < 100) putStrLn("You are elderly")
          else putStrLn("You are probably lying.")
    } yield ()

  /**
   * EXERCISE 3
   *
   * Translate the following procedural program into ZIO.
   */
  def decode1(read: () => Byte): Either[Byte, Int] = {
    val b = read()
    if (b < 0) Left(b)
    else {
      Right(
        b.toInt +
          (read().toInt << 8) +
          (read().toInt << 16) +
          (read().toInt << 24)
      )
    }
  }
  def decode2[E](read: IO[E, Byte]): IO[E, Either[Byte, Int]] = ???
}

object zio_interop extends DefaultRuntime {

  import scala.concurrent.ExecutionContext.global
  import scala.concurrent.Future

  /**
   * EXERCISE 1
   *
   * Using `Fiber#toFuture`, convert the following `Fiber` into a `Future`.
   */
  val fiber: Fiber[Throwable, Int] = Fiber.succeed(1)
  val fToFuture: UIO[Future[Int]]  = fiber.toFuture

  /**
   * EXERCISE 2
   *
   * Using `Fiber.fromFuture`, convert the following `Future` into a `Fiber`.
   */
  lazy val future1                     = Future(Thread.sleep(1000))(global)
  val fToFiber: Fiber[Throwable, Unit] = Fiber.fromFuture(future1)

  /**
   * EXERCISE 3
   *
   * Using `Task#toFuture`, unsafely convert the following `Task` into `Future`.
   */
  val task1: Task[Int]            = IO.effect("wrong".toInt)
  val tToFuture: UIO[Future[Int]] = task1.toFuture

  /**
   * EXERCISE 4
   *
   * Use `Task.fromFuture` to convert the following Scala `Future` into a
   * ZIO `Task`.
   */
  lazy val future2        = Future.successful("Hello World")
  val task2: Task[String] = Task.fromFuture(_ => future2)

  /**
   * EXERCISE 5
   *
   * Use `Task.fromTry` to convert the `Try` into a ZIO `Task`.
   */
  val tryValue: Try[String]   = scala.util.Failure(new Throwable("Uh oh"))
  val tryEffect: Task[String] = ZIO.fromTry(tryValue)

  /**
   * EXERCISE 6
   *
   * Use `IO.fromOption` to convert the `Option` into a ZIO `IO`.
   */
  val optionValue  = Some("foo")
  val optionEffect = ZIO.fromOption(optionValue)

  /**
   * EXERCISE 7
   *
   * Use `IO.fromEither` to convert the `Either` into a ZIO `IO`.
   */
  val eitherValue  = Right("foo")
  val eitherEffect = ZIO.fromEither(eitherValue)
}

/**
 * ZIO's version of try / finally, try-with-resources.
 */
object zio_resources {
  import java.io.{ File, FileInputStream }
  class InputStream private (is: FileInputStream) {
    def read: IO[Exception, Option[Byte]] =
      IO.effectTotal(is.read).map(i => if (i < 0) None else Some(i.toByte))
    def close: IO[Exception, Unit] =
      IO.effectTotal(is.close())
  }
  object InputStream {
    def openFile(file: File): IO[Exception, InputStream] =
      IO.effectTotal(new InputStream(new FileInputStream(file)))
  }

  /**
   * This following program is the classic paradigm for resource handling using try / finally
   */
  object classic {
    trait Handle
    def openFile(file: String): Handle        = ???
    def closeFile(handle: Handle): Unit       = ???
    def readFile(handle: Handle): Array[Byte] = ???

    // Classic paradigm for safe resource handling using
    // try / finally:
    def safeResource(file: String): Unit = {
      var handle: Handle = null.asInstanceOf[Handle]

      try {
        handle = openFile(file)

        readFile(handle)
      } finally if (handle != null) closeFile(handle)
    }

    def zioSafeResource(file: String) = Managed.make(UIO(openFile(file)))(x => UIO(closeFile(_)))

    def finallyPuzzler(): Unit =
      try {
        try throw new Error("e1")
        finally throw new Error("e2")
      } catch {
        case e: Error => println(e)
      }
  }

  /**
   * EXERCISE 1
   *
   * Rewrite the following procedural program to ZIO, using `IO.fail` and the
   * `ensuring` method.
   */
  var i = 0
  def noChange1(): Unit =
    try {
      i += 1
      throw new Exception("Boom!")
    } finally i -= 1

  val noChange2: Task[Unit] =
    UIO(i + 1) *> Task
      .fail(new Exception("boom"))
      .ensuring(UIO(i -= 1))

  /**
   * EXERCISE 2
   *
   * Rewrite the following procedural program to ZIO, using `IO.fail` and the
   * `ensuring` method of the `IO` object.
   */
  def tryCatch1(): Unit =
    try throw new Exception("Uh oh")
    finally println("On the way out...")

  val tryCatch2: ZIO[Console, Exception, Nothing] =
    IO.fail(new Exception("oh uh!")).ensuring(putStrLn("on the way out"))

  /**
   * EXERCISE 3
   *
   * Rewrite the `readFile1` function to use `bracket` so resources can be
   * safely cleaned up in the event of errors, defects, or interruption.
   */
  def readFile1(file: File): IO[Exception, List[Byte]] = {
    def readAll(is: InputStream, acc: List[Byte]): IO[Exception, List[Byte]] =
      is.read.flatMap {
        case None       => IO.succeed(acc.reverse)
        case Some(byte) => readAll(is, byte :: acc)
      }

    for {
      stream <- InputStream.openFile(file)
      bytes  <- readAll(stream, Nil)
      _      <- stream.close
    } yield bytes
  }

  def readAll(is: InputStream, acc: List[Byte]): IO[Exception, List[Byte]] =
    is.read.flatMap {
      case None       => IO.succeed(acc.reverse)
      case Some(byte) => readAll(is, byte :: acc)
    }

  def readFile2(file: File): IO[Exception, List[Byte]] =
    InputStream.openFile(file).bracket(_.close.orDie)(readAll(_, Nil))

  /**
   * EXERCISE 4
   *
   * Implement the `tryCatchFinally` method using `bracket` or `ensuring`.
   */
  def tryCatchFinally[E, A](try0: IO[E, A])(catch0: PartialFunction[E, IO[E, A]])(finally0: UIO[Unit]): IO[E, A] = ???

  /**
   * EXERCISE 5
   *
   * Use the `bracket` method to rewrite the following snippet to ZIO.
   */
  def readFileTCF1(file: File): List[Byte] = {
    var fis: FileInputStream = null

    try {
      fis = new FileInputStream(file)
      val array = Array.ofDim[Byte](file.length.toInt)
      fis.read(array)
      array.toList
    } catch {
      case e: java.io.IOException => Nil
    } finally if (fis != null) fis.close()
  }
  def readFileTCF2(file: File): Task[List[Byte]] = ???

  /**
   *`Managed[E, A]` is a managed resource of type `A`, which may be used by
   * invoking the `use` method of the resource. The resource will be automatically
   * acquired before the resource is used, and automatically released after the
   * resource is used.
   */
  /**
   * EXERCISE 6
   *
   * Using the `Managed.make` constructor, create a `Managed` resource
   * for a `FileInputStream`.
   */
  def managedFile(file: File): Managed[Throwable, FileInputStream] =
    ???

  /**
   * EXERCISE 7
   *
   * Use the `Managed#use` method to consume the `FileInputStream`, reading
   * all content into a byte array (see Exercise 5).
   */
  def readFileTCF3(file: File): Task[List[Byte]] =
    managedFile(file).use { inputStream =>
      ???
    }
}

object zio_environment {
  import zio.console.Console
  import zio.console
  import zio.clock.Clock
  import zio.clock
  import zio.random.Random
  import zio.random

  /**
   * The Default Modules in ZIO:
   *
   * Console   (putStrLn, getStrLn)
   * Clock     (currentTime, sleep, nanoTime)
   * Random    (nextInt, nextBoolean, ...)
   * System    (env)
   * Blocking  (blocking, effectBlocking)
   * Scheduler (scheduledExecutor)
   */
  /**
   * EXERCISE 1
   *
   * Write the type of a program that requires `scalaz.zio.clock.Clock` and which
   * could fail with `E` or succeed with `A`.
   */
  type ClockIO[E, A] = ZIO[Clock, E, A]

  /**
   * EXERCISE 2
   *
   * Write the type of a program that requires `scalaz.zio.console.Console` and
   * which could fail with `E` or succeed with A`:
   */
  type ConsoleIO[E, A] = ???

  /**
   * EXERCISE 3
   *
   * Write the type of a program that requires `scalaz.zio.system.System` and
   * which could fail with E or succeed with A:
   */
  type SystemIO[E, A] = ???

  /**
   * EXERCISE 4
   *
   * Write the type of a program that requires `scalaz.zio.random.Random` and
   * which could fail with `E` or succeed with `A`:
   */
  type RandomIO[E, A] = ???

  /**
   * EXERCISE 5
   *
   * Write the type of a program that requires `Clock` and `System` and which
   * could fail with `E` or succeed with `A`:
   */
  type ClockWithSystemIO[E, A] = ZIO[Clock with System, E, A]

  /**
   * EXERCISE 6
   *
   * Write the type of a program that requires `Console` and `System` and
   * which could fail with `E` or succeed with `A`:
   */
  type ConsoleWithSystemIO[E, A] = ZIO[Console with System, E, A]

  /**
   * EXERCISE 7
   *
   * Write the type of a program that requires `Clock`, `System` and `Random`
   * and which could fail with `E` or succeed with `A`:
   */
  type ClockWithSystemWithRandom[E, A] = ZIO[Console with zio.console.Console with zio.random.Random, E, A]

  /**
   * EXERCISE 8
   *
   * Write the type of a program that requires `Clock`, `Console`, `System` and
   * `Random` and which could fail with `E` or succeed with `A`:
   */
  type ClockWithConsoleWithSystemWithRandom[E, A] = ???

  /**
   * EXERCISE 9
   *
   * Using `zio.console.putStrLn`, write a hello world program, and identify
   * the correct ZIO type to use.
   */
  val helloWorld: ZIO[???, ???, ???] = ???

  /**
   * EXERCISE 10
   *
   * Using `zio.console.getStrLn` and `zio.console.putStrLn`, create an
   * interactive program and identify the correct ZIO type to use.
   */
  def interactiveProgram: ZIO[???, ???, ???] = ???

  /**
   * EXERCISE 11
   *
   * In a for comprehension, call various methods in zio.clock._, zio.console._,
   * and zio.random._, and identify the composite return type.
   */
  val program: ZIO[Clock with Random with Console, Nothing, Long] = for {
    _    <- zio.console.putStrLn("hello my friend")
    _    <- zio.random.nextInt(12)
    time <- zio.clock.nanoTime
  } yield time

  /**
   * Build a new Service called `Configuration`
   * - define the module
   * - define the interface
   * - define the helper functions (host, port)
   * - implement a trait `Live` that extends the module.
   * - implement all helper functions.
   */
  /**
   * EXERCISE 12
   *
   * Build a `Config` module that has a reference to a `Config.Service` trait.
   */
  trait Config {
    val config: Config.Service
  }

  object Config {
    // Service: definition of the methods provided by module:
    trait Service {
      val port: Int
      val host: String
    }

    /**
     * EXERCISE 13
     *
     * Implement a production version of the `Config` module.
     */
    trait Live extends Config {
      val config: Config.Service = new Config.Service {
        override val port: Int    = 1991
        override val host: String = "localhost"
      }
    }
    object Live extends Live
  }

  /**
   * EXERCISE 14
   *
   * Using `ZIO.accessM`, implement helpers, which access the `Config` module
   * and delegate to the functions inside the `Config` service.
   */
  object helpers {
    val port: ZIO[Config, Nothing, Int]    = ZIO.accessM(m => UIO.apply(m.config.port))
    val host: ZIO[Config, Nothing, String] = ZIO.accessM(m => UIO.apply(m.config.host))
  }

  /**
   * EXERCISE 15
   *
   * Write a program that depends on `Config` and `Console` and use the Scala
   * compiler to infer the correct type.
   */
  val configProgram: ZIO[Config with Console, Nothing, Unit] = for {
    port <- helpers.port
    _    <- putStrLn(s"the port is $port")
  } yield ()

  /**
   * EXERCISE 16
   *
   * Give the `configProgram` its dependencies by supplying it with both `Config`
   * and `Console` modules, and determine the type of the resulting effect.
   */
  val provided: IO[Nothing, Unit] = configProgram.provide(new Config.Live with Console.Live{})

  /**
   * EXERCISE 17
   *
   * Create a `Runtime[Config with Console]` that can be used to run any
   * effect that has a dependency on `Config`:
   */
  val ConfigRuntime: Runtime[Config with Console] =
    Runtime(??? : Config with Console, PlatformLive.Default)

  /**
   * EXERCISE 18
   *
   * Define a ZIO value that describes an effect which uses Config with
   * Console that displays the port and host in the Console and fails
   * with a String if the host name contains `:`
   */
  val simpleConfigProgram: ZIO[Config, String, Unit] = ???

  /**
   * EXERCISE 19
   *
   * Run the `simpleConfigProgram` using `ConfigRuntime.unsafeRun`.
   */
  val run: ??? = simpleConfigProgram ?

  /**
   * Build a file system service
   */
  /**
   * EXERCISE 20
   *
   * Build a module for a `FileSystem`.
   */
  trait FileSystem {
    val filesystem: FileSystem.Service[Any]
  }

  object FileSystem {

    /**
     * EXERCISE 21
     *
     * Create a service defining the capabilities of a `FileSystem`.
     */
    trait Service[R] {}

    /**
     * EXERCISE 22
     *
     * Create a production implementation of the `FileSystem` module.
     */
    trait Live extends FileSystem with Console {
      val filesystem: ??? = ???
    }
    object Live extends Live with Console.Live
  }

  /**
   * EXERCISE 23
   *
   * Using `ZIO.accessM`, create helpers.
   */
  object fs extends FileSystem.Service[FileSystem] {}

  /**
   * EXERCISE 24
   *
   * Write a simple program that uses `FileSystem with Console`.
   */
  val fileProgram: ZIO[FileSystem with Console, ???, ???] =
    ???

  /**
   * EXERCISE 25
   *
   * Create a `Runtime` that can execute effects that require
   * `FileSystem with Console`.
   */
  val FSRuntime: Runtime[FileSystem with Console] =
    ???

  /**
   * EXERCISE 26
   *
   * Execute `fileProgram` using `FSRuntime.unsafeRun`.
   */
  lazy val fileProgramLive: ??? = FSRuntime.unsafeRun(fileProgram)

  /**
   * EXERCISE 27
   *
   * Implement a mock file system module.
   */
  trait MockFileSystem extends FileSystem {
    val filesystem = ???
  }

  /**
   * EXERCISE 28
   *
   * Using `ZIO#provide` with the mock file system module, and a default
   * runtime, execute `fileProgram`.
   */
  lazy val fileProgramTest: ??? = new DefaultRuntime {}.unsafeRun {
    fileProgram.provide(???)
  }
}

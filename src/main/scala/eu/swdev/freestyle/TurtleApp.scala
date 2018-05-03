package eu.swdev.freestyle

import cats.data.StateT
import freestyle.free._
import freestyle.free.implicits._
import freestyle.free.effects.state
import cats.implicits._
import cats.effect.IO
import cats.free.{Free, FreeApplicative}

import scala.io.StdIn

object TurtleApp extends App {

  @free
  trait Turtle {
    def move(distance: Int): FS[Unit]
    def turn(degrees: Int): FS[Unit]
  }

  @free
  trait Terminal {
    def ask(prompt: String): FS[String]
    def tell(line: String): FS[Unit]
  }

  @free
  trait Log {
    def info(msg: String): FS[Unit]
  }

  @free
  trait Draw {
    def line(from: Position, to: Position): FS[Unit]
  }

  case class Position(x: Int, y: Int) {
    def move(rad: Double, distance: Int): Position =
      Position((x + Math.cos(rad) * distance).toInt, (y + Math.sin(rad) * distance).toInt)
  }

  case class TurtleState(
      directionInDegrees: Int,
      position: Position
  ) {
    def directionInRad: Double = Math.PI * directionInDegrees / 180;

    def move(distance: Int): TurtleState = copy(position = position.move(directionInRad, distance))

    def turn(degrees: Int): TurtleState = copy(directionInDegrees = directionInDegrees + degrees)
  }

  @module
  trait HighLevelInstructions {
    val turtle: Turtle
    val terminal: Terminal
  }

  val st = state[TurtleState]

  @module
  trait LowLevelInstructions {
    val log: Log
    val stat: st.StateM
    val draw: Draw
    val terminal: Terminal
  }

  // a program that recursively reads commands from the terminal and translates them into corresponding turtle operations
  def program[F[_]](implicit instructionSet: HighLevelInstructions[F]): FreeS[F, Unit] = {

    val cmdQuit = """\s*q(?:uit)?\s*""".r
    val cmdMove = """\s*m(?:ove)?\s+(\d+)""".r
    val cmdTurn = """\s*t(?:urn)?\s+(\d+)""".r

    import instructionSet.terminal._
    import instructionSet.turtle._

    lazy val repl: FreeS[F, Unit] = for {

      line <- ask("enter command (m[ove] <dist>|t[urn] <dist>|q[uit])")
      res <- line match {
        case cmdQuit() =>
          FreeS.pure[F, Unit](())
        case cmdMove(distance) =>
          for {
            _ <- move(distance.toInt)
            res <- repl
          } yield res
        case cmdTurn(degrees) =>
          for {
            _ <- turn(degrees.toInt)
            res <- repl
          } yield res
        case _ =>
          for {
            _ <- tell(s"unknown command: $line")
            res <- repl
          } yield res
      }

    } yield res

    for {
      _ <- tell("Welcome to the turtle app")
      res <- repl
    } yield res
  }

  // translates turtle operations into sequences of "LowLevelInstructions"
  implicit def turtle2intermediary[F[_]](implicit instructionSet: LowLevelInstructions[F]): Turtle.Handler[FreeS[F, ?]] = new Turtle.Handler[FreeS[F, ?]] {

    import instructionSet.draw._
    import instructionSet.log._
    import instructionSet.stat._

    override protected[this] def move(distance: Int): FreeS[F, Unit] = for {
      s <- get
      _ <- info(s"move - current position: ${s.position}; distance: $distance")
      _ <- set(s.move(distance))
      _ <- line(s.position, s.position.move(s.directionInRad, distance))
    } yield ()

    override protected[this] def turn(degrees: Int): FreeS[F, Unit] = for {
      s <- get
      _ <- info(s"turn - current direction: ${s.directionInDegrees}; degrees: $degrees")
      _ <- set(s.turn(degrees))
    } yield ()
  }

  // Reencode terminal operation at different levels.
  //
  // The terminal algebra is used in HighLevelInstructions and in LowLevelInstructions.
  // When HighLevelInstructions are translated into LowLevelInstructions then terminal operations must also be
  // translated.
  //
  // TODO: Can this be avoided? Using the same algebra at different levels seems to be a common use case.
  implicit def terminal2terminalHandler[F[_]](implicit ev: Terminal[F]): Terminal.Handler[FreeS[F, ?]] = new Terminal.Handler[FreeS[F, ?]] {
    override protected[this] def ask(prompt: String): FreeS[F, String] = ev.ask(prompt)

    override protected[this] def tell(line: String): FreeS[F, Unit] = ev.tell(line)
  }

  type StateIo[A] = StateT[IO, TurtleState, A]

  implicit val terminal2StateIoHandler = new Terminal.Handler[StateIo] {
    override def tell(s: String): StateIo[Unit] = StateT.liftF(IO { println(s) })

    override def ask(s: String): StateIo[String] = StateT.liftF(IO { println(s); StdIn.readLine() })
  }

  implicit val log2StateIoHandler: Log.Handler[StateIo] = new Log.Handler[StateIo] {
    override protected[this] def info(msg: String): StateIo[Unit] = StateT.liftF(IO(println(s"log.info - msg: $msg")))
  }

  implicit val draw2StateIoHandler: Draw.Handler[StateIo] = new Draw.Handler[StateIo] {
    override protected[this] def line(from: Position, to: Position): StateIo[Unit] = StateT
        .liftF(IO(println(s"draw - from: $from; to: $to")))
  }

  // TODO: Can the built-in State support of Freestyle be used to handle the transformation from st.StateM into StateIo?
  implicit val state2StateIoHandler: st.StateM.Handler[StateIo] = new st.StateM.Handler[StateIo] {
    override protected[this] def get: StateIo[TurtleState] = StateT.get

    override protected[this] def set(s: TurtleState): StateIo[Unit] = StateT.set(s)

    override protected[this] def modify(f: TurtleState => TurtleState): StateIo[Unit] = StateT.modify(f)

    override protected[this] def inspect[A](f: TurtleState => A): StateIo[A] = StateT.inspect(f)

    override type PP$110
  }

  program[HighLevelInstructions.Op]
      // translate the turtle program into an intermediary representation
      .interpret[FreeS[LowLevelInstructions.Op, ?]]
      // interpret the program in terms of StateIo
      .interpret[StateIo]
      // provide the initial state
      .run(TurtleState(0, Position(0, 0)))
      // run IO
      .unsafeRunSync()

}

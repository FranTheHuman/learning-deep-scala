package katas

import cats.effect.{ExitCode, IO, IOApp}
import code_source.telemetry.TelemetryClient

import scala.language.postfixOps

/**
 * The Situation
 *
 * You just started your new job as a developer at the Ferrari Formula1 team.
 * But you’re not really convinced by the legacy code-base.
 * Now you want to write unit tests for them, and that is harder than it needs to be.
 * The code snippet fails to follow one or more of the SOLID principles.
 *
 * For this exercise, you should identify which SOLID principles are violated.
 * There is only one class you are interested in writing tests for right now.
 * As a first step, try to get some kind of test in place before you change the class at all.
 * If the tests are hard to write, is that because of the problems with SOLID principles?
 *
 * When you have some kind of test to lean on, refactor the code and make it testable.
 * Take care when refactoring not to alter the functionality, or change interfaces which other
 * client code may rely on. (Imagine there is client code in another repository that you can’t see right now).
 * Add more tests to cover the functionality of the particular class you’ve been asked to get under test.
 *
 * Apply the unit testing style and framework you are most comfortable with.
 * You can choose to use stubs or mocks or none at all.
 * If you do, you are free to use the mocking tool that you prefer.
 */

/**
 * Your Task
 *
 * Write the unit tests for the TelemetryDiagnosticControls class.
 * The responsibility of the TelemetryDiagnosticControls class is to establish a connection
 * to the telemetry server (through the TelemetryClient),
 * send a diagnostic request and successfully receive the response that contains the diagnostic info.
 * The TelemetryClient class provided for the exercise fakes the behavior of the real TelemetryClient class,
 * and can respond with either the diagnostic information or a random sequence.
 * The real TelemetryClient class would connect and communicate with the telemetry server via tcp/ip.
 */

object TelemetryApp extends IOApp.Simple {

  // 1 - MODELO DE DATOS
  type TelemetryResult[A] = IO[Either[Throwable, A]]

  case class TelemetryError(msg: String)
  case class Configuration(strConn: String, message: String)

  // 2- CONTATO DE COMPORTAMIENTO

  sealed trait DiagnosticChecker[F[_], A, B, C] {

    def restart(fd: A => A, fc: B => A): F[A]

    def restartUntil(n: Int, fs: A => C)(fd: A => A)(fc: B => A): F[A]

    def send(send: B => A): F[A]

    def receive(receive: A => B): F[B]

  }

  // 3- IMPLEMENTACION DE COMPORTAMIENTO
  case class TelemetryDiagnosticChecker(configuration: Configuration)
    extends DiagnosticChecker[TelemetryResult, Unit, String, Boolean] {

    override def restart(disconnect: Unit => Unit, connect: String => Unit): TelemetryResult[Unit] =
      (IO(disconnect) *> IO(connect(configuration.strConn))).attempt

    override def send(send: String => Unit): TelemetryResult[Unit] =
      IO { send(configuration.message) }.attempt

    override def receive(receive: Unit => String): TelemetryResult[String] =
      IO { receive() }.attempt

    override def restartUntil(
      retries: Int,
      getOnlineStatus: Unit => Boolean
    )(disconnect: Unit => Unit)(connect: String => Unit): TelemetryResult[Unit] = {

      def restart: TelemetryResult[Unit] = (IO(disconnect) *> IO(connect(configuration.strConn))).attempt

      def getStatus: Boolean = getOnlineStatus()

      def until(n: Int): TelemetryResult[Unit] = {
        if (n <= 0) IO(Left(new Exception("Unable to connect.")))
        else restart flatMap {
          case Left(_) => until(n - 1)
          case Right(value) => if (getStatus) IO(Right(value)) else until(n - 1)
        }
      }

      until(retries)
    }

  }

  // 4- DEFINICION EJECUCION
  def checkTransmission(configuration: Configuration, client: TelemetryClient): IO[String] = {

    def check(telemetry: TelemetryDiagnosticChecker, tl: TelemetryClient): IO[String] = for {
      done <- telemetry.restartUntil(3, _ => tl.getOnlineStatus)(_ => tl.disconnect)(tl.connect)
      _ <- done.fold(e => IO.raiseError(e).attempt, _ => telemetry.send(tl.send))
      response <- telemetry.receive(_ => tl.receive)
    } yield response match {
      case Right(response) => response
      case Left(e) => TelemetryError(e.getMessage).msg // TODO: HANDLER ERRORS
    }

    check(TelemetryDiagnosticChecker(configuration), client)
  }

  // 5- Ejecucion
  override def run: IO[Unit] =
    checkTransmission(Configuration("*111#", "AT#UD"), new TelemetryClient)
      .map(result => println(result))
      .as(ExitCode.Success)

}
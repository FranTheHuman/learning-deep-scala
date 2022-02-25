package real_world_app.okcfp

import scalaz._
import scalaz.Scalaz._

object Main {
  def main(args: Array[String]): Unit = {
    val emitRecords: () => Seq[RawUser] = Source.emit(Config.current)
    val result: Result = go(emitRecords)
    println(result.toString) // side-effect print to see results
  }

  private def go(emit: () => Seq[RawUser]): Result = {
    emit()
      .map(transform)
      .map(toResult)
      .foldLeft(Result.zero)(_ +: _)
  }

  private def toResult(v: \/[TransformError, DomainUser]): Result = {
    v match {
      case \/-(_) => Result(1, 0)
      case -\/(e) => {
        println(s"Transform Error: ${e.error}")
        Result(0, 1)
      }
    }
  }

  private def transform(r: RawUser): TransformError \/ DomainUser = {
//    for {
//      person <- r.person
//      phone <- PhoneNumber.from(r.phone)
//    } yield DomainUser(person, phone)
    (r.person |@| PhoneNumber.from(r.phone))(DomainUser)
  }
}

object Source {
  def emit(conf: Config)(): Seq[RawUser] = {
    // do something cool with config
    RawData.generateRawUsers
  }
}

trait Config
case class FileConfig() extends Config

object Config {
  def current: Config = FileConfig()
}

case class Result(successes: Int, failures: Int) {
  def +:(that: Result) =
    Result(
      this.successes + that.successes,
      this.failures + that.failures)
}

object Result {
  def zero = Result(0, 0)
}

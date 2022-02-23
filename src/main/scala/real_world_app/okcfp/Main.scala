package real_world_app.okcfp

object Main {
  def main(args: Array[String]): Unit = ???
  private def go(emit: () => Seq[RawUser]): Result = ???
  private def toResult(v: DomainUser): Result = ???
  private def transform(r: RawUser): DomainUser = ???
}

object Source {
  def emit(conf: Config)(): Seq[RawUser] = ???
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

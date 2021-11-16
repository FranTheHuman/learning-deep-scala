package essential_scala

object Monads extends App {

  // 6.6.2.1 Adding All the Things ++

  /*
      Add together all the options to create a new option. Add together all the sequences to create a new sequence.
      Add together all the trys to create a new try. Use a for comprehension for each. It shouldn’t take you long!
   */
    import scala.util.Try

    val opt1 = Some(1)
    val opt2 = Some(2)
    val opt3 = Some(3)

    val seq1 = Seq(1)
    val seq2 = Seq(2)
    val seq3 = Seq(3)

    val try1 = Try(1)
    val try2 = Try(2)
    val try3 = Try(3)

    opt1 flatMap( a => opt2 flatMap( b => opt3 map ( c => a + b + c)))
    seq1 flatMap( a => seq2 flatMap( b => seq3 map ( c => a + b + c)))
    try1 flatMap( a => try2 flatMap( b => try3 map ( c => a + b + c)))

}

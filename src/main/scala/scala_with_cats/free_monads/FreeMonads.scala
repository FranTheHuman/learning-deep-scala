package scala_with_cats.free_monads

object FreeMonads extends App {

    // ------------- What is it?

    // -> construction which allows you to build a monad from any Functor.
    // -> Like other monads, it is a pure way to represent and manipulate computations.

    // Provide a practical way to:

      // * represent stateful computations as data, and run them
      // * run recursive computations in a stack-safe way
      // * build an embedded DSL (domain-specific language)
      // * retarget a computation to another interpreter using natural transformations

  /*

  https://medium.com/barely-functional/freer-doesnt-come-for-free-c9fade793501

  In short, freer monads let you separate the high-level “what am I trying to do” from the low-level “how to actually do it.”

  This is true, but this is a valid observation for any system where you use a “interface/implementation” dichotomy.
  You get it with regular traits/case classes in Scala or MTL typeclasses / monad transformer instances in Haskell.
  The details vary mostly in usability.

  the resurgence of the so called “final tagless style” is not really much more than that: interfaces and implementations,
  with the twist that they are parameterized by a type parameter F[_] for added abstraction.

   */

    // ------------- Case study

    /*

    Let’s imagine that we want to create a DSL for a key-value store. We want to be able to do three things with keys:

      put a value into the store, associated with its key.
      get a value from the store given its key.
      delete a value from the store given its key.

    The idea is to write a sequence of these operations in the embedded DSL as a “program”, compile the “program”,
    and finally execute the “program” to interact with the actual key-value store.

      put("toto", 3)
      get("toto") // returns 3
      delete("toto")

    But we want:

      the computation to be represented as a pure, immutable value
      to separate the creation and execution of the program
      to be able to support many different methods of execution

    A. Create an ADT representing your grammar

        ADT stands for Algebraic Data Type. In this context,
        it refers to a closed set of types which can be combined to build up complex, recursive values.

        We need to create an ADT to represent our key-value operations:

     */

        sealed trait KVStoreA[A]
        case class Put[T](key: String, value: T) extends KVStoreA[Unit]
        case class Get[T](key: String) extends KVStoreA[Option[T]]
        case class Delete(key: String) extends KVStoreA[Unit]

    /*

      B. Free your ADT

        - There are five basic steps to “freeing” the ADT:

          1. Create a type based on Free[_] and KVStoreA[_].
          2. Create smart constructors for KVStore[_] using liftF.
          3. Build a program out of key-value DSL operations.
          4. Build a compiler for programs of DSL operations.
          5. Execute our compiled program.

          --- 1. Create a Free type based on your ADT ---

     */

        import cats.free.Free
        type KVStore[A] = Free[KVStoreA, A]

      // --- 2. Create smart constructors using liftF ---

    /*
          These methods will make working with our DSL a lot nicer,
          and will lift KVStoreA[_] values into our KVStore[_] monad (note the missing “A” in the second type).
     */

        import cats.free.Free.liftF

        // Put returns nothing (i.e. Unit).
        def put[T](key: String, value: T): KVStore[Unit] =
          liftF[KVStoreA, Unit](Put[T](key, value))

        // Get returns a T value.
        def get[T](key: String): KVStore[Option[T]] =
          liftF[KVStoreA, Option[T]](Get[T](key))

        // Delete returns nothing (i.e. Unit).
        def delete(key: String): KVStore[Unit] =
          liftF(Delete(key))


        // Update composes get and set, and returns nothing.
        def update[T](key: String, f: T => T): KVStore[Unit] =
          for {
            vMaybe <- get[T](key)
            _ <- vMaybe.map(v => put[T](key, f(v))).getOrElse(Free.pure(()))
          } yield ()
//          get[T](key) flatMap { someKv =>
//            someKv.map { v =>
//              put[T](key, f(v))
//            }.getOrElse(Free.pure(()))
//          }

        // --- 3. Build a program ---

        /*
            Now that we can construct KVStore[_] values we can use our DSL to write “programs” using a for-comprehension:
         */

          def program: KVStore[Option[Int]] =
            for {
              _ <- put("wild-cats", 2)
              _ <- update[Int]("wild-cats", (_ + 12))
              _ <- put("tame-cats", 5)
              n <- get[Int]("wild-cats")
              _ <- delete("tame-cats")
            } yield n

          /*
              This looks like a monadic flow. However, it just builds a recursive data structure representing the sequence
              of operations.
           */

        // --- 4. Write a compiler for your program ---

        /*

            As you may have understood now, Free[_] is used to create an embedded DSL.
            By itself, this DSL only represents a sequence of operations (defined by a recursive data structure);
            it doesn’t produce anything.

            Free[_] is a programming language inside your programming language!

            So, like any other programming language, we need to compile our abstract language into an effective
            language and then run it.

            To do this, we will use a natural transformation between type containers.
            Natural transformations go between types like F[_] and G[_]
            (this particular transformation would be written as FunctionK[F,G]
            or as done here using the symbolic alternative as F ~> G).

            In our case, we will use a simple mutable map to represent our key value store:

         */

          import cats.arrow.FunctionK
          import cats.{Id, ~>}
          import scala.collection.mutable

          // the program will crash if a key is not found,
          // or if a type is incorrectly specified.
          def impureCompiler: KVStoreA ~> Id  =
            new (KVStoreA ~> Id) {

              // a very simple (and imprecise) key-value store
              val kvs = mutable.Map.empty[String, Any]

              def apply[A](fa: KVStoreA[A]): Id[A] =
                fa match {
                  case Put(key, value) =>
                    println(s"put($key, $value)")
                    kvs(key) = value
                    Id(())
                  case Get(key) =>
                    println(s"get($key)")
                    kvs.get(key).map(_.asInstanceOf[A]).get
                  case Delete(key) =>
                    println(s"delete($key)")
                    kvs.remove(key)
                    Id(())
                }
            }


          // --- 5. Run your program ---

          /*

              The final step is naturally running your program after compiling it.

              Free[_] is just a recursive structure that can be seen as sequence of operations producing other operations.
              In this way it is similar to List[_]. We often use folds (e.g. foldRight) to obtain a single value from a list;
              this recurses over the structure, combining its contents.

              The idea behind running a Free[_] is exactly the same. We fold the recursive structure by:

                consuming each operation.
                compiling the operation into our effective language using impureCompiler (applying its effects if any).
                computing next operation.
                continue recursively until reaching a Pure state, and returning it.

              This operation is called Free.foldMap:


              M must be a Monad to be flattenable (the famous monoid aspect under Monad). As Id is a Monad,
              we can use foldMap.

              To run your Free with previous impureCompiler:
           */

          val result: Option[Int] = program.foldMap(impureCompiler)

        // --- 6. Use a pure compiler (optional) ---

        /*

          The previous examples used an effectful natural transformation.
          This works, but you might prefer folding your Free in a “purer” way.
          The State data structure can be used to keep track of the program state in an immutable map,
          avoiding mutation altogether.

         */

        object Compiler {
          import cats.data.State

          type KVStoreState[A] = State[Map[String, Any], A]
          val pureCompiler: KVStoreA ~> KVStoreState = new (KVStoreA ~> KVStoreState) {
            override def apply[A](fa: KVStoreA[A]): KVStoreState[A] =
              fa match {
                case Put(key, value) =>
                  // State.modify(_.(key, value))
                  ???
                case Get(key) =>
                  State.inspect(_.get(key).map(_.asInstanceOf[A]))
                case Delete(key) =>
                  // State.modify(_ - key)
                  ???
              }
          }
        }

      // it is just a clever construction that allows us to build a very simple Monad from any functor.
      // we deduce that the free monad is basically a construction that:
        /*
            takes a functor
            adds the pointed part (e.g. pure)
            adds the monadic behavior (e.g. flatMap)
         */

}

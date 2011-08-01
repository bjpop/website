import scala.collection.mutable.Map

class Fib() {
   def fib(n:BigInt):BigInt =
      if (n <= 1) 1 else this.fib(n-1) + this.fib(n-2)
}

class FibMemo () extends Fib() {
   val memo:Map[BigInt,BigInt] = Map()
   override def fib(n:BigInt):BigInt = {
      if (memo.contains(n))
          memo(n)
      else {
         val result = super.fib(n)
         memo(n) = result
         result
      }
   }
}

object Main {
   def main(args: Array[String]) {
      val fibber = new FibMemo()
      // val fibber = new Fib()
      (0 to 100) map (x => println(fibber.fib(x)))
   }
}

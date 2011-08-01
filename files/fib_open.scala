import scala.collection.mutable.Map

object Main {

   def fix[T](f:(T=>T)=>(T=>T)):T=>T =
      f((x:T) => fix(f)(x))

   def fibOpen(f:BigInt=>BigInt)(n:BigInt):BigInt =
      if (n <= 1) 1 else f(n-1) + f(n-2)

   val memo:Map[BigInt,BigInt] = Map()

   def fibMemo(f:BigInt=>BigInt)(n:BigInt):BigInt = {
      if (memo.contains(n))
          memo(n)
      else {
         val result = f(n)
         memo(n) = result
         result
      }
   }

   val fibSlow:BigInt=>BigInt = fix(fibOpen)
   val fibFast:BigInt=>BigInt = fix(fibMemo _ compose fibOpen)

   def main(args: Array[String]) =
      (0 to 100) map (x => println(fibFast(x)))
}

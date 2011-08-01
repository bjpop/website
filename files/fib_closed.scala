object Main {

   def fib(n:BigInt):BigInt =
      if (n <= 1) 1 else fib(n-1) + fib(n-2)

   def main(args: Array[String]) =
      (0 to 100) map (x => println(fib(x)))
}

package page_1

object p10 {

	val primes: Stream[Int] = 2 #:: Stream.from(3).filter(i =>
		primes.takeWhile(p => p * p <= i).forall(p => i % p != 0))

	def f(limit: Int): Long = {
		primes.takeWhile(_ < limit).foldLeft(0L) { case (sum, p) => sum + p }
	}

	def main(args: Array[String]): Unit = {
		println(f(10))
		println(f(2000000))
	}

}

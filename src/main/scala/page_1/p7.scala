package page_1

object p7 {

	// ref: http://stackoverflow.com/q/15594227
	val primes: Stream[Int] = 2 #:: Stream.from(3, 2).filter(i =>
		primes.takeWhile(p => p * p <= i).forall(p => i % p != 0))

	def main(args: Array[String]): Unit = {
		println(primes.apply(6 - 1))
		println(primes.apply(10001 - 1))
	}

}

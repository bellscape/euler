package page_31

object p35 {

	val ceils: Array[Int] = {
		def tail(last: Int): Stream[Int] = last #:: tail(last * 10)
		tail(1).take(7).toArray
	}

	def is_circular(prime: Int): Boolean = {
		import p34.width
		val w = width(prime)
		check_circular(prime, ceils(w), w - 1)
	}
	private def check_circular(n: Int, ceil: Int, left: Int): Boolean = {
		if (left <= 0) {
			true
		} else {
			import page_21.p27.is_prime
			val shifted = n * 10
			val digit = shifted / ceil
			val next = shifted % ceil + digit
			is_prime(next) && check_circular(next, ceil, left - 1)
		}
	}

	def main(args: Array[String]): Unit = {
		import page_21.p27.primes

		println(primes.takeWhile(_ < 100)
			.filter(is_circular)
			.toIndexedSeq)

		println(primes.takeWhile(_ < 1000000)
			.count(is_circular))

	}

}

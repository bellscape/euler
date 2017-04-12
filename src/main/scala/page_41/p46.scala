package page_41

object p46 {

	import page_21.p27.is_prime
	val odd_composite_number: Stream[Int] = Stream.from(9, 2).filterNot(is_prime)

	def fit_rule(n: Int): Boolean = {
		Stream.from(1).map(i => n - 2 * i * i)
			.takeWhile(_ > 0)
			.exists(is_prime)
	}

	def main(args: Array[String]): Unit = {
		println(fit_rule(9))
		println(fit_rule(33))

		val counter_example = odd_composite_number.find(n => !fit_rule(n)).get
		println(counter_example)
	}

}

package page_1

object p1 {

	def f(limit: Int): Int = {
		Stream.from(1).takeWhile(_ < limit)
			.filter(x => x % 3 == 0 || x % 5 == 0)
			.sum
	}

	def main(args: Array[String]): Unit = {
		println(f(10))
		println(f(1000))
	}

}

/*
better:
	def SumDivisibleBy(n) = {
		val p = (limit/n).toInt
		(1 + p) * p / 2 * n
	}
	SumDivisibleBy(3) + SumDivisibleBy(5) - SumDivisibleBy(15)
*/

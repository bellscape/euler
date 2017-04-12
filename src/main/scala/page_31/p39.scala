package page_31

object p39 {

	// a^2+b^2=c^2, p=a+b+c
	// p/(1+√2) < c < p/2

	val _1_plus_sqrt_2: Double = 1 + Math.sqrt(2)
	def count_solutions(p: Int): Int = {
		val min_c = (p / _1_plus_sqrt_2).ceil.toInt
		val max_c = (p - 1) / 2
		(min_c to max_c).map { c =>
			count_solutions(p - c, c * c, c - 1)
		}.sum
	}
	def count_solutions(sum: Int, square_sum: Int, max_b: Int): Int = {
		val a = sum - max_b
		if (a > max_b) return 0

		val is_solution = a * a + max_b * max_b == square_sum
		(if (is_solution) 1 else 0) + count_solutions(sum, square_sum, max_b - 1)
	}

	def main(args: Array[String]): Unit = {
		println(count_solutions(120))

		println((1 to 1000).maxBy(count_solutions))
	}

}

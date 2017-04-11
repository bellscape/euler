package page_1

object p6 {

	def f(max: Int): Int = {
		val sum_of_squares = (1 to max).map(i => i * i).sum
		val sum = (1 to max).sum
		sum * sum - sum_of_squares
	}

	def main(args: Array[String]): Unit = {
		println(f(10))
		println(f(100))
	}

}

// 可用公式：
// sum = n(n+1)/2
// sum-of-squares = (2n+1)(n+1)n/6

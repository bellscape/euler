package page_1

object p12 {

	val triangle_numbers: Stream[Int] = {
		def tail(sum: Int, i: Int): Stream[Int] = sum #:: tail(sum + i, i + 1)
		tail(1, 2)
	}

	def count_divisors(n: Int): Int = {
		val max_divisor = Math.sqrt(n).toInt
		val is_square = max_divisor * max_divisor == n

		val small_divisor_count = (1 to max_divisor).count(n % _ == 0)
		small_divisor_count * 2 - (if (is_square) 1 else 0)
	}

	def main(args: Array[String]): Unit = {
		println(triangle_numbers.find(count_divisors(_) >= 5))
		println(triangle_numbers.find(count_divisors(_) >= 500))
	}

}

/*
better:
	N = p1^a1 * p2^a2 * ... * pn^an
	count_divisors(N) = (a1+1)*(a2+1)*...*(an+1)
 */

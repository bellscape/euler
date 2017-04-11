package page_1

object p3 {

	def find_factor(m: Long, min_factor: Long): Long = {
		if (m % min_factor == 0) return min_factor
		if (min_factor * min_factor > m) return 0
		val next = if (min_factor == 2) 3 else min_factor + 2
		find_factor(m, next)
	}

	def largest_factor(m: Long, min_factor: Long = 2): Long = {
		val factor = find_factor(m, min_factor)
		if (factor == 0) return m
		largest_factor(m / factor, factor)
	}

	def main(args: Array[String]): Unit = {
		println(largest_factor(13195))
		println(largest_factor(600851475143L))
	}

}

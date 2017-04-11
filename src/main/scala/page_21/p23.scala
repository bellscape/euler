package page_21

import scala.collection.mutable

object p23 {

	val cache: mutable.HashMap[Int, Boolean] = mutable.HashMap.empty
	def is_abundant(n: Int): Boolean = {
		cache.getOrElseUpdate(n, p21.list_divisors(n).sum > n)
	}

	val abundant_numbers: Stream[Int] = Stream.from(1).filter(is_abundant)

	def is_sum_of_two_abundant_nums(n: Int): Boolean = {
		abundant_numbers.takeWhile(_ <= n / 2)
			.exists(a => is_abundant(a) && is_abundant(n - a))
	}


	def main(args: Array[String]): Unit = {
		println(is_abundant(12))
		println(is_sum_of_two_abundant_nums(24))

		println((1 to 28123)
			.filterNot(is_sum_of_two_abundant_nums)
			.sum)
	}

}

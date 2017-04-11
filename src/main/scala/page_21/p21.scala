package page_21

import scala.collection.mutable

object p21 {

	def list_divisors(n: Int): Seq[Int] = {
		val max_divisor = Math.sqrt(n).toInt
		(1 to max_divisor).flatMap { i =>
			if (n % i == 0) {
				if (i == 1 || (i == max_divisor && n / i == max_divisor)) {
					Seq(i)
				} else {
					Seq(i, n / i)
				}
			} else None
		}
	}

	val cache: mutable.HashMap[Int, Int] = mutable.HashMap.empty
	def sum_divisors(n: Int): Int = {
		cache.getOrElseUpdate(n, list_divisors(n).sum)
	}

	def is_amicable(n: Int): Boolean = {
		val d = sum_divisors(n)
		d != n && sum_divisors(d) == n
	}

	def main(args: Array[String]): Unit = {
		println(list_divisors(220).sorted)
		println(sum_divisors(220))
		println(sum_divisors(284))
		println(is_amicable(220))
		println(is_amicable(221))

		val amicable_sum = (1 until 10000).filter(is_amicable).sum
		println(s"amicable_sum: $amicable_sum")
	}

}

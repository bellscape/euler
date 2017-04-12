package page_41

object p50 {
	import page_21.p27.{is_prime, primes}

	// 指定序列长&上限，找符合条件的序列
	// (sum, head)
	def list_sums(length: Int, sum_until: Int): Stream[(Int, Int)] = {
		def tail(sum: Int, heads: Stream[Int], nexts: Stream[Int]): Stream[(Int, Int)] = {
			if (sum < sum_until) {
				val next_sum = sum - heads.head + nexts.head
				(sum, heads.head) #:: tail(next_sum, heads.tail, nexts.tail)
			} else Stream.empty
		}

		var first_sum = 0
		var next_prime = primes
		for (_ <- 0 until length) {
			first_sum += next_prime.head
			next_prime = next_prime.tail
		}
		tail(first_sum, primes, next_prime)
	}
	def list_prime_sums(length: Int, sum_until: Int): Stream[(Int, Int)] = {
		list_sums(length, sum_until).filter { case (sum, _) => is_prime(sum) }
	}

	// 假设全使用最小的质数，看数量是多少
	def max_possible_length(sum_until: Int, current: Int = 0, left_primes: Stream[Int] = primes): Int = {
		if (sum_until <= 1) current
		else max_possible_length(sum_until - left_primes.head, current + 1, left_primes.tail)
	}

	def find_max_length(n: Int): (Int, Int) = {
		val max_length = max_possible_length(n)
		(1 to max_length).reverse
			.flatMap(length => list_prime_sums(length, n))
			.head
	}

	def main(args: Array[String]): Unit = {
		println(list_prime_sums(6, 100).toIndexedSeq)
		println(max_possible_length(100))

		for (n <- Seq(100, 1000, 1000000)) {
			val (sum, start) = find_max_length(n)
			println(s"max length under $n: $sum = $start + ...")
		}

	}

}

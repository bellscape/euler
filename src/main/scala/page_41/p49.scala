package page_41

object p49 {

	import page_21.p27.{is_prime, primes}

	def are_permutations(a: Int, b: Int, c: Int): Boolean = {
		val a_sorted = a.toString.sorted
		b.toString.sorted == a_sorted && c.toString.sorted == a_sorted
	}

	def is_ok(first_prime: Int, second_prime: Int): Boolean = {
		val third = second_prime * 2 - first_prime
		third < 10000 && is_prime(third) && are_permutations(first_prime, second_prime, third)
	}

	def list_all(): Stream[(Int, Int)] = {
		val choices = primes.dropWhile(_ < 1000).takeWhile(_ <= 9999).toArray
		(0 to choices.length - 3).toStream.flatMap { first_idx =>
			val first = choices(first_idx)
			(first_idx + 1 to choices.length - 2).toStream.map { second_idx =>
				(first, choices(second_idx))
			}
		}.filter { case (first, second) => is_ok(first, second) }
	}

	def main(args: Array[String]): Unit = {
		for ((first, second) <- list_all()) {
			val seq = Seq(first, second, second * 2 - first)
			println(seq)
			println(seq.mkString)
		}
	}

}

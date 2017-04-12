package page_31

object p37 {
	import page_21.p27.is_prime

	val right_truncatable: Stream[Int] = {
		def tail(last_pack: Seq[Int]): Stream[Int] = {
			val next_pack = last_pack.flatMap(prefix => Seq(1, 3, 7, 9).map(i => prefix * 10 + i))
				.filter(is_prime)
			if (next_pack.isEmpty) Stream.empty
			else next_pack.toStream ++ tail(next_pack)
		}
		tail(Seq(2, 3, 5, 7))
	}

	def is_left_truncatable(prime: Int): Boolean = {
		var ceil = 1
		while (ceil < prime)
			ceil *= 10
		is_left_truncatable(prime, ceil / 10)
	}
	// n=23, ceil=10
	private def is_left_truncatable(n: Int, ceil: Int): Boolean = {
		val next = n % ceil
		is_prime(next) && (ceil <= 10 || is_left_truncatable(next, ceil / 10))
	}

	//		tail(10, Seq(2, 3, 5, 7)).filter(_ > 10)

	def main(args: Array[String]): Unit = {
		val right_ok = right_truncatable.toIndexedSeq
		println(right_ok)
		val both_ok = right_ok.filter(is_left_truncatable)
		println(both_ok)
		println(s"sum: ${both_ok.sum}")
	}

}

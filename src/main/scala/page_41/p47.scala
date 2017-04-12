package page_41

object p47 {

	import page_21.p27.{is_prime, primes}
	def factors_ok(n: Int, factors: Int, min_factor: Stream[Int] = primes): Boolean = {
		if (factors == 0 && n == 1) true
		else if (factors == 1 && is_prime(n)) true
		else {
			val factor = min_factor.head
			if (n % factor == 0) { // found factor
				var next = n / factor
				while (next % factor == 0)
					next = next / factor
				factors_ok(next, factors - 1, min_factor.tail)
			} else if (factor * factor > n) {
				false
			} else {
				factors_ok(n, factors, min_factor.tail)
			}
		}
	}


	// 找连续n个时候，隔n个检查一个，检查通过后再从检查点向左、右检查
	def find(center: Int, size: Int, factors: Int): Option[Int] = {
		// center 必须符合
		if (!factors_ok(center, factors)) return None

		var left_passed = 0
		while (left_passed + 1 < size && factors_ok(center - 1 - left_passed, factors)) {
			left_passed += 1
		}
		var right_passed = 0
		while (left_passed + 1 + right_passed < size && factors_ok(center + 1 + right_passed, factors)) {
			right_passed += 1
		}

		if (left_passed + 1 + right_passed >= size) Some(center - left_passed)
		else None
	}


	def main(args: Array[String]): Unit = {
		println(factors_ok(14, 2))
		println(factors_ok(644, 3))
		println(factors_ok(645, 3))
		println(factors_ok(646, 3))
		println(factors_ok(644, 4)) // false

		println(find(15, 2, 2))
		println(find(15, 2, 3)) // None
		println(find(644, 3, 3))

		val head = Stream.from(4, 4).flatMap(center => find(center, 4, 4)).head
		println(s"head: $head")
	}

}

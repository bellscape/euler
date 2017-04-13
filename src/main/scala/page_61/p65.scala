package page_61

object p65 {

	type Fraction = (BigInt, BigInt)
	def reciprocal_and_add(f: Fraction, i: Int): Fraction = {
		val (d, n) = f // 倒置
		val n2 = n + i * d
		val shared = n2.gcd(d)
		(n2 / shared, d / shared)
	}

	def get_summand(idx: Int): Int = {
		// 0->2, 1->1, 2->2, 3->1, 4->1, 5->4, 6->1
		if (idx % 3 == 2) {
			(idx + 1) * 2 / 3
		} else if (idx == 0) {
			2
		} else 1
	}

	def calc_e(idx: Int, last: Fraction = (BigInt(1), BigInt(0))): Fraction = {
		if (idx < 0) last
		else calc_e(idx - 1, reciprocal_and_add(last, get_summand(idx)))
	}

	def main(args: Array[String]): Unit = {
		println((0 to 10).map(get_summand).mkString)
		println((0 to 10).map(max_idx => calc_e(max_idx)))

		val n_10 = calc_e(10 - 1)._1
		println(s"numerator 10th: $n_10   sum of digits: ${n_10.toString.map(_ - '0').sum}")

		val n_100 = calc_e(100 - 1)._1
		println(s"numerator 100th: $n_100   sum of digits: ${n_100.toString.map(_ - '0').sum}")
	}

}

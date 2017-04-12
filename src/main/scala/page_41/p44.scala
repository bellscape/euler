package page_41

object p44 {

	// P(x) = x(3x-1)/2
	// P(a),P(b) < P(c) < P(d), P(c)-P(b)=P(a), P(c)+P(b)=P(d), 求P(a).min

	// 先假定已知P(a), 从小到大找P(b)
	// 定义Gap(x) = P(x+1)-P(x) = 3x+1
	// 因为P(c)=P(a)+P(b)需要存在，所以有P(b)>=Gap(a), P(a)>=Gap(b)

	def pentagonal(x: Int): Long = x.toLong * (3 * x.toLong - 1) / 2
	def gap(x: Int): Long = 3 * x + 1

	def idx_of_pentagonal(p: Long): Int = {
		// better: 加cache应该还能提高速度
		((Math.sqrt(24 * p + 1) + 1) / 6).round.toInt
	}
	def idx_of_gap(gap: Long): Int = {
		((gap - 1) / 3).toInt
	}

	def is_pentagonal(p: Long): Boolean = {
		p == pentagonal(idx_of_pentagonal(p))
	}


	def is_acceptable(a: Int): Boolean = {
		val min_b = idx_of_pentagonal(gap(a))
		val Pa = pentagonal(a)
		val max_b = idx_of_gap(Pa)
		println(s"a=$a, b in [$min_b, $max_b]")
		(min_b to max_b).exists(b => is_acceptable(a, b, Pa))
	}
	def is_acceptable(a: Int, b: Int, Pa: Long): Boolean = {
		val Pb = pentagonal(b)
		val Pc = Pa + Pb
		val Pd = Pb + Pc
		is_pentagonal(Pc) && is_pentagonal(Pd)
	}


	def main(args: Array[String]): Unit = {
		val a = Stream.from(1).find(is_acceptable).get
		println(s"a = $a")
		println(s"P(a) = ${pentagonal(a)}")
	}

}

package page_51

object p57 {

	// 增长很快
	// step 17 = 3880899/2744210
	// step 18 = 9369319/6625109
	type Fraction = (BigInt, BigInt)

	def add(i: Int, f: Fraction): Fraction = {
		val (n, d) = f
		val n2 = n + i * d
		val shared = n2.gcd(d)
		val (n3, d3) = (n2 / shared, d / shared)

		(n3, d3)
	}
	def reciprocal(f: Fraction): Fraction = {
		val (n, d) = f
		(d, n)
	}

	def calc(step: Int): Fraction = {
		var f: Fraction = (1, 2)
		for (i <- 1 until step) {
			f = reciprocal(add(2, f))
		}
		add(1, f)
	}

	def main(args: Array[String]): Unit = {
		for (step <- 1 to 4) {
			println(calc(step))
		}

		println(s"count: ${
			(1 to 1000).count { step =>
				val (n, d) = calc(step)
				// println(s"step $step = $n/$d")
				n.toString.length > d.toString.length
			}
		}")
	}

}

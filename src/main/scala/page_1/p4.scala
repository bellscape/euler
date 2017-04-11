package page_1

object p4 {

	def accept(m: Int): Boolean = {
		(999 to 317 by -1) // sqrt(100001) > 316
			.exists(i => m % i == 0 && m / i < 1000)
	}
	def get_factor(m: Int): Int = {
		(999 to 317 by -1)
			.find(i => m % i == 0 && m / i < 1000).get
	}

	// abc => abccba
	def gen(seed: Int): Int = {
		val a = seed / 100
		val b = (seed % 100) / 10
		val c = seed % 10
		seed * 1000 + c * 100 + b * 10 + a
	}

	def main(args: Array[String]): Unit = {
		val m = (999 to 100 by -1).toStream
			.map(gen)
			.find(accept).get

		val a = get_factor(m)
		println(s"$m = $a * ${m / a}")
	}

}

/*
better:
	m = 100000a + 10000b + 1000c + 100c + 10b + a = 11(9091a + 910b + 100c)
	所以accept可以使用(990 to 110 by -11)
 */

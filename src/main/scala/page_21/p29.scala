package page_21

object p29 {

	def f(max: Int): Int = {
		(2 to max).flatMap(a => (2 to max).map(b => (a, b)))
			.map { case (a, b) => BigInt(a).pow(b) }
			.distinct.size
	}

	def main(args: Array[String]): Unit = {
		println(f(5))
		println(f(100))
	}

}

package page_1

object p1 {

	def f(limit: Int): Int = {
		(1 until limit)
			.filter(x => x % 3 == 0 || x % 5 == 0)
			.sum
	}

	def main(args: Array[String]): Unit = {
		println(f(10))
		println(f(1000))
	}

}

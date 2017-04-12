package page_51

object p52 {

	def check(x: Int): Boolean = {
		val x_sorted = x.toString.sorted
		(2 to 6).reverse.forall(times => (times * x).toString.sorted == x_sorted)
	}

	def main(args: Array[String]): Unit = {
		println(Stream.from(1).find(check).get)
	}

}

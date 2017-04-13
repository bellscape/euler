package page_1

object p5 {

	def gcd(a: Int, b: Int): Int = {
		if (b == 0) a else gcd(b, a % b)
	}

	def f(max: Int): Int = {
		(1 to max).foldLeft(1) { case (product, i) =>
			val factor = gcd(product, i)
			product * (i / factor)
		}
	}

	def main(args: Array[String]): Unit = {
		println(f(10))
		println(f(20))
	}

}

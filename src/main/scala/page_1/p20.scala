package page_1

object p20 {

	def f(n: Int): Int = {
		val factorial = (1 to n).foldLeft(BigInt(1)) { case (product, i) =>
			product * i
		}
		factorial.toString.map(c => c - '0').sum
	}

	def main(args: Array[String]): Unit = {
		println(f(10))
		println(f(100))
	}

}

package page_51

object p56 {

	def sum_digits(a: Int, b: Int): Int = {
		BigInt(a).pow(b).toString.map(c => c - '0').sum
	}

	def main(args: Array[String]): Unit = {
		val max = (2 to 99).toStream.flatMap { a =>
			(2 to 99).toStream.map(b => (a, b))
		}.map { case (a, b) => sum_digits(a, b) }.max
		println(max)
	}

}

package page_51

object p58 {
	import page_21.p27.is_prime

	def get_corners(width: Int): Seq[Int] = {
		(-3 to 0).map(i => i * (width - 1) + width * width)
	}

	// def count_primes()

	def main(args: Array[String]): Unit = {
		println(get_corners(5))
		println(get_corners(7))

		var numerator = 0
		var denominator = 1
		var width = 1
		while (true) {
			width += 2
			numerator += get_corners(width).count(is_prime)
			denominator += 4

			val ratio = numerator.toDouble / denominator
			println(s"w $width: $numerator/$denominator = $ratio")
			if (ratio < 0.1)
				return
		}

	}

}

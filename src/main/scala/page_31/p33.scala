package page_31

object p33 {

	// 分子ab或ba，分母bc或cb，约去b后为a/c
	// 1. a,b,c != 0	// 因为：0只可能出现在个位，即trivial
	// 2. a,b,c三者不等	// 因为：若a=b或b=c，则约去=="/11"，则a=b=c，即trivial
	//							若a=c，则约去结果=1，即分子=分母，即trivial
	// 3. b不能在同侧		// 因为：在同侧则必须a=c，即分子=分母，即trivial

	// 缩写：n=numerator分子，d=denominator分母
	type Fraction = (Int, Int) // Fraction(n, d)

	def reduce(n: Int, d: Int): Fraction = {
		import page_1.p5.gcd
		val divisor = gcd(n, d)
		(n / divisor, d / divisor)
	}

	def frac_equal(n1: Int, d1: Int, n2: Int, d2: Int): Boolean = {
		val f1 = reduce(n1, d1)
		val f2 = reduce(n2, d2)
		f1 == f2
	}

	def list_these_fractions(): Seq[(Int, Int)] = {
		(1 to 9).toStream.flatMap { a =>
			(1 to 9).toStream
				.filterNot(_ == a)
				.flatMap { c =>
					(1 to 9).toStream
						.filterNot(_ == a)
						.filterNot(_ == c).flatMap { b =>
						Stream(
							(a * 10 + b, b * 10 + c, a, c),
							(b * 10 + a, c * 10 + b, a, c)
						)
					}
				}
		}
			.filter { case (n1, d1, n2, d2) => n1 < d1 && n2 < d2 }
			.filter { case (n1, d1, n2, d2) => frac_equal(n1, d1, n2, d2) }
			.map { case (n1, d1, n2, d2) => (n1, d1) }
			.toIndexedSeq
	}

	def main(args: Array[String]): Unit = {
		val fractions = list_these_fractions()
		println(fractions)

		val Seq(product_n, product_d) = fractions
			.map { case (n, d) => Seq(n, d) }.transpose
			.map(_.product)
		val (reduced_n, reduced_d) = reduce(product_n, product_d)
		println(s"$reduced_n/$reduced_d")
	}

}

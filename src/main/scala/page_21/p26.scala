package page_21

object p26 {

	// e = d/2^m/5^n
	// e==1 => 0
	// 100..00 % e == 1 => count(0)

	def cycle_len(n: Int): Int = n match {
		case x if x % 2 == 0 => cycle_len(n / 2)
		case x if x % 5 == 0 => cycle_len(n / 5)
		case 1 => 0
		case _ =>
			Stream.from(1).find { len =>
				BigInt(10).pow(len).mod(n) == 1
			}.get
	}

	def main(args: Array[String]): Unit = {
		for (n <- 2 to 10) {
			println(s"cycle len $n => ${cycle_len(n)}")
		}

		val m = (1 until 1000).maxBy(cycle_len)
		println(s"cycle len $m => ${cycle_len(m)}")
	}

}

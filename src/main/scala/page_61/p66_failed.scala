package page_61

object p66_failed {

	def calc_sq_x(d: Int, x: BigInt, sq_x: BigInt, y: BigInt, sq_y: BigInt): BigInt = {
		// z = x^2 - Dy^2 - 1
		// val sq_x = x * x
		// val sq_y = y * y
		val z = sq_x - d * sq_y - 1
		if (z == 0) x
		// else if (next_x > 100000000000000L) 0
		else if (z > 0) {
			val next_y = y + 1
			calc_sq_x(d, x, sq_x, next_y, next_y * next_y)
		} else {
			val next_x = x + 1
			calc_sq_x(d, next_x, next_x * next_x, y, sq_y)
		}
	}

	var largest_x = BigInt(0)
	var largest_x_D = 0
	def analyse(d: Int): Unit = {
		val x = calc_sq_x(d, 1, 1, 1, 1)
		if (x > largest_x) {
			largest_x = x
			largest_x_D = d
		}
		if (x > 10000 || x == 0)
			println(s"d=$d> x = $x   (max x = $largest_x, d=$largest_x_D)")
	}

	def main(args: Array[String]): Unit = {
		val escape = Stream.from(1).map(x => x * x).takeWhile(_ <= 1000).toSet
		(1 to 1000).filterNot(escape).foreach(analyse)
		println(s"largest x: $largest_x, D=$largest_x_D")
	}

}

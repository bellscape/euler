package page_1

object p9 {

	def f(sum: Int): Int = {
		// a^2 + b^2 = c^2
		(1 to sum / 3).toStream.flatMap { a =>

			// 二分查找b
			def find(b_from: Int, b_to: Int): Option[Int] = {
				if (b_from > b_to) return None
				val b = (b_from + b_to) / 2
				val c = sum - a - b
				val diff = a * a + b * b - c * c
				if (diff == 0) {
					println(s"$a^2 + $b^2 = $c^2")
					Some(a * b * c)
				} else if (diff > 0) {
					find(b_from, b - 1)
				} else {
					find(b + 1, b_to)
				}
			}

			find(a + 1, 500)
		}.head
	}

	def main(args: Array[String]): Unit = {
		println(f(12))
		println(f(1000))
	}

}

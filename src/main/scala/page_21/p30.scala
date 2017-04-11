package page_21

object p30 {

	var powered: Array[Int] = _
	var powered_max: Int = _
	var last_diff: Array[Int] = _
	def update_constants(pow: Int): Unit = {
		powered = (0 to 9).map(i => Math.pow(i, pow).toInt).toArray
		powered_max = powered.last
		last_diff = powered.zipWithIndex.map { case (d, i) => d - i }
	}

	// 1634 = 1^4 + 6^4 + 3^4 + 4^4
	// prefix=0,    place=1000, left=4, diff=0
	// prefix=1000, place=100,  left=3, diff=999 =1000-1^4
	// prefix=1600, place=10,   left=2, diff=303 =1600-1^4-6^4
	// prefix=1630, place=1,    left=1, diff=252 =1630-1^4-6^4-3^4
	def list_possibility(prefix: Int, place: Int, left_len: Int, diff: Int): Stream[(Int, Int, Int, Int)] = {
		if (left_len > 1) { // expand
			if (diff.abs > powered_max * left_len) {
				Stream.empty
			} else {
				powered.zipWithIndex.toStream.flatMap { case (p, i) =>
					list_possibility(prefix + i * place, place / 10, left_len - 1, diff + i * place - p)
				}
			}
		} else { // filter
			if (diff < 0) {
				Stream.empty
			} else {
				import scala.collection.Searching._
				last_diff.search(diff) match {
					// last_diff(0) == last_diff(1)
					case Found(i) if i <= 1 => Stream((prefix, 0, 0, 0), (prefix + 1, 0, 0, 0))
					case Found(i) => Stream((prefix + i, 0, 0, 0))
					case _ => Stream.empty
				}
			}
		}
	}

	def list_these_numbers(pow: Int, max_digits: Int): Seq[Int] = {
		update_constants(pow)
		list_possibility(0, Math.pow(10, max_digits - 1).toInt, max_digits, 0)
			.map(_._1).dropWhile(_ <= 1).toIndexedSeq
	}

	def main(args: Array[String]): Unit = {
		// max number for 4th power: 9^4 * 5 = 32805
		val numbers_4 = list_these_numbers(4, 5)
		println(numbers_4)
		println(numbers_4.sum)

		// max number for 5th power: 9^5 * 6 = 354294
		val numbers_5 = list_these_numbers(5, 6)
		println(numbers_5)
		println(numbers_5.sum)
	}

}

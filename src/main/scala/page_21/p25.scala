package page_21

import scala.annotation.tailrec

object p25 {

	@tailrec
	def find_idx(a: BigInt, b: BigInt, a_idx: Int, digits: Int): Int = {
		if (a.toString.length >= digits) return a_idx

		val c = a + b
		find_idx(b, c, a_idx + 1, digits)
	}

	def main(args: Array[String]): Unit = {
		println(find_idx(BigInt(1), BigInt(1), 1, 3))
		println(find_idx(BigInt(1), BigInt(1), 1, 1000))
	}

}

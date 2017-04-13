package page_61

object p63 {


	def count(n: Int): Int = {
		// 10^n /10 <= root^n < 10^n
		// root ∈ [1,9]

		(1 to 9).toStream
			.map(root => BigInt(root).pow(n).toString.length)
			.dropWhile(len => len < n)
			.takeWhile(len => len == n)
			.size
	}

	def main(args: Array[String]): Unit = {
		// n的上界：10^n /10 <= 9^n
		val counts = (1 to 100).map(count)

		counts.zipWithIndex.foreach { case (count, i) =>
			val n = i + 1
			println(s"n=$n count=$count")
		}

		println(s"sum: ${counts.sum}")
	}

}

package page_51

object p53 {

	val min_value = 1000000

	// C(n,r+1) = C(n,r) *(n-r) /(r+1)
	def count(n: Int): Int = {
		// r ∈ [1,n]
		// 从头开始，找到第一个大个
		var r = 0
		var c = 1
		val last_test_r = (n + 1) / 2
		while (c < min_value && r < last_test_r) {
			c = c * (n - r) / (r + 1)
			// println(s"C($n,${r+1})=$c")
			r += 1
		}

		if (c < min_value) 0
		else n + 1 - 2 * r // 减去两边小个数量
	}


	def main(args: Array[String]): Unit = {
		println(s"n=23: ${count(23)}") // 10,11,12,13

		val counts = (1 to 100).map(n => count(n))
		counts.zipWithIndex.map { case (c, i) => s"${i + 1}->$c" }.grouped(10).foreach(println)

		val sum = counts.sum
		println(s"n 1~100: $sum")
	}

}

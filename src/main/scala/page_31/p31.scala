package page_31

object p31 {

	val denominations = Array(200, 100, 50, 20, 10, 5, 2, 1)
	val d_size: Int = denominations.length

	def ways(n: Int, d_i: Int = 0): Int = {
		if (d_i >= d_size || n < 0) {
			0
		} else if (n == 0) {
			1
		} else {
			ways(n, d_i + 1) + ways(n - denominations(d_i), d_i)
		}
	}

	def main(args: Array[String]): Unit = {
		println(ways(200))
	}

}

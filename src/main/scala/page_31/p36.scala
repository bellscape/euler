package page_31

object p36 {

	def is_palindromic(n: Int): Boolean = {
		val s = Integer.toString(n)
		if (s.reverse != s) return false
		val s_bin = Integer.toString(n, 2)
		s_bin.reverse == s_bin
	}

	def main(args: Array[String]): Unit = {
		println(is_palindromic(585))
		println(is_palindromic(586))

		val sum = (1 until 1000000)
			.filter(is_palindromic)
			.sum
		println(s"sum: $sum")
	}

}

// better: 构造palindromes，而不是挨个测试

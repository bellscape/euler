package page_41

object p48 {

	var ceil = 1000L

	def mul(a: Long, b: Int): Long = {
		a * b % ceil
	}
	def pow(base: Int, exponent: Int, start: Long = 1): Long = {
		if (exponent <= 0) start
		else pow(base, exponent - 1, mul(start, base))
	}
	def f(n: Int): Long = {
		(1 to n).map(i => pow(i, i)).sum % ceil
	}

	def main(args: Array[String]): Unit = {
		ceil = 1000L
		println(mul(999, 10))
		println(pow(2, 10))
		println(f(10))

		ceil = 10000000000L
		println(f(1000))
	}

}

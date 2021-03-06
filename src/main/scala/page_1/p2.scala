package page_1

object p2 {

	// ref: http://stackoverflow.com/a/7394965
	val fibonacci: Stream[Int] = {
		def tail(a: Int, b: Int): Stream[Int] = a #:: tail(b, a + b)
		tail(1, 2)
	}

	def f(limit: Int): Long = {
		fibonacci
			.takeWhile(_ < limit)
			.filter(_ % 2 == 0)
			.sum
	}

	def main(args: Array[String]): Unit = {
		println(f(20))
		println(f(4000000))
	}

}

/*
可利用规律：
	奇、奇、偶 => 隔三取一
	fib(n) = 4fib(n-3) + f(n-6)
*/

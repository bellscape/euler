package page_61

object p64 {

	/* ------------------------- calculator ------------------------- */

	var N = 23
	var max_b = 4

	// b, c => (√N + b) / c
	type Expression = (Int, Int)

	def extract(last: Expression): (Int, Expression) = {
		// (√23+4) / 7  =  1 + 1/( 7/( √23-3 ) )
		val (b, c) = last // N=23, b=4, c=7
		val floor = (max_b + b) / c // floor=1
		val b2 = b - c * floor // -3

		// 7/(√23-3) => 7(√23+3) / 23-3*3
		val c2 = N - b2 * b2
		if (c2 % c != 0) {
			throw new IllegalStateException()
		}

		(floor, (-b2, c2 / c))
	}
	def test_extractor(): Unit = {
		N = 23
		max_b = 4
		var e: Expression = (0, 1)
		for (i <- 1 to 10) {
			val (in, fr) = extract(e)
			println(s"step $i: $in, $fr")
			e = fr
		}
	}

	def new_stream(n: Int): Stream[(Int, Expression)] = {
		N = n
		max_b = Math.sqrt(n).floor.toInt
		if (max_b * max_b == N) return Stream.empty

		def tail(last: Expression): Stream[(Int, Expression)] = {
			val (in, fr) = extract(last)
			(in, fr) #:: tail(fr)
		}
		tail((0, 1))
	}
	def test_stream(): Unit = {
		for (n <- 1 to 100) {
			val s = new_stream(n)
			if (s.nonEmpty)
				println(s"$n: ${s.map(_._1).take(30).mkString(" ")}")
		}
		// 貌似所有的循环都是从第二位开始的
	}

	/* ------------------------- stat ------------------------- */

	def calc_cycle(n: Int): Int = {
		var stream = new_stream(n)
		if (stream.isEmpty) return 0
		stream = stream.tail

		val reference = stream.head
		Stream.from(1).find { _ =>
			stream = stream.tail
			stream.head == reference
		}.get
	}

	def count(max_n: Int): Int = {
		(1 to max_n).count(n => calc_cycle(n) % 2 == 1)
	}

	def main(args: Array[String]): Unit = {
		println(calc_cycle(23))
		println(count(13))
		println(count(10000))
	}

}

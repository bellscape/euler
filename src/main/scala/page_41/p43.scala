package page_41

object p43 {

	import page_31.p32.{Memory, memory_available, use_memory}
	val empty_memory: Memory = (0 to 9).map(_ => false).toArray

	// 因为越靠后，整除筛掉的比例越大，所以从后向前构造
	val checkers = Array(1, 1, 1, 17, 13, 11, 7, 5, 3, 2, 1)

	// post_width=3, ceil=1000, checker=17
	def list_pandigital(post_width: Int, ceil: Long, postfix: Stream[(Long, Memory)]): Stream[Long] = {
		val checker = checkers(post_width)
		val checked = if (checker <= 1) postfix else postfix.filter { case (p, _) => (p / (ceil / 1000)) % checker == 0 }
		if (post_width >= 10) {
			checked.map(_._1)
		} else checked.flatMap { case (p, mem) =>
			list_pandigital(post_width + 1, ceil * 10, memory_available(mem)
				.map(i => (i * ceil + p, use_memory(mem, i))))
		}
	}

	def list_pandigital(): Stream[Long] = {
		list_pandigital(0, 1, Stream((0, empty_memory)))
	}

	def main(args: Array[String]): Unit = {
		val list = list_pandigital().toIndexedSeq
		println(list)

		println(s"sum: ${list.sum}")
	}

}

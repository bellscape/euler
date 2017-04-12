package page_31

object p38 {

	val ceilings: Array[Int] = {
		def tail(last: Int): Stream[Int] = last #:: tail(last * 10)
		tail(1).take(7).toArray
	}


	type Memory = Array[Boolean] // used
	def empty_memory: Array[Boolean] = (0 to 9).map(_ == 0).toArray

	def save_memory(mem: Memory, i: Int): Boolean = {
		if (i == 0) true
		else {
			val digit = i % 10
			if (mem(digit)) false
			else {
				mem(digit) = true
				save_memory(mem, i / 10)
			}
		}
	}


	def is_pandigital(head: Int, n: Int, mem: Memory = empty_memory): Boolean = {
		if (n <= 0) return true
		if (save_memory(mem, head * n)) {
			is_pandigital(head, n - 1, mem)
		} else false
	}

	// head, n
	def list_head_candidates(n: Int): Stream[(Int, Int)] = {
		val min_width = 9 / n
		val min_width_count = n - (9 - min_width * n)
		val widths = (0 until n).map(i => min_width + (if (i < min_width_count) 0 else 1))

		val head_min = widths.zipWithIndex.map { case (w, i) =>
			(ceilings(w - 1).toFloat / (i + 1)).ceil.toInt
		}.max
		val head_max = widths.zipWithIndex.map { case (w, i) =>
			(ceilings(w) - 1) / (i + 1)
		}.min

		(head_min to head_max).toStream
			.map(head => (head, n))
	}

	// head, n
	def list_pandigital(): Stream[(Int, Int)] = {
		(2 to 9).toStream
			.flatMap(list_head_candidates)
			.filter { case (head, n) => is_pandigital(head, n) }
	}

	def to_pandigital(head: Int, n: Int): Int = {
		val products = (1 to n).map(_ * head)
		println(s"sample: $products")
		products.map(_.toString).mkString.toInt
	}

	def main(args: Array[String]): Unit = {
		val max_pandigital = list_pandigital()
			.map { case (head, n) => to_pandigital(head, n) }
			.max
		println(max_pandigital)
	}

}

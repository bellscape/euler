package page_31

object p32 {

	/* ------------------------- memory ------------------------- */
	// 不使用oo，以提高效率

	type Memory = Array[Boolean] // Memory(used: Array[Boolean])

	val empty_memory: Memory = (0 to 9).map(_ == 0).toArray

	def use_memory(mem: Memory, i: Int): Memory = {
		val new_mem = mem.clone()
		new_mem(i) = true
		new_mem
	}
	def memory_available(mem: Memory): Stream[Int] = {
		mem.zipWithIndex.toStream.filterNot(_._1).map(_._2)
	}
	def memory_explore(mem: Memory): Stream[(Int, Memory)] = {
		memory_available(mem).map(i => (i, use_memory(mem, i)))
	}

	// assert width(n) == mem.free.size
	// 会破坏mem中数据
	def memory_match(mem: Memory, n: Int): Boolean = {
		if (n == 0) true
		else {
			val digit = n % 10
			if (mem(digit)) false
			else {
				mem(digit) = true
				memory_match(mem, n / 10)
			}
		}
	}


	/* ------------------------- logic ------------------------- */

	def choose_num(mem: Memory, width: Int): Stream[(Int, Memory)] = {
		if (width <= 1) {
			memory_available(mem).map { d => (d, use_memory(mem, d)) }
		} else {
			choose_num(mem, width - 1).flatMap { case (n1, m2) =>
				choose_num(m2, 1).map { case (n2, m3) => (n1 * 10 + n2, m3) }
			}
		}
	}

	// a*b=c
	def list_c(a_width: Int, b_width: Int): Stream[(Int, Int, Int)] = {
		choose_num(empty_memory, a_width).flatMap { case (a, m2) =>
			choose_num(m2, b_width).flatMap { case (b, m3) =>
				val c = a * b
				if (c >= 1000 && c <= 9999 && memory_match(m3, c)) Some((a, b, c))
				else None
			}
		}
	}

	def main(args: Array[String]): Unit = {
		// possible: .*....=...., ..*...=....
		val total = (list_c(1, 4) ++ list_c(2, 3)).toIndexedSeq
		total.foreach(println)

		println(total.map(_._3).distinct.sum)
	}

}

package page_31

object p34 {

	val factorials: Array[Int] = {
		def tail(last: Int, next_i: Int): Stream[Int] = last #:: tail(last * next_i, next_i + 1)
		tail(1, 1).take(10).toArray
	}

	/* ------------------------- memory ------------------------- */

	type Memory = Array[Int] // num_count, mem[10]=width, mem[11]=sum, mem[12]=min_sum=10^width
	val empty_memory: Memory = (0 to 12).map(i => 0).toArray

	def use_memory(mem: Memory, i: Int): Memory = {
		val new_mem = mem.clone()
		new_mem(i) += 1
		new_mem(10) += 1
		new_mem(11) += factorials(i)
		new_mem(12) = if (new_mem(10) == 1) 1 else new_mem(12) * 10
		new_mem
	}

	def memory_is_final(mem: Memory): Boolean = {
		val width = mem(10)
		val sum = mem(11)
		val min_sum = mem(12)
		if (sum < min_sum || sum >= min_sum * 10 || width <= 1) return false

		memory_check_digit(mem, sum)
	}
	private def memory_check_digit(mem: Memory, n: Int): Boolean = {
		if (n == 0) return true

		val digit = n % 10
		val expected = mem(digit)
		if (expected <= 0) return false

		mem(digit) = expected - 1
		memory_check_digit(mem, n / 10)
	}

	/* ------------------------- logic ------------------------- */

	def width(n: Int): Int = {
		if (n < 10) 1 else 1 + width(n / 10)
	}

	def list_num(mem: Memory, max_digit: Int): Stream[Int] = {
		val width = mem(10)
		val left_width = 7 - width // 9!=362880, 8*9!=2903040<10^7 =>最多7个数字
		val sum = mem(11)
		val min_sum = mem(12)

		// 剪枝：已使用了太多数字，全部使用max_digit也不能让sum有这么大 => 放弃
		if (sum < min_sum) { // 当前sum太小
			if (sum + factorials(max_digit) < min_sum * 10) // 加数字后sum也太小
				return Stream.empty
		}

		if (max_digit < 0 || left_width <= 0) {
			if (memory_is_final(mem)) Stream(sum)
			else Stream.empty
		} else {
			list_num(use_memory(mem, max_digit), max_digit) ++
				list_num(mem, max_digit - 1)
		}
	}

	def main(args: Array[String]): Unit = {
		println(factorials.toSeq)

		val all = list_num(empty_memory, 9).toIndexedSeq
		println(all)
		println(s"sum: ${all.sum}")

	}

}

package page_41

// 找最大的pandigital素数
// 9位数太多，从后往前找
object p41 {
	import page_21.p27.is_prime
	import page_31.p32.{Memory, empty_memory, memory_available, use_memory}

	def list_pandigital(width: Int, left: Int, prefix: Stream[(Int, Memory)]): Stream[Int] = {
		if (left == 0) {
			prefix.map(_._1)
		} else {
			prefix.flatMap { case (p, mem) =>
				list_pandigital(width, left - 1,
					memory_available(mem).reverse
						.filter(_ <= width)
						.map(append_digit => (p * 10 + append_digit, use_memory(mem, append_digit))))
			}
		}
	}

	def find_prime_pandigital(): Int = {
		val all_pandigital = (2 to 9).reverse
			.flatMap(width => list_pandigital(width, width, Stream((0, empty_memory))))
		all_pandigital.filter(is_prime).head
	}


	def main(args: Array[String]): Unit = {
		println(find_prime_pandigital())
	}

}

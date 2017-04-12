package page_51

object p51 {
	import page_21.p27.is_prime

	/* ------------------------- replacement ------------------------- */
	// 指定长度，生成替换位置：2 => n*, *n

	type Replacement = Stream[Boolean] // (is_replacement)
	def replacement_to_string(pattern: Replacement): String =
		pattern.map(r => if (r) "*" else "n").mkString

	// (replacement, replace_count)
	def gen_replacements(count: Int): Stream[Replacement] = {
		(1 until count).toStream
			.flatMap(num_count => gen_replacements(num_count, count - num_count))
	}
	private def gen_replacements(num_count: Int, replace_count: Int): Stream[Replacement] = {
		if (num_count == 0) Stream((0 until replace_count).map(_ => true).toStream)
		else if (replace_count == 0) Stream((0 until num_count).map(_ => false).toStream)
		else {
			val num_first = gen_replacements(num_count - 1, replace_count)
				.map(tail => Stream.cons(false, tail))
			val replace_first = gen_replacements(num_count, replace_count - 1)
				.map(tail => Stream.cons(true, tail))
			num_first ++ replace_first
		}
	}

	/* ------------------------- pattern ------------------------- */
	// 指定替换位置，候选pattern：*n => *0, *1, ...

	type Pattern = Stream[Byte] // 0~9, -1=replacement
	def pattern_to_str(pattern: Pattern): String =
		pattern.map(i => if (i < 0) "*" else i.toString).mkString

	def gen_patterns(replacement: Replacement, is_head: Boolean = true, last_result: Int = 0): Stream[Pattern] = {
		if (replacement.isEmpty) return Stream(Stream.empty)

		val start_with_replace = replacement.head
		if (start_with_replace) {
			return gen_patterns(replacement.tail, is_head = false).map(tail => Stream.cons[Byte](-1, tail))
		}

		val min_digit = if (is_head) 1 else 0
		val max_digit = if (last_result == 0) 9 else last_result.toString.head - '0'
		(min_digit to max_digit).map(_.toByte).toStream.flatMap { head_digit =>
			gen_patterns(replacement.tail, is_head = false).map(tail => Stream.cons[Byte](head_digit, tail))
		}
	}

	/* ------------------------- count ------------------------- */

	def replace(pattern: Pattern): Seq[Int] = {
		val min_digit = if (pattern.head < 0) 1 else 0
		(min_digit to 9).map(i => replace(pattern, i))
	}
	private def replace(pattern: Pattern, i: Int, result: Int = 0): Int = {
		if (pattern.isEmpty) result
		else {
			val digit = pattern.head
			replace(pattern.tail, i, result * 10 + (if (digit < 0) i else digit))
		}
	}

	def calc_pattern(pattern: Pattern, min_primes: Int): Option[Int] = {
		val numbers = replace(pattern)
		val prime_count = numbers.count(is_prime)
		if (prime_count < min_primes) None
		else numbers.find(is_prime)
	}

	/* ------------------------- routine ------------------------- */

	def find(min_primes: Int, width: Int = 2): Int = {
		var last_result = 0
		for (replacement <- gen_replacements(width)) {
			for (pattern <- gen_patterns(replacement, last_result = last_result);
				 result <- calc_pattern(pattern, min_primes)) {
				if (last_result == 0 || last_result > result)
					last_result = result
			}
		}

		if (last_result > 0) last_result
		else find(min_primes, width + 1)
	}


	def main(args: Array[String]): Unit = {
		println(gen_replacements(3).map(replacement_to_string).toIndexedSeq)
		println(gen_patterns(Stream(true, false)).map(pattern_to_str).toIndexedSeq)
		println(gen_patterns(Stream(false, true), last_result = 40).map(pattern_to_str).toIndexedSeq)
		println(replace(Stream(-1, 3)).filter(is_prime))

		for (min_primes <- Seq(6, 7, 8)) {
			println(s"$min_primes primes: " + find(min_primes))
		}
	}

}

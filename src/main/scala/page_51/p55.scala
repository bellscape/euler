package page_51

object p55 {

	def is_palindrome(n: Int): Boolean = {
		val s = Integer.toString(n)
		s.reverse == s
	}
	def reverse(n: Int): Int = {
		n.toString.reverse.toInt
	}
	def is_Lychrel_number(n: Int, step: Int = 0): Boolean = {
		val reversed = reverse(n)
		if (reversed == n && step > 0) false
		else if (step > 50) true
		else {
			val next = n + reversed
			if (next > 100000000) throw new IllegalStateException(s"n = $n")
			is_Lychrel_number(next, step + 1)
		}
	}

	def is_Lychrel_number_2(n: BigInt, step: Int = 0): Boolean = {
		val reversed = BigInt(n.toString.reverse)
		if (reversed == n && step > 0) false
		else if (step > 50) true
		else {
			val next = n + reversed
			is_Lychrel_number_2(next, step + 1)
		}
	}

	def main(args: Array[String]): Unit = {
		println(!is_Lychrel_number(47))
		println(!is_Lychrel_number(349))

		val count = (1 until 10000).count { i =>
			// println(i)
			try {
				is_Lychrel_number(i)
			} catch {
				case e: IllegalStateException => is_Lychrel_number_2(BigInt(i))
			}
		}
		println(s"count: $count")
	}

}

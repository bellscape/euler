package page_21

object p28 {

	def f(w: Int): Int = {
		if (w == 1) 1
		else Stream.from(w * w, 1 - w).take(4).sum + f(w - 2)
	}

	def main(args: Array[String]): Unit = {
		println(f(5))
		println(f(1001))
	}

}

package page_1

import scala.collection.mutable

object p15 {

	val cache: mutable.HashMap[(Int, Int), Long] = mutable.HashMap.empty

	def f(a: Int, b: Int): Long = {
		if (a > b) return f(b, a) // 对称
		if (a == 0 || b == 0) return 1
		cache.getOrElseUpdate((a, b), {
			f(a - 1, b) + f(a, b - 1)
		})
	}

	def main(args: Array[String]): Unit = {
		println(f(2, 2))
		println(f(20, 20))
	}

}

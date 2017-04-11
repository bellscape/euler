package page_1

import scala.collection.mutable

object p14 {

	val cache: mutable.HashMap[Long, Int] = mutable.HashMap.empty
	cache(1) = 1

	// Int => overflow => java.lang.StackOverflowError
	def f(n: Long): Int = {
		cache.getOrElseUpdate(n, {
			if (n % 2 == 0) 1 + f(n / 2)
			else 1 + f(3 * n + 1)
		})
	}

	def get_seq(n: Long): Stream[Long] = {
		if (n == 1) Seq(1L).toStream
		else n #:: get_seq(if (n % 2 == 0) n / 2 else 3 * n + 1)
	}

	def main(args: Array[String]): Unit = {
		println(f(13))

		val n = (1L to 1000000L).maxBy(f)
		println(n)
		println(f(n))
		println(get_seq(n).toIndexedSeq)
	}

}

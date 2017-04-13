package page_51

import scala.collection.mutable

object p60 {
	import page_21.p27.{is_prime, primes}

	/* ------------------------- common cache ------------------------- */
	type Cache = mutable.HashMap[List[Int], mutable.HashSet[Int]]
	def new_cache(): Cache = mutable.HashMap.empty

	// 找更长的群
	// for cache(3):(2=>1), last_group:(2), raw:(1), return:(3), insert to cache(4)
	def find_group_fit(cache: Cache, last_group: Seq[List[Int]], raw: Set[Int]): Seq[List[Int]] = {
		last_group.flatMap { low =>
			cache.get(low) match {
				case Some(highs) => highs.intersect(raw).map(high => high :: low)
				case _ => None
			}
		}
	}

	// 保存当前长度的群
	// for cache(3):(2=>1), last_group:(2)
	def save_group(cache: Cache, last_group: Seq[List[Int]], high: Int): Unit = {
		last_group.foreach { low =>
			cache.getOrElseUpdate(low, mutable.HashSet.empty) += high
		}
	}

	// 计算此长度所有群的最小sum
	def calc_min_sum(last_group: Seq[List[Int]]): Int = last_group.map(_.sum).min

	/* ------------------------- cache ------------------------- */

	val tuple_2_cache: Cache = new_cache()
	val tuple_3_cache: Cache = new_cache()
	val tuple_4_cache: Cache = new_cache()

	var tuple_2_min_sum = 0
	var tuple_3_min_sum = 0
	var tuple_4_min_sum = 0
	var tuple_5_min_sum = 0

	def can_finish(last_prime: Int): Boolean = {
		// java.lang.NumberFormatException: For input string: "2148110007"
		if (last_prime > 20000) return true // 合并后超过int限制

		tuple_5_min_sum != 0 &&
			tuple_4_min_sum + last_prime > tuple_5_min_sum &&
			tuple_3_min_sum + 2 * last_prime > tuple_5_min_sum &&
			tuple_2_min_sum + 3 * last_prime > tuple_5_min_sum
	}

	/* ------------------------- logic ------------------------- */

	def list_concatable(prime: Int): Seq[Int] = {
		val p1_str = prime.toString
		primes.takeWhile(_ < prime).filter { p =>
			val p2_str = p.toString
			is_prime((p1_str + p2_str).toInt) && is_prime((p2_str + p1_str).toInt)
		}
	}

	def print_found(i: Int, group: Seq[List[Int]], prime: Int): Unit = {
		group.foreach(g => println(s"found group $i: ${(prime :: g).sorted.mkString(",")}"))
	}

	def analyse(prime: Int): Unit = {
		val tuple_1_raw = list_concatable(prime).toIndexedSeq
		if (tuple_1_raw.isEmpty) return
		val set_1 = tuple_1_raw.toSet

		val group_1 = tuple_1_raw.map(i => List(i))
		val group_2 = find_group_fit(tuple_2_cache, group_1, set_1)
		save_group(tuple_2_cache, group_1, prime)
		if (tuple_2_min_sum == 0) tuple_2_min_sum = prime + calc_min_sum(group_1)
		if (group_2.isEmpty) return

		// print_found(3, group_2, prime)

		val group_3 = find_group_fit(tuple_3_cache, group_2, set_1)
		save_group(tuple_3_cache, group_2, prime)
		if (tuple_3_min_sum == 0) tuple_3_min_sum = prime + calc_min_sum(group_2)
		if (group_3.isEmpty) return

		print_found(4, group_3, prime)

		val group_4 = find_group_fit(tuple_4_cache, group_3, set_1)
		save_group(tuple_4_cache, group_3, prime)
		if (tuple_4_min_sum == 0) tuple_4_min_sum = prime + calc_min_sum(group_3)
		if (group_4.isEmpty) return

		print_found(5, group_4, prime)
		val sum = prime + calc_min_sum(group_4)
		if (tuple_5_min_sum == 0 || tuple_5_min_sum > sum) tuple_5_min_sum = sum

		println(s"group 5 min sum: $tuple_5_min_sum")
		println(s"group 4 min sum: $tuple_4_min_sum")
		println(s"group 3 min sum: $tuple_3_min_sum")
		println(s"group 2 min sum: $tuple_2_min_sum")

	}

	def main(args: Array[String]): Unit = {
		// primes.take(20).foreach(analyse)
		for (prime <- primes) {
			analyse(prime)
			if (can_finish(prime)) {
				println(s"finish with group 5 min sum: $tuple_5_min_sum")
			}
		}
	}

}

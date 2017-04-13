package page_61

import scala.collection.mutable

object p62 {

	/*
	 * stage 1: 笨计数str.sorted=>(min_root,count)，找到第一个解
	 * stage 2: 如有更小的解，则一定已经遇到过了。所以不再记录新的条目，耗到超过其他数的可能上限即可
	 */

	var max_count: Int = 0
	var stage: Int = 1
	def cubed(root: Int): Long = root.toLong * root * root

	/* ------------------------- stage 1 ------------------------- */

	// cube_max => (count, min_root)
	val cache: mutable.HashMap[Long, Array[Int]] = mutable.HashMap.empty

	def run_stage_1(root: Int): Unit = {
		val cube = cubed(root)
		val cube_max = cube.toString.sorted.reverse.toString.toLong

		val entry = cache.getOrElseUpdate(cube_max, Array(0, root))
		entry(0) += 1

		if (entry(0) >= max_count)
			switch_to_stage_2(entry(1), cube)
	}

	/* ------------------------- stage 2 ------------------------- */

	var min_root = 0
	var max_explored = 0L

	var safety_checks: Stream[(Long, Array[Int])] = _

	def switch_to_stage_2(root: Int, explored: Long): Unit = {
		val cube = cubed(root)
		println(s"$max_count > found: $root^3 = $cube  ($max_count)  switch to stage 2")
		stage = 2
		min_root = root
		max_explored = explored

		// cache -= cube_max
		val (useless, checks) = cache.toIndexedSeq.partition(_._1 <= cube)
		cache --= useless.map(_._1)
		safety_checks = checks.toStream
	}

	def run_stage_2(root: Int): Unit = {
		val cube = cubed(root)
		max_explored = cube
		val cube_max = cube.toString.sorted.reverse.toString.toLong
		cache.get(cube_max).foreach { entry =>
			entry(0) += 1

			if (entry(0) >= max_count) {
				// cache -= cube_max
				if (entry(1) < min_root) {
					min_root = entry(1)
					println(s"$max_count > found: $min_root^3 = ${cubed(min_root)}  (${entry(0)})")
				}
			}
		}

		while (safety_checks.nonEmpty && !stage_2_check_is_useful(safety_checks.head))
			safety_checks = safety_checks.tail
	}

	def stage_2_check_is_useful(check: (Long, Array[Int])): Boolean = {
		val (cube_max, entry) = check
		cube_max > max_explored && entry(1) < min_root
	}

	/* ------------------------- routine ------------------------- */

	def can_finish(next_root: Int): Boolean = {
		//		if (next_root > 999) {
		//			// 999999999 > 999^3
		//			// 先在int范围之下执行，不够再说
		//			println(s"$max_count > stop on integer limit")
		//			return true
		//		}
		// if (max_count == 5) return next_root >= 10000
		stage == 2 && safety_checks.isEmpty
	}

	def run(permutation_count: Int): Unit = {
		max_count = permutation_count
		stage = 1
		cache.clear()

		for (root <- Stream.from(1)) {
			if (can_finish(root)) {
				val (root, cube) = (min_root, cubed(min_root))
				println(s"$max_count > result: $root^3 = $cube")
				return
			} else if (stage == 1) {
				run_stage_1(root)
			} else {
				run_stage_2(root)
			}
		}
		throw new IllegalStateException()
	}

	def main(args: Array[String]): Unit = {
		run(3)
		run(5)
	}

}

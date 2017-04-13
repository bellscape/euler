package page_61

object p61 {

	// row = prefix -> postfixes
	val rows: Seq[Map[Int, Seq[Int]]] = {
		val formulas: Seq[Int => Int] = Seq(
			x => x * (x + 1) / 2,
			x => x * x,
			x => x * (3 * x - 1) / 2,
			x => x * (2 * x - 1),
			x => x * (5 * x - 3) / 2,
			x => x * (3 * x - 2)
		)
		formulas.map { f =>
			Stream.from(1).map(f)
				.dropWhile(_ < 1000).takeWhile(_ < 10000)
				.groupBy(_ / 100)
				.mapValues(_.map(_ % 100).toIndexedSeq)
		}
	}

	var period = 0

	def explore(visited: Int, order: Seq[Int], history: Stream[List[Int]]): Stream[List[Int]] = {
		if (visited == period) {
			history.filter(cycle => cycle.head == cycle.last)
		} else {
			val row = rows(order(visited))
			if (visited == 0) { // first
				val next = row.toStream.flatMap { case (pre, posts) =>
					val tail = List(pre)
					posts.toStream.map(post => post :: tail)
				}
				explore(visited + 1, order, next)
			} else {
				val next = history.flatMap { tail =>
					row.getOrElse(tail.head, Seq.empty).toStream
						.map(head => head :: tail)
				}
				explore(visited + 1, order, next)
			}
		}
	}

	def run(cycle_period: Int): Unit = {
		period = cycle_period

		// 所有可能的顺序（第一个序列能匹配可能性最大，放到最前，让最后一个基本都能匹配上）
		val orders = (1 until cycle_period).permutations.toStream.map(last => 0 +: last)
		// possible_orders.foreach(println)

		val solutions = orders.flatMap(order => explore(0, order, null))
		println(s"solution ($period): ${solutions.headOption}")
		println(s"sum ($period): ${solutions.headOption.map(_.drop(1).sum * 101)}")

	}

	def main(args: Array[String]): Unit = {
		println("size of rows:" + rows.map(_.size))

		run(3)
		run(6)

		// (1 to 5).permutations.foreach(println)

	}

}

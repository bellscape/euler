package page_41

/*
此方法不可行，is_pentagonal判断次数过多，D增长太慢
checking: 250339, (83446)10444810651, (83447)10445060990
checking: 250464, (27828)1161582462, (27831)1161832926
checking: 250475, (16696)418126276, (16701)418376751
*/
object p44_failed {

	def pentagonal(x: Int): Long = x.toLong * (3 * x.toLong - 1) / 2

	def is_pentagonal(n: Long): Boolean = {
		val x: Long = ((Math.sqrt(24 * n + 1) + 1) / 6).round
		n == x * (3 * x - 1) / 2
	}

	val pentagonals: Stream[Long] = Stream.from(1).map(pentagonal)
	// def is_pentagonal(n: Long): Boolean = pentagonals.find(_ >= n).contains(n)

	// 按D从小到大排序，返回i,j
	val pentagonal_idx_pairs: Stream[(Int, Int)] = {
		// arr(shift-1) = head_i
		def tail(arr: Array[Int]): Stream[(Int, Int)] = {
			val (arr_head, arr_i) = arr.zipWithIndex.minBy { case (head, i) =>
				pentagonal(head + 1 + i) - pentagonal(head)
			}
			arr(arr_i) += 1
			val idx_pair_head = (arr_head, arr_head + 1 + arr_i)
			idx_pair_head #:: tail(if (arr_i + 1 < arr.length) arr else {
				arr ++ Array(1)
			})
		}
		tail(Array(1))
	}

	def main(args: Array[String]): Unit = {
		println((1 to 10).map(pentagonal))
		println(pentagonals.take(10).toIndexedSeq)
		pentagonal_idx_pairs.take(10).foreach { case (i, j) =>
			val pi = pentagonal(i)
			val pj = pentagonal(j)
			println(s"$i,$j => ${pj - pi} = $pj - $pi // ${is_pentagonal(pi)}, ${is_pentagonal(pj - pi)}")
		}

		println("-" * 60)

		val min_D = pentagonal_idx_pairs
			.find { case (i, j) =>
				val pi = pentagonal(i)
				val pj = pentagonal(j)
				if (Math.random() < 0.001) println(s"checking: ${pj - pi}, ($i)$pi, ($j)$pj")
				is_pentagonal(pj - pi) && is_pentagonal(pj + pi)
			}.map { case (i, j) => pentagonal(j) - pentagonal(i) }.get
		println(s"D = $min_D")
	}
}

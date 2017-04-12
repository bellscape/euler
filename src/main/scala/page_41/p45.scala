package page_41

object p45 {

	val triangles: Stream[Long] = Stream.from(1).map(x => x.toLong * (x + 1) / 2)
	val pentagonals: Stream[Long] = Stream.from(1).map(x => x.toLong * (3 * x - 1) / 2)
	val hexagonals: Stream[Long] = Stream.from(1).map(x => x.toLong * (2 * x - 1))

	def find(min_head: Long): Long = {
		var head = min_head
		var a = triangles
		var b = pentagonals
		var c = hexagonals
		while (true) {
			if (a.head < head) {
				a = a.tail
			} else if (a.head > head) {
				head = a.head
				// println(s"head = $head")
			} else if (b.head < head) {
				b = b.tail
			} else if (b.head > head) {
				head = b.head
				// println(s"head = $head")
			} else if (c.head < head) {
				c = c.tail
			} else if (c.head > head) {
				head = c.head
				// println(s"head = $head")
			} else {
				return head
			}
		}
		throw new IllegalStateException()
	}

	def main(args: Array[String]): Unit = {
		var start = 0L
		for (i <- 1 to 3) {
			start = find(start + 1)
			println(start)
		}
	}

}

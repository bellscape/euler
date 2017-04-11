package page_21

import scala.io.Source

object p22 {

	val src_sorted: Array[String] = Source.fromFile("data/page_1/p022_names.txt").getLines().mkString
		.split("""\W+""").filter(_.nonEmpty).sorted

	def alphabetical_value(name: String): Int = {
		name.map(c => c - 'A' + 1).sum
	}

	def main(args: Array[String]): Unit = {
		println(alphabetical_value("COLIN")) // 53
		println(src_sorted(938 - 1)) // COLIN

		val sum = src_sorted.zipWithIndex.map { case (name, i) => alphabetical_value(name) * (i + 1) }.sum
		println(sum)
	}

}

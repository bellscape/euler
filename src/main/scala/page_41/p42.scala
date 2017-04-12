package page_41

import scala.io.Source

object p42 {

	val triangle_numbers: Stream[Int] = Stream.from(1).map(i => i * (i + 1) / 2)

	def word_value(word: String): Int = {
		word.toCharArray.map(c => c - 'A' + 1).sum
	}

	def is_triangle_word(word: String): Boolean = {
		val value = word_value(word)
		triangle_numbers.find(_ >= value).contains(value)
	}

	def main(args: Array[String]): Unit = {
		println(triangle_numbers.take(10).toIndexedSeq)
		println(word_value("SKY"))
		println(is_triangle_word("SKY"))

		val word_count = Source.fromFile("data/page_1/p042_words.txt").getLines().mkString
			.split("""\W+""").filter(_.nonEmpty)
			.count(is_triangle_word)
		println(word_count)
	}

}

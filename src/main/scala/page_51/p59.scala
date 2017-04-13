package page_51

import scala.io.Source

object p59 {

	// 1. cipher中所有数字<128
	// => key和明文都只在ascii范围内，第一位均为0

	// 2. 2~3位的分布：0=883 (73.5%), 1=32 (2.7%), 2=286 (23.8%)
	// ascii 2~3位：00控制符、01空格/符号/数字、10大写字母、11小写字母
	// => key为小写字母，明文：0=小写字母、1=大写字母、2=符号&数字

	// 3. 孤立出现的2理解为空格，从空格推测key

	val src: Array[Byte] = Source.fromFile("data/page_1/p059_cipher.txt").getLines().next().split(",").map(_.toInt.toByte)
	def to_bin(b: Byte): String = b.toBinaryString.reverse.padTo(7, "0").reverse.mkString

	def print_hr(): Unit = println("-" * 60)
	def print_src(s: String): Unit = s.grouped(121).foreach(println)
	def print_src(): Unit = src.grouped(10).foreach { row => println(row.map(to_bin).mkString(" ")) }

	def guess_key(len: Int, hints: Map[Char, Seq[Int]]): Unit = {
		val digits = hints.toSeq.flatMap { case (c, all_pos) => all_pos.map(pos => (pos % len, c)) }
			.groupBy(_._1).mapValues(occurs => occurs.map(_._2).groupBy(c => c).mapValues(_.size))
		println(s"key.len=$len: ${
			(0 until len).map(digits.get).map {
				case None => "?"
				case Some(charmap) => charmap.toSeq.sortBy(-_._2).map { case (c, count) => s"$c($count)" }.mkString(",")
			}.mkString(" || ")
		}")
	}

	def decode(key: String): String = {
		src.zipWithIndex.map { case (b, i) =>
			(b ^ key.charAt(i % key.length)).toChar
		}.mkString
	}

	def main(args: Array[String]): Unit = {
		// step 1
		println(src.length) // 1201
		println(src.max) // 94

		// step 2
		print_hr()
		print_src(src.map(_ >> 5).mkString)
		println(src.map(_ >> 5).groupBy(i => i).toSeq
			.map { case (i, g) => f"$i=${g.length} (${g.length * 100.0 / src.length}%.1f%%)" }
			.sorted.mkString(", "))

		// step 3
		print_hr()
		print_src(src.zipWithIndex.map { case (b, i) =>
			val typ = b >> 5
			if (typ == 2) (src(i) ^ ' ').toChar
			else "."
		}.mkString)
		val hints = src.indices.drop(1).dropRight(1)
			.filter(i => (src(i) >> 5) == 2 && (src(i - 1) >> 5) != 2 && (src(i + 1) >> 5) != 2)
			.map(i => (i, (src(i) ^ ' ').toChar))
			.groupBy(_._2).filter(_._2.length > 1)
			.mapValues(_.map(_._1))
		// hints.foreach(println)
		guess_key(3, hints)
		guess_key(4, hints)
		guess_key(5, hints)

		// step 4
		print_hr()
		val text = decode("god")
		print_src(text)
		print_hr()
		println(text.map(_.toInt).sum)
	}

}

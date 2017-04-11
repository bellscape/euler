package page_21

object p24 {

	def list(alphabet: Array[Int]): Stream[Stream[Int]] = {
		if (alphabet.length <= 1) {
			Seq(alphabet.toStream).toStream
		} else {
			alphabet.zipWithIndex.toStream.flatMap { case (digit, i) =>
				val new_alphabet = alphabet.take(i) ++ alphabet.takeRight(alphabet.length - i - 1)
				list(new_alphabet).map { tail => digit #:: tail }
			}
		}
	}

	def main(args: Array[String]): Unit = {
		println(list((0 to 2).toArray).last.toIndexedSeq)

		val digits = list((0 to 9).toArray).apply(1000000 - 1).toIndexedSeq
		println(digits)
		println(digits.map(i => ('0' + i).toChar).mkString)
	}

}

package page_1

object p17 {

	// one, two, three, four, five => 3, 3, 5, 4, 4
	// 342 = three hundred and forty-two => 5+7 +3+ 5+3 = 23
	// 115 = one hundred and fifteen => 3+7 +3+ 7 = 20

	// n = abc
	def count(n: Int): Int = n match {
		case 1 => 3 // one
		case 2 => 3 // two
		case 3 => 5 // three
		case 4 => 4 // four
		case 5 => 4 // five
		case 6 => 3 // six
		case 7 => 5 // seven
		case 8 => 5 // eight
		case 9 => 4 // nine
		case 10 => 3 // ten
		case 11 => 6 // eleven
		case 12 => 6 // twelve
		case 13 => 8 // thirteen
		case 14 => 8 // fourteen
		case 15 => 7 // fifteen
		case 16 => 7 // sixteen
		case 17 => 9 // seventeen
		case 18 => 8 // eighteen
		case 19 => 8 // nineteen
		case 20 => 6 // twenty
		case 30 => 6 // thirty
		case 40 => 5 // forty
		case 50 => 5 // fifty
		case 60 => 5 // sixty
		case 70 => 7 // seventy
		case 80 => 6 // eighty
		case 90 => 6 // ninety
		case 1000 => 11 // one thousand
		case x if x > 99 =>
			val a = n / 100
			val bc = n % 100
			count(a) + 7 /*hundred*/ + (if (bc == 0) 0 else 3 /*and*/ + count(bc))
		case _ =>
			val c = n % 10
			count(n - c) + count(c)
	}

	def main(args: Array[String]): Unit = {
		println(count(342))
		println(count(115))
		println((1 to 1000).map(count).sum)
	}

}

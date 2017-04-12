package page_31

object p40 {

	// 理解为分段小数：1~9, 10~99, 100~999, ...
	// 1~9:		width=1, count=9,   start=1
	// 10~99:	width=2, count=90,  start=10
	// 100~999:	width=3, count=900, start=100

	def get_digit(n: Int): Int = {
		// 确定所在段
		var width = 1
		var count = 9
		var start_number = 1
		var pos_from = 1
		var pos_until = pos_from + width * count
		while (pos_until <= n) {
			width += 1
			count *= 10
			start_number *= 10
			pos_from = pos_until
			pos_until = pos_from + width * count
		}

		// 确定对应数字
		val pos_offset = n - pos_from
		val number_offset = pos_offset / width
		val number = start_number + number_offset
		val digit_offset = pos_offset - number_offset * width

		number.toString.charAt(digit_offset) - '0'
	}


	def main(args: Array[String]): Unit = {
		println(get_digit(12))

		val product = Seq(1, 10, 100, 1000, 10000, 100000, 1000000)
			.map(get_digit).product
		println(product)
	}

}

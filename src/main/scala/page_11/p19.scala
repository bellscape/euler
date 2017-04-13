package page_11

object p19 {

	// 0=Sun, 1=Mon, 2=Tue, ...

	def is_leap_year(year: Int): Boolean = {
		year % 4 == 0 && (year % 100 != 0 || year % 400 == 0)
	}

	def month_len(year: Int): Seq[Int] = {
		val leap = is_leap_year(year)
		Seq(31, if (leap) 29 else 28, 31, 30, 31, 30,
			31, 31, 30, 31, 30, 31)
	}

	def day_of_the_week(start: Int, len: Int): Int = {
		(start + len) % 7
	}

	def main(args: Array[String]): Unit = {
		val jan_1_1900 = 1
		val jan_1_1901 = day_of_the_week(jan_1_1900, month_len(1900).sum)

		val (_, century_sum) = (1901 to 2000)
			.flatMap(month_len)
			.foldLeft((jan_1_1901, 0)) { case ((day, sum), period_len) =>
				(day_of_the_week(day, period_len),
					if (day == 0) sum + 1 else sum)
			}

		println(century_sum)
	}

}

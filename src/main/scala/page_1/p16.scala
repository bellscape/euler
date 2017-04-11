package page_1

import java.math.BigInteger

object p16 {

	def f(exponent: Int): Int = {
		new BigInteger("2").pow(exponent).toString
			.map(c => c - '0').sum
	}

	def main(args: Array[String]): Unit = {
		println(f(15))
		println(f(1000))
	}

}

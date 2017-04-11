package page_21

import scala.collection.mutable

object p27 {

	// s = n^2 + an + b, n<b, s<int.max


	// p7.primes + cache
	val prime_cache: mutable.HashMap[Int, Boolean] = mutable.HashMap.empty
	val primes: Stream[Int] = 2 #:: Stream.from(3, 2).filter(is_prime)
	def is_prime(n: Int): Boolean = {
		if (n < 2) return false
		prime_cache.getOrElseUpdate(n, {
			val max_divider = Math.sqrt(n).toInt
			primes.takeWhile(_ <= max_divider).forall(p => n % p != 0)
		})
	}

	def count_primes(a: Int, b: Int): Int = {
		Stream.from(0).takeWhile(n => is_prime(n * n + a * n + b)).size
	}

	def main(args: Array[String]): Unit = {
		val (best_a, best_b) = (-1000 to 1000).flatMap(a => (-1000 to 1000).map(b => (a, b)))
			.maxBy { case (a, b) => count_primes(a, b) }
		println(best_a, best_b, best_a * best_b)
		println(count_primes(best_a, best_b))
	}

}

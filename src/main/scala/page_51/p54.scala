package page_51

import scala.io.Source

object p54 {

	// 2,3,....,9,10,J,Q,K,A
	val high_card = 1
	val one_pair = 2
	val two_pairs = 3
	val three_of_a_kind = 4
	val straight = 5
	val flush = 6
	val full_house = 7
	val four_of_a_kind = 8
	val straight_flush = 9
	val royal_flush = 10
	def level_to_str(level: Int): String = level match {
		case 1 => "high card"
		case 2 => "one pair"
		case 3 => "two pair"
		case 4 => "three of a kind"
		case 5 => "straight"
		case 6 => "flush"
		case 7 => "full house"
		case 8 => "four of a kind"
		case 9 => "straight flush"
		case 10 => "royal flush"
	}

	// 判断类型并排序
	def analyse(cards_str: Seq[String]): (Int, Seq[Int]) = {
		val cards = cards_str.map { s =>
			(s.head match {
				case 'T' => 10
				case 'J' => 11
				case 'Q' => 12
				case 'K' => 13
				case 'A' => 14
				case c => c - '0'
			}, s.last)
		}.sortBy { case (v, suit) => -v * 1000 + suit }
		val card_values = cards.map(_._1)
		val card_suits = cards.map(_._2)

		val is_flush = card_suits.distinct.size == 1
		val is_straight = card_values.sliding(2).forall { case Seq(a, b) => a == b + 1 }

		if (is_flush && is_straight && card_values.head == 14)
			return (royal_flush, card_values)
		if (is_flush && is_straight)
			return (straight_flush, card_values)

		val kinds = cards.groupBy(_._1).values.toSeq
			.sortBy(c => -c.size * 1000 - c.head._1).toIndexedSeq
		val kinds_size = kinds.map(_.size)
		val card_values_by_kinds = kinds.flatten.map(_._1)

		if (kinds_size.head == 4)
			return (four_of_a_kind, card_values_by_kinds)
		if (kinds_size.head == 3 && kinds_size(1) == 2)
			return (full_house, card_values_by_kinds)

		if (is_flush)
			return (flush, card_values)
		if (is_straight)
			return (straight, card_values)

		if (kinds_size.head == 3)
			return (three_of_a_kind, card_values_by_kinds)
		if (kinds_size.head == 2 && kinds_size(1) == 2)
			return (two_pairs, card_values_by_kinds)
		if (kinds_size.head == 2)
			return (one_pair, card_values_by_kinds)

		(high_card, card_values)
	}

	def player_1_wins(board: String): Boolean = {
		val cards = board.trim.split("""\s+""")
		if (cards.length != 10) throw new IllegalArgumentException

		val (p1_level, p1_cards) = analyse(cards.take(5))
		val (p2_level, p2_cards) = analyse(cards.drop(5))
		if (p1_level != p2_level) {
			println(s"$board: ${level_to_str(p1_level)} ${if (p1_level > p2_level) ">" else "<"} ${level_to_str(p2_level)}")
			p1_level > p2_level
		} else {
			val result = p1_cards.zip(p2_cards)
				.find { case (p1_card, p2_card) => p1_card != p2_card }
				.map { case (p1_card, p2_card) => p1_card > p2_card }
				.getOrElse(throw new IllegalStateException())
			println(s"$board: ${level_to_str(p1_level)}: $p1_cards ${if (result) ">" else "<"} $p2_cards")
			result
		}
	}

	//noinspection SimplifyBoolean
	def main(args: Array[String]): Unit = {
		println(analyse("AD TD JD KD QD".split(" ")))
		println(analyse("9D TD JD KD QD".split(" ")))
		println(analyse("9D 8D 8S 8D 8S".split(" ")))
		println(analyse("3D 3D 3S 5D 5S".split(" ")))
		println(analyse("3H 3H 3H 4H 5H".split(" ")))
		println(analyse("4H 5H 6H 7H 8D".split(" ")))

		println(player_1_wins("5H 5C 6S 7S KD   2C 3S 8S 8D TD") == false)
		println(player_1_wins("5D 8C 9S JS AC   2C 5C 7D 8S QH") == true)
		println(player_1_wins("2D 9C AS AH AC   3D 6D 7D TD QD") == false)
		println(player_1_wins("4D 6S 9H QH QC   3D 6D 7H QD QS") == true)
		println(player_1_wins("2H 2D 4C 4D 4S   3C 3D 3S 9S 9D") == true)

		val p1_wins = Source.fromFile("data/page_1/p054_poker.txt").getLines().filter(_.nonEmpty)
			.count(player_1_wins)
		println(s"p1 win: $p1_wins")
	}

}

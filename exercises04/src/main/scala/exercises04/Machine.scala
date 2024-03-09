package exercises04

import exercises04.Machine.Input.{Coin, Turn}

case class Machine(locked: Boolean, candies: Int, coins: Int)

/**
  * Реализуйте вендинговый аппарат по торговле барбарисками. Правила работы аппарата следующие:
  * если в закрытый аппарат вставляется монета (Coin), то аппартат открывается
  * если повернуть ручку (Turn) у открытого аппарата, то выйдет барбариска, и аппарат закроется
  * если в аппарате кончились барбариски, то он никак не реагирует. в этом случае надо вернуть список оставшихся Inputs и закончить
  * другие действия приводят к пропуску Input
  * если Input кончился, то заканчиваем
  * Подразумевается, что вы будете использовать паттерн-матчинг и рекурсию, так как while var isInstanceOf запрещены.
  */
object Machine {
  sealed trait Input
  object Input {
    case object Coin extends Input
    case object Turn extends Input
  }

  @scala.annotation.tailrec
  def run(machine: Machine, inputs: List[Input]): (Machine, List[Input]) = {
    inputs match {
      case Nil                       => (machine, inputs)
      case _ if machine.candies == 0 => (machine, inputs)
      case head :: tail if head == Coin && machine.locked =>
        run(machine.copy(locked = false, coins = machine.coins + 1), tail)
      case head :: tail if head == Turn && !machine.locked =>
        run(machine.copy(locked = true, candies = machine.candies - 1), tail)
      case _ :: tail => run(machine, tail)
    }
  }

}

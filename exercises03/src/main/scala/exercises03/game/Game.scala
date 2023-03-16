package exercises03.game

object Game {
  def parseState(input: String, number: Int): State = {
    if (input.forall(_.isDigit)) {
      input.toIntOption match {
        case x if x.get < number => NumberIsBigger
        case x if x.get > number => NumberIsSmaller
        case _               => Guessed
      }
    } else {
      input match {
        case "I give up" => GiveUp
        case _           => WrongInput
      }
    }
  }

  def action(state: State, number: Int): GameController => Unit = state match {
    case GiveUp          => _.giveUp(number)
    case Guessed         => _.guessed()
    case NumberIsBigger  => _.numberIsBigger()
    case NumberIsSmaller => _.numberIsSmaller()
    case WrongInput      => _.wrongInput()
    case _               => _.nextLine()
  }

  def completed(state: State): Boolean =
    state == Guessed || state == GiveUp
}

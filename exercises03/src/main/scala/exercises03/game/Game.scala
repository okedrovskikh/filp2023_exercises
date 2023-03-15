package exercises03.game

object Game {
  def parseState(input: String, number: Int): State = {
    if (input.forall(_.isDigit)) {
      input.toInt match {
        case x if x < number => NumberIsBigger
        case x if x > number => NumberIsSmaller
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
    case GiveUp          => controller => controller.giveUp(number)
    case Guessed         => controller => controller.guessed()
    case NumberIsBigger  => controller => controller.numberIsBigger()
    case NumberIsSmaller => controller => controller.numberIsSmaller()
    case WrongInput      => controller => controller.wrongInput()
    case _               => controller => controller.nextLine()
  }

  def completed(state: State): Boolean =
    state == Guessed || state == GiveUp
}

package engine.mechanism
import Mechanism.HuntCount
import engine.model.{Animal, World}

object Turn {
  @annotation.tailrec
  def loop(worldTurn: WorldTurn): WorldTurn = worldTurn.hunters match {
    case Nil => worldTurn
    case current :: remainingHunters if worldTurn.eaten contains current =>
      loop(worldTurn.copy(hunters = remainingHunters))
    case current :: remainingHunters =>
      hunterTurn(current, worldTurn.copy(hunters = remainingHunters))
  }

  def hunterTurn(hunter: Animal, worldTurn: WorldTurn): WorldTurn =
    if (Mechanism.testForStarvation(hunter))
      loop(worldTurn.starvation(hunter))
    else {
      @annotation.tailrec
      def huntLoop(count: Int = HuntCount(hunter),
                   worldTurn: WorldTurn = worldTurn): WorldTurn = count match {
        case 0 =>
          loop(worldTurn.failChase(hunter))
        case _ =>
          Mechanism.choosePrey(hunter, worldTurn.preys) match {
            case None =>
              loop(worldTurn.failChase(hunter))
            case Some(prey) =>
              if (Mechanism.chase(hunter, prey))
                loop(worldTurn.successfulChase(hunter, prey))
              else
                huntLoop(count - 1, worldTurn.unsuccessfulChase(hunter, prey))
          }
      }
      huntLoop()
    }

  def apply(world: World): World = {
    val finalTurn = loop(WorldTurn(world))

    consolidate(finalTurn)
  }

  def consolidate(worldTurn: WorldTurn): World = {
    worldTurn.initial.copy(
      turn = worldTurn.initial.turn + 1,
      species = worldTurn.initial.species.map { s =>
        val turn = worldTurn.species(s.id)
        val starved = if (s.isPlant) 0 else turn.starved
        val grow =
          if (s.isPlant)
            worldTurn.initial.maxSpecies * 50
          else
            turn.meat * 95 / 100 / s.body
        val count = s.count - turn.eaten - starved
        val totalCount =
          if (s.isPlant) math.min(9999, count + grow) else count + grow

        s.copy(
          count = totalCount,
          hunger = if (s.isPlant) 0 else turn.totalHunger * 10 / count,
          grow = grow,
          starved = starved,
          captured = turn.eaten
        )
      }
    )
  }
}

package engine.mechanism
import engine.model.{Animal, World}

case class WorldTurn(initial: World,
                     hunters: Seq[Animal],
                     preys: Set[Animal],
                     species: Map[Long, SpeciesTurn],
                     eaten: Set[Animal]) {
  def starvation(starving: Animal): WorldTurn =
    this.copy(
      preys = this.preys - starving,
      species = this.species + (starving.species.id -> this
        .species(starving.species.id)
        .addStarved(starving))
    )
  def successfulChase(hunter: Animal, prey: Animal): WorldTurn =
    this.copy(
      preys = this.preys - prey,
      species = this.species
        + (hunter.species.id -> this
          .species(hunter.species.id)
          .eat(hunter, prey.body))
        + (prey.species.id -> this.species(prey.species.id).addEaten(prey))
    )
  def unsuccessfulChase(hunter: Animal, prey: Animal): WorldTurn =
    this.copy(preys = this.preys - prey)
  def failChase(hunter: Animal): WorldTurn =
    this.copy(
      species = this.species
        + (hunter.species.id -> this
          .species(hunter.species.id)
          .failHunt(hunter))
    )
}
object WorldTurn {
  def apply(initial: World): WorldTurn = {
    val currentSpecies =
      initial.species.map(s => SpeciesTurn.apply(s, s.count * s.hunger / 10))

    val temp = for {
      species <- currentSpecies
      hunters = species.createAnimals
    } yield (hunters, util.Random.shuffle(hunters).take(hunters.size / 2))

    val hunters =
      util.Random.shuffle(temp.flatMap(_._1))
    val preys = temp.flatMap(_._2)

    new WorldTurn(
      initial,
      hunters,
      preys.toSet,
      currentSpecies.map(s => s.species.id -> s).toMap,
      Set.empty
    )
  }
}
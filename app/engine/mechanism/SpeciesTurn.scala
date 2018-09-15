package engine.mechanism
import engine.model.{Animal, Species}

case class SpeciesTurn(species: Species,
                       totalHunger: Int,
                       starved: Int = 0,
                       meat: Int = 0,
                       eaten: Int = 0) {
  def isPlant: Boolean = species.isPlant
  def createAnimals: Seq[Animal] =
    (1 to species.count)
      .map(id => Animal(id = id, species = species, hunger = hungerFromId(id)))
  def hungerFromId(id: Int): Int =
    if (id < species.hunger % 10 * species.count / 10) species.hunger / 10 + 1
    else species.hunger / 10
  def addStarved(starving: Animal): SpeciesTurn =
    this.copy(
      starved = this.starved + 1,
      totalHunger = this.totalHunger - starving.hunger
    )
  def failHunt(hunter: Animal): SpeciesTurn =
    this.copy(totalHunger = this.totalHunger + 1)
  def addMeat(body: Int): SpeciesTurn = this.copy(meat = this.meat + body)
  def eat(hunter: Animal, body: Int): SpeciesTurn =
    this.copy(
      meat = this.meat + body,
      totalHunger = this.totalHunger - hunter.hunger
    )
  def addEaten(prey: Animal): SpeciesTurn =
    this.copy(
      eaten = this.eaten + 1,
      totalHunger = this.totalHunger - prey.hunger
    )
}

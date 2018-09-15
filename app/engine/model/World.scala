package engine.model
import engine.mechanism.Turn


case class World(turn: Int,
                 maxSpecies: Int,
                 maxId: Int,
                 species: List[Species]) {
  def startSpecies(user: String,
                   herbivore: Boolean = false,
                   nose: Int = 0,
                   eyes: Int = 0,
                   ears: Int = 0,
                   mouth: Int = 0,
                   paws: Int = 0,
                   tail: Int = 0,
                   legs: Int = 0,
                   brain: Int = 0): Either[String, World] = {
    val oldSpecies = this.species.filterNot(_.user == user)
    val newSpecies = Species(
      id = this.maxId + 1,
      name = "Unnamed",
      user = user,
      herbivore = herbivore,
      nose = nose,
      eyes = eyes,
      ears = ears,
      mouth = mouth,
      paws = paws,
      tail = tail,
      legs = legs,
      brain = brain
    )

    if (isValid(newSpecies, oldSpecies))
      Right(
        this.copy(
          maxId = this.maxId + 1,
          species = oldSpecies :+ newSpecies,
          maxSpecies = math.max(maxSpecies, oldSpecies.size + 1)
        )
      )
    else
      Left(
        "Cannot add a species with same body, min and max attributes as existing one"
      )
  }

  def next = Turn(this)

  def mutate(id: Int,
             user: String,
             herbivore: Boolean = false,
             nose: Int = 0,
             eyes: Int = 0,
             ears: Int = 0,
             mouth: Int = 0,
             paws: Int = 0,
             tail: Int = 0,
             legs: Int = 0,
             brain: Int = 0): Either[String, World] = {
    val newSpecies = Species(
      id = this.maxId + 1,
      name = "Unnamed",
      user = user,
      herbivore = herbivore,
      nose = nose,
      eyes = eyes,
      ears = ears,
      mouth = mouth,
      paws = paws,
      tail = tail,
      legs = legs,
      brain = brain
    )
    val mutated = this.species.find(_.id == id).filter(_.user == user)
    if (mutated.isEmpty)
      Left("You cannot mutate a species that is not yours")
    else if ((mutated.get.characteristics.sum - newSpecies.characteristics.sum).abs > 2)
      Left(
        "The total of all characteristics of the parent species may not differ from the characteristic total of the new species by more than two"
      )
    else if (mutated.get.characteristics.zip(newSpecies.characteristics).count {
               case (a, b) => a != b
             } > 2)
      Left("You can change a maximum of two characteristics")
    else if (!isValid(newSpecies, species))
      Left(
        "You may not create a species with exactly the same Body, Min and Max as an existing species."
      )
    else
      Right(
        this.copy(
          maxId = this.maxId + 1,
          species = this.species :+ newSpecies,
          maxSpecies = math.max(this.maxSpecies, this.species.size + 1)
        )
      )
  }

  def isValid(newSpecies: Species, oldSpecies: List[Species]): Boolean =
    !oldSpecies
      .exists(
        s =>
          s.body == newSpecies.body && s.min == newSpecies.min && s.max == newSpecies.max
      )

  def rename(id: Int, user: String, newName: String): Either[String, World] = {
    if(!this.species.filter(_.user == user).exists(_.id == id))
      Left("You cannot rename a species that you don't own")
    else
      Right(
        this.copy(species = this.species.map{
          case s if s.id == id => s.copy(name = newName)
          case s => s
        })
      )
  }
}

object World {
  val initial = World(
    turn = 0,
    maxSpecies = 1,
    maxId = 1,
    species = List(Species(id = 1, name = "Plant", user = "system"))
  )
}

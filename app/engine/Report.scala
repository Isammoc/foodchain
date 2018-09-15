package engine
import engine.model.{Species, World}

object Report {

  def apply(world: World, user: Option[String] = None): String =
    s"""Turn ${world.turn}
       |
     |""".stripMargin +
      scoreReport(world) +
      "\n\n" +
      mainReport(world) +
      user.map(user => "\n\n" + privateReport(world, user)).getOrElse("")

  private def mainReport(world: World): String =
    s"""Species                Num  Hun  Gro  Cap  Stv  Bod  Preys-On
       |-------                ---  ---  ---  ---  ---  ---  --------
       |""".stripMargin + world.species.map(lineMainReport).mkString("\n")

  private def lineMainReport(species: Species): String =
    left(21, s"${species.name}(${species.id})") +
      right(5, species.count.toString) +
      right(5, f"${species.hunger.toFloat / 10}%3.1f") +
      right(5, species.grow.toString) +
      right(5, species.captured.toString) +
      right(5, species.starved.toString) +
      right(5, species.body.toString) +
      right(
        10,
        if (species.isPlant) "---" else s"${species.min}-${species.max}"
      ) +
      (if (species.herbivore)
         " [H]"
       else
         "")

  private def scoreReport(world: World): String =
    """Posn Species                           Player
      |---- -------                           ------
      |""".stripMargin +
      world.species
        .filter(_.isMutable)
        .groupBy(_.user)
        .toList
        .sortBy(row => (-row._2.size, -row._2.map(_.count).sum))
        .map { case (user, species) => (user, species.map(_.id).mkString(",")) }
        .zipWithIndex
        .map(line => lineScoreReport(line._2, line._1._1, line._1._2))
        .mkString("\n")

  private def lineScoreReport(index: Int, user: String, ids: String): String =
    right(3, (index + 1).toString) +
      "  " + left(34, ids) + user

  private def privateReport(world: World, user: String) =
    """ Id Name          N  Y  R  M  P  T  L  B
      |--- ----         -- -- -- -- -- -- -- --
      |""".stripMargin + world.species
      .filter(_.user == user)
      .map(linePrivateReport)
      .mkString("\n")

  private def linePrivateReport(species: Species) =
    right(3, species.id.toString) +
      " " + left(12, species.name) +
      right(3, species.nose.toString) +
      right(3, species.eyes.toString) +
      right(3, species.ears.toString) +
      right(3, species.mouth.toString) +
      right(3, species.paws.toString) +
      right(3, species.tail.toString) +
      right(3, species.legs.toString) +
      right(3, species.brain.toString)

  private def right(cols: Int, value: String) =
    " " * (cols - value.length) + value
  private def left(cols: Int, value: String) =
    value + " " * (cols - value.length)
}

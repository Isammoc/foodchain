package engine.mechanism
import engine.model.Animal

object Mechanism {
  def testForStarvation(animal: Animal): Boolean =
    Test.simple(animal.hunger, animal.resilience)

  def testForCatch(hunter: Animal, prey: Animal): Boolean =
    Test.squared(hunter.chase, prey.evade)

  def testForKill(hunter: Animal, prey: Animal): Boolean =
    Test.squared(hunter.power, prey.power)

  def chase(hunter: Animal, prey: Animal): Boolean =
    testForCatch(hunter, prey) && testForKill(hunter, prey)

  def choosePrey(hunter: Animal, preys: Iterable[Animal]): Option[Animal] = {
    val possible = preys
      .filter(p => hunter.min <= p.body && p.body <= hunter.max)
      .filterNot(_.species.id == hunter.id)
      .filter(_.isPlant == hunter.herbivore)
      .toSeq
    if (possible.isEmpty)
      None
    else
      Some(possible(util.Random.nextInt(possible.size)))
  }

  object HuntCount {
    private def lessFour(hunt: Int) = (x: Int) => hunt * x / 2
    private def lessNine(hunt: Int) = (x: Int) => 2 + hunt * x / 3
    private def lessNineteen(hunt: Int) = (x: Int) => 2 + 5 / 3 + hunt * x / 4
    private def other(hunt: Int) = (x: Int) => 2 + 5 / 3 + 5 / 2 + hunt * x / 5

    private def forHunt(hunt: Int) = hunt match {
      case _ if hunt <= 4  => lessFour(hunt)
      case _ if hunt <= 9  => lessNine(hunt)
      case _ if hunt <= 19 => lessNineteen(hunt)
      case _                  => other(hunt)
    }

    def apply(hunter: Animal): Int =
      forHunt(hunter.hunt)(hunter.id) - forHunt(hunter.hunt)(hunter.id - 1)
  }

  object Test {
    def simple(a: Int, b: Int): Boolean =
      if (a == 0 && b == 0)
        util.Random.nextInt(2) < 1
      else
        util.Random.nextInt(a + b) < a

    def squared(a: Int, b: Int): Boolean =
      if (a == 0 && b == 0)
        util.Random.nextInt(2) < 1
      else
        util.Random.nextInt((a + b) * (a + b)) < a * a
  }
}

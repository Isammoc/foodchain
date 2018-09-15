package engine.model

case class Animal(id: Int, species: Species, hunger: Int) {
  def isPlant : Boolean = species.isPlant
  def herbivore: Boolean  = species.herbivore
  def resilience : Int = species.resilience
  def body: Int  = species.body
  def hunt: Int  = species.hunt
  def evade: Int  = species.evade
  def power: Int  = species.power
  def chase: Int  = species.chase
  def min: Int  = species.min
  def max: Int  = species.max
}

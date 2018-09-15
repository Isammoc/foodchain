package engine.model

case class Species(id: Long,
                   name: String,
                   user: String,
                   herbivore: Boolean = false,
                   nose: Int = 0,
                   eyes: Int = 0,
                   ears: Int = 0,
                   mouth: Int = 0,
                   paws: Int = 0,
                   tail: Int = 0,
                   legs: Int = 0,
                   brain: Int = 0,
                   count: Int = 50,
                   hunger: Int = 0,
                   grow: Int = 0,
                   captured: Int = 0,
                   starved: Int = 0) {
  val hunt: Int = nose + paws + legs
  val chase: Int = 2 * eyes + 2 * tail + legs
  val evade: Int = ears + 3 * tail + 2 * legs
  val power: Int = mouth + paws
  val body: Int =
    if (isPlant) 5 else nose + ears + mouth + paws + 2 * tail + 2 * legs
  val min: Int = nose + eyes + mouth + paws + tail + legs
  val max: Int = nose + ears + 2 * mouth + tail + legs + 2 * brain
  val resilience: Int = math.max(0, nose + eyes + ears + mouth + tail - brain)

  def characteristics = List(nose, eyes, ears, mouth, paws, tail, legs, brain)

  def isMutable: Boolean = !isPlant && count >= 100
  def isPlant: Boolean = id == 1
}

package preprocessing

case class BlockParameters(
  ChirpSize: Int = 1024,
  QueueDepth: Int = 2048,
  MaxChirpsPerFrame: Int = 256,
  UseBlockRam: Boolean = false,
  GenLast: Boolean = true) {
  // add some requirements
}

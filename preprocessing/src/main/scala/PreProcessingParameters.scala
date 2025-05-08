package preprocessing

case class PreProcessingParameters(
  MaxChirpSize: Int = 1024,
  MaxChirpsPerFrame: Int = 256,
  CrcParams: CRCParameters = CRCParameters(
    dataBytes = 2,
    polynomial = 0x04C11DB7,
    init = 0xFFFFFFFFL,
    reflectIn = false,
    reflectOut = false,
    xorOut = 0x00000000L
  ),
  BufferParams: BufferParameters = BufferParameters(
    insertBuffers = false,
    size = 2
  )
)

case class BufferParameters (
  insertBuffers: Boolean = true,
  size: Int = 2
) {
  assert(if(insertBuffers) size > 1 else true, f"When enabled, buffers size should be larger then 1. Set buffer size is $size")
}

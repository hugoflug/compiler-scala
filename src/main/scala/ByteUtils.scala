import java.nio.ByteBuffer

object ByteUtils {
  implicit class XString(val x: String) extends AnyVal {
    def hex: Array[Byte] =
      x.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray
  }

  implicit class XInt(val x: Int) extends AnyVal {
    def u4: Array[Byte] =
      Array(((x >> 24) & 0xFF).toByte, ((x >> 16) & 0xFF).toByte, ((x >> 8) & 0xFF).toByte, (x & 0xFF).toByte)
    def u2: Array[Byte] =
      Array(((x >> 8) & 0xFF).toByte, (x & 0xFF).toByte)
    def u1: Array[Byte] =
      Array(x.toByte)
    def s2: Array[Byte] = {
      val buf = ByteBuffer.allocate(2)
      buf.putShort(x.toShort)
      buf.array
    }
    def s4: Array[Byte] = {
      val buf = ByteBuffer.allocate(4)
      buf.putInt(x)
      buf.array
    }
  }
}

import java.nio.ByteBuffer

import fs2.{Pure, Stream}

object ByteUtils {
  implicit class XString(val x: String) extends AnyVal {
    def hex: Stream[Pure, Byte] =
      Stream.emits(x.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray)
  }

  implicit class XInt(val x: Int) extends AnyVal {
    def u4: Stream[Pure, Byte] =
      Stream(((x >> 24) & 0xFF).toByte, ((x >> 16) & 0xFF).toByte, ((x >> 8) & 0xFF).toByte, (x & 0xFF).toByte)
    def u2: Stream[Pure, Byte] =
      Stream(((x >> 8) & 0xFF).toByte, (x & 0xFF).toByte)
    def u1: Stream[Pure, Byte] =
      Stream(x.toByte)
    def s2: Stream[Pure, Byte] = {
      val buf = ByteBuffer.allocate(2)
      buf.putShort(x.toShort)
      Stream.emits(buf.array)
    }
    def s4: Stream[Pure, Byte] = {
      val buf = ByteBuffer.allocate(4)
      buf.putInt(x)
      Stream.emits(buf.array)
    }
  }
}

import java.nio.ByteBuffer

import scala.language.implicitConversions

object ByteArrayConversions {
  implicit def int2ByteArray(x: Int): Array[Byte] = {
    val buf = ByteBuffer.allocate(4)
    buf.putInt(x)
    buf.array
  }

  implicit def short2ByteArray(x: Short): Array[Byte] = {
    val buf = ByteBuffer.allocate(2)
    buf.putShort(x)
    buf.array
  }

  implicit def long2ByteArray(x: Long): Array[Byte] = {
    val buf = ByteBuffer.allocate(8)
    buf.putLong(x)
    buf.array
  }

  implicit def str2ByteArray(x: String): Array[Byte] =
      x.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray
}

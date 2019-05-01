import java.nio.ByteBuffer

object ByteUtils {
  implicit class XString(val x: String) extends AnyVal {
    def bytes: Array[Byte] =
      x.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray
  }
/*
  implicit class XShort(val x: Short) extends AnyVal  {
    def signed: Array[Byte] = {
      val buf = ByteBuffer.allocate(2)
      buf.putShort(x)
      buf.array
    }

    def unsigned: Array[Byte] =
      Array(((x >> 8) & 0xFF).toByte, (x & 0xFF).toByte)
  }
*/
  /*
          bytes[off] = (byte) ((value >> 24) & 0xFF);
        bytes[off+1] = (byte) ((value >> 16) & 0xFF);
        bytes[off+2] = (byte) ((value >> 8) & 0xFF);
        bytes[off+3] = (byte) (value & 0xFF);
   */

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

  }
/*
  implicit class XLong(val x: Long) extends AnyVal with Byteable {
    def toByteArray: Array[Byte] = {
      val buf = ByteBuffer.allocate(8)
      buf.putLong(x)
      buf.array
    }
  }

  implicit class XFloat(val x: Float) extends AnyVal with Byteable {
    def toByteArray: Array[Byte] = {
      val buf = ByteBuffer.allocate(4)
      buf.putFloat(x)
      buf.array
    }
  }

  implicit class XDouble(val x: Double) extends AnyVal with Byteable {
    def toByteArray: Array[Byte] = {
      val buf = ByteBuffer.allocate(8)
      buf.putDouble(x)
      buf.array
    }
  }

 */
}

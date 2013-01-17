package me.wkwk.util

/**
 * Created with IntelliJ IDEA.
 * User: yuichi
 * Date: 2012/11/12
 * Time: 23:17
 * To change this template use File | Settings | File Templates.
 */
object Base64URL {
  val BASE64URL = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"

  @deprecated
  def encode(input:Long):String =
    encodeBuffer((0 until 8).map(i => (input >> (i * 8)).asInstanceOf[Byte]).toArray)

  def encode(input:String):String = encodeBuffer(input.getBytes())

  def encodeBuffer(buf:Array[Byte]):String = {
    val binary = toBinary(buf)
    val cnt = binary.size / 6

    (for(i <- 0 to cnt-1) yield (binary.substring(i*6, (i*6)+6))).map(
      b => BASE64URL(Integer.parseInt(b, 2))
    ).mkString
  }

  def decode(input:String):String = new String(decodeBuffer(input))

  def decodeBuffer(input:String):Array[Byte] = {
    checkChar(input)

    input.map {
      BASE64URL.indexOf(_)
    }.map {
      i => ("000000" + i.toBinaryString).takeRight(6)
    }.mkString.toList.grouped(8).filter(bits =>
      bits.size == 8
    ).map(b =>
      b.reverse
        .zipWithIndex
        .foldLeft(0){case (n, (c, i)) => (n + (scala.math.pow(2, i) * c.asDigit).toInt)}
        .toByte
    ).toArray
  }

  private def toBinary(buf:Array[Byte]):String = {
    val binary =
      buf.map(
        _ & 0xff
      ).map(
        i => ("00000000" + i.toBinaryString).takeRight(8)
      ).mkString

    fill(binary, "0", 6)
  }

  private def fill( s:String, fill:String, n:Int) = {
    s + ( fill * ( n - ( s.length % n )))
  }

  private def checkChar(input:String) = {
    input.foreach{c =>
      if(!BASE64URL.contains(c))
        throw new Exception("input string is incorrect : %s".format(input))
    }
  }

}

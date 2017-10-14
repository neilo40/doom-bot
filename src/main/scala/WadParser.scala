import java.io.{File, FileInputStream}
import java.nio.{ByteBuffer, ByteOrder, MappedByteBuffer}
import java.nio.file.{Files, Paths}
import java.nio.channels.FileChannel.MapMode._

case class Wad(wadType: String, numLumps: Int, levels: List[Level]) {
  override def toString: String = "[Wad] type: " + wadType + ", lumps: " + numLumps + ", levels: " + levels
}

case class Level(name: String, lumps: Map[String, Lump], lines: Option[List[WadLine]]=None){
  def addLump(lump: Lump): Level = {
    val newLumps: Map[String, Lump] = lumps + (lump.name -> lump)
    Level(name, newLumps)
  }

  def setLines(lines: List[WadLine]): Level = {
    Level(this.name, this.lumps, Some(lines))
  }

  override def toString: String = "[Level] name: " + name
}

case class Lump(name: String, data: List[Byte]) {
  override def toString: String = "[Lump] name: " + name
}

case class WadLine(a: Vertex, b: Vertex, oneSided: Boolean) {
  override def toString: String = s"[Line] $a <-> $b, oneSided: $oneSided"
}

case class Vertex(x: Int, y: Int) {
  override def toString: String = s"($x, $y)"
}

object WadParser {
  val HEADER_SIZE = 12

  def createStreamFromFile(): MappedByteBuffer = {
    val file = new File("C:\\Users\\neil\\Downloads\\doom1.wad")
    val fileSize = file.length
    val stream = new FileInputStream(file)
    val buffer = stream.getChannel.map(READ_ONLY, 0, fileSize)
    buffer.order(ByteOrder.LITTLE_ENDIAN)
    buffer
  }

  def extractBytesFromFile(): List[Byte] =
    Files.readAllBytes(Paths.get("C:\\Users\\neil\\Downloads\\doom1.wad")).toList

  def extractWadType(bytes: List[Byte]): String = bytes.take(4).map(_.toChar).mkString

  def extractNumLumps(bytes: List[Byte]): Int =
    ByteBuffer.wrap(bytes.slice(4, 8).toArray).order(ByteOrder.LITTLE_ENDIAN).getInt()

  def extractData(bytes: List[Byte]): (Int, List[Byte]) = {
    val dataLenBytes = bytes.slice(8, 12)
    val dataLen = ByteBuffer.wrap(dataLenBytes.toArray).order(ByteOrder.LITTLE_ENDIAN).getInt() - HEADER_SIZE
    val dataBytes = bytes.slice(12, dataLen)
    (dataLen, dataBytes)
  }

  def extractLump(lumpBytes: List[Byte], data: List[Byte]): Lump = {
    val filePos = ByteBuffer.wrap(lumpBytes.take(4).toArray).order(ByteOrder.LITTLE_ENDIAN).getInt() - HEADER_SIZE
    val size = ByteBuffer.wrap(lumpBytes.slice(4, 8).toArray).order(ByteOrder.LITTLE_ENDIAN).getInt()
    val name = lumpBytes.drop(8).map(_.toChar).mkString.trim()
    Lump(name, data.slice(filePos, filePos + size))
  }

  def extractLumps(bytes: List[Byte], dataLen: Int, data: List[Byte]): List[Lump] =
    bytes.drop(dataLen + HEADER_SIZE).sliding(16, 16).map(extractLump(_, data)).toList

  private def levelNameMatched(currentLevel: Option[Level], levelName: String, name: String,
                               remainingLumps: List[Lump]): List[Level] = {
    if (currentLevel.isDefined){
      List(currentLevel.get) ++ extractLevels(Some(Level(levelName, Map())), remainingLumps.tail, levelName)
    } else {
      extractLevels(Some(Level(levelName, Map())), remainingLumps.tail, levelName)
    }
  }

  private def nonLevelNameMatched(currentLevel: Option[Level], name: String, currentLump: Lump,
                                  remainingLumps: List[Lump]): List[Level] = {
    if (name == "START"){
      extractLevels(None, remainingLumps.tail, "START")
    } else {
      extractLevels(Some(currentLevel.get.addLump(currentLump)), remainingLumps.tail, name)
    }
  }

  def extractLevels(currentLevel: Option[Level], remainingLumps: List[Lump], name: String): List[Level] = {
    val currentLump = remainingLumps.headOption.getOrElse(Lump("FINISH", List()))
    val pattern = "(E[0-9]M[0-9])".r
    currentLump.name match {
      case pattern(levelName) => levelNameMatched(currentLevel, levelName, name, remainingLumps)
      case "FINISH" => List(currentLevel.get)
      case _ => nonLevelNameMatched(currentLevel, name, currentLump, remainingLumps)
    }
  }

  private def extractVertex(bytes: List[Byte]): Vertex = {
    val x = ByteBuffer.wrap(bytes.take(2).toArray).order(ByteOrder.LITTLE_ENDIAN).getShort().toInt
    val y = ByteBuffer.wrap(bytes.slice(2, 4).toArray).order(ByteOrder.LITTLE_ENDIAN).getShort().toInt
    Vertex(x, y)
  }

  def extractVerticesForLevel(level: Level): List[Vertex] = {
    val vertexData = level.lumps("VERTEXES").data
    vertexData.sliding(4, 4).map(extractVertex).toList
  }

  private def extractLine(bytes: List[Byte], vertices: List[Vertex]): WadLine = {
    val aIndex = ByteBuffer.wrap(bytes.take(2).toArray).order(ByteOrder.LITTLE_ENDIAN).getShort().toInt
    val bIndex = ByteBuffer.wrap(bytes.slice(2, 4).toArray).order(ByteOrder.LITTLE_ENDIAN).getShort().toInt
    val leftSide = ByteBuffer.wrap(bytes.slice(10, 12).toArray).order(ByteOrder.LITTLE_ENDIAN).getShort().toInt
    val rightSide = ByteBuffer.wrap(bytes.slice(12, 14).toArray).order(ByteOrder.LITTLE_ENDIAN).getShort().toInt
    WadLine(vertices(aIndex), vertices(bIndex), leftSide == -1 || rightSide == -1)
  }

  def extractLinesForLevel(level: Level): List[WadLine] = {
    val vertices = extractVerticesForLevel(level)
    val linedefs = level.lumps("LINEDEFS").data
    linedefs.sliding(14, 14).map(extractLine(_, vertices)).toList
  }

  def addLinesToLevel(level: Level): Level = {
    level.setLines(extractLinesForLevel(level))
  }

  def createWad(): Wad = {
    val bytes = extractBytesFromFile()
    val wadType = extractWadType(bytes)
    val numLumps = extractNumLumps(bytes)
    val (dataLen, data) = extractData(bytes)
    val lumps = extractLumps(bytes, dataLen, data)
    val levels = extractLevels(None, lumps, "START").map(addLinesToLevel)

    Wad(wadType, numLumps, levels)
  }
}

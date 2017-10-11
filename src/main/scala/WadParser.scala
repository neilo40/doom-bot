import java.nio.{ByteBuffer, ByteOrder}
import java.nio.file.{Files, Paths}

case class Wad(wadType: Option[String]=None,
               numLumps: Option[Int]=None,
               data: Option[List[Byte]]=None,
               levels: Option[Map[String, Level]]=None,
               bytes: List[Byte]) {

  def setNumLumps(numLumps: Int, bytes: List[Byte]): Wad =
    Wad(this.wadType, Some(numLumps), this.data, this.levels, bytes)

  def setData(data: List[Byte], bytes: List[Byte]): Wad =
    Wad(this.wadType, this.numLumps, Some(data), this.levels, bytes)

  def addLevel(level: Level): Wad = {
    val newLevels = this.levels match {
      case Some(x) => x + (level.name -> level)
      case None => Map(level.name -> level)
    }
    Wad(this.wadType, this.numLumps, this.data, Some(newLevels), this.bytes)
  }

  def setLevels(levels: Map[String, Level]): Wad =
    Wad(this.wadType, this.numLumps, this.data, Some(levels), this.bytes)

  override def toString: String = "[Wad] type: " + wadType.getOrElse("Unknown") +
    ", lumps: " + numLumps.getOrElse("Unknown") + ", levels: " + levels.getOrElse("Unknown")
}

case class Lump(name: String, size: Int, filePos: Int, data: List[Byte]) {
  override def toString: String = "[Lump] name: " + name + ", filePos: " + filePos + ", size: " + size
}

case class Line(a: Vertex, b: Vertex, oneSided: Boolean)

case class Vertex(x: Int, y: Int)

case class Level(name: String, lumps: Map[String, Lump], lines: Option[List[Line]]=None){
  def addLump(lump: Lump): Level = {
    val newLumps: Map[String, Lump] = lumps + (lump.name -> lump)
    Level(name, newLumps)
  }

  override def toString: String = "[Level] name: " + name
}

object WadParser extends App{
  val HEADER_SIZE = 12

  def retBytesFromFile(): Wad =
    Wad(bytes = Files.readAllBytes(Paths.get("C:\\Users\\neil\\Downloads\\doom1.wad")).toList)

  def retWadType(): Wad = {
    val res = retBytesFromFile().bytes.splitAt(4)
    Wad(wadType = Some(res._1.map(_.toChar).mkString), bytes = res._2)
  }

  def retNumLumps(): Wad = {
    val wad = retWadType()
    val res = wad.bytes.splitAt(4)
    val numLumps = ByteBuffer.wrap(res._1.toArray).order(ByteOrder.LITTLE_ENDIAN).getInt()
    wad.setNumLumps(numLumps, res._2)
  }

  def extractData(): Wad = {
    val wad = retNumLumps()
    val res = wad.bytes.splitAt(4)
    val dataLen = ByteBuffer.wrap(res._1.toArray).order(ByteOrder.LITTLE_ENDIAN).getInt() - HEADER_SIZE
    val res2 = res._2.splitAt(dataLen)
    wad.setData(res2._1, res2._2)
  }

  def extractLump(lumpBytes: List[Byte], wad: Wad): Lump = {
    val filePos = ByteBuffer.wrap(lumpBytes.slice(0, 4).toArray).order(ByteOrder.LITTLE_ENDIAN).getInt() - HEADER_SIZE
    val size = ByteBuffer.wrap(lumpBytes.slice(4, 8).toArray).order(ByteOrder.LITTLE_ENDIAN).getInt()
    val name = lumpBytes.drop(8).map(_.toChar).mkString.trim()
    Lump(name, size, filePos, wad.data.getOrElse(List()).slice(filePos, filePos + size))
  }

  private def levelNameMatched(currentLevel: Option[Level], levelName: String, name: String,
                               remainingLumps: List[Lump]): Map[String, Level] = {
    if (currentLevel.isDefined){
      Map(name -> currentLevel.get) ++ extractLevelsRec(Some(Level(levelName, Map())), remainingLumps.tail, levelName)
    } else {
      extractLevelsRec(Some(Level(levelName, Map())), remainingLumps.tail, levelName)
    }
  }

  private def nonLevelNameMatched(currentLevel: Option[Level], name: String, currentLump: Lump,
                                  remainingLumps: List[Lump]): Map[String, Level] = {
    if (name == "START"){
      extractLevelsRec(None, remainingLumps.tail, "START")
    } else {
      extractLevelsRec(Some(currentLevel.get.addLump(currentLump)), remainingLumps.tail, name)
    }
  }

  def extractLevelsRec(currentLevel: Option[Level], remainingLumps: List[Lump], name: String): Map[String, Level] = {
    val currentLump = remainingLumps.headOption.getOrElse(Lump("FINISH", 0, 0, List()))
    println(currentLump.name)
    val pattern = "(E[0-9]M[0-9])".r
    currentLump.name match {
      case pattern(levelName) => levelNameMatched(currentLevel, levelName, name, remainingLumps)
      case "FINISH" => Map(name -> currentLevel.getOrElse(Level("unknown", Map())))
      case _ => nonLevelNameMatched(currentLevel, name, currentLump, remainingLumps)
    }
  }

  def constructWad(): Wad = {
    val wad = extractData()
    val lumps = wad.bytes.sliding(16, 16).map(extractLump(_, wad)).toList
    wad.setLevels(extractLevelsRec(None, lumps, "START"))
  }

  val wad = constructWad()

  private def extractVertex(bytes: List[Byte]): Vertex = {
    val x = ByteBuffer.wrap(bytes.slice(0, 2).toArray).order(ByteOrder.LITTLE_ENDIAN).getInt()
    val y = ByteBuffer.wrap(bytes.slice(2, 4).toArray).order(ByteOrder.LITTLE_ENDIAN).getInt()
    Vertex(x, y)
  }

  def getVerticesForLevel(level: Level): List[Vertex] = {
    val vertexData = level.lumps("VERTEXES").data
    vertexData.sliding(4, 4).map(extractVertex).toList
  }

  private def extractLine(bytes: List[Byte], vertices: List[Vertex]): Line = {

  }

  def getLinesForLevel(level: Level): List[Line] = {
    val vertices = getVerticesForLevel(level)
    val linedefs = level.lumps("LINEDEFS").data
    linedefs.sliding(16,16).map(extractLine(_, vertices)).toList
  }

}

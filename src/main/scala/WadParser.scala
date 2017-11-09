import java.io.{File, FileInputStream}
import java.nio.{ByteBuffer, ByteOrder, MappedByteBuffer}
import java.nio.channels.FileChannel.MapMode._
import math.abs

object WadParser {
  val HEADER_SIZE = 12
  val DOOR_LINEDEF_TYPES = List(1, 117, 63, 114, 29, 111, 90, 105, 4, 108, 31, 118, 61, 115, 103, 112, 86, 106,
    2, 109, 46, 42, 116, 50, 113, 75, 107, 3, 110, 196, 175, 76, 16)
  val DOOR_SWITCH_TYPES = List(103)

  private def createStream(fromFile: String): MappedByteBuffer = {
    val file = new File(fromFile)
    val fileSize = file.length
    val stream = new FileInputStream(file)
    val buffer = stream.getChannel.map(READ_ONLY, 0, fileSize)
    buffer.order(ByteOrder.LITTLE_ENDIAN)
    buffer
  }

  private def extractWadType(byteStream: MappedByteBuffer): String = {
    val wadTypeBytes = new Array[Byte](4)
    byteStream.get(wadTypeBytes, 0, 4)
    wadTypeBytes.map(_.toChar).mkString
  }

  private def extractNumLumps(byteStream: MappedByteBuffer): Int = byteStream.getInt()

  private def extractData(byteStream: MappedByteBuffer): ByteBuffer = {
    val dataEnd = byteStream.getInt()
    val dataBytes = byteStream.slice()
    byteStream.position(dataEnd)
    dataBytes
  }

  private def extractLump(byteStream: MappedByteBuffer, data: ByteBuffer): Lump = {
    val filePos = byteStream.getInt() - HEADER_SIZE
    val size = byteStream.getInt()
    val nameBytes = new Array[Byte](8)
    byteStream.get(nameBytes, 0, 8)
    val name = nameBytes.map(_.toChar).mkString.trim()
    val dataBytes = new Array[Byte](size)
    if (filePos >= 0) {
      data.position(filePos)
      data.get(dataBytes, 0, size)
      Lump(name, dataBytes.toList)
    } else {
      Lump(name, List())
    }
  }

  private def extractLumps(byteStream: MappedByteBuffer, data: ByteBuffer): List[Lump] =
    byteStream.remaining() match {
      case 0 => List()
      case _ => extractLump(byteStream, data) +: extractLumps(byteStream, data)
    }

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

  private def extractLevels(currentLevel: Option[Level], remainingLumps: List[Lump], name: String): List[Level] = {
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

  private def extractVerticesForLevel(level: Level): List[Vertex] = {
    val vertexData = level.lumps("VERTEXES").data
    vertexData.sliding(4, 4).map(extractVertex).toList
  }

  private def extractLine(bytes: List[Byte], vertices: List[Vertex], level: Level): WadLine = {
    val aIndex = ByteBuffer.wrap(bytes.take(2).toArray).order(ByteOrder.LITTLE_ENDIAN).getShort().toInt
    val bIndex = ByteBuffer.wrap(bytes.slice(2, 4).toArray).order(ByteOrder.LITTLE_ENDIAN).getShort().toInt
    val flags = ByteBuffer.wrap(bytes.slice(4, 6).toArray).order(ByteOrder.LITTLE_ENDIAN).getShort().toInt
    val specialType = ByteBuffer.wrap(bytes.slice(6, 8).toArray).order(ByteOrder.LITTLE_ENDIAN).getShort().toInt
    val sectorTag = ByteBuffer.wrap(bytes.slice(8, 10).toArray).order(ByteOrder.LITTLE_ENDIAN).getShort().toInt
    val leftSide = ByteBuffer.wrap(bytes.slice(10, 12).toArray).order(ByteOrder.LITTLE_ENDIAN).getShort().toInt
    val rightSide = ByteBuffer.wrap(bytes.slice(12, 14).toArray).order(ByteOrder.LITTLE_ENDIAN).getShort().toInt
    val leftSideDef = if (leftSide == -1) None else Some(level.sideDefs.get(leftSide))
    val rightSideDef = if (rightSide == -1) None else Some(level.sideDefs.get(rightSide))
    val leftHeight = leftSideDef.getOrElse(rightSideDef.get).sector.get.floorHeight
    val rightHeight = rightSideDef.getOrElse(leftSideDef.get).sector.get.floorHeight
    val heightDifference = abs(leftHeight - rightHeight)
    val oneSided = leftSide == -1 || rightSide == -1 || (flags & 0x0001) == 1 || heightDifference > 20 //TODO: rename this to traversible
    WadLine(vertices(aIndex), vertices(bIndex), oneSided, Some(sectorTag), Some(specialType),
      rightSideDef, leftSideDef)
  }

  private def extractLinesForLevel(level: Level): List[WadLine] = {
    val vertices = extractVerticesForLevel(level)
    val linedefs = level.lumps("LINEDEFS").data
    linedefs.sliding(14, 14).map(extractLine(_, vertices, level)).toList
  }

  private def extractSector(bytes: List[Byte]): Sector = {
    val floorHeight = ByteBuffer.wrap(bytes.take(2).toArray).order(ByteOrder.LITTLE_ENDIAN).getShort().toInt
    val ceilingHeight = ByteBuffer.wrap(bytes.slice(2, 4).toArray).order(ByteOrder.LITTLE_ENDIAN).getShort().toInt
    val sectorType = ByteBuffer.wrap(bytes.slice(22, 24).toArray).order(ByteOrder.LITTLE_ENDIAN).getShort().toInt
    val tag = ByteBuffer.wrap(bytes.slice(24, 26).toArray).order(ByteOrder.LITTLE_ENDIAN).getShort().toInt
    Sector(sectorType, tag, floorHeight, ceilingHeight)
  }

  private def extractSectorsForLevel(level: Level): List[Sector] = {
    val sectorBytes = level.lumps("SECTORS").data
    (sectorBytes.sliding(26, 26) map extractSector).toList
  }

  private def extractSideDef(bytes: List[Byte], level: Level): SideDef = {
    val sectorId = ByteBuffer.wrap(bytes.slice(28, 30).toArray).order(ByteOrder.LITTLE_ENDIAN).getShort().toInt
    val sector = level.sectors.get(sectorId)
    SideDef(sectorId, Some(sector))
  }

  private def extractSideDefsForLevel(level: Level): List[SideDef] = {
    val sideDefBytes = level.lumps("SIDEDEFS").data
    sideDefBytes.sliding(30, 30).map(extractSideDef(_, level)).toList
  }

  private def extractExit(level: Level): Vertex = {
    val normalExitLines = level.lines.get.find( line => {
      List(11, 52, 197).contains(line.lineType.get)
    })
    normalExitLines match {
      case Some(x) => x.a
      case _ => level.playerStart.get
    }
  }

  def doorLinedefs(level: Level): List[DoorSwitch] = {
    val doorLines = level.lines.getOrElse(List()).filter(line => DOOR_SWITCH_TYPES.contains(line.lineType.getOrElse(-1)))
    doorLines map DoorSwitch.fromWadLine
  }

  private def addMiscDataToLevel(level: Level): Level = {
    //TODO: make this better - only create level once.  Need to do sectors first, then sidedefs, then linedefs
    val levelWithSectors = level.setSectors(extractSectorsForLevel(level))
    val levelWithSideDefs = levelWithSectors.setSideDefs(extractSideDefsForLevel(levelWithSectors))
    val levelWithLines = levelWithSideDefs.setLines(extractLinesForLevel(levelWithSideDefs))

    levelWithLines.setPlayerStart(extractPlayerStart(levelWithLines))
    levelWithLines.setExit(extractExit(levelWithLines))
    levelWithLines.setDoorSwitches(doorLinedefs(levelWithLines))
    levelWithLines
  }

  private def extractThing(bytes: List[Byte]): Thing = {
    val position = extractVertex(bytes.take(4))
    val angle = ByteBuffer.wrap(bytes.slice(4, 6).toArray).order(ByteOrder.LITTLE_ENDIAN).getShort().toInt
    val doomId = ByteBuffer.wrap(bytes.slice(6, 8).toArray).order(ByteOrder.LITTLE_ENDIAN).getShort().toInt
    Thing(position, angle, doomId)
  }

  private def extractPlayerStart(level: Level): Vertex = {
    val thingsLump = level.lumps("THINGS").data
    val things = (thingsLump.sliding(10, 10) map extractThing).toList
    things.find(_.doomId == 1).get.position
  }

  def createWad(fromFile: String = "/home/neil/Downloads/doom1.wad"): Wad = {
    val byteStream = createStream(fromFile)
    val wadType = extractWadType(byteStream)
    val numLumps = extractNumLumps(byteStream)
    val data = extractData(byteStream)
    val lumps = extractLumps(byteStream, data)
    val levels = extractLevels(None, lumps, "START").map(addMiscDataToLevel)
    levels foreach {level => level.quadTree = Some(LineMXQuadTree.createQuadTree(level))}

    Wad(wadType, numLumps, levels)
  }
}

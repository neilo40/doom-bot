
import math.{sqrt,pow}

object Multiplayer extends App {

  val BLAST_RADIUS = 300
  val HUNT_MOVE_SPEED = 100
  val FINISHING_MOVE_SPEED = 5
  val SLEEP = 100

  while (true) {
    val allPlayers = GameInterface.getPlayers
    val (players, enemies) = allPlayers partition(_.consoleplayer == true)

    val player = players.head

    //FIXME: This doesn't work
    if (player.health <= 0) GameInterface.use()
    selectBestWeapon(player)

    //TODO: go and get armour?

    if (!player.weapons.shotgun){
      //get shotgun
      val shotgun = GameInterface.getAllShotguns.minBy(_.distance)
      GameInterface.turnTowards(player, shotgun.position)
      GameInterface.move(HUNT_MOVE_SPEED)
    } else if (player.ammo.Shells < 1) {
      //get shells
      val targetShells = GameInterface.getAllShells.minBy(_.distance)
      GameInterface.turnTowards(player, targetShells.position)
      GameInterface.move(HUNT_MOVE_SPEED)
    } else {
      if (enemies.nonEmpty) {
        //kill someone
        val biggestThreat = nearestEnemy(player, enemies)

        GameInterface.turnTowards(player, biggestThreat.position)

        if (distance(player.position, biggestThreat.position) < BLAST_RADIUS) {
          //TODO: check if facing enemy
          GameInterface.shoot()
          GameInterface.move(FINISHING_MOVE_SPEED)
        } else {
          GameInterface.move(HUNT_MOVE_SPEED)
        }
      }
    }
    Thread.sleep(SLEEP)
  }

  def nearestEnemy(player: Player, enemies: List[Player]): Player = {
    val aliveEnemies = enemies filter (_.health > 0)
    aliveEnemies.minBy(e => distance(e.position, player.position))
  }

  def distance(v1: Vertex, v2: Vertex): Double =
    sqrt(pow(v1.x - v2.x, 2) + pow(v1.y - v2.y, 2))

  def selectBestWeapon(player: Player): Unit =
    if (player.weapon != 3 && player.weapons.shotgun) GameInterface.selectWeapon(3)
}

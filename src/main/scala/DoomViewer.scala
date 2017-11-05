import scala.concurrent.Future
import scalafx.application
import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.paint.Color._
import scalafx.scene.control.{Button, ComboBox, ToggleButton}
import scalafx.scene.input.MouseEvent
import scalafx.scene.layout.{HBox, Pane, VBox}
import scalafx.Includes._
import scalafx.event.ActionEvent
import scala.concurrent.ExecutionContext.Implicits.global

object DoomViewer extends JFXApp{

  val mapPane: Pane = new Pane{
    onMouseClicked = (e: MouseEvent) => ViewController.paneClicked(e.x, e.y)
  }

  val boundingBoxesButton: ToggleButton = new ToggleButton{
    text = "Show Bounds"
    onMouseClicked = (e: MouseEvent) => ViewController.boundingBoxToggle(this.selected.value)
  }

  val showQuadTreeButton: ToggleButton = new ToggleButton{
    text = "Show QuadTree"
    onMouseClicked = (e: MouseEvent) => ViewController.quadTreeToggle(this.selected.value)
  }

  val mapComboBox: ComboBox[String] = new ComboBox[String]{
    onAction = (e: ActionEvent) => {
      boundingBoxesButton.selected = false
      showQuadTreeButton.selected = false
      Future {ViewController.changeLevel(this.value.value)}
    }
  }

  val drawObjectsButton: Button = new Button {
    text = "Draw Objects"
    onMouseClicked = (e: MouseEvent) => Future {ViewController.drawWorldObjects()}
  }

  val loadWadButton: Button = new Button {
    text = "Reload Wad"
    onMouseClicked = (e: MouseEvent) => Future {ViewController.loadWad()}
  }

  val generateGridButton: Button = new Button {
    text = "Generate Grid"
    onMouseClicked = (e: MouseEvent) => Future {ViewController.generatePath()}
  }

  val findPathButton: Button = new Button {
    text = "Find Path"
    onMouseClicked = (e: MouseEvent) => Future {AStar.findPathCallback(ViewController.getLevel)}
  }

  val clearButton: Button = new Button {
    text = "Clear"
    onMouseClicked = (e: MouseEvent) => Future{ViewController.clearScreen()}
  }

  val startBotButton: Button = new Button {
    text = "Start Bot"
    onMouseClicked = (e: MouseEvent) => {
      ViewController.BOT_RUNNING = true
      Future{ViewController.startBot()}
      //ViewController.startBot()  // use this line to get stacktrace if the bot crashes
    }
  }

  val stopBotButton: Button = new Button {
    text = "Stop Bot"
    onMouseClicked = (e: MouseEvent) => {
      ViewController.BOT_RUNNING = false
    }
  }

  stage = new application.JFXApp.PrimaryStage {
    width = ViewController.CANVAS_WIDTH + ViewController.BUTTON_BAR_WIDTH + 200
    height = ViewController.CANVAS_HEIGHT
    title = "Doom Viewer"
    scene = new Scene {
      fill = White
      content = new HBox {
        children = Seq(
          new VBox{
            prefWidth = ViewController.BUTTON_BAR_WIDTH
            children = Seq(loadWadButton, mapComboBox, drawObjectsButton, boundingBoxesButton, showQuadTreeButton,
              generateGridButton, findPathButton, clearButton, startBotButton, stopBotButton)
          },
          mapPane
        )
      }
    }
  }

  stage.show()
  ViewController.loadWad()
}

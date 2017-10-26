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
    onMouseClicked = (e: MouseEvent) => WadViewUtils.paneClicked(e.x, e.y)
  }

  val boundingBoxesButton: ToggleButton = new ToggleButton{
    text = "Show Bounds"
    onMouseClicked = (e: MouseEvent) => WadViewUtils.boundingBoxToggle(this.selected.value)
  }

  val showQuadTreeButton: ToggleButton = new ToggleButton{
    text = "Show QuadTree"
    onMouseClicked = (e: MouseEvent) => WadViewUtils.quadTreeToggle(this.selected.value)
  }

  val mapComboBox: ComboBox[String] = new ComboBox[String]{
    onAction = (e: ActionEvent) => {
      boundingBoxesButton.selected = false
      showQuadTreeButton.selected = false
      WadViewUtils.changeLevel(this.value.value)
    }
  }

  val loadWadButton: Button = new Button {
    text = "Reload Wad"
    onMouseClicked = (e: MouseEvent) => Future {WadViewUtils.loadWad()}
  }

  val generateGridButton: Button = new Button {
    text = "Generate Grid"
    onMouseClicked = (e: MouseEvent) => Future {WadViewUtils.generatePath()}
  }

  val findPathButton: Button = new Button {
    text = "Find Path"
    onMouseClicked = (e: MouseEvent) => Future {AStar.findPathCallback()}
  }

  val clearButton: Button = new Button {
    text = "Clear"
    onMouseClicked = (e: MouseEvent) => Future{WadViewUtils.clearScreen()}
  }

  val startBotButton: Button = new Button {
    text = "Start Bot"
    onMouseClicked = (e: MouseEvent) => Future{WadViewUtils.startBot()}
  }

  val stopBotButton: Button = new Button {
    text = "Stop Bot"
    //TODO: how do we stop the bot?
  }

  stage = new application.JFXApp.PrimaryStage {
    width = WadViewUtils.CANVAS_WIDTH + WadViewUtils.BUTTON_BAR_WIDTH + 200
    height = WadViewUtils.CANVAS_HEIGHT
    title = "Doom Viewer"
    scene = new Scene {
      fill = White
      content = new HBox {
        children = Seq(
          new VBox{
            prefWidth = WadViewUtils.BUTTON_BAR_WIDTH
            children = Seq(loadWadButton, mapComboBox, boundingBoxesButton, showQuadTreeButton, generateGridButton,
              findPathButton, clearButton, startBotButton, stopBotButton)
          },
          mapPane
        )
      }
    }
  }

  stage.show()
  WadViewUtils.loadWad()
}

import scalafx.application
import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.paint.Color._
import scalafx.scene.control.{Button, ComboBox, ToggleButton}
import scalafx.scene.input.MouseEvent
import scalafx.scene.layout.{HBox, Pane, VBox}
import scalafx.Includes._
import scalafx.event.ActionEvent

object DoomViewer extends JFXApp{

  val mapPane = new Pane{
    onMouseClicked = (e: MouseEvent) => WadViewUtils.paneClicked(e.x, e.y)
  }

  val boundingBoxesButton = new ToggleButton{
    text = "Show Bounds"
    onMouseClicked = (e: MouseEvent) => WadViewUtils.boundingBoxToggle(this.selected.value)
  }

  val showQuadTreeButton = new ToggleButton{
    text = "Show QuadTree"
    onMouseClicked = (e: MouseEvent) => WadViewUtils.quadTreeToggle(this.selected.value)
  }

  val mapComboBox = new ComboBox[String]{
    onAction = (e: ActionEvent) => {
      boundingBoxesButton.selected = false
      showQuadTreeButton.selected = false
      WadViewUtils.changeLevel(this.value.value)
    }
  }

  val loadWadButton = new Button {
    text = "Reload Wad"
    onMouseClicked = (e: MouseEvent) => WadViewUtils.loadWad()
  }

  val generatePathButton = new Button {
    text = "Generate Path"
    onMouseClicked = (e: MouseEvent) => WadViewUtils.generatePath()
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
            children = Seq(loadWadButton, mapComboBox, boundingBoxesButton, showQuadTreeButton, generatePathButton)
          },
          mapPane
        )
      }
    }
  }

  stage.show()
  WadViewUtils.loadWad()
}

import scalafx.application
import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.paint.Color._
import scalafx.scene.control.{Button, ComboBox}
import scalafx.scene.input.MouseEvent
import scalafx.scene.layout.{HBox, Pane, VBox}
import scalafx.Includes._
import scalafx.event.ActionEvent

object DoomViewer extends JFXApp{

  val mapPane = new Pane{}

  val mapComboBox = new ComboBox[String]{
    onAction = (e: ActionEvent) => WadViewUtils.changeLevel(mapPane, this.value.value)
  }

  stage = new application.JFXApp.PrimaryStage {
    width = WadViewUtils.CANVAS_WIDTH + WadViewUtils.BUTTON_BAR_WIDTH
    height = WadViewUtils.CANVAS_HEIGHT
    title = "Doom Viewer"
    scene = new Scene {
      fill = White
      content = new HBox {
        children = Seq(
          new VBox{
            prefWidth = WadViewUtils.BUTTON_BAR_WIDTH
            children = Seq(new Button {
                text = "Load Wad"
                onMouseClicked = (e: MouseEvent) => WadViewUtils.loadWad(mapPane, mapComboBox)
              },
              mapComboBox)
          },
          mapPane
        )
      }
    }
  }
}

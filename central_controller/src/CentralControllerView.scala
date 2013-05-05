import scala.swing._
import scala.swing.event._
import java.awt.GridLayout

class CentralControllerView extends MainFrame {

	private val startButton = new Button("Start")
	private val stopButton = new Button("Stop")
	private val textArea = new TextArea	
	resizable = false
	
	title = "Central Controller GUI"
	val s = new Dimension(300,200)
    minimumSize = s
    maximumSize = s
    preferredSize = s
	
	import javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE
    peer.setDefaultCloseOperation(DO_NOTHING_ON_CLOSE)
	
	private def showCloseDialog() {
		Dialog.showConfirmation(parent = null,
			title = "Exit",
			message = "Are you sure you want to quit?"
		) match {
			case Dialog.Result.Ok => sys.exit(0)
			case _ => ()
		}
	}
	
	override def closeOperation() { showCloseDialog() }
	
	contents = new BorderPanel {
		add(new FlowPanel {
			contents += startButton
			contents += stopButton
		}, BorderPanel.Position.North)
		add(textArea, BorderPanel.Position.Center)
	}
	
	visible = true
	
	def setStartOperation(doOperation : Unit => Unit) {
		startButton.listenTo(startButton)
    	startButton.reactions += { 	
    		case e:ButtonClicked => {
    			doOperation()
    		}
    	}
	}
	
	def setStopOperation(doOperation : Unit => Unit) {
		stopButton.listenTo(startButton)
    	stopButton.reactions += { 	
    		case e:ButtonClicked => {
    			doOperation()
    			println("Clicked")
    		}
    	}
	}
	
	def write(message : String) {
		textArea.text += message
	}
	
}

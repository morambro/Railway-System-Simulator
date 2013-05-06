import scala.swing._
import scala.swing.event._
import java.awt.GridLayout
import java.util.Date
import java.text.SimpleDateFormat

class MyTextArea extends TextArea {
	
	val format = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
	
	lineWrap = true
	
	def addRow(t:String) {
		this.text += "[" + format.format(new Date) + "] " + t + "\n"
	}
	
}

class CentralControllerView extends MainFrame {

	private val startButton = new Button("Start")
	private val stopButton = new Button("Stop")
	private val textArea = new MyTextArea
	
	
	title = "Central Controller GUI"
	val s = new Dimension(500,400)
	resizable = false
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
	
	import javax.swing.ScrollPaneConstants._
	
	val sc = new ScrollPane(textArea){
		horizontalScrollBarPolicy = new scala.swing.ScrollPane.BarPolicy.Value(HORIZONTAL_SCROLLBAR_NEVER,VERTICAL_SCROLLBAR_ALWAYS)
	}
	
	contents = new BorderPanel {
		add(new FlowPanel {
			contents += startButton
			contents += stopButton
		}, BorderPanel.Position.North)
		add(sc, BorderPanel.Position.Center)
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
	
	def disableButtons() {
		startButton.enabled = false
		stopButton.enabled = false
	}
	
	def setStopOperation(doOperation : Unit => Unit) {
		stopButton.listenTo(stopButton)
    	stopButton.reactions += { 	
    		case e:ButtonClicked => {
    			doOperation()
    			println("Clicked")
    		}
    	}
	}
	
	def write(message : String) {
		textArea.addRow(message)
	}
	
}

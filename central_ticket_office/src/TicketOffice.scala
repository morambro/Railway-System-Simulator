object Main extends App {
	override def main(argv : Array[String]) {
		if (argv.length < 3) {
			println("""Please insert:
   1) The ticket server address;
   2) The name server address;
   3) The json environment description.""")
			return
		}
		
		println(argv(0))
		println(argv(1))
		println(argv(2))
		
		// Create the Message Receiver
		//MessagesReceiver(argv(0),argv(2))
		MessagesReceiver.start
		MessagesReceiver ! Init(argv(0))
		
		// Set Name Server Address
		TicketCreator.NAME_SERVER_ADDRESS = argv(1)
		
		// Start BookingManager and PrintsSerializer actors
		BookingManager.start
		PrintsSerializer.start
	}
}



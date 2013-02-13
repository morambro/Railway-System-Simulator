default:
	gnatmake -gnat2012 -D out src/main.adb
	
clean:
	rm out/*
	rm main

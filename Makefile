default:
	gnatmake -D out src/main.adb
	
clean:
	rm out/*
	rm main

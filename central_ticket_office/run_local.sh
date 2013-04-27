cd out
# scala -classpath  ../../lib/yami4.jar json-smart-1.1.1.jar Main $@
scala -classpath  ../../lib/yami4.jar:../../lib/json-smart-1.1.1.jar Main  tcp://localhost:9999 tcp://localhost:1234 ../railway/res/links.json
cd ..

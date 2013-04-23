cd central_ticket_office/out
# scala -classpath  ../../lib/yami4.jar json-smart-1.1.1.jar Main $@
scala -classpath  ../../lib/yami4.jar:../json-smart-1.1.1.jar Main ../railway/res/links.json tcp://localhost:1234 tcp://localhost:9999
cd ..

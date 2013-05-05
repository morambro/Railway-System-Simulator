cd central_ticket_office/out
# scala -classpath  ../../lib/yami4.jar json-smart-1.1.1.jar Main $@
scala -classpath  ../out/:../../lib/yami4.jar:../../lib/json-smart-1.1.1.jar:../../lib/joda-time-2.2.jar Main $@
cd ..

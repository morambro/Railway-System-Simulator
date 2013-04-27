echo "Loading Name Server..." 
gnome-terminal --hide-menubar --title="Name Server" --command="sh name_server/run.sh tcp://localhost:1234"
echo "done"

echo "Load Central Controller..."
gnome-terminal --hide-menubar --title="Central Controller" --command="sh central_controller/run.sh tcp://localhost:8888 tcp://localhost:1234"
echo "done"

echo "Load Central Ticket Office..." 
gnome-terminal --hide-menubar --title="Central Ticket Office" --command="sh central_ticket_office/run.sh tcp://localhost:9999 tcp://localhost:1234 ../railway/res/links.json"
echo "done"

echo "Load Http Server..."
gnome-terminal --hide-menubar --title="Server" --command="sh run_play.sh"
echo "done"

echo "Load Node_1..."
gnome-terminal --hide-menubar --title="Node_1" --command="sh railway/run.sh Node_1 tcp://localhost:4455"
echo "done"

echo "Load Node_2..."
gnome-terminal --hide-menubar --title="Node_2" --command="sh railway/run.sh Node_2 tcp://localhost:5544"
echo "done"

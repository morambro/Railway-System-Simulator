echo "Loading Name Server..." 
gnome-terminal --hide-menubar --title="Name Server" --command="sh name_server/run.sh tcp://localhost:1234"
echo "done"

echo "Load Central Controller..."
gnome-terminal --hide-menubar --title="Central Controller" --command="sh central_controller/run.sh tcp://localhost:8888 tcp://localhost:1234"
echo "done"

echo "Load Central Ticket Office..." 
gnome-terminal --hide-menubar --title="Central Ticket Office" --command="sh central_ticket_office/run.sh tcp://localhost:9999 tcp://localhost:1234 ../railway/res/links.json"
echo "done"

echo "Load Node_1..."
gnome-terminal --hide-menubar --title="Node_1" --command="sh railway/run.sh Node_1 tcp://localhost:4455"
echo "done"

echo "Load Node_2..."
gnome-terminal --hide-menubar --title="Node_2" --command="sh railway/run.sh Node_2 tcp://localhost:5544"
echo "done"

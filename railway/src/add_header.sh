FILES=$(grep -r -L -w 'GNU')
HEADER="header"
for f in $FILES 
do
	echo "Adding header to $f"
	cat $HEADER $f >'e' 
	mv 'e' $f
done 


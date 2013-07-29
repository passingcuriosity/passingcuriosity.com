
build:
	site build


clean:
	site clean

sync:
	rsync -vr _site/ forceit.in:/var/www/passingcuriosity.com/htdocs

push: build sync

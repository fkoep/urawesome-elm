all:
	elm make src/Main.elm --output=elm.js

watch:
	while true; do \
		inotifywait -e modify,create,delete,move -r ./src && \
		make \
	; done 

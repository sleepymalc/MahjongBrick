all:
	elm make src/Breakout.elm --output build/elm.js
	elm make src/Breakout.elm
	cp index.html build/index.html
	cp -r src/img build
	cp -r src/bgm build
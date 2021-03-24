all:
	elm make src/Xuan.elm --output build/elm.js
	elm make src/Xuan.elm
	cp index.html build/index.html
	cp -r src/img build
	cp -r src/bgm build
app.js: Main.elm
	elm-make --output app.js
	# fragile :|
	sed -i '/addPublicModule(Elm\['Main'\])/a Elm.Main.embed(document.getElementById("app"));' app.js

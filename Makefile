app.js: Main.elm
	elm-make --output app.js
	# fragile :|
	gsed -i '/addPublicModule(Elm\['Main'\])/a Elm.Main.embed(document.getElementById("app"));' app.js

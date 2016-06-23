dist/js/app.bundle.js: Main.elm
	mkdir -p dist/js
	elm-make Main.elm --output $@
	# fragile :|
	gsed -i '/addPublicModule(Elm\[.Main.\]/a Elm.Main.embed(document.getElementById("app"), {token: window.apiToken, apiUrl: window.config.apiUri});' $@

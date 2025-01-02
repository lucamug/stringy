// Initialize your Elm program
var app = Elm.Main.init({
    node: document.getElementById("app"),
    flags: 
        { env: env
        , locationHref: location.href
        , innerWidth: window.innerWidth
        }
});

// Inform app of browser navigation (the BACK and FORWARD buttons)
window.addEventListener('popstate', function () {
    app.ports.onUrlChange.send(location.href);
});

// Change the URL upon request, inform app of the change.
app.ports.pushUrl.subscribe(function(args) {
    history.pushState({}, '', args.url);
    if (args.sendItBack) {
        app.ports.onUrlChange.send(location.href);
    }
});

// Updates HTML outside Elm responsability
app.ports.changeMeta.subscribe((args) => {
	const element = document.querySelector(args.querySelector);
	if (element) {element[args.fieldName] = args.content;}
});

const app = Elm.Main.init({ flags: localStorage.session || null });

app.ports.sendDarkMode.subscribe((isDark) => {
  const html = document.querySelector("html") || null;
  if (isDark) {
    html.setAttribute("class", "dark");
    document.body.style = "background-color: black";
  } else {
    html.removeAttribute("class", "dark");
    document.body.style = "";
  }
});

app.ports.storeSession.subscribe((session) => {
  localStorage.session = session;

  // Report that the new session was stored succesfully.
  setTimeout(() => {
    app.ports.onSessionChange.send(session);
  }, 0);
});

document.documentElement.addEventListener("click", (e) => {
  app.ports.reportClick.send(true);
});

window.addEventListener(
  "storage",
  (event) => {
    if (event.storageArea === localStorage && event.key === "session") {
      app.ports.onSessionChange.send(event.newValue);
    }
  },
  false
);

const app = Elm.Main.init({ flags: localStorage.session || null });

app.ports.isDarkModeLS.send(localStorage.getItem("darkMode") === "1");
app.ports.sendDarkMode.subscribe((isDark) => {
  const html = document.querySelector("html") || null;
  if (isDark) {
    localStorage.setItem("darkMode", "1");
    html.setAttribute("class", "dark");
    document.body.style = "background-color: black";
  } else {
    localStorage.removeItem("darkMode");
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
    console.log(event);
    if (event.storageArea === localStorage && event.key === "session") {
      app.ports.onSessionChange.send(event.newValue);
    }
    if (event.storageArea === localStorage && event.key === "darkMode") {
      app.ports.onDarkMode.send(event.newValue);
    }
  },
  false
);

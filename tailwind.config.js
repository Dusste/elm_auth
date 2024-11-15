/** @type {import('tailwindcss').Config} */
export default {
  darkMode: "selector",
  content: ["./src/**/*.elm", "./*.js", "./public/index.html"],
  theme: {
    extend: {
      height: {
        128: "32rem",
      },
    },
    // colors: {
    //   // Using modern `rgb`
    //   primary: "rgb(var(--color-primary))",
    //   secondary: "rgb(var(--color-secondary))",

    //   // Using modern `hsl`
    //   primary: "hsl(var(--color-primary))",
    //   secondary: "hsl(var(--color-secondary))",

    //   // Using legacy `rgba`
    //   primary: "rgba(var(--color-primary), 1)",
    //   secondary: "rgba(var(--color-secondary), 1)",
    // },
  },
  plugins: [
    require("@tailwindcss/forms")({
      strategy: "base", // only generate global styles
    }),
    function ({ addBase, theme }) {
      addBase({
        h1: { fontSize: theme("fontSize.3xl") },
        h2: { fontSize: theme("fontSize.2xl") },
        h3: { fontSize: theme("fontSize.xl") },
        h4: { fontSize: theme("fontSize.l") },
      });
    },
  ],
  safelist: [
    { pattern: /^bg-(red|green|yellow|sky)-200$/ },
    { pattern: /^border-(red|green|yellow|sky)-500$/ },
    { pattern: /^text-(red|green|yellow|sky)-500$/ },
  ],
};

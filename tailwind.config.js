/** @type {import('tailwindcss').Config} */
export default {
  content: ["./src/**/*.elm", "./*.js", "./public/index.html"],
  theme: {
    extend: {
      height: {
        128: "32rem",
      },
    },
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
};

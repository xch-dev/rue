/** @type {import('tailwindcss').Config} */
module.exports = {
  darkMode: "selector",
  content: {
    files: ["*.html", "./src/**/*.rs", "../rue-web-derive/src/**/*.rs"],
  },
  plugins: [],
};

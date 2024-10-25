/** @type {import('tailwindcss').Config} */
module.exports = {
  content: ["./src/**/*.{purs,html,js}"],
  theme: {
    extend: {
      fontFamily: {
        Noto: ["Noto Sans JP", "sans-serif"],
        NotoEmoji: ["Noto Color Emoji", "sans-serif"],
        NotoJP: ["Noto Serif JP", "serif"]
      },
    },
  },
  plugins: [],
}


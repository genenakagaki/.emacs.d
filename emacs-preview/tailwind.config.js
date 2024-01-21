/** @type {import('tailwindcss').Config} */
const colors = require('tailwindcss/colors');

const colorScheme = {
  primary: '#3D1F03',
}

module.exports = {
  content: ["./src/**/*.cljs"],
  theme: {
    colors: {
      transparent: 'transparent',
      current: 'currentColor',
      black: colors.black,
      white: colors.white,
      gray: colors.gray,
      primary: {
        DEFAULT: colorScheme.primary,
        variant: '#232F33',
      },
      secondary: {
        DEFAULT: '#F05800',
        variant: '#29A198'
      },
      background: '#FCF0D9', 
      surface: '#FCF0D9',
      error: '#B00020',
      on: {
        primary: '#FFFFFF',
        secondary: '#FFFFFF',
        background: colorScheme.primary,
        surface: '#000000',
        error: '#FFFFFF',
      },
    },
    extend: {
      fontSize: {
        subtitle: '1rem',
        body: '',
      },
      opacity: {
        'high': '.87',
        'med': '.60',
        'low': '.30',
      }
    },
  },
  plugins: [
    require('@tailwindcss/typography'),
  ],
}

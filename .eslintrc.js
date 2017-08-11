module.exports = {
  "extends": [
    "airbnb-base",
    "plugin:node/recommended",
    "plugin:react/recommended",
  ],
  "plugins": [
    "import",
    "node",
    "react",
  ],
  "env": {
    "es6": true,
    "node": true,
  },
  "parserOptions": {
    "ecmaFeatures": {
      "jsx": true
    }
  },
  "rules": {
    // ref: http://qiita.com/M-ISO/items/4cd183e2496c2937a53e
    "no-continue": "off",
    "no-underscore-dangle": "off",
    "arrow-body-style": "off",

    // plugins
    "import/no-unresolved": "off",
    "node/no-unsupported-features": ["error", {"version": 6}],
    "node/no-missing-require": "off",
  }
};

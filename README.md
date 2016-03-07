## RFC6570 URI templates [![Build Status](https://travis-ci.org/deadtrickster/cl-uri-template.svg?branch=master)](https://travis-ci.org/deadtrickster/cl-uri-template) [![Coverage Status](https://coveralls.io/repos/deadtrickster/cl-uri-template/badge.svg?branch=master&service=github)](https://coveralls.io/github/deadtrickster/cl-uri-template?branch=master)

Currently implemented using tons of regex, subseqs and other hairy stuff.
Should be usable anyway.

Uses test cases from [here](https://github.com/uri-templates)

```lisp
(expand "http:www{.domain*}{/top,next}{?q:20}"
  {i "domain" "qwe.com" "top" "toptoptop" "next" "qwe" "q" "zaqxswcdevfrbgtnhymju,ki"})

"http:www.qwe.com/toptoptop/qwe?q=zaqxswcdevfrbgtnhymj"
```

## License
MIT


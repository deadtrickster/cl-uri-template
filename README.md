## RFC6570 URI templates

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


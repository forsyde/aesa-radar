# bird2code.sed
/^>/ !p
/^>/ {
  i\
```haskell

  :loop
  N
  /\n>[^\n]*$/{
    b loop
  }
  s/^> //
  s/\(\n\)> /\1/g
  s/\n$//
  a\
```\

  p
}

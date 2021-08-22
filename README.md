A comment is given a name and a body with the syntax:
```
# identifier1 #
this is the body
##

# another2 #
another comment :p
##
```

The main function composes a presentation:
```
main:
  identifier1
  another2
```

A function is written:
```
increment:
  with x do x + 1 please
```

We have lists:
```
l123: [1,2,3]

listId:
  with ls do
    when ls is
      [] do [] please
      for x:xs do x : xs please
  please
```

A comment can be created from another value by applying the function `comment` to it.
The main function can use "calculated" comments if they appear inside parenthesis
```
main:
  identifier1
  (comment 1)
  (comment [1,2,3])
  (comment ((with x do x + 1) 1))
  another2
```

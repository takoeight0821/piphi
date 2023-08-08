# Piphi Language Reference

## Examples

### Hello World

```
runIO : IO () -> ()
print : String -> IO ()
do : {a} -> a
_$_ : (a -> b) -> a -> b

main: () = runIO $ do {
  print "Hello World!"
}
```

### Fibonacci

A **function** that has arguments are defined as comma separated list of **clause**s.

```
def fib: Int -> Int = {
  0 -> 0,
  1 -> 1,
  n -> fib (n - 1) + fib (n - 2),
}
```

### Fibonacci Stream

Each field of a **codata** is evaluated lazily.

```
codata Stream = {
  head : Int,
  tail : Stream,
}

def fib : Stream = {
  head # -> 1,
  head (tail #) -> 1,
  tail (tail #) -> zipWith (+) fib (tail fib),
}
```

### Cat and Dog

```
codata Animal = {
  name : String,
}

codata Cat = {
  meow : String,
} + Animal

codata Dog = {
  bark : String,
} + Animal

def cat : Cat = {
  name # -> "cat",
  meow # -> "meow",
}

def dog : Dog = {
  name # -> "dog",
  bark # -> "bark",
}

def rename : Animal -> String -> Animal = {
  name (# a newName) -> { name # -> newName },
}

def catInHouse : Cat = rename cat "Taro"
```

# Piphi Language Reference

## Examples

### Hello World

```
# getLine : IO String ~ (String -> Answer) -> Answer
# print : String -> IO () ~ String -> (() -> Answer) -> Answer
# type IO a = (a -> Answer) -> Answer

def main: IO () = { return ->
  with line = getLine
  print "Hello World!" return
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

Functions without arguments are desugared to functions with a single argument of type `()`.

### Fibonacci Stream

Each field of a **codata** is evaluated lazily.

```
type Stream = {
  head : Int,
  tail : Stream,
}

def fib : Stream = {
  head # -> 1,
  head (tail #) -> 1,
  tail (tail #) -> zipWith (+) fib (tail fib),
}
```

```
type Lazy a = {
  force : a,
}

lazy : {a} -> Lazy a
lazy a = { force # -> a () }
```

### Cat and Dog

```
type Animal = {
  name : String,
}

type Cat = Animal + {
  meow : String,
}

type Dog = Animal + {
  bark : String,
}

type Car = {
  name : String,
  speed : Int,
}

def cat : Cat = {
  name # -> "cat",
  meow # -> "meow",
}

def dog : Dog = {
  name # -> "dog",
  bark # -> "bark",
}

def rename : Animal -> String -> Animal = {
  name (# a newName) -> { name # -> newName }
}

-- rename returns Animal instead of Cat
-- `a as T` casts `a` to `T` (only works for upcast)
def catInHouse : Animal = rename (cat as Animal) "Taro"

def car : Car = { name # -> "car", speed # -> 100 }

-- rename returns Animal instead of Car
def newCar : Animal = rename (car as Animal) "new car"

-- ... matches any field and delegate to the original codata
def genericRename : { name : String | r } -> String -> { name : String | r } = {
  name (# a newName) -> { name # -> newName, ... # -> ... a },
  ... (# a _) -> ... a,
}

def catInHouse2 : Cat = genericRename cat "Taro"
def newCar2 : Car = genericRename car "new car"
```

### Inline function

```
def add : Int -> Int -> Int = {
  a b -> a + b,
}

def add2 : Int -> Int = add 2

def add3 : Int -> Int = { a -> add 3 a }

def add4 : Int -> Int = {
  a ->
    # Define a function `inc` and call it
    def inc : Int -> Int = { b -> b + 1 }
    inc (add3 a)
}
```
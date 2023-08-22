# Piphi （ドラフト）

## 概要

PiPhi（仮称）は、関数とオブジェクトの融合を目指したプログラミング言語です。
PiPhiでは、**余パターン**を使った統一的な記法で、関数とオブジェクトの両方を定義できます。
また、近年様々な言語で使われるようになった**代数的データ型**と**パターンマッチ**もサポートしています。

## コンパクトな関数プログラミング言語としてのPiPhi

PiPhiのベースとなっているのは、LispやMLなどの関数プログラミング言語です。

### 関数

`関数 引数1 引数2`のように、関数と引数を並べることで、関数を呼び出します。
`--`から始まる行はコメントです。

```
-- result: 3
add 1 2
```

関数は、`{}`と`#`、`->`を使って次のように記述します。

```
-- 引数１つをそのまま返す恒等関数
def id = { # x -> x }

-- 三つの引数を足す関数
def add3 = { # x y z -> add x (add y z) }
```

定義する関数を`#`としたとき、`# x`の形の式を計算した結果は`x`である、という気持ちで読むと良いでしょう。
例えば、`add3 1 2 3`という式は、`add3 x y z`の形をしています。
よって、`x = 1, y = 2, z = 3`のときの`add x (add y z)`の値である`6`が返ります。

### 代数的データ型

代数的データ型は、木構造のデータ型を表現する仕組みです。*タグ付き共用体*や*ヴァリアント*とも呼ばれます。

例として、整数の四則演算の式を表す代数的データ型を定義します。

```
data Expr = Num Int | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr
```

`Expr`型を使うと、`1 + 2 * 3`は、`Mul (Add (Num 1) (Num 2)) (Num 3)`と表現できます。

代数的データ型は、*パターンマッチ*と組み合わせることで真価を発揮します。
`Expr`型の値を受け取って、実際に計算した結果を返す関数を定義してみましょう。

```
def calc = {
  # (Num n) -> n,
  # (Add e1 e2) -> add (calc e1) (calc e2),
  # (Sub e1 e2) -> sub (calc e1) (calc e2),
  # (Mul e1 e2) -> mul (calc e1) (calc e2),
  # (Div e1 e2) -> div (calc e1) (calc e2),
}
```

例えば、`calc (Add (Num 1) (Num 2))`という式は、3行目のパターン`# (Add e1 e2)`にマッチします。

## 余データ

代数的データ型を使ったプログラムでは、値がどのように構築されたか（パターン）をもとに処理を行います。
*余データ*を使うと、値がどのように分解できるか（余パターン）をもとに処理を行うことができます。

最も単純な余データとして、2つの整数のペアを表す`Pair`型を定義します。

```
codata Pair = {
  .first : Int,
  .second : Int,
}
```

`Pair`型の値を定義しましょう。`pair`は、`1`と`2`のペアです。

```
def pair = {
  .first # -> 1,
  .second # -> 2,
}
```

`.first pair`は`1`を返し、`.second pair`は`2`を返します。
関数定義のときと同様に、定義する余データを`#`としたとき、`.first #`の形の式の値は`1`である、という気持ちで読むことができます。

余パターンの巧妙な例として、**無限長**の整数のリストを表す`Stream`型を定義します。

```
codata Stream = {
  .head : Int,
  .tail : Stream,
}
```

`Stream`型の値を定義しましょう。`iota n`は、`n, n+1, n+2, ...`という整数のリストです。

```
def iota = {
  .head (# n) -> n,
  .tail (# n) -> iota (add n 1),
}
```

`#`が`iota`自身を表していると思って、いくつかの例を試してみましょう。

- `.head (iota 1)` -> `1`
- `.tail (iota 1)` -> `iota (add 1 1)` -> `iota 2`
- `.head (.tail (iota 1))` -> `.head (iota 2)` -> `2`

`iota n`の各フィールドの値は、`.head`や`.tail`が適用されたときに初めて計算されます。
このため、`iota n`は無限ループすることなく無限長の整数のリストを表現できます。

## Examples

### Hello World

```
# getLine : IO String ~ (String -> Answer) -> Answer
# print : String -> IO () ~ String -> (() -> Answer) -> Answer
# type IO a = (a -> Answer) -> Answer

def main: IO () = { return ->
  with line = getLine;
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
codata Stream = {
  .head : Int,
  .tail : Stream,
}

def fib : Stream = {
  .head # -> 1,
  .head (.tail #) -> 1,
  .tail (.tail #) -> zipWith (+) fib (.tail fib),
}
```

### Inline function

```
def add : Int -> Int -> Int = {
  # a b -> a + b,
}

def add2 : Int -> Int = add 2

def add3 : Int -> Int = { # a -> add 3 a }

def add4 : Int -> Int = {
  # a ->
    -- Define a function `inc` and call it
    def inc : Int -> Int = { b -> b + 1 };
    inc (add3 a)
}
```
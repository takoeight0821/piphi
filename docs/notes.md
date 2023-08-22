## デザインノート

- ASTはイミュータブル・複製しまくる
- アイディア元：malgo-lang/malgo (by author), zachsully/dl

## codataのflatten

```
F(# x y ..) = # x y ...
F(.x #) = .x #
F(.x (# a b ...)) = # a b ... -> .x #
F(.x (.y #)) = .y # -> .x #
F(.x (.y (# a b ...))) = # a b ... -> .y # -> .x #

余パターンの連続 (# パターンの連続)のケースが適格。
F(# (.x _)) = error -- # 以降に余パターンがあるのはエラー

F({.head # -> X, .head (.tail #) -> Y, .tail (.tail #) -> Z}, \k -> k)
-- パターンの階層を揃える。
F({_ (.head #) -> X, .head (.tail #) -> Y, .tail (.tail #) -> Z}, \k -> k)
-- パターンリストを構築する
F({[_, .head #] -> X, [.head, .tail #] -> Y, [.tail, .tail #] -> Z}, \k -> k)
-- パターンリストの末尾を取り出す。
-- 残りのパターンがすべて_なら、節を構築する (.head # -> X)
-- そうでないなら、ホールkを使って節を構築する（.tail # -> k）
= F({[.head #] -> Y, [.tail #] -> Z}, \k -> {.head # -> X, .tail # -> k})
-- 構築したcodataをホールkへ代入する
= {.head # -> X, .tail # -> {.head # -> Y, .tail # -> Z}}

F({.head (# x) -> X, .tail (# 0) -> Y, .tail (# (Succ x)) -> Z}, \k -> k)
= F({[.head, # x] -> X, [.tail, # 0] -> Y, [.tail, # (Succ x)] -> Z}, \k -> k)
-- ガードをパターンリストに追加する
= F({[.head # | x] -> X, [.tail # | 0] -> Y, [.tail # | Succ x] -> Z}, \k -> { # x' -> k x' })
-- case式を構築して、残っている節をマージする
= F({[.head # | x] -> X, [.tail # | x] -> case x { 0 -> Y, Succ x -> Z }}, \k -> { # x' -> k x' })
-- 構築したcase式をホールkへ代入する
= { # x' -> { .head # -> let x = x' in X, .tail # -> case x' { 0 -> Y, Succ x -> Z } }}


F({.head (# x p) -> X, .tail (# 0 p) -> Y, .tail (# (Succ x) p) -> Z}, \k -> k)
= F({[.head, # x p] -> X, [.tail, # 0 p] -> Y, [.tail, # (Succ x) p] -> Z}, \k -> k)
-- ガードをパターンリストに追加する
= F({[.head # | x, p] -> X, [.tail # | 0, p] -> Y, [.tail # | Succ x, p] -> Z}, \k -> { # x' p' -> k x' p'})
-- case式を構築して、残っている節をマージする
= F({[.head # | x, v] -> case x, v { x, p -> X }, [.tail # | x, v] -> case x, v { 0, p -> Y, Succ x, p -> Z }}, \k -> { # x' p' -> k x' p'})
-- 構築したcase式をホールkへ代入する
= { # x' p' -> { .head # -> let x = x' in case p' { p -> X }, .tail # -> case x', p' { 0, p -> Y, Succ x, p -> Z } }}
```

[https://jesper.sikanda.be/files/elaborating-dependent-copattern-matching.pdf]のcase treeの構築に相当するのかなぁ。
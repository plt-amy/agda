module Give where

open import Common.Nat

add : Nat -> Nat -> Nat
add zero y = y
add (suc x) y = suc {! !}

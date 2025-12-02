module Test where

x = 5
y = 2 * 5 + x

result = y * 10

fac :: (Integral a) => a -> a
fac n = product [1..n]

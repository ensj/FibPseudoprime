module Main where

import Lib
import Criterion.Main

main :: IO ()
main = defaultMain [ 
    bgroup "Carl Test Factory" [
        bench "[1, 50)" $ (nf factory) [1, 2 .. 49]
        , bench "[50, 100)" $ (nf factory) [100, 102 .. 149]
        , bench "[100, 150)" $ (nf factory) [100, 101 .. 149]
        , bench "[150, 200)" $ (nf factory) [151, 152 .. 199]
        , bench "[200, 250)" $ (nf factory) [200, 201 .. 249]
        , bench "[250, 300)" $ (nf factory) [250, 251 .. 299]
        , bench "[300, 350)" $ (nf factory) [300, 301 .. 349]
        , bench "[350, 400)" $ (nf factory) [350, 351 .. 399]
        ]
    ]

factory :: [Int] -> [(Integer, [Integer], [Integer])]
factory targets = map carlTest targets 
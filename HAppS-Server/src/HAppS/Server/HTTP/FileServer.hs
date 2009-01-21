{-# OPTIONS -fth -cpp -fglasgow-exts -fallow-overlapping-instances  #-}
module Main where
--module HAppS.Server.HTTP.FileServer where

import HAppS
--import HAppS.Server.HTTP.FileServe (fileServe)
import System

main = asMain (httpH ".")
httpH path = noState:
             [h (Prefix ()) GET $ respIO $ fileServe path ]



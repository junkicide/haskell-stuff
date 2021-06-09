module DigitalRoot where

digitalRoot :: Integer -> Integer
digitalRoot n
  | n<10 = n
  | n>=10 = digitalRoot $ sum $ map (\x -> read (x:[]) :: Integer) $ show n
             

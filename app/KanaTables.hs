module KanaTables where

import Data.Map (fromList)

kana = fromList [
     ("vowels_column",  ["あ", "い", "う", "え", "お"]),
     ("k_column",       ["か", "き", "く", "け", "こ"]),
     ("s_column",       ["さ", "し", "す", "せ", "そ"]),
     ("t_column",       ["た", "ち", "つ", "て", "と"]),
     ("n_column",       ["な", "に", "ぬ", "ね", "の"]),
     ("h_column",       ["は", "ひ", "ふ", "へ", "ほ"]),
     ("m_column",       ["ま", "み", "む", "め", "も"]),
     ("y_column",       ["や", "ゆ", "よ"]),
     ("r_column",       ["ら", "り", "る", "れ", "ろ"]),
     ("w_column",       ["わ", "を"]),
     ("special_column", ["ん"])
     ]

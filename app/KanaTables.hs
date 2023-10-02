module KanaTables (hiraganaTable, katakanaTable, romajiHiraganaTable, romajiKatakanaTable, (!)) where

import Data.Map (fromList, (!))

-- TODO: Add the dakuten and handakuten characters

hiraganaTable = fromList [
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

katakanaTable = fromList [
     ("vowels_column",  ["ア", "イ", "ウ", "エ", "オ"]),
     ("k_column",       ["カ", "キ", "ク", "ケ", "コ"]),
     ("s_column",       ["サ", "シ", "ス", "セ", "ソ"]),
     ("t_column",       ["タ", "チ", "ツ", "テ", "ト"]),
     ("n_column",       ["ナ", "ニ", "ヌ", "ネ", "ノ"]),
     ("h_column",       ["ハ", "ヒ", "フ", "ヘ", "ホ"]),
     ("m_column",       ["マ", "ミ", "ム", "メ", "モ"]),
     ("y_column",       ["ヤ", "ユ", "ヨ"]),
     ("r_column",       ["ラ", "リ", "ル", "レ", "ロ"]),
     ("w_column",       ["ワ", "ヲ"]),
     ("special_column", ["ン"])
     ]


-- TODO: Maybe do something smarter here because romajiKatakanaTable and romajiHiraganaTable have exactly the same values, only the keys are different
romajiHiraganaTable = fromList [

     (hiraganaTable ! "vowels_column" !! 0, "a"),
     (hiraganaTable ! "vowels_column" !! 1, "i"),
     (hiraganaTable ! "vowels_column" !! 2, "u"),
     (hiraganaTable ! "vowels_column" !! 3, "e"),
     (hiraganaTable ! "vowels_column" !! 4, "o"),

     (hiraganaTable ! "k_column" !! 0, "ka"),
     (hiraganaTable ! "k_column" !! 1, "ki"),
     (hiraganaTable ! "k_column" !! 2, "ku"),
     (hiraganaTable ! "k_column" !! 3, "ke"),
     (hiraganaTable ! "k_column" !! 4, "ko"),

     (hiraganaTable ! "s_column" !! 0, "sa"),
     (hiraganaTable ! "s_column" !! 1, "shi"),
     (hiraganaTable ! "s_column" !! 2, "su"),
     (hiraganaTable ! "s_column" !! 3, "se"),
     (hiraganaTable ! "s_column" !! 4, "so"),

     (hiraganaTable ! "t_column" !! 0, "ta"),
     (hiraganaTable ! "t_column" !! 1, "chi"),
     (hiraganaTable ! "t_column" !! 2, "tsu"),
     (hiraganaTable ! "t_column" !! 3, "te"),
     (hiraganaTable ! "t_column" !! 4, "to"),

     (hiraganaTable ! "n_column" !! 0, "na"),
     (hiraganaTable ! "n_column" !! 1, "ni"),
     (hiraganaTable ! "n_column" !! 2, "nu"),
     (hiraganaTable ! "n_column" !! 3, "ne"),
     (hiraganaTable ! "n_column" !! 4, "no"),

     (hiraganaTable ! "h_column" !! 0, "ha"),
     (hiraganaTable ! "h_column" !! 1, "hi"),
     (hiraganaTable ! "h_column" !! 2, "fu"),
     (hiraganaTable ! "h_column" !! 3, "he"),
     (hiraganaTable ! "h_column" !! 4, "ho"),

     (hiraganaTable ! "m_column" !! 0, "ma"),
     (hiraganaTable ! "m_column" !! 1, "mi"),
     (hiraganaTable ! "m_column" !! 2, "mu"),
     (hiraganaTable ! "m_column" !! 3, "me"),
     (hiraganaTable ! "m_column" !! 4, "mo"),

     (hiraganaTable ! "y_column" !! 0, "ya"),
     (hiraganaTable ! "y_column" !! 1, "yu"),
     (hiraganaTable ! "y_column" !! 2, "yo"),

     (hiraganaTable ! "r_column" !! 0, "ra"),
     (hiraganaTable ! "r_column" !! 1, "ri"),
     (hiraganaTable ! "r_column" !! 2, "ru"),
     (hiraganaTable ! "r_column" !! 3, "re"),
     (hiraganaTable ! "r_column" !! 4, "ro"),

     (hiraganaTable ! "w_column" !! 0, "wa"),
     (hiraganaTable ! "w_column" !! 1, "wo"),

     (hiraganaTable ! "special_column" !! 0, "n")
     ]


romajiKatakanaTable = fromList [

     (katakanaTable ! "vowels_column" !! 0, "a"),
     (katakanaTable ! "vowels_column" !! 1, "i"),
     (katakanaTable ! "vowels_column" !! 2, "u"),
     (katakanaTable ! "vowels_column" !! 3, "e"),
     (katakanaTable ! "vowels_column" !! 4, "o"),

     (katakanaTable ! "k_column" !! 0, "ka"),
     (katakanaTable ! "k_column" !! 1, "ki"),
     (katakanaTable ! "k_column" !! 2, "ku"),
     (katakanaTable ! "k_column" !! 3, "ke"),
     (katakanaTable ! "k_column" !! 4, "ko"),

     (katakanaTable ! "s_column" !! 0, "sa"),
     (katakanaTable ! "s_column" !! 1, "shi"),
     (katakanaTable ! "s_column" !! 2, "su"),
     (katakanaTable ! "s_column" !! 3, "se"),
     (katakanaTable ! "s_column" !! 4, "so"),

     (katakanaTable ! "t_column" !! 0, "ta"),
     (katakanaTable ! "t_column" !! 1, "chi"),
     (katakanaTable ! "t_column" !! 2, "tsu"),
     (katakanaTable ! "t_column" !! 3, "te"),
     (katakanaTable ! "t_column" !! 4, "to"),

     (katakanaTable ! "n_column" !! 0, "na"),
     (katakanaTable ! "n_column" !! 1, "ni"),
     (katakanaTable ! "n_column" !! 2, "nu"),
     (katakanaTable ! "n_column" !! 3, "ne"),
     (katakanaTable ! "n_column" !! 4, "no"),

     (katakanaTable ! "h_column" !! 0, "ha"),
     (katakanaTable ! "h_column" !! 1, "hi"),
     (katakanaTable ! "h_column" !! 2, "fu"),
     (katakanaTable ! "h_column" !! 3, "he"),
     (katakanaTable ! "h_column" !! 4, "ho"),

     (katakanaTable ! "m_column" !! 0, "ma"),
     (katakanaTable ! "m_column" !! 1, "mi"),
     (katakanaTable ! "m_column" !! 2, "mu"),
     (katakanaTable ! "m_column" !! 3, "me"),
     (katakanaTable ! "m_column" !! 4, "mo"),

     (katakanaTable ! "y_column" !! 0, "ya"),
     (katakanaTable ! "y_column" !! 1, "yu"),
     (katakanaTable ! "y_column" !! 2, "yo"),

     (katakanaTable ! "r_column" !! 0, "ra"),
     (katakanaTable ! "r_column" !! 1, "ri"),
     (katakanaTable ! "r_column" !! 2, "ru"),
     (katakanaTable ! "r_column" !! 3, "re"),
     (katakanaTable ! "r_column" !! 4, "ro"),

     (katakanaTable ! "w_column" !! 0, "wa"),
     (katakanaTable ! "w_column" !! 1, "wo"),

     (katakanaTable ! "special_column" !! 0, "n")
     ]

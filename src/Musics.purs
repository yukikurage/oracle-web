module Musics where

import Data.Map (Map)
import Data.Map as M
import Data.Tuple (Tuple(..))
import MusicData (MusicData)

data Musics = Backlight | Welcome | Antivirus | Syscall | Qed

musics :: Map String MusicData
musics = M.fromFoldable
  [ Tuple "backlight"
      { title: "後光"
      , icon: "✧"
      , singer: "ナースロボ_タイプT"
      , lyricsFile: "./lyrics/backlight.txt"
      }
  , Tuple "welcome"
      { title: "ようこそ"
      , icon: "⌽"
      , singer: "ナースロボ_タイプT & 雨晴はう"
      , lyricsFile: "./lyrics/welcome.txt"
      }
  , Tuple "antivirus"
      { title: "アンチウイルス"
      , icon: "⍁"
      , singer: "四国めたん"
      , lyricsFile: "./lyrics/antivirus.txt"
      }
  , Tuple "syscall"
      { title: "SYSCALL"
      , icon: "☾"
      , singer: "ずんだもん"
      , lyricsFile: "./lyrics/syscall.txt"
      }
  , Tuple "qed"
      { title: "◼︎"
      , icon: "◼︎"
      , singer: "◼︎"
      , lyricsFile: "./lyrics/qed.txt"
      }
  ]

order :: Array String
order = [ "backlight", "welcome", "antivirus", "syscall", "qed" ]

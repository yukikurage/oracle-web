module Musics where

import Prelude

import Data.Enum (class Enum)
import Data.Generic.Rep (class Generic)
import MusicData (MusicData)

data Musics = Backlight | Welcome | Antivirus | Syscall | Qed

musics :: Array MusicData
musics =
  [ { title: "後光"
    , icon: "✧"
    , singer: "ナースロボ_タイプT"
    , lyricsFile: "aaaaa"
    }
  , { title: "ようこそ"
    , icon: "⌽"
    , singer: "ナースロボ_タイプT & 雨晴はう"
    , lyricsFile: "aaaaa"
    }
  , { title: "アンチウイルス"
    , icon: "⍁"
    , singer: "四国めたん"
    , lyricsFile: "aaaaa"
    }
  , { title: "SYSCALL"
    , icon: "☾"
    , singer: "ずんだもん"
    , lyricsFile: "./lyrics/syscall.txt"
    }
  , { title: "◼︎"
    , icon: "◼︎"
    , singer: "◼︎"
    , lyricsFile: "aaaaa"
    }
  ]

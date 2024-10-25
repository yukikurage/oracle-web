module Main where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as AF
import Affjax.Web as AW
import Data.Array (index)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Maybe (isJust, maybe)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (error)
import Jelly.Aff (awaitBody)
import Jelly.Component (Component, hooks, text, textSig)
import Jelly.Element as JE
import Jelly.Hooks (class MonadHooks, runHooks_, useAff)
import Jelly.Hydrate (mount)
import Jelly.Prop (on, (:=), (@=))
import Jelly.Signal (newState, readSignal, writeChannel)
import MusicData (MusicData)
import Musics as Musics
import Web.HTML.Event.EventTypes (click)

main :: Effect Unit
main = launchAff_ do
  bodyMaybe <- awaitBody
  liftEffect $ traverse_ (runHooks_ <<< mount (component Musics.musics)) bodyMaybe

getFile :: String -> Aff String
getFile path = do
  response <- AX.get AW.driver (AF.String identity) path
  case response of
    Left _ -> do
      error $ "Failed to fetch file: " <> path
      pure ""
    Right res -> pure res.body

component :: forall m. MonadHooks m => Array MusicData -> Component m
component musics = hooks do
  currentMusicIndexSig /\ setCurrentMusicIndex <- newState (-1 :: Int)

  let currentMusicDataSig = currentMusicIndexSig <#> \idx -> index musics idx
  let isMusicOpenedSig = isJust <$> currentMusicDataSig

  pure do
    JE.div [ "class" := "w-screen h-screen text-5xl flex flex-col gap-6 justify-center items-center w-screen h-screen font-noto" ] do
      JE.img
        [ "src" := "./Background.png"
        , "class" @=
            pure "w-screen h-screen fixed blur-sm scale-125 -z-10 object-cover transition-all "
            <> ifM isMusicOpenedSig (pure "opacity-50") (pure "opacity-100")
        ]
      JE.img
        [ "src" := "Logo@8x.png"
        , "class" @= pure "w-96 transition-all "
            <> ifM isMusicOpenedSig (pure "opacity-0") (pure "opacity-100")
        ]
      JE.div
        [ "class" @= pure "flex gap-8 font-sans transition-all "
            <> ifM isMusicOpenedSig (pure "opacity-0 pointer-events-none") (pure "opacity-100 pointer-events-auto")
        ]
        do
          forWithIndex_ musics \idx musicData ->
            JE.button
              [ "class" := "w-12 h-12 flex justify-center items-center"
              , on click
                  ( \_ -> do
                      isMusicOpened <- readSignal isMusicOpenedSig
                      when (not isMusicOpened) $ writeChannel setCurrentMusicIndex idx
                  )
              ]
              do
                JE.div [ "class" := "w-10 h-10 flex justify-center items-center text-5xl hover:scale-105 transition-all" ] $ text musicData.icon

      forWithIndex_ musics \idx musicData ->
        JE.div
          [ "class" @= pure "w-full h-full fixed flex items-start justify-center overflow-y-auto transition-all "
              <> ifM (currentMusicIndexSig <#> (_ == idx)) (pure "opacity-100 pointer-events-auto") (pure "opacity-0 pointer-events-none")
          ]
          do
            musicComponent musicData

musicComponent :: forall m. MonadHooks m => MusicData -> Component m
musicComponent musicData = hooks do
  lyrics <- useAff $ pure $ getFile musicData.lyricsFile

  pure do
    JE.div [ "class" := "flex flex-nowrap flex-col justify-center w-3/4 gap-6 p-5" ] do
      JE.div [ "class" := "w-full min-h-fit flex flex-col items-center justify-start gap-8 p-5 shrink-0" ] do
        JE.div [ "class" := "text-6xl font-sans" ] $ text musicData.icon
        JE.div [ "class" := "text-4xl font-NotoJP font-bold scale-y-125" ] $ text musicData.title
        JE.div [ "class" := "text-lg font-NotoJP scale-y-125" ] $ text $ "feat. " <> musicData.singer
      JE.div [ "class" := "w-full max-h-80 overflow-y-auto text-base font-NotoJP scale-y-125 whitespace-pre" ] do
        textSig $ maybe "404" identity <$> lyrics
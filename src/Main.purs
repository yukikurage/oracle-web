module Main where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as AF
import Affjax.Web as AW
import Control.Safely (for_)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), isJust, maybe)
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
import Jelly.Router (class Router, runRouterT, useCurrentRoute, usePushRoute)
import Jelly.Signal (readSignal)
import MusicData (MusicData)
import Musics as Musics
import Oracle.Routing (Page(..))
import Oracle.Routing as P
import Web.Event.Event (stopPropagation)
import Web.HTML.Event.EventTypes (click)

main :: Effect Unit
main = launchAff_ do
  bodyMaybe <- awaitBody
  liftEffect $ traverse_ (runHooks_ <<< runRouterT <<< mount (component Musics.musics Musics.order)) bodyMaybe

getFile :: String -> Aff String
getFile path = do
  response <- AX.get AW.driver (AF.String identity) path
  case response of
    Left _ -> do
      error $ "Failed to fetch file: " <> path
      pure ""
    Right res -> pure res.body

component :: forall m. MonadHooks m => Router m => Map String MusicData -> Array String -> Component m
component musics order = hooks do
  currentRoute <- useCurrentRoute
  let currentPage = P.fromPath <$> currentRoute

  let
    currentMusicDataSig = currentPage <#> case _ of
      Right (Music idx) -> M.lookup idx musics
      _ -> Nothing
  let isMusicOpenedSig = isJust <$> currentMusicDataSig

  let onClose = usePushRoute (P.toPath Home) :: m Unit

  pure do
    JE.div [ "class" := "w-screen h-screen text-5xl flex flex-col gap-6 justify-center items-center w-screen h-screen font-noto" ] do
      JE.img
        [ "src" := "./Background.png"
        , "class" @=
            pure "w-screen h-screen fixed scale-125 -z-10 object-cover transition-all "
            <> ifM isMusicOpenedSig (pure "opacity-50 blur-md") (pure "opacity-100 blur-sm")
        ]
      JE.img
        [ "src" := "Logo@8x.png"
        , "class" @= pure "w-72 md:w-96 transition-all "
            <> ifM isMusicOpenedSig (pure "opacity-0") (pure "opacity-100")
        ]
      JE.div
        [ "class" @= pure "flex gap-4 md:gap-8 font-sans transition-all "
            <> ifM isMusicOpenedSig (pure "opacity-0 pointer-events-none") (pure "opacity-100 pointer-events-auto")
        ]
        do
          for_ order \idx ->
            JE.button
              [ "class" := "w-12 h-12 flex justify-center items-center"
              , on click
                  ( \_ -> do
                      isMusicOpened <- readSignal isMusicOpenedSig
                      when (not isMusicOpened) $ usePushRoute (P.toPath $ Music idx)
                  )
              ]
              case M.lookup idx musics of
                Just musicData -> JE.div [ "class" := "w-10 h-10 flex justify-center items-center text-5xl hover:scale-110 transition-all" ] $ text musicData.icon
                Nothing -> mempty
      for_ order \idx ->
        JE.div
          [ "class" @= pure "w-full h-full absolute flex items-start justify-center overflow-y-auto transition-all cursor-pointer "
              <> ifM
                ( currentPage <#> case _ of
                    Right (Music i) -> i == idx
                    _ -> false
                )
                (pure "opacity-100 pointer-events-auto")
                (pure "opacity-0 pointer-events-none")
          , on click \_ -> onClose
          ]
          case M.lookup idx musics of
            Just musicData -> musicComponent musicData onClose
            Nothing -> mempty

musicComponent :: forall m. MonadHooks m => MusicData -> m Unit -> Component m
musicComponent musicData onClose = hooks do
  lyrics <- useAff $ pure $ getFile musicData.lyricsFile

  pure do
    JE.div [ "class" := "relative flex flex-col justify-center w-full md:w-3/4 gap-6 p-5 cursor-auto", on click \e -> liftEffect $ stopPropagation e ] do
      JE.button [ "class" := "absolute text-4xl right-5 top-5 h-24 w-24 flex justify-center items-center hover:scale-110 transition-all", on click \_ -> onClose ] $ text "✕"
      JE.div [ "class" := "w-full min-h-fit flex flex-col items-center justify-start gap-8 p-5 shrink-0" ] do
        JE.div [ "class" := "text-6xl font-sans" ] $ text musicData.icon
        JE.div [ "class" := "text-4xl font-NotoJP font-bold scale-y-125" ] $ text musicData.title
        JE.div [ "class" := "text-lg font-NotoJP scale-y-125" ] $ text $ "feat. " <> musicData.singer
      JE.div [ "class" := "w-full max-h-fit text-lg font-NotoJP whitespace-pre-wrap py-12" ] do
        textSig $ maybe "404" identity <$> lyrics

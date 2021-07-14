module App.Button where

import Prelude

-- import Control.Monad.Loops (whileM)
import Halogen as H
import Halogen.HTML (elementNS)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Effect.Random (randomInt)
import Effect.Class (liftEffect, class MonadEffect)
import Web.TouchEvent.EventTypes (touchend)
import Data.Maybe (fromMaybe, Maybe(..))
import Data.Array ((!!))
import Data.String.Utils (lines)
import Debug.Trace (trace)

type Input = Array String

type State = { random :: Int, strings :: String, input :: String }

data Action = Submit 
            | Randomise 
            | UpdateInput String

setRandom :: forall output m. MonadEffect m => Int -> H.HalogenM State Action () output m Unit
setRandom num = H.modify_ \st -> st { random = num}

handleAction :: forall output m. MonadEffect m =>  Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Submit ->
    liftEffect randomNum >>= setRandom

  UpdateInput s →
    H.modify_ \st -> st { input = s }
    


  Randomise ->
    pure unit

component :: forall q i o m. MonadEffect m => H.Component q i o m
component =
  H.mkComponent
    { initialState: \_ -> {strings: "One\nTwo\nThree", random:0, input: "Three\nFour\nFive"}
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

render :: forall cs m. State -> H.ComponentHTML Action cs m
render state =
      let cells = lines state.strings
      in 
        HH.div 
                [HP.id "root"]
                [HH.table []
                    [HH.tr []
                        [renderCell cells 0
                        , renderCell cells 1
                        , renderCell cells 2
                        , renderCell cells 3
                        , renderCell cells 4]
                    , HH.tr []
                        [renderCell cells 5
                        , renderCell cells 6
                        , renderCell cells 7
                        , renderCell cells 8
                        , renderCell cells 9]
                    , HH.tr []
                        [renderCell cells 10
                        , renderCell cells 11
                        , HH.td [] [HH.text (show state.random)]
                        , renderCell cells 12
                        ,renderCell cells 13]
                    , HH.tr []
                        [renderCell cells 14
                        , renderCell cells 15
                        , renderCell cells 16
                        , renderCell cells 17
                        , renderCell cells 18]
                    , HH.tr []
                        [renderCell cells 19
                        , renderCell cells 20
                        , renderCell cells 21
                        , renderCell cells 22
                        , renderCell cells 23]
                    ]
                    , HH.button
                      [ HE.onClick \_ -> Randomise ]
                      [ HH.text "Randomise" ]
                    , HH.textarea [HP.placeholder "Insert Text"]
                    ,HH.input
                      [ HP.value state.input
                      , HE.onValueInput UpdateInput
                      ]
                    , HH.button
                      [ HE.onClick \_ -> Submit ]
                      [ HH.text "Submit" ]
                ]
          
printCell :: Array String -> Int -> String
printCell state i = fromMaybe "Empty" (state !! i)

renderCell :: forall cs m. Array String -> Int -> H.ComponentHTML Action cs m
renderCell state i = HH.td [] [HH.text (printCell state i)]

-- renderRow :: forall cs m. Array String -> Int -> H.ComponentHTML Action cs m
-- renderRow state j = 
--   let cells = lines state.strings
--       in
--       HH.tr [] [renderCell cells (j*5+0)]
--       HH.tr [] [renderCell cells (j*5+1)]
--       HH.tr [] [renderCell cells (j*5+2)]
--       HH.tr [] [renderCell cells (j*5+3)]
--       HH.tr [] [renderCell cells (j*5+4)]



strings :: Array String
strings = ["lt26QwyJuj", "GoJceODFrE", "MyqifQOAV7", "yvZN1BjAkG", "BmgWdgPdVS", "kMilYizKkf", "veiJOiyugY", "A6EOtll1Vo", "RP9421wPhb", "F4YsBchyPK", "wkZlDviAQU", "gSd9G7XO0D", "c7Fl1szOyi", "ZTYFxk7Z97", "T6Krx2Ttj8", "VO8dlUVA41", "IimSw5ebLD", "TKoKmacpHG", "lcIx7oipfB", "OxCVrNPyga", "iHLvCyloCz", "Ins7ajcMIE", "7u1YCuQXVu", "ZSVS6Rj9gr"]

randomNum = randomInt 0 23


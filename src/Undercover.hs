{-# LANGUAGE OverloadedStrings, TemplateHaskell, ViewPatterns #-}
module Undercover where

import Network.IRC.Client
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import qualified Data.Text as T
import Text.Read (readMaybe)
import Control.Monad.Trans
import Control.Monad
import Control.Lens
import Data.List
import qualified Data.Map as M
import Data.Maybe
import System.Random.Shuffle
import System.Random

{- !register / !start 
 - !end 
 - !reveal <nickname> : reveal the identity of the specified player
 -}

gamechan = "#undercover"

type User = T.Text

data PlayerStatus = PlayerStatus {_classicPlayers :: [User],
                                  _undercoverPlayers :: [User],
                                  _mrWhite :: Maybe User,
                                  _classicWord :: T.Text,
                                  _undercoverWord :: T.Text,
                                  _playingOrder :: [User]}
                      deriving Show

data PlayerDecisions = PlayerDecisions { _wordsSaids :: M.Map User [T.Text],
                                         _votes :: M.Map User (Maybe User)
                                       }

data Game = Started  { _players :: PlayerStatus,
                       _decisions :: PlayerDecisions,
                       _database :: [[T.Text]]}
          | Pending {_registeredPlayers :: [User], _database :: [[T.Text]]}

makeLenses ''PlayerStatus
makeLenses ''PlayerDecisions
makeLenses ''Game

{- EventHandler :: (Event Text -> Maybe b) -> (Source Text -> b -> IRC s ()) -> EventHandler s	 -}

undercoverHandler = EventHandler matchGamechanMessage undercoverHandler'

matchGamechanMessage :: Event T.Text -> Maybe T.Text
matchGamechanMessage e = case _message e of
                        (Privmsg target (Right raw)) -> if (target == gamechan) then Just raw else Nothing
                        _ -> Nothing
undercoverHandler' :: (Source T.Text) -> T.Text -> IRC Game ()
undercoverHandler' (Channel chan user) raw = do
    gameT <- get userState <$> getIRCState
    let ws = T.words raw
    case ws of
        [] -> pure ()
        (cmd:rest) -> do 
                  game <- liftIO $ atomically $ readTVar gameT 
                  newgame <- onCommand user cmd rest game
                  liftIO $ atomically $ writeTVar gameT newgame
    pure ()




onCommand :: User -> T.Text -> [T.Text] -> Game -> IRC Game Game
onCommand user "!register" _  g@(Pending _ _) = do
    privmsg gamechan $ T.unwords [user, "has been registered"]
    pure $ onRegister g user
onCommand user "!register" _ g = privmsg gamechan "The game is already started" >> pure g
onCommand user "!end" _ g@(Started _ _ _) = privmsg gamechan "The game is finished" >> pure (onEnd g)
onCommand user "!end" _ g = privmsg gamechan "The game wasn't started dude." >> pure g
onCommand user "!list" _ g@(Pending l _) = (privmsg gamechan $ T.unwords $ ["Participants are"] ++ l) >> pure g
onCommand user "!unregister" _ (Pending l db) = pure $ Pending (delete user l) db
onCommand user "!reveal" _ g = onReveal user g
onCommand user "!start" ws g@(Pending _ _) = initGame g ws
onCommand user "!aide" _ g = do
    privmsg user "!register : s'inscrire à la prochaine partie"
    privmsg user "!start : lancer une partie"
    privmsg user "!order : montre l'ordre des joueurs durant le tour"
    privmsg user "!say/!unsay : annoncer un mot (ou retirer le dernier mot dit)"
    privmsg user "!words : connaître les mots dits"
    privmsg user "!vote : voter contre quelqu'un (écrase le vote précédent)"
    privmsg user "!votestatus : synthétiser les votes"
    privmsg user "!reveal : reveler son identité"
    privmsg user "!end : finir la partie"
    pure g
onCommand user "!rules" _ g = do
    notice user "Au début de la partie, un mot secret est distribué à chaque joueur. Tous ont le même, à l'exception de l'un d'entre eux qui est l'agent secret qui reçoit un mot différent. À chaque tour, les joueurs doivent dire un mot (en rapport ou non) avec leur secret. À la fin du tour, les joueurs votent pour éliminer l'un d'entre eux."
    notice user "Le joueur éliminé révèle son identité en tappant \"!reveal\". La partie se termine quand l'undercover est éliminé, ou qu'il ne reste plus que deux joueurs."
    pure g

onCommand user "!order" _ g@(Started _ _ _) = privmsg gamechan (T.unwords ("Playing order is: ":_playingOrder (_players g)) ) >> pure g
onCommand user "!words" _ g@(Started _ _ _)  = onWords g
onCommand user "!say" wlist g@(Started _ _ _) = onSay user (T.unwords wlist) g
onCommand user "!unsay" wlist g@(Started _ _ _) = onUnsay user g
onCommand user "!vote" (name:_) g@(Started _ _ _) = onVote user name g
onCommand user "!votestatus" _ g@(Started _ _ _) = onVotestatus g
onCommand user _ _ g = pure g

initGame :: Game -> [T.Text] -> IRC Game Game
intGame g@(Pending [] _) _ = pure g
initGame g [] = prepareGame 1 1 False g
initGame g [(readMaybe . T.unpack -> Just n)] = prepareGame n n False g
initGame g [(readMaybe . T.unpack -> Just n),"true"] = prepareGame n n True g 
initGame g [(readMaybe . T.unpack -> Just mini),(readMaybe . T.unpack -> Just maxi)] = prepareGame mini maxi False g 
initGame g [w1,w2,w3] = case (readMaybe $ T.unpack w1, readMaybe $ T.unpack w2) of
    (Just mini, Just maxi) -> prepareGame mini maxi (w3 == "true") g
    otherwise              -> privmsg gamechan "Parse error." >> pure g
initGame g _ = pure g
        
{-
initGame g@(Pending players db) (nbminS:nbmaxS:mrwhiteP:_) = case (readMaybe $ T.unpack nbminS, readMaybe $ T.unpack nbmaxS, readMaybe $ T.unpack mrwhiteP) of
    (Just nbmin, Just nbmax, Just mrwhite) ->  initGame' nbmin nbmax mrwhite
    otherwise -> privmsg gamechan "l'un des arguments n'est pas un nombre" >> pure g
  where initGame' nbmin nbmax mrwhite
                        | otherwise = do
                randomizedplayers <- liftIO $  shuffleM players
                randomizedorder <- liftIO $  shuffleM players
                randomEntry <- liftIO $ pickRandom db
                nbUndercovers <- liftIO $ randomRIO (nbmin, nbmax)
-}

prepareGame nbmin nbmax mrwhiteP g@(Pending players db)
    | nbmin < 0 || nbmax < 0 = privmsg gamechan "l'un des arguments est négatif"  >> pure g
    | nbmin > nbmax = privmsg gamechan "erreur : min > max" >> pure g
    | nbmax > (if mrwhiteP then length players - 2 else length players - 1) = privmsg gamechan "erreur: trop d'undercovers" >> pure g
    | otherwise = do
        nbUndercovers <- liftIO $ randomRIO (nbmin,nbmax)
        liftIO $ putStrLn "generation du status"
        status <- liftIO $ generateDistribution nbUndercovers mrwhiteP g
        liftIO $ putStrLn $ "status fait: " ++ show status
        let initWordSaids = foldr (\el acc -> M.insert el [] acc) M.empty players
            initVotes = foldr (\el acc -> M.insert el Nothing acc) M.empty players
        liftIO $ putStrLn "distribution"
        distribute status
        liftIO $ putStrLn "distribution faite"
        privmsg gamechan $ T.pack $ "Il y a " ++ show nbmin ++ "-" ++ show nbmax ++ " agent(s) sous couverture." ++ if isNothing (_mrWhite status) then "" else "Attention, M. White est présent !"
        pure $ Started status (PlayerDecisions initWordSaids initVotes) db


{-| Génère une répartition de joueurs et retourne les joueurs "normaux", les undercover, le mot "normal", le mot de l'undercover, et l'ordre de jeu |-}
generateDistribution :: Int -> Bool -> Game -> IO PlayerStatus
generateDistribution nbundercovers mrwhiteP g@(Pending players db) = do
        randomizedorder <- liftIO $  shuffleM players
        randomEntry <- liftIO $ pickRandom db
        ws <- liftIO $ shuffleM randomEntry
        undercoverw <- liftIO $ pickRandom ws
        classicw <- liftIO $ pickRandom $ delete undercoverw ws
        
        let (undercovers,rest) = splitAt nbundercovers randomizedorder
            classics= if mrwhiteP then init rest else rest
            mrwhite = if mrwhiteP then Just (last rest) else Nothing

        playingorder' <- liftIO $ shuffleM players
        mrwhitepos <- randomRIO (2,length players)
        let playingorder = case mrwhite of
                            Nothing -> playingorder'
                            Just pi -> insertAt mrwhitepos pi $ delete pi playingorder'
        pure (PlayerStatus classics undercovers mrwhite classicw undercoverw playingorder)

insertAt i v l = let (begin,end) = splitAt i l
                 in begin ++ [v] ++ end

distribute status = do 
    forM (_playingOrder status) $ distribute'
    privmsg gamechan $ T.unwords $ "Playing order is":_playingOrder status
 where distribute' pi
            | isJust (_mrWhite status) && pi == fromJust (_mrWhite status) = privmsg pi "tu es Mr White !"
            | pi `elem` _undercoverPlayers status = privmsg pi (_undercoverWord status)
            | otherwise = privmsg pi (_classicWord status)


privmsg chan txt = send (Privmsg chan $ Right txt)
notice target txt = send (Notice target $ Right txt)


pickRandom :: [a] -> IO a
pickRandom [] = error "empty list"
pickRandom l = do
    idx <- randomRIO (0, length l -1)
    pure $ l !! idx
    

onRegister :: Game -> User -> Game
onRegister (Pending l db) user = Pending (nub $ user:l) db
onRegister g _ = g

onEnd :: Game -> Game
onEnd g = Pending [] $ _database g

onReveal user g@(Pending _ _) = pure g
onReveal user g = do
    ret <- onReveal' user g
    pure $ clearVotes ret
onReveal' user g
    | user `elem` _classicPlayers (_players g) = do
        privmsg gamechan $ "Tu es un simple joueur." 
        pure g
    | user `elem` _undercoverPlayers (_players g) = do
        privmsg gamechan $ "Tu es l'undercover, sneaky rat."
        pure g
    | isJust (_mrWhite $ _players g) && user == fromJust (_mrWhite $ _players g) = privmsg gamechan "mrwhite" >> pure g
    | otherwise = pure g


onSay :: User -> T.Text -> Game -> IRC Game Game
onSay user words g = case user `M.lookup` _wordsSaids (_decisions g) of
                Nothing -> pure g
                Just _ -> pure $ g & decisions . wordsSaids %~ M.adjust (++ [words]) user 
onUnsay :: User -> Game -> IRC Game Game
onUnsay user g = case user `M.lookup` _wordsSaids (_decisions g) of
                Nothing -> pure g
                Just (x:xs) -> pure $ g & decisions . wordsSaids %~ M.adjust (init) user 
                Just _ -> pure g


onWords :: Game -> IRC Game Game
onWords  g = do
    let dumpEntry (k, v) = T.concat [k, " (", T.unwords v,")"]
    privmsg gamechan $ T.unwords $ (\pi -> dumpEntry (pi, fromJust $ M.lookup pi $ _wordsSaids $ _decisions g )) <$> (_playingOrder $ _players g) 
    pure g
onVote user name g = case user `M.lookup` _votes (_decisions g) of
    Nothing -> pure g
    Just x -> pure $ g & decisions . votes %~ M.insert user (Just name)
onVotestatus g = privmsg gamechan votestring >> pure g
    where computeVote user = length [vote | vote <- M.elems $ _votes $ _decisions g, isJust vote, fromJust vote == user]
          votestatus = computeVote <$> (_playingOrder $ _players g)
          votestring = T.unwords [T.concat[user,"(",T.pack (show nbvotes),")"] | (user, nbvotes) <- zip (_playingOrder $ _players g) votestatus]
clearVotes g = g & decisions . votes %~ reset
    where reset db = (\_ -> Nothing) <$> db


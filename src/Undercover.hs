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
                                  _playingOrder :: [User],
                                  _revealed :: [User],
                                  _nbRevealedUC :: Int,
                                  _mrWhiteRevealed :: Bool}
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
    let ws = T.words raw
    case ws of
        [] -> pure ()
        (cmd:rest) -> do 
                  pending <- isPending
                  onCommand user cmd

                  liftIO $ putStrLn (show pending)
                  if pending then
                    onPendingCommand user cmd rest
                  else
                    onStartedCommand user cmd rest



onPendingCommand :: User -> T.Text -> [T.Text] -> IRC Game ()
onPendingCommand user "!unregister" _ = registeredPlayers %= delete user
onPendingCommand user "!register" _  = onRegister user
onPendingCommand user "!list" _  = do
    l <- use registeredPlayers
    privmsg gamechan $ T.unwords $ ["Participants are"] ++ l
onPendingCommand user "!end" _ = privmsg gamechan "The game wasn't started dude."
onPendingCommand user "!start" ws = do 
    g <- getState
    initGame g ws >>= putState
onPendingCommand _ _ _ = pure ()

onStartedCommand user "!end" _  = onEnd
onStartedCommand user "!order" _  = do
    g <- getState
    privmsg gamechan (T.unwords ("Playing order is: ":_playingOrder (_players g)) ) 
onStartedCommand user "!words" _  = onWords
onStartedCommand user "!say" wlist = onSay user (T.unwords wlist) 
onStartedCommand user "!unsay" wlist =  getState >>= onUnsay user >>= putState
onStartedCommand user "!vote" (name:_)  = getState >>= onVote user name >>= putState
onStartedCommand user "!votestatus" _ = getState >>= onVotestatus >>= putState
onStartedCommand user "!reveal" _ = onReveal user


onStartedCommand _ _ _ = pure ()


onCommand user "!aide" = do
    privmsg user "!register : s'inscrire à la prochaine partie"
    privmsg user "!start : lancer une partie"
    privmsg user "!order : montre l'ordre des joueurs durant le tour"
    privmsg user "!say/!unsay : annoncer un mot (ou retirer le dernier mot dit)"
    privmsg user "!words : connaître les mots dits"
    privmsg user "!vote : voter contre quelqu'un (écrase le vote précédent)"
    privmsg user "!votestatus : synthétiser les votes"
    privmsg user "!reveal : reveler son identité"
    privmsg user "!end : finir la partie"
onCommand user "!rules" = do
    notice user "Au début de la partie, un mot secret est distribué à chaque joueur. Tous ont le même, à l'exception de l'un d'entre eux qui est l'agent secret qui reçoit un mot différent. À chaque tour, les joueurs doivent dire un mot (en rapport ou non) avec leur secret. À la fin du tour, les joueurs votent pour éliminer l'un d'entre eux."
    notice user "Le joueur éliminé révèle son identité en tappant \"!reveal\". La partie se termine quand l'undercover est éliminé, ou qu'il ne reste plus que deux joueurs."
onCommand user _ = pure ()



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



prepareGame nbmin nbmax mrwhiteP g@(Pending players db)
    | nbmin < 0 || nbmax < 0 = privmsg gamechan "l'un des arguments est négatif"  >> pure g
    | nbmin > nbmax = privmsg gamechan "erreur : min > max" >> pure g
    | nbmax > (if mrwhiteP then length players - 2 else length players - 1) = privmsg gamechan "erreur: trop d'undercovers" >> pure g
    | otherwise = do
        nbUndercovers <- liftIO $ randomRIO (nbmin,nbmax)
        status <- liftIO $ generateDistribution nbUndercovers mrwhiteP g
        liftIO $ putStrLn $ "status généré : " ++ show status
        let initWordSaids = foldr (\el acc -> M.insert el [] acc) M.empty players
            initVotes = foldr (\el acc -> M.insert el Nothing acc) M.empty players
        distribute status
        privmsg gamechan $ T.pack $ "Il y a " ++ show nbmin ++ "-" ++ show nbmax ++ " agent(s) sous couverture." ++ if isNothing (_mrWhite status) then "" else " Attention, M. White est présent !"
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
        pure (PlayerStatus classics undercovers mrwhite classicw undercoverw playingorder [] 0 (not mrwhiteP))

insertAt i v l = let (begin,end) = splitAt i l
                 in begin ++ [v] ++ end

distribute status = do 
    forM (_playingOrder status) $ distribute'
    privmsg gamechan $ T.unwords $ "Playing order is":_playingOrder status
 where distribute' pi
            | isJust (_mrWhite status) && pi == fromJust (_mrWhite status) = privmsg pi "tu es Mr White !"
            | pi `elem` _undercoverPlayers status = privmsg pi $ T.unwords ["Ton mot est:",(_undercoverWord status)]
            | otherwise = privmsg pi $ T.unwords ["Ton mot est: ", (_classicWord status)]


privmsg chan txt = send (Privmsg chan $ Right txt)
notice target txt = send (Notice target $ Right txt)


pickRandom :: [a] -> IO a
pickRandom [] = error "empty list"
pickRandom l = do
    idx <- randomRIO (0, length l -1)
    pure $ l !! idx
    

onRegister :: User -> IRC Game ()
onRegister user = do 
    registeredPlayers %= nub . (user:)
    privmsg gamechan $ T.unwords [user, "est inscrit."]

onEnd :: IRC Game ()
onEnd = do
    privmsg gamechan "La partie est finie." 
    g <- getState
    putState $ Pending [] $ _database g

onReveal user = do
    players . playingOrder %= delete user 
    players . revealed %= (user:)
    g <- getState 
    onReveal' g
    clearVotes 
    isFinished

 where onReveal' g 
        | user `elem` _classicPlayers (_players g) = do
            privmsg gamechan $ "Tu es un simple joueur." 
        | user `elem` _undercoverPlayers (_players g) = do
            privmsg gamechan $ "Tu es l'undercover, sneaky rat."
            players . nbRevealedUC += 1
        | isJust (_mrWhite $ _players g) && user == fromJust (_mrWhite $ _players g) = do
            privmsg gamechan "Tu es M. White" >> pure g
            players . mrWhiteRevealed .= True
        | otherwise = pure ()
       isFinished = do
           g <- getState
           let nbucrevealed = _nbRevealedUC $ _players g
               mrwhitefound = _mrWhiteRevealed $ _players g
               uclist = _undercoverPlayers $ _players g
               revealedlist = _revealed $ _players g
               nbremaininguc = length uclist - nbucrevealed
               nbremainingplayers = (length $ _playingOrder $ _players g) - nbremaininguc
           if nbucrevealed == length uclist && mrwhitefound 
                then do
                    privmsg gamechan "Tous les intrus ont étés trouvés."
                    onEnd
            else if mrwhitefound && nbremaininguc >= nbremainingplayers
                then do
                    privmsg gamechan "Les undercover ont gagné !"
                    onEnd
            else if not mrwhitefound && length (_playingOrder $ _players g) == 1
                then do 
                    privmsg gamechan "M. White a gagné !"
            else pure ()


onSay :: User -> T.Text -> IRC Game ()
onSay user words = do
    g <- getState
    ret <- onSay' g
    putState ret

  where onSay' g = case user `M.lookup` _wordsSaids (_decisions g) of
                                 Nothing -> pure g
                                 Just _ -> pure $ g & decisions . wordsSaids %~ M.adjust (++ [words]) user 
onUnsay :: User -> Game -> IRC Game Game
onUnsay user g = case user `M.lookup` _wordsSaids (_decisions g) of
                Nothing -> pure g
                Just (x:xs) -> pure $ g & decisions . wordsSaids %~ M.adjust (init) user 
                Just _ -> pure g


onWords :: IRC Game ()
onWords = do
    g <- getState
    let dumpEntry (k, v) = T.concat [k, " (", T.unwords v,")"]
    privmsg gamechan $ T.unwords $ (\pi -> dumpEntry (pi, fromJust $ M.lookup pi $ _wordsSaids $ _decisions g )) <$> (_playingOrder $ _players g) 
onVote user name g = case user `M.lookup` _votes (_decisions g) of
    Nothing -> pure g
    Just x -> pure $ g & decisions . votes %~ M.insert user (Just name)
onVotestatus g = privmsg gamechan votestring >> pure g
    where computeVote user = length [vote | vote <- M.elems $ _votes $ _decisions g, isJust vote, fromJust vote == user]
          votestatus = computeVote <$> (_playingOrder $ _players g)
          votestring = T.unwords [T.concat[user,"(",T.pack (show nbvotes),")"] | (user, nbvotes) <- zip (_playingOrder $ _players g) votestatus]
clearVotes :: IRC Game ()
clearVotes = decisions . votes %= reset
    where reset db = (\_ -> Nothing) <$> db

getState :: IRC Game Game
getState = do
    tmp <- get userState <$> getIRCState 
    liftIO $ atomically $ readTVar tmp
putState :: Game -> IRC Game ()    
putState s = do
    tmp <- get userState <$> getIRCState 
    liftIO $ atomically $ writeTVar tmp s



isPending :: IRC Game Bool
isPending = do
    s <- getState
    case s of
        Pending _ _ -> pure True
        _             -> pure False
isStarted :: IRC Game Bool
isStarted = not <$> isPending


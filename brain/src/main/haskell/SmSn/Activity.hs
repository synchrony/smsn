{-
:! ghc -c SmSn/Activity.hs
:load SmSn/Activity

SmSn.Activity.main
-}

module SmSn.Activity where

import SmSn.Bayesian

import Data.Map as M
import Data.Maybe as Y
import Data.List as L
import Data.List.Split as S

type Id = String

data Action = ChangeValue Id
              | Create Id
              | Link Id Id
              | SetProps Id
              | Unlink Id Id
              | View Id
type LogEntry = (Integer, Action)

data Vertex = Vertex {
  timeProp :: Integer, idProp :: String, weightProp :: Double, priorityProp :: Double, sourceProp :: String,
  titleProp :: String, aliasProp :: String, shortcutProp :: String } deriving Show

main = do
  activityText <- readFile "/Volumes/encrypted/joshkb/stats/activity.log"
  verticesText <- readFile "/tmp/joshkb-vertices.tsv"
  let logEntries = loadActivity activityText
  let vertices = loadVertices verticesText
  let idTitleMap = toIdTitleMap vertices
  let ids = deduplicate $ viewIds idTitleMap logEntries
  let wm = createMap 2 ids
      bms = bayesianMapWithStarts wm
  beginOut (showEntity idTitleMap) bms

deduplicate :: Eq a => [a] -> [a]
deduplicate (x1:(x2:xs)) = if (x1 == x2) then deduplicate (x2:xs) else x1:(deduplicate (x2:xs))
deduplicate _ = []

showEntity idTitleMap id = title ++ "\n"
  where title = Y.fromMaybe "" $ M.lookup id idTitleMap

viewIds :: Map String String -> [LogEntry] -> [String]
viewIds idTitleMap entries = L.map viewId views
  where views = L.filter isViewWithTitle entries
        viewId (time, View id) = id
        isViewWithTitle (time, View id) = Y.maybe False (\t -> True) $ M.lookup id idTitleMap
        isViewWithTitle _ = False

toIdTitleMap :: [Vertex] -> Map String String
toIdTitleMap vertices = M.fromList pairs
  where pairs = L.map (\v -> (idProp v, titleProp v)) vertices

loadActivity :: String -> [LogEntry]
loadActivity text = L.map (\l -> toLogEntry $ S.splitOn "\t" l) $ lines text

toLogEntry :: [String] -> LogEntry
toLogEntry [time', "change-value", id] = (read time', ChangeValue id)
toLogEntry [time', "create", id] = (read time', Create id)
toLogEntry [time', "link", fromId, toId] = (read time', Link fromId toId)
toLogEntry [time', "set-props", id] = (read time', SetProps id)
toLogEntry [time', "unlink", fromId, toId] = (read time', Unlink fromId toId)
toLogEntry [time', "view", id] = (read time', View id)

loadVertices :: String -> [Vertex]
loadVertices text = toVertexList dataLines
  -- skip the header line
  where dataLines = tail $ lines text

toVertexList :: [String] -> [Vertex]
toVertexList lines = L.map (\s -> toVertex $ S.splitOn "\t" s) lines

toVertex :: [String] -> Vertex
toVertex [time', id', weight', priority', source', _, _, _, title', alias', shortcut']
  = Vertex (read time') id' (read weight') (read priority') source' title' alias' shortcut'

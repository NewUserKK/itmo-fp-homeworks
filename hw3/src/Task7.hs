{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Task7
 ( changeExtensions
 , allFiles
 , removeIfEmpty
 , move
 , getPath
 ) where

import Data.List.Split (splitOn)
import Lens.Micro
import Control.Monad (join)
import Task5.FS
import Task6 (cd)
import Data.Maybe (fromMaybe)

--Базовая версия
--
--Реализуйте следующие функции, используя линзы:
--
--    Изменить в директории расширения всех файлов на другое (нерекурсивно).
--    Получить имена всех файлов и директорий рекурсивно.
--    Удалить выбранную поддиректорию, только если она пустая.
--
--Усложнённое задание
--
--    Получить полный путь к файлу с названием файла относительно стартового узла FS.
--
--Интерфейс должен быть следующим:
--
--ghci> myDir ^? move "A" . move "B" . getPath  -- myDir is labeled by 'root'
--Just "root/A/B/"

changeExtensions :: String -> FS -> FS
changeExtensions newExtension =
  _Dir.contents.traversed._File.name %~ (modifyExtension newExtension)

move :: String -> Traversal' FS FS
move dirName f dir@(Dir{}) = 
  ((dir ^? cd dirName) <&> (f . changeName)) `orElse` pure dir 
  where
    changeName :: FS -> FS
    changeName = name %~ ((dir^.name ++ "/") ++)
move _ _ file@(File{})= pure file

getPath :: Traversal' FS String
getPath = name

allFiles :: FS -> [String]
allFiles fs = (fs ^. name) : (join $ (fs ^.. _Dir . contents . traversed) <&> allFiles)

removeIfEmpty :: String -> FS -> FS
removeIfEmpty dirName =
  _Dir . contents %~ 
    filter (\f -> 
      f^.name /= dirName || (null $ f^?_Dir) || (not . null $ f^.contents)
    )

modifyExtension :: String -> String -> String
modifyExtension newExtension fileName =
  case splitOn "." fileName of
    first:_ -> first ++ "." ++ newExtension
    [] -> "." ++ newExtension

orElse :: Maybe a -> a -> a
orElse = flip fromMaybe
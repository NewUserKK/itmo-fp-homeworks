{-# OPTIONS_GHC -fno-warn-type-defaults #-}  -- too annoying, sorry
{-# LANGUAGE OverloadedStrings #-}

module CVSSpec where

import qualified Data.ByteString.Lazy as BS
import Test.Hspec
import CVS
import Filesystem
import TestUtils
import Data.Maybe (fromJust)
import File (documentContent)
import Path (Path)

spec :: Spec
spec = do
  describe "CVS.cvsInit" $ do
    it "initializes CVS in given directory" $ do
      let expected = Just rootCVS
      let actual = do
            _ <- cvsInit (toPath "/")
            getFileByPath (toPath "/.cvs")
      actual `evalShouldBe` expected
    it "fails to initialize if CVS already exists" $ do
      let actual = do
            _ <- cvsInit (toPath "/")
            _ <- cvsInit (toPath "/dir1")
            getFileByPath (toPath "/dir1/.cvs")
      evalShouldThrow actual
  describe "CVS.cvsAdd" $ do
    it "adds file to CVS with 0 revision" $ do
      let expected = Just True
      let actual = do
            _ <- cvsInit (toPath "/")
            _ <- cvsAdd (toPath "/dir1")
            getCVSRevision (toPath "/dir1/file1.txt") 0 >>= return . (True <$)
      actual `evalShouldBe` expected
    it "fails to add if CVS does not exist" $ do
      let actual = do
            _ <- cvsAdd (toPath "/dir1")
            getCVSRevision (toPath "/dir1") 0
      evalShouldThrow actual
  describe "CVS.cvsUpdate" $ do
    it "updates file within CVS" $ do
      let expected = Just True
      let actual = do
            _ <- cvsInit (toPath "/")
            _ <- cvsAdd (toPath "/dir1")
            _ <- cvsUpdate (toPath "/dir1/file1.txt") "upd"
            getCVSRevision (toPath "/dir1/file1.txt") 1 >>= return . (True <$)
      actual `evalShouldBe` expected
    it "fails to update if CVS does not exist" $ do
      let actual = do
            _ <- cvsUpdate (toPath "/dir1/file1.txt") "upd"
            getCVSRevision (toPath "/dir1") 0
      evalShouldThrow actual
    it "fails to update if file not added to CVS" $ do
      let actual = do
            _ <- cvsInit (toPath "/")
            _ <- cvsUpdate (toPath "/dir1/file1.txt") "upd"
            getCVSRevision (toPath "/dir1") 0
      evalShouldThrow actual
  describe "CVS.getCVSRevision" $ do
    it "retrieves CVS revision by number" $ do
      let expected = 1
      let actual = do
            _ <- cvsInit (toPath "/")
            _ <- cvsAdd (toPath "/dir1")
            _ <- cvsUpdate (toPath "/dir1/file1.txt") "upd"
            rev <- fromJust <$> getCVSRevision (toPath "/dir1/file1.txt") 1
            commitIndex <$> getCommitInfo rev
      actual `evalShouldBe` expected
    it "returns Nothing if revision does not exist" $ do
      let expected = Nothing
      let actual = do
            _ <- cvsInit (toPath "/")
            _ <- cvsAdd (toPath "/dir1")
            getCVSRevision (toPath "/dir1/file1.txt") 1
      actual `evalShouldBe` expected
  describe "CVS.removeFromCVS" $ do
    it "removes file from CVS" $ do
      let actual = do
            let path = toPath "/dir1/file1.txt"
            _ <- cvsInit (toPath "/")
            _ <- cvsAdd (toPath "/dir1")
            _ <- getCVSRevision path 0
            removeFromCVS path
            getCVSRevision path 0
      evalShouldThrow actual
    it "throws if file is not added to CVS" $ do
      let actual = do
            _ <- cvsInit (toPath "/")
            removeFromCVS (toPath "/dir1/file1.txt")
      evalShouldThrow actual
  describe "CVS.removeRevision" $ do
    it "removes revision of the file from CVS" $ do
      let expected = Nothing
      let actual = do
            let path = toPath "/dir1/file1.txt"
            _ <- cvsInit (toPath "/")
            _ <- cvsAdd (toPath "/dir1")
            _ <- cvsUpdate path "upd"
            _ <- getCVSRevision path 1
            removeRevision path 1
            getCVSRevision path 1
      actual `evalShouldBe` expected
    it "throws if revision does not exist" $ do
      let actual = do
            _ <- cvsInit (toPath "/")
            _ <- cvsAdd (toPath "/dir1")
            removeRevision (toPath "/dir1/file1.txt") 1
      evalShouldThrow actual
    it "throws if file is not added to CVS" $ do
      let actual = do
            _ <- cvsInit (toPath "/")
            removeRevision (toPath "/dir1/file1.txt") 1
      evalShouldThrow actual
  describe "CVS.getAllRevisionsOfDirectory" $ do
    it "retrieves revisions of all files within directory" $ do
      let expectedSize = 2
      let actual = do
            let path = toPath "/"
            _ <- cvsInit path
            _ <- cvsAdd path
            revs <- getDirectoryByPathOrError path >>= getAllRevisionsOfDirectory
            return $ length revs
      actual `evalShouldBe` expectedSize
  describe "CVS.getAllRevisionsOfDocument" $ do
    it "retrieves revisions of document" $ do
      let expectedSize = 3
      let actual = do
            let path = toPath "/file1.txt"
            _ <- cvsInit $ toPath "/"
            _ <- cvsAdd path
            _ <- cvsUpdate path "upd1"
            _ <- cvsUpdate path "upd2"
            revs <- getDocumentByPathOrError path >>= getAllRevisionsOfDocument
            return $ length revs
      actual `evalShouldBe` expectedSize
  describe "CVS.getCommitInfo" $ do
    it "retrieves correct commit info for revision" $ do
      let path = toPath "/file1.txt"
      let expected = CommitInfo {
          commitIndex = 1
        , commitRealFilePath = path
        , commitMessage = "upd1"
        }
      let actual = do
            _ <- cvsInit $ toPath "/"
            _ <- cvsAdd path
            _ <- cvsUpdate path "upd1"
            getCVSRevisionOrError path 1 >>= getCommitInfo
      actual `evalShouldBe` expected
  describe "CVS.getFileFromRevision" $ do
    it "retrieves correct file from revision" $ do
      let path = toPath "/file1.txt"
      let expected = documentContent file1
      let actual = do
            _ <- cvsInit $ toPath "/"
            _ <- cvsAdd path
            rev <- getCVSRevisionOrError path 0
            getFileFromRevision rev >>= return . documentContent
      actual `evalShouldBe` expected
  describe "CVS.mergeRevisions" $ do
    it "left merge returns left file" $ do
      let path = toPath "/file1.txt"
      let oldContent = documentContent file1
      let newContent = "abc"
      let expected = (oldContent, oldContent)
      let actual = testMerge path newContent MergeLeft
      actual `evalShouldBe` expected
    it "right merge returns right file" $ do
      let path = toPath "/file1.txt"
      let newContent = "abc"
      let expected = (newContent, newContent)
      let actual = testMerge path newContent MergeRight
      actual `evalShouldBe` expected
    it "both merge returns concatenations of contents" $ do
      let path = toPath "/file1.txt"
      let oldContent = documentContent file1
      let newContent = "abc"
      let expectedContent = oldContent <> "\n" <> newContent
      let expected = (expectedContent, expectedContent)
      let actual = testMerge path newContent MergeBoth
      actual `evalShouldBe` expected

testMerge :: Path -> BS.ByteString -> MergeStrategy -> FileSystem (BS.ByteString, BS.ByteString)
testMerge path newContent strategy = do
  _ <- cvsInit $ toPath "/"
  _ <- cvsAdd path
  let new = file1 { documentContent = newContent }
  _ <- createFile path new True
  _ <- cvsUpdate path "upd"
  rev0 <- getCVSRevisionOrError path 0
  rev1 <- getCVSRevisionOrError path 1
  _ <- mergeRevisions rev0 rev1 strategy
  rev2 <- getCVSRevisionOrError path 2
  rev2File <- getFileFromRevision rev2
  let rev2Content = documentContent rev2File
  updatedFile <- getDocumentByPathOrError path
  return (rev2Content, documentContent updatedFile)

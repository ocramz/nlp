{-# language OverloadedStrings #-}
{-# language LambdaCase #-}
module NLP.WordNet31.Parsers where

import Data.Functor ( (<$) )
import Control.Applicative ( (<|>) )
import Control.Monad (void, replicateM)

import qualified Data.Attoparsec.Text as A
import qualified Data.Attoparsec.Text.Lazy as AL
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T


{- |

For each syntactic category, two files are needed to represent the contents of the WordNet database - index. pos and data. pos , where pos is noun , verb , adj and adv . The other auxiliary files are used by the WordNet library's searching functions and are needed to run the various WordNet browsers.

Each index file is an alphabetized list of all the words found in WordNet in the corresponding part of speech. On each line, following the word, is a list of byte offsets (synset_offset s) in the corresponding data file, one for each synset containing the word. Words in the index file are in lower case only, regardless of how they were entered in the lexicographer files. This folds various orthographic representations of the word into one line enabling database searches to be case insensitive. See wninput(5WN) for a detailed description of the lexicographer files

A data file for a syntactic category contains information corresponding to the synsets that were specified in the lexicographer files, with relational pointers resolved to synset_offset s. Each line corresponds to a synset. Pointers are followed and hierarchies traversed by moving from one synset to another via the synset_offset s.


-}


{-
-- examples from index.noun :

compliance n 3 4 ! @ ~ + 3 1 01206166 04648510 01169416  
noncompliance n 1 4 ! @ ~ + 1 0 01182197
-}

{-
lemma (meanings) (pointers) 
-}

data IndexRow = IndexRow Lemma POSp Integer [ByteOffset] deriving (Eq, Show)

indexRow :: A.Parser IndexRow
indexRow = do
  l <- lemma <* skipSpace
  p <- pos <* skipSpace
  nsyns <- A.decimal <* skipSpace
  nptrs <- A.decimal <* skipSpace
  ptrss <- ptrs nptrs p
  void A.decimal <* skipSpace
  ntagsense <- A.decimal <* skipSpace
  offs <- offsets nsyns <* A.endOfLine
  pure $ IndexRow l ptrss ntagsense offs
  
  
--   ps <- ptrs
--   skipSpace

type Lemma = T.Text
type ByteOffset = Int

lemma :: AL.Parser Lemma
lemma = A.takeWhile (A.inClass "a-z_")

-- | Part-of-speech
data POS = Noun | Verb | Adj |  Adv deriving (Eq, Show)
data POSp = NounP [PtrSymNoun] | VerbP [PtrSymVerb] | AdjP [PtrSymAdj] |  AdvP [PtrSymAdv] deriving (Eq, Show)

pos :: A.Parser POS 
pos =
  (Noun  <$ A.char 'n') <|>
  (Verb  <$ A.char 'v') <|>
  (Adj  <$ A.char 'a') <|>
  (Adv  <$ A.char 'r')

ptrs :: Int -> POS -> A.Parser POSp
ptrs n pp = 
  case pp of
    Noun -> NounP <$> reps n ptrSymNoun
    Verb -> VerbP <$> reps n ptrSymVerb
    Adj -> AdjP <$> reps n ptrSymAdj
    Adv -> AdvP <$> reps n ptrSymAdv    

reps :: Int -> A.Parser a -> A.Parser [a]
reps n f = replicateM n (f <* skipSpace)

offsets :: Int -> A.Parser [ByteOffset]
offsets n = reps n A.decimal



-- synsetCnt = A.decimal

-- pCnt = A.decimal

-- data Pos = Noun [PtrSymNoun] | 

{-
The pointer_symbol s for nouns are:

    !    Antonym 
    @    Hypernym 
    @i    Instance Hypernym 
     ~    Hyponym 
     ~i    Instance Hyponym 
    #m    Member holonym 
    #s    Substance holonym 
    #p    Part holonym 
    %m    Member meronym 
    %s    Substance meronym 
    %p    Part meronym 
    =    Attribute 
    +    Derivationally related form         
    ;c    Domain of synset - TOPIC 
    -c    Member of this domain - TOPIC 
    ;r    Domain of synset - REGION 
    -r    Member of this domain - REGION 
    ;u    Domain of synset - USAGE 
    -u    Member of this domain - USAGE 
-}
data PtrSymNoun = AntoN | HyperN | IHyperN | HypoN | IHypoN | MHoloN | SHoloN | PHoloN | MMeroN | SMeroN | PMeroN | AttrN | DerivRelN | DSynTopicN | MDomTopicN | DSynRegionN | MDomRegionN | DSynUsageN | MDomUsageN deriving (Eq, Show)

ptrSymNoun :: A.Parser PtrSymNoun
ptrSymNoun =
  (AntoN <$ A.char '!') <|>
  (HyperN <$ A.char '@') <|>
  (IHyperN <$ A.string "@i") <|>
  (HypoN <$ A.char '~') <|>
  (IHypoN <$ A.string "~i" ) <|>
  (MHoloN <$ A.string "#m") <|>
  (SHoloN <$ A.string "#s") <|>
  (PHoloN <$ A.string "#p") <|>
  (MMeroN <$ A.string "%m") <|>
  (SMeroN <$ A.string "%s") <|>
  (PMeroN <$ A.string "%p") <|>
  (AttrN <$ A.char '=') <|>
  (DerivRelN <$ A.char '+') <|>
  (DSynTopicN <$ A.string ";c") <|>
  (MDomTopicN <$ A.string "-c") <|>
  (DSynRegionN <$ A.string ";r") <|>
  (MDomRegionN <$ A.string "-r") <|>
  (DSynUsageN <$ A.string ";u") <|>
  (MDomUsageN <$ A.string "-u") 

{-
The pointer_symbol s for verbs are:

    !    Antonym 
    @    Hypernym 
     ~    Hyponym 
    *    Entailment 
    >    Cause 
    ^    Also see 
    $    Verb Group 
    +    Derivationally related form         
    ;c    Domain of synset - TOPIC 
    ;r    Domain of synset - REGION 
    ;u    Domain of synset - USAGE 
-}  
data PtrSymVerb = AntoV | HyperV | HypoV | EntailV | CauseV | AlsoSeeV | VerbGroupV | DerivRelV | DSynTopicV | DSynRegionV | DSynUsageV deriving (Eq, Show)

ptrSymVerb :: A.Parser PtrSymVerb
ptrSymVerb =
  (AntoV <$ A.char '!') <|>
  (HyperV <$ A.char '@') <|>
  (HypoV <$ A.char '~') <|>
  (EntailV <$ A.char '*') <|>
  (CauseV <$ A.char '>') <|>
  (AlsoSeeV <$ A.char '^') <|>
  (VerbGroupV <$ A.char '$') <|>
  (DerivRelV <$ A.char '+') <|>
  (DSynTopicV <$ A.string ";c") <|>
  (DSynRegionV <$ A.string ";r") <|>
  (DSynUsageV <$ A.string ";u") 

{-
The pointer_symbol s for adjectives are:

    !    Antonym 
    &    Similar to 
    <    Participle of verb 
    \    Pertainym (pertains to noun) 
    =    Attribute 
    ^    Also see 
    ;c    Domain of synset - TOPIC 
    ;r    Domain of synset - REGION 
    ;u    Domain of synset - USAGE 
-}
data PtrSymAdj = AntoAdj | SimilarToAdj | ParticipleAdj | PertainymAdj | AttributeAdj | AlsoSeeAdj | DSynTopicAdj | DSynRegionAdj | DSynUsageAdj deriving (Eq, Show)

ptrSymAdj :: A.Parser PtrSymAdj
ptrSymAdj =
  (AntoAdj <$ A.char '!') <|>
  (SimilarToAdj <$ A.char '&') <|>
  (ParticipleAdj <$ A.char '<') <|>
  (PertainymAdj <$ A.char '\\') <|>
  (AlsoSeeAdj <$ A.char '^') <|>
  (DSynTopicAdj <$ A.string ";c") <|>
  (DSynRegionAdj <$ A.string ";r") <|>
  (DSynUsageAdj <$ A.string ";u")   

{-
The pointer_symbol s for adverbs are:

    !    Antonym 
    \    Derived from adjective 
    ;c    Domain of synset - TOPIC 
    ;r    Domain of synset - REGION 
    ;u    Domain of synset - USAGE 
-}
data PtrSymAdv = AntoAdv | DerivedFromAdjAdv | DSynTopicAdv | DSynRegionAdv | DSynUsageAdv deriving (Eq, Show)

ptrSymAdv :: A.Parser PtrSymAdv
ptrSymAdv =
  (AntoAdv <$ A.char '!') <|>
  (DerivedFromAdjAdv <$ A.char '\\') <|>
  (DSynTopicAdv <$ A.string ";c") <|>
  (DSynRegionAdv <$ A.string ";r") <|>
  (DSynUsageAdv <$ A.string ";u")   



{- |
Each index file begins with several lines containing a copyright notice, version number, and license agreement. These lines all begin with two spaces and the line number so they do not interfere with the binary search algorithm that is used to look up entries in the index files. All other lines are in the following format. In the field descriptions, number always refers to a decimal integer unless otherwise defined.

-- examples from index.noun :

compliance n 3 4 ! @ ~ + 3 1 01206166 04648510 01169416  
noncompliance n 1 4 ! @ ~ + 1 0 01182197


lemma  pos  synset_cnt  p_cnt  [ptr_symbol...]  sense_cnt  tagsense_cnt   synset_offset  [synset_offset...] 

lemma
    lower case ASCII text of word or collocation. Collocations are formed by joining individual words with an underscore (_ ) character.

pos
    Syntactic category: n for noun files, v for verb files, a for adjective files, r for adverb files.

All remaining fields are with respect to senses of lemma in pos .

synset_cnt
    Number of synsets that lemma is in. This is the number of senses of the word in WordNet. See Sense Numbers below for a discussion of how sense numbers are assigned and the order of synset_offset s in the index files.

p_cnt
    Number of different pointers that lemma has in all synsets containing it.

ptr_symbol
    A space separated list of p_cnt different types of pointers that lemma has in all synsets containing it. See wninput(5WN) for a list of pointer_symbol s. If all senses of lemma have no pointers, this field is omitted and p_cnt is 0 .

sense_cnt
    Same as sense_cnt above. This is redundant, but the field was preserved for compatibility reasons.

tagsense_cnt
    Number of senses of lemma that are ranked according to their frequency of occurrence in semantic concordance texts.

synset_offset
    Byte offset in data.pos file of a synset containing lemma . Each synset_offset in the list corresponds to a different sense of lemma in WordNet. synset_offset is an 8 digit, zero-filled decimal integer that can be used with fseek(3) to read a synset from the data file. When passed to read_synset(3WN) along with the syntactic category, a data structure containing the parsed synset is returned. 
-}

skipSpace :: A.Parser ()
skipSpace = void A.space

-- index = do
--   l <- lemma
--   p <- pos <* skipSpace
--   pCnt <- A.decimal <* skipSpace
  
  



{- |
-- example from data.noun "compliance" :

0101 + 02548492 v 0101 ! 01182197 n 0101 ~ 01169875 n 0000 | the act of obeying; dutiful or submissive behavior with respect to another person

-}

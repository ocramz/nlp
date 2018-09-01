{-# language OverloadedStrings #-}
{-# language LambdaCase #-}
module NLP.WordNet31.Parsers where

import Data.Functor ( (<$) )
import Control.Applicative ( (<|>) )
import Control.Monad (replicateM)

import qualified Data.Attoparsec.Text as A
import qualified Data.Attoparsec.Text.Lazy as AL
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T


{- |

For each syntactic category, two files are needed to represent the contents of the WordNet database - index. pos and data. pos , where pos is noun , verb , adj and adv . The other auxiliary files are used by the WordNet library's searching functions and are needed to run the various WordNet browsers.

Each index file is an alphabetized list of all the words found in WordNet in the corresponding part of speech. On each line, following the word, is a list of byte offsets (synset_offset s) in the corresponding data file, one for each synset containing the word. Words in the index file are in lower case only, regardless of how they were entered in the lexicographer files. This folds various orthographic representations of the word into one line enabling database searches to be case insensitive. See wninput(5WN) for a detailed description of the lexicographer files

A data file for a syntactic category contains information corresponding to the synsets that were specified in the lexicographer files, with relational pointers resolved to synset_offset s. Each line corresponds to a synset. Pointers are followed and hierarchies traversed by moving from one synset to another via the synset_offset s.



-- data.noun "compliance" :

0101 + 02548492 v 0101 ! 01182197 n 0101 ~ 01169875 n 0000 | the act of obeying; dutiful or submissive behavior with respect to another person
-}

type Lemma = T.Text

lemma :: AL.Parser Lemma
lemma = A.takeWhile (A.inClass "a-z_")

data Pos = Noun | Verb | Adj | Adv deriving (Eq, Show, Ord)

noun :: A.Parser Pos
noun =
  (Noun <$ A.char 'n') <|>
  (Verb <$ A.char 'v') <|>
  (Adj <$ A.char 'a') <|>
  (Adv <$ A.char 'r')

synsetCnt = A.decimal

pCnt = A.decimal

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

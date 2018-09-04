module NLP.WordNet31.Types where

import qualified Data.Text as T
import Data.Int (Int64(..))
import Data.Char (toLower)
import qualified Data.Map as M

import qualified Data.List.NonEmpty as NE

type Lemma = T.Text
type ByteOffset = Int64


newtype LemmaDB a = LemmaDB (M.Map Lemma a)



-- | A row of an index file (e.g. index.noun)
data IndexRow = IndexRow {
    ixLemma :: Lemma 
  , ixPOSp :: POSp  -- ^ Part-of-speech label, containing its parsed 'pointer_symbol' s
  , ixNTagsense :: Integer -- ^ Number of senses of lemma that are ranked according to their frequency of occurrence in semantic concordance texts.
  , ixPtrs :: NE.NonEmpty ByteOffset -- ^ Byte offset in @data.pos@ file of a synset containing lemma, where @pos@ can be either 'noun', 'verb', 'adj' or 'adv' . Each 'synset_offset' in the list corresponds to a different sense of lemma in WordNet. 'synset_offset' is an 8 digit, zero-filled decimal integer that can be used with @fseek(3)@ to read a synset from the data file. 
  } deriving (Eq, Show)

-- | Part-of-speech
data POSp = NounP [PtrSymNoun] | VerbP [PtrSymVerb] | AdjP [PtrSymAdj] |  AdvP [PtrSymAdv] deriving (Eq, Show)

data POS = Noun | Verb | Adj |  Adv deriving (Eq, Show)
showPOS :: POS -> String
showPOS p = map toLower $ show p

    
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


{-
The pointer_symbol s for adverbs are:

    !    Antonym 
    \    Derived from adjective 
    ;c    Domain of synset - TOPIC 
    ;r    Domain of synset - REGION 
    ;u    Domain of synset - USAGE 
-}
data PtrSymAdv = AntoAdv | DerivedFromAdjAdv | DSynTopicAdv | DSynRegionAdv | DSynUsageAdv deriving (Eq, Show)

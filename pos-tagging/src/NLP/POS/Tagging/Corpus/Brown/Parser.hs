{-# language DeriveGeneric, OverloadedStrings  #-}
{-
Parser for the Brown corpus

Manual : http://clu.uni.no/icame/brown/bcm.html
-}
module NLP.POS.Tagging.Corpus.Brown.Parser (Tag(..), Tagged(..), posTag) where

import GHC.Generics

import Control.Applicative
import Data.Functor ((<$))
import qualified Data.List.NonEmpty as NE
import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T
import qualified Data.Text.IO as T (readFile)


-- | Tags used in the Brown corpus 
data Tag =
  Sentence -- ^   . 	sentence (. ; ? *)
  | LParen -- ^ ( 	left paren
  | RParen -- ^ ) 	right paren
  | Not -- ^ * 	not, n't
  | Dash -- ^ -- 	dash
  | Comma -- ^ , 	comma
  | Colon -- ^ : 	colon
  | ABL -- ^ ABL 	pre-qualifier (quite, rather)
  | ABN -- ^ ABN 	pre-quantifier (half, all)
  | ABX -- ^ ABX 	pre-quantifier (both)
  | AP -- ^ AP 	post-determiner (many, several, next)
  | AT -- ^ AT 	article (a, the, no)
  | BE -- ^ BE 	be
  | BED -- ^ BED 	were
  | BEDZ -- ^ BEDZ 	was
  | BEG -- ^ BEG 	being
  | BEM -- ^ BEM 	am
  | BEN -- ^ BEN 	been
  | BER -- ^ BER 	are, art
  | BEZ -- ^ BEZ 	is
  | CC -- ^ CC 	coordinating conjunction (and, or)
  | CD -- ^ CD 	cardinal numeral (one, two, 2, etc.)
  | CS -- ^ CS 	subordinating conjunction (if, although)
  | DO -- ^ DO 	do
  | DOD -- ^ DOD 	did
  | DOZ -- ^ DOZ 	does
  | DT -- ^ DT 	singular determiner/quantifier (this, that)
  | DTI -- ^ DTI 	singular or plural determiner/quantifier (some, any)
  | DTS -- ^ DTS 	plural determiner (these, those)
  | DTX -- ^ DTX 	determiner/double conjunction (either)
  | EX -- ^ EX 	existential there
  | FW -- ^ FW 	foreign word (hyphenated before regular tag)
  | HV -- ^ HV 	have
  | HVD -- ^ HVD 	had (past tense)
  | HVG -- ^ HVG 	having
  | HVN -- ^ HVN 	had (past participle)
  | IN -- ^ IN 	preposition
  | JJ -- ^ JJ 	adjective
  | JJR -- ^ JJR 	comparative adjective
  | JJS -- ^ JJS 	semantically superlative adjective (chief, top)
  | JJT -- ^ JJT 	morphologically superlative adjective (biggest)
  | MD -- ^ MD 	modal auxiliary (can, should, will)
  | NC -- ^ NC 	cited word (hyphenated after regular tag)
  | NN -- ^ NN 	singular or mass noun
  | NNPoss -- ^ NN$ 	possessive singular noun
  | NNS -- ^ NNS 	plural noun
  | NNSPoss -- ^ NNS$ 	possessive plural noun
  | NP -- ^ NP 	proper noun or part of name phrase
  | NPPoss -- ^ NP$ 	possessive proper noun
  | NPS -- ^ NPS 	plural proper noun
  | NPSPoss -- ^ NPS$ 	possessive plural proper noun
  | NR -- ^ NR  	adverbial noun (home, today, west)
  | OD -- ^ OD 	ordinal numeral (first, 2nd)
  | PN -- ^ PN 	nominal pronoun (everybody, nothing)
  | PNPoss -- ^ PN$ 	possessive nominal pronoun
  | PPPoss -- ^ PP$ 	possessive personal pronoun (my, our)
  | PPPoss2 -- ^ PP$$ 	second (nominal) possessive pronoun (mine, ours)
  | PPL -- ^ PPL 	singular reflexive/intensive personal pronoun (myself)
  | PPLS -- ^ PPLS 	plural reflexive/intensive personal pronoun (ourselves)
  | PPO -- ^ PPO 	objective personal pronoun (me, him, it, them)
  | PPS -- ^ PPS 	3rd. singular nominative pronoun (he, she, it, one)
  | PPSS -- ^ PPSS 	other nominative personal pronoun (I, we, they, you)
  | PRP -- ^ PRP 	Personal pronoun
  | PRPPoss -- ^ PRP$ 	Possessive pronoun
  | QL -- ^ QL 	qualifier (very, fairly)
  | QLP -- ^ QLP 	post-qualifier (enough, indeed)
  | RB -- ^ RB 	adverb
  | RBR -- ^ RBR 	comparative adverb
  | RBT -- ^ RBT 	superlative adverb
  | RN -- ^ RN 	nominal adverb (here, then, indoors)
  | RP -- ^ RP 	adverb/particle (about, off, up)
  | TO -- ^ TO 	infinitive marker to
  | UH -- ^ UH 	interjection, exclamation
  | VB -- ^ VB 	verb, base form
  | VBD -- ^ VBD 	verb, past tense
  | VBG -- ^ VBG 	verb, present participle/gerund
  | VBN -- ^ VBN 	verb, past participle
  | VBP -- ^ VBP 	verb, non 3rd person, singular, present
  | VBZ -- ^ VBZ 	verb, 3rd. singular present
  | WDT -- ^ WDT 	wh- determiner (what, which)
  | WPPoss -- ^ WP$ 	possessive wh- pronoun (whose)
  | WPO -- ^ WPO 	objective wh- pronoun (whom, which, that)
  | WPS -- ^ WPS 	nominative wh- pronoun (who, which, that)
  | WQL -- ^ WQL 	wh- qualifier (how)
  | WRB -- ^ WRB 	wh- adverb (how, where, when)
  -- | HLMod  -- ^ Headline capitalization modifier
  -- | TLMod  -- ^ Title capitalization modifier
  -- | NCMod  -- ^ Emphasized word modifier
  -- | FWMod  -- ^ Foreign word modifier
  deriving (Eq, Show, Enum, Generic)

-- data Tagged a = Tagged {
--     taggedPos :: a 
--   , tagPrimary :: Tag
--   , tagModifiers :: [Tag]
--   } deriving (Eq, Show)

data Tagged a = Tagged {
    taggedPos :: a
  , tags :: NE.NonEmpty Tag 
                       } deriving (Eq, Show)


-- | Part-of-speech basic parser
posTag :: A.Parser Tag 
posTag =
  (Sentence <$ A.satisfy (A.inClass ".:?") ) <|>
  (LParen <$ A.char '(') <|>
  (RParen <$ A.char ')') <|>
  (Not <$ A.char '*') <|>
  (Dash <$ A.string "--") <|>
  (Comma <$ A.char ',') <|>
  (Colon <$ A.char ':') <|>
  (ABL <$ A.string "abl") <|>
  (ABN <$ A.string "abn") <|>
  (ABX <$ A.string "abx") <|>
  (AP <$ A.string "ap") <|>
  (AT <$ A.string "at") <|>
  (BEDZ <$ A.string "bedz") <|>
  (BED <$ A.string "bed") <|>
  (BE <$ A.string "be") <|>  
  (BEG <$ A.string "beg") <|>
  (BEM <$ A.string "bem") <|>
  (BEN <$ A.string "ben") <|>
  (BER <$ A.string "ber") <|>
  (BEZ <$ A.string "bez") <|>
  (CC <$ A.string "cc") <|>
  (CD <$ A.string "cd") <|>
  (CS <$ A.string "cs") <|>
  (DOD <$ A.string "dod") <|>
  (DOZ <$ A.string "doz") <|>
  (DO <$ A.string "do") <|>  
  (DTI <$ A.string "dti") <|>
  (DTS <$ A.string "dts") <|>
  (DTX <$ A.string "dtx") <|>
  (DT <$ A.string "dt") <|>  
  (EX <$ A.string "ex") <|>
  (FW <$ A.string "fw") <|>
  (HV <$ A.string "hv") <|>
  (HVD <$ A.string "hvd") <|>
  (HVG <$ A.string "hvg") <|>
  (HVN <$ A.string "hvn") <|>
  (IN <$ A.string "in") <|>
  (JJR <$ A.string "jjr") <|>
  (JJS <$ A.string "jjs") <|>
  (JJT <$ A.string "jjt") <|>
  (JJ <$ A.string "jj") <|>  
  (MD <$ A.string "md") <|>
  (NC <$ A.string "nc") <|>
  (NNPoss <$ A.string "nn$") <|>
  (NNS <$ A.string "nns") <|>
  (NNSPoss <$ A.string "nns$") <|>
  (NN <$ A.string "nn") <|>  
  (NP <$ A.string "np") <|>
  (NPPoss <$ A.string "np$") <|>
  (NPS <$ A.string "nps") <|>
  (NPSPoss <$ A.string "nps$") <|>
  (NR <$ A.string "nr") <|>
  (OD <$ A.string "od") <|>
  (PNPoss <$ A.string "pn$") <|>
  (PN <$ A.string "pn") <|>  
  (PPPoss2 <$ A.string "pp$$") <|>
  (PPPoss <$ A.string "pp$") <|>  
  (PPLS <$ A.string "ppls") <|>
  (PPL <$ A.string "ppl") <|>  
  (PPO <$ A.string "ppo") <|>
  (PPSS <$ A.string "ppss") <|>
  (PPS <$ A.string "pps") <|>  
  (PRP <$ A.string "prp") <|>
  (QL <$ A.string "ql") <|>
  (QLP <$ A.string "qlp") <|>
  (RB <$ A.string "rb") <|>
  (RBR <$ A.string "rbr") <|>
  (RBT <$ A.string "rbt") <|>
  (RN <$ A.string "rn") <|>
  (RP <$ A.string "rp") <|>
  (TO <$ A.string "to") <|>
  (UH <$ A.string "uh") <|>
  (VBD <$ A.string "vbd") <|>
  (VBG <$ A.string "vbg") <|>
  (VBN <$ A.string "vbn") <|>
  (VBP <$ A.string "vbp") <|>
  (VBZ <$ A.string "vbz") <|>
  (VB <$ A.string "vb") <|>  
  (WDT <$ A.string "wdt") <|>
  (WPPoss <$ A.string "wp$") <|>
  (WPO <$ A.string "wpo") <|>
  (WPS <$ A.string "wps") <|>
  (WQL <$ A.string "wql") <|>
  (WRB <$ A.string "wrb") 
  -- (HLMod <$ A.string "hl") <|>
  -- (TLMod <$ A.string "tl") <|>
  -- (NCMod <$ A.string "nc") <|>
  -- (FWMod <$ A.string "fw")

-- data PosTag

-- title = posTag <* (A.char '-' >> A.string "tl")
 

-- multiPosTag :: A.Parser (NE.NonEmpty Tag)
-- multiPosTag = NE.fromList <$> A.sepBy posTag (A.string "+")

multi :: A.Parser a -> A.Parser (NE.NonEmpty a)
multi p = NE.fromList <$> A.sepBy p (A.string "+")

foreignWord :: A.Parser Tag
foreignWord = A.string "fw-" *> posTag

citedNoun :: A.Parser Tag
citedNoun = posTag <* A.string "-nc"


cap :: A.Parser Tag
cap = headlineCap <|> titleCap
  where
    headlineCap = posTag <* A.string "-hl"
    titleCap = posTag <* A.string "-tl"




-- | Test data

t0 :: T.Text
t0 = "While/cs emphasizing/vbg that/cs technical/jj details/nns were/bed not/* fully/rb worked/vbn out/rp ,/,"

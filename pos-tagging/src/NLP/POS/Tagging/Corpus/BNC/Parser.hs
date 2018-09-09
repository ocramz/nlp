{-# language DeriveGeneric, OverloadedStrings  #-}
{-
Parser for the BNC corpus ,XML edition 

Manual : http://www.natcorp.ox.ac.uk/docs/URG/index.html


Multiword units are marked using an additional XML element (<mw>) which carries the wordclass assigned to the whole sequence. Within the <mw> element, the individual orthographic words are also marked, using the <w> element in the same way as elsewhere. For example, the multiword unit of course is marked up as follows:
<mw c5="AV0"> 
  <w c5="PRF" hw="of" pos="PREP">of </w> 
  <w c5="NN1" hw="course" pos="SUBST">course </w>
</mw>


-}
module NLP.POS.Tagging.Corpus.BNC.Parser where

import GHC.Generics
import Data.String 
import Data.Functor (($>), (<$))
import Control.Applicative
import Control.Monad 
import qualified Data.ByteString as BS
import qualified Data.Attoparsec.ByteString as A
import Xeno.DOM
import Xeno.Types


data Tag =
    AJ0 -- ^Adjective (general or positive) (e.g. good, old, beautiful)
  | AJC -- ^Comparative adjective (e.g. better, older)
  | AJS -- ^Superlative adjective (e.g. best, oldest)
  | AT0 -- ^Article (e.g. the, a, an, no)
  | AV0 -- ^General adverb: an adverb not subclassified as AVP or AVQ (see below) (e.g. often, well, longer (adv.), furthest.
  | AVP -- ^Adverb particle (e.g. up, off, out)
  | AVQ -- ^Wh-adverb (e.g. when, where, how, why, wherever)
  | CJC -- ^Coordinating conjunction (e.g. and, or, but)
  | CJS -- ^Subordinating conjunction (e.g. although, when)
  | CJT -- ^The subordinating conjunction that
  | CRD -- ^Cardinal number (e.g. one, 3, fifty-five, 3609)
  | DPS -- ^Possessive determiner-pronoun (e.g. your, their, his)
  | DT0 -- ^General determiner-pronoun: i.e. a determiner-pronoun which is not a DTQ or an AT0.
  | DTQ -- ^Wh-determiner-pronoun (e.g. which, what, whose, whichever)
  | EX0 -- ^Existential there, i.e. there occurring in the there is ... or there are ... construction
  | ITJ -- ^Interjection or other isolate (e.g. oh, yes, mhm, wow)
  | NN0 -- ^Common noun, neutral for number (e.g. aircraft, data, committee)
  | NN1 -- ^Singular common noun (e.g. pencil, goose, time, revelation)
  | NN2 -- ^Plural common noun (e.g. pencils, geese, times, revelations)
  | NP0 -- ^Proper noun (e.g. London, Michael, Mars, IBM)
  | ORD -- ^Ordinal numeral (e.g. first, sixth, 77th, last) .
  | PNI -- ^Indefinite pronoun (e.g. none, everything, one [as pronoun], nobody)
  | PNP -- ^Personal pronoun (e.g. I, you, them, ours)
  | PNQ -- ^Wh-pronoun (e.g. who, whoever, whom)
  | PNX -- ^Reflexive pronoun (e.g. myself, yourself, itself, ourselves)
  | POS -- ^The possessive or genitive marker 's or '
  | PRF -- ^The preposition of
  | PRP -- ^Preposition (except for of) (e.g. about, at, in, on, on behalf of, with)
  | PUL -- ^Punctuation: left bracket - i.e. ( or [
  | PUN -- ^Punctuation: general separating mark - i.e. . , ! , : ; - or ?
  | PUQ -- ^Punctuation: quotation mark - i.e. ' or "
  | PUR -- ^Punctuation: right bracket - i.e. ) or ]
  | TO0 -- ^Infinitive marker to
  | UNC -- ^Unclassified items which are not appropriately considered as items of the English lexicon.
  | VBB -- ^The present tense forms of the verb BE, except for is, 's: i.e. am, are, 'm, 're and be [subjunctive or imperative]
  | VBD -- ^The past tense forms of the verb BE: was and were
  | VBG -- ^The -ing form of the verb BE: being
  | VBI -- ^The infinitive form of the verb BE: be
  | VBN -- ^The past participle form of the verb BE: been
  | VBZ -- ^The -s form of the verb BE: is, 's
  | VDB -- ^The finite base form of the verb DO: do
  | VDD -- ^The past tense form of the verb DO: did
  | VDG -- ^The -ing form of the verb DO: doing
  | VDI -- ^The infinitive form of the verb DO: do
  | VDN -- ^The past participle form of the verb DO: done
  | VDZ -- ^The -s form of the verb DO: does, 's
  | VHB -- ^The finite base form of the verb HAVE: have, 've
  | VHD -- ^The past tense form of the verb HAVE: had, 'd
  | VHG -- ^The -ing form of the verb HAVE: having
  | VHI -- ^The infinitive form of the verb HAVE: have
  | VHN -- ^The past participle form of the verb HAVE: had
  | VHZ -- ^The -s form of the verb HAVE: has, 's
  | VM0 -- ^Modal auxiliary verb (e.g. will, would, can, could, 'll, 'd)
  | VVB -- ^The finite base form of lexical verbs (e.g. forget, send, live, return) [Including the imperative and present subjunctive]
  | VVD -- ^The past tense form of lexical verbs (e.g. forgot, sent, lived, returned)
  | VVG -- ^The -ing form of lexical verbs (e.g. forgetting, sending, living, returning)
  | VVI -- ^The infinitive form of lexical verbs (e.g. forget, send, live, return)
  | VVN -- ^The past participle form of lexical verbs (e.g. forgotten, sent, lived, returned)
  | VVZ -- ^The -s form of lexical verbs (e.g. forgets, sends, lives, returns)
  | XX0 -- ^The negative particle not or n't
  | ZZ0 -- ^Alphabetical symbols (e.g. A, a, B, b, c, d)
  deriving (Eq, Show, Enum, Generic)

tag :: A.Parser Tag
tag =
  (A.string "AJ0" $> AJ0) <|> 
  (A.string "AJC" $> AJC) <|> 
  (A.string "AJS" $> AJS) <|> 
  (A.string "AT0" $> AT0) <|> 
  (A.string "AV0" $> AV0) <|> 
  (A.string "AVP" $> AVP) <|> 
  (A.string "AVQ" $> AVQ) <|> 
  (A.string "CJC" $> CJC) <|> 
  (A.string "CJS" $> CJS) <|> 
  (A.string "CJT" $> CJT) <|> 
  (A.string "CRD" $> CRD) <|> 
  (A.string "DPS" $> DPS) <|> 
  (A.string "DT0" $> DT0) <|> 
  (A.string "DTQ" $> DTQ) <|> 
  (A.string "EX0" $> EX0) <|> 
  (A.string "ITJ" $> ITJ) <|> 
  (A.string "NN0" $> NN0) <|> 
  (A.string "NN1" $> NN1) <|> 
  (A.string "NN2" $> NN2) <|> 
  (A.string "NP0" $> NP0) <|> 
  (A.string "ORD" $> ORD) <|> 
  (A.string "PNI" $> PNI) <|> 
  (A.string "PNP" $> PNP) <|> 
  (A.string "PNQ" $> PNQ) <|> 
  (A.string "PNX" $> PNX) <|> 
  (A.string "POS" $> POS) <|> 
  (A.string "PRF" $> PRF) <|> 
  (A.string "PRP" $> PRP) <|> 
  (A.string "PUL" $> PUL) <|> 
  (A.string "PUN" $> PUN) <|> 
  (A.string "PUQ" $> PUQ) <|> 
  (A.string "PUR" $> PUR) <|> 
  (A.string "TO0" $> TO0) <|> 
  (A.string "UNC" $> UNC) <|> 
  (A.string "VBB" $> VBB) <|> 
  (A.string "VBD" $> VBD) <|> 
  (A.string "VBG" $> VBG) <|> 
  (A.string "VBI" $> VBI) <|> 
  (A.string "VBN" $> VBN) <|> 
  (A.string "VBZ" $> VBZ) <|> 
  (A.string "VDB" $> VDB) <|> 
  (A.string "VDD" $> VDD) <|> 
  (A.string "VDG" $> VDG) <|> 
  (A.string "VDI" $> VDI) <|> 
  (A.string "VDN" $> VDN) <|> 
  (A.string "VDZ" $> VDZ) <|> 
  (A.string "VHB" $> VHB) <|> 
  (A.string "VHD" $> VHD) <|> 
  (A.string "VHG" $> VHG) <|> 
  (A.string "VHI" $> VHI) <|> 
  (A.string "VHN" $> VHN) <|> 
  (A.string "VHZ" $> VHZ) <|> 
  (A.string "VM0" $> VM0) <|> 
  (A.string "VVB" $> VVB) <|> 
  (A.string "VVD" $> VVD) <|> 
  (A.string "VVG" $> VVG) <|> 
  (A.string "VVI" $> VVI) <|> 
  (A.string "VVN" $> VVN) <|> 
  (A.string "VVZ" $> VVZ) <|> 
  (A.string "XX0" $> XX0) <|> 
  (A.string "ZZ0" $> ZZ0)   


-- code gen helper 
mkPosP :: IO ()
mkPosP =
  putStrLn $ concatMap mkStr ps where
  mkStr p = concat ["(A.string \"", p, "\" $> ", p, ") <|> \n"]
  ps = words "AJ0 AJC AJS AT0 AV0 AVP AVQ CJC CJS CJT CRD DPS DT0 DTQ EX0 ITJ NN0 NN1 NN2 NP0 ORD PNI PNP PNQ PNX POS PRF PRP PUL PUN PUQ PUR TO0 UNC VBB VBD VBG VBI VBN VBZ VDB VDD VDG VDI VDN VDZ VHB VHD VHG VHI VHN VHZ VM0 VVB VVD VVG VVI VVN VVZ XX0 ZZ0"


-- | Part of speech 
data POS = PREP | ART | SUBST | VERB | PRON | CONJ | UNC_ deriving (Eq, Show)

pos :: A.Parser POS
pos =
  (A.string "PREP" $> PREP) <|>
  (A.string "ART" $> ART) <|>
  (A.string "SUBST" $> SUBST) <|>
  (A.string "VERB" $> VERB) <|>
  (A.string "PRON" $> PRON) <|>
  (A.string "CONJ" $> CONJ) <|>
  (A.string "UNC" $> UNC_)
  


-- c5 attrs =
--   let maybs = sequenceA [lookup "c5", lookup "hw", lookup "pos"]
--   in sequenceA (maybs attrs)

-- w unk = unk <$> tag <*> str <*> pos where
--   str = A.many1 $ A.satisfy (A.inClass "a-z")

-- anyString :: A.Parser BS.ByteString
anyString = BS.pack <$> A.many1 (A.satisfy (A.inClass "a-zA-Z' "))



-- hwe :: (Eq i, IsString e, IsString i) => [(i, b)] -> Either e b
-- hwe as = lookupE "\"hw\" field not found" "hw" as

-- tage :: (Eq i, IsString i) => [(i, BS.ByteString)] -> Either String Tag
-- tage as = do
--   c5v <- lookupE "\"c5\" tag field not found" "c5" as
--   A.parseOnly tag c5v

-- pose :: (Eq i, IsString i) => [(i, BS.ByteString)] -> Either String POS
-- pose as = do
--   p <- lookupE "\"pos\" tag field not found" "pos" as
--   A.parseOnly pos p


-- | Parse a single word and gather text and tags into a W structure
parseWord :: BS.ByteString -> Either String (W BS.ByteString)
parseWord bs =
  case parse bs of
    Left _ -> Left "moo"
    Right x -> parseW x

-- parseW :: Node -> Either String (W BS.ByteString)
parseW n = do
  w <- getContentText $ head $ contents n
  (h, c5, p) <- asdf $ attributes n
  pure $ W w h c5 p
  
-- asdf :: [(a, BS.ByteString)] -> Either String (W BS.ByteString)
asdf as =
  let
    (c5:hw:p:[]) = map snd as
  in do
    a <- A.parseOnly tag c5
    b <- A.parseOnly anyString hw
    c <- A.parseOnly pos p
    pure (b, a, c)

    
    
            
getContentText :: IsString a => Content -> Either a BS.ByteString
getContentText c = case c of
  (Text t) -> Right t
  _ -> Left "Not a Text node"




-- | A word tag W contains a headword, the C5 token and the inferred part-of-speech tag
data W a = W { ww :: a,  wHw :: a, wC5 :: Tag, wPos :: POS } deriving (Eq, Show)

-- | A word can be either simple or multiple (e.g. phrasal verbs are tagged as multi-words <mw>)
data Word a = Word a | MWord [a] deriving (Eq, Show)


-- | Example text

t0 :: BS.ByteString
t0 = "<s n=\"78\"><w c5=\"PRP\" hw=\"for\" pos=\"PREP\">For </w><w c5=\"AT0\" hw=\"the\" pos=\"ART\">the </w><w c5=\"DT0\" hw=\"same\" pos=\"ADJ\">same </w><w c5=\"NN1\" hw=\"quality\" pos=\"SUBST\">quality </w><w c5=\"VM0\" hw=\"can\" pos=\"VERB\">can </w><w c5=\"VBI\" hw=\"be\" pos=\"VERB\">be </w><w c5=\"VVN\" hw=\"interpret\" pos=\"VERB\">interpreted </w><w c5=\"AV0\" hw=\"differently\" pos=\"ADV\">differently </w><mw c5=\"PRP\"><w c5=\"VVG\" hw=\"accord\" pos=\"VERB\">according </w><w c5=\"PRP\" hw=\"to\" pos=\"PREP\">to </w></mw><w c5=\"PNI-CRD\" hw=\"one\" pos=\"PRON\">one</w><w c5=\"POS\" hw=\"'s\" pos=\"UNC\">'s </w><w c5=\"NN1\" hw=\"point\" pos=\"SUBST\">point </w><w c5=\"PRF\" hw=\"of\" pos=\"PREP\">of </w><w c5=\"NN1\" hw=\"view\" pos=\"SUBST\">view</w><c c5=\"PUN\">, </c><w c5=\"PNI\" hw=\"one\" pos=\"PRON\">one</w><w c5=\"POS\" hw=\"'s\" pos=\"UNC\">'s </w><w c5=\"NN1\" hw=\"experience\" pos=\"SUBST\">experience</w><c c5=\"PUN\">, </c><w c5=\"CJC\" hw=\"and\" pos=\"CONJ\">and </w><w c5=\"PNI\" hw=\"one\" pos=\"PRON\">one</w><w c5=\"POS\" hw=\"'s\" pos=\"UNC\">'s </w><w c5=\"NN2\" hw=\"feeling\" pos=\"SUBST\">feelings</w><c c5=\"PUN\">.</c></s>"

t1 :: BS.ByteString
t1 = "<quote><p> <s n=\"258\"><w c5=\"AT0\" hw=\"the\" pos=\"ART\">The </w><w c5=\"AJ0\" hw=\"unclothed\" pos=\"ADJ\">unclothed </w><w c5=\"NN1\" hw=\"body\" pos=\"SUBST\">body </w><w c5=\"VBZ\" hw=\"be\" pos=\"VERB\">is </w><w c5=\"XX0\" hw=\"not\" pos=\"ADV\">not </w><w c5=\"AT0\" hw=\"a\" pos=\"ART\">a </w><c c5=\"PUQ\">‘</c><w c5=\"NN1\" hw=\"self\" pos=\"SUBST\">self </w><w c5=\"CJC\" hw=\"but\" pos=\"CONJ\">but </w><w c5=\"AT0\" hw=\"a\" pos=\"ART\">a </w><w c5=\"AJ0\" hw=\"socialised\" pos=\"ADJ\">socialised </w><w c5=\"NN1\" hw=\"body\" pos=\"SUBST\">body</w><c c5=\"PUN\">, </c><w c5=\"AT0\" hw=\"a\" pos=\"ART\">a </w><w c5=\"NN1\" hw=\"body\" pos=\"SUBST\">body </w><w c5=\"CJT-DT0\" hw=\"that\" pos=\"CONJ\">that </w><w c5=\"VBZ\" hw=\"be\" pos=\"VERB\">is </w><w c5=\"VVN\" hw=\"open\" pos=\"VERB\">opened </w><w c5=\"PRP\" hw=\"by\" pos=\"PREP\">by </w><w c5=\"NN2\" hw=\"instrument\" pos=\"SUBST\">instruments</w><c c5=\"PUN\">, </c><w c5=\"VVD\" hw=\"technologize\" pos=\"VERB\">technologized</w><c c5=\"PUN\">, </c><w c5=\"VVD-VVN\" hw=\"wound\" pos=\"VERB\">wounded</w><c c5=\"PUN\">, </c><w c5=\"DPS\" hw=\"it\" pos=\"PRON\">its </w><w c5=\"NN2\" hw=\"organ\" pos=\"SUBST\">organs </w><w c5=\"VVN\" hw=\"display\" pos=\"VERB\">displayed </w><w c5=\"PRP\" hw=\"to\" pos=\"PREP\">to </w><w c5=\"AT0\" hw=\"the\" pos=\"ART\">the </w><w c5=\"AJ0\" hw=\"outside\" pos=\"ADJ\">outside </w><w c5=\"NN1\" hw=\"world\" pos=\"SUBST\">world</w><c c5=\"PUN\">.</c></s> \n <s n=\"259\"><w c5=\"AT0\" hw=\"the\" pos=\"ART\">The </w><c c5=\"PUQ\">‘</c><w c5=\"AJ0\" hw=\"inner\" pos=\"ADJ\">inner</w><c c5=\"PUQ\">’ </c><w c5=\"NN1-NP0\" hw=\"frida\" pos=\"SUBST\">Frida </w><w c5=\"VBZ\" hw=\"be\" pos=\"VERB\">is </w><w c5=\"VVN\" hw=\"control\" pos=\"VERB\">controlled </w><w c5=\"PRP\" hw=\"by\" pos=\"PREP\">by </w><w c5=\"AJ0\" hw=\"modern\" pos=\"ADJ\">modern </w><w c5=\"NN1\" hw=\"society\" pos=\"SUBST\">society </w><w c5=\"AV0\" hw=\"far\" pos=\"ADV\">far </w><w c5=\"DT0\" hw=\"more\" pos=\"ADJ\">more </w><w c5=\"CJS\" hw=\"than\" pos=\"CONJ\">than </w><w c5=\"AT0\" hw=\"the\" pos=\"ART\">the </w><w c5=\"AJ0\" hw=\"clothed\" pos=\"ADJ\">clothed </w><w c5=\"NP0-NN1\" hw=\"frida\" pos=\"SUBST\">Frida</w><c c5=\"PUN\">, </c><w c5=\"PNQ\" hw=\"who\" pos=\"PRON\">who </w><w c5=\"AV0\" hw=\"often\" pos=\"ADV\">often </w><w c5=\"VVZ\" hw=\"mark\" pos=\"VERB\">marks </w><w c5=\"DPS\" hw=\"she\" pos=\"PRON\">her </w><w c5=\"NN1\" hw=\"deviation\" pos=\"SUBST\">deviation </w><w c5=\"PRP\" hw=\"from\" pos=\"PREP\">from </w><w c5=\"AT0\" hw=\"the\" pos=\"ART\">the </w><w c5=\"NN1\" hw=\"norm\" pos=\"SUBST\">norm </w><w c5=\"PRP\" hw=\"by\" pos=\"PREP\">by </w><w c5=\"AV0\" hw=\"defiantly\" pos=\"ADV\">defiantly </w><w c5=\"VVG\" hw=\"return\" pos=\"VERB\">returning </w><w c5=\"AT0\" hw=\"the\" pos=\"ART\">the </w><w c5=\"NN1\" hw=\"gaze\" pos=\"SUBST\">gaze </w><w c5=\"PRF\" hw=\"of\" pos=\"PREP\">of </w><w c5=\"AT0\" hw=\"the\" pos=\"ART\">the </w><w c5=\"NN1\" hw=\"viewer\" pos=\"SUBST\">viewer</w><c c5=\"PUN\">.</c></s></p></quote>"

t2 :: BS.ByteString
t2 = "<w c5=\"PRP\" hw=\"for\" pos=\"PREP\">For </w>"

--

lookupE :: Eq i => e -> i -> [(i, b)] -> Either e b
lookupE e i ixs = maybeEither e (lookup i ixs)

maybeEither :: a -> Maybe b -> Either a b
maybeEither e = maybe (Left e) Right

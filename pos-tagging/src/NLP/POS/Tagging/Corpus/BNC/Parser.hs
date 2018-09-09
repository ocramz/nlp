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
import Data.Functor (($>), (<$))
import Control.Applicative 
import qualified Data.ByteString as BS
import qualified Data.Attoparsec.ByteString as A
import Xeno.DOM
-- import Xeno.Types.XenoException


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

posTag :: A.Parser Tag
posTag =
  (A.string "AJ0" $> AJ0) <|>
  (A.string "AJC" $> AJC) <|>
  (A.string "AJS" $> AJS) <|>
  (A.string "AT0" $> AT0) <|>
  (A.string "AV0" $> AV0) <|>
  (A.string "AVP" $> AVP) 


mkPosP p =
  putStrLn $ concat ["(A.string \"", p, "\" $> ", p, ") <|> \n"]


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
  



data W a = W { wC5 :: Tag, wHw :: a, wPos :: POS } deriving (Eq, Show)

data Word a = Word a | MWord [a] deriving (Eq, Show)


-- | Example text

t0 :: BS.ByteString
t0 = "<s n=\"78\"><w c5=\"PRP\" hw=\"for\" pos=\"PREP\">For </w><w c5=\"AT0\" hw=\"the\" pos=\"ART\">the </w><w c5=\"DT0\" hw=\"same\" pos=\"ADJ\">same </w><w c5=\"NN1\" hw=\"quality\" pos=\"SUBST\">quality </w><w c5=\"VM0\" hw=\"can\" pos=\"VERB\">can </w><w c5=\"VBI\" hw=\"be\" pos=\"VERB\">be </w><w c5=\"VVN\" hw=\"interpret\" pos=\"VERB\">interpreted </w><w c5=\"AV0\" hw=\"differently\" pos=\"ADV\">differently </w><mw c5=\"PRP\"><w c5=\"VVG\" hw=\"accord\" pos=\"VERB\">according </w><w c5=\"PRP\" hw=\"to\" pos=\"PREP\">to </w></mw><w c5=\"PNI-CRD\" hw=\"one\" pos=\"PRON\">one</w><w c5=\"POS\" hw=\"'s\" pos=\"UNC\">'s </w><w c5=\"NN1\" hw=\"point\" pos=\"SUBST\">point </w><w c5=\"PRF\" hw=\"of\" pos=\"PREP\">of </w><w c5=\"NN1\" hw=\"view\" pos=\"SUBST\">view</w><c c5=\"PUN\">, </c><w c5=\"PNI\" hw=\"one\" pos=\"PRON\">one</w><w c5=\"POS\" hw=\"'s\" pos=\"UNC\">'s </w><w c5=\"NN1\" hw=\"experience\" pos=\"SUBST\">experience</w><c c5=\"PUN\">, </c><w c5=\"CJC\" hw=\"and\" pos=\"CONJ\">and </w><w c5=\"PNI\" hw=\"one\" pos=\"PRON\">one</w><w c5=\"POS\" hw=\"'s\" pos=\"UNC\">'s </w><w c5=\"NN2\" hw=\"feeling\" pos=\"SUBST\">feelings</w><c c5=\"PUN\">.</c></s>"

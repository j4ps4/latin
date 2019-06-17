{-# LANGUAGE ExistentialQuantification #-}
module Lib (CmdLine(..), runApp) where
import           Control.Monad       (forM, forM_, liftM, unless, when)
import           Data.Char           (toLower)
import           Data.Function       (on)
import           Data.List           (elemIndex, find, groupBy, intercalate,
                                      nub, nubBy, sortOn, zip4, (\\))
import           Data.List.Utils     (subIndex)
import           Data.Maybe          (catMaybes, fromMaybe)
import           Data.Maybe.Utils
import           Data.String.Utils   (strip)
import           Database.SQLite
import           System.Console.ANSI
import           System.Directory    (doesFileExist, getHomeDirectory)
import           System.Exit
import           System.FilePath     ((</>))
import           System.IO
import           Text.Printf

data CmdLine = CmdLine { verbose   :: Bool
                       , translate :: Bool
                       , caseInv   :: Bool
                       , color     :: Color
                       , html      :: Bool
                       , itoj      :: Bool
                       , output    :: String
                       , query     :: [String] }

data Genus = Masculine | Feminine | Neuter | Common
    deriving (Eq, Read)

instance Show Genus where
    show Masculine = "masc."
    show Feminine  = "fem."
    show Neuter    = "neut."
    show Common    = "comm."

data Declension = First | FirstAbus | Second | SecondR | Third | ThirdI | ThirdIm | Fourth
    | Fifth | FirstGreek | SecondGreek | Undeclined | VisDecl | JesusDecl
    | DeusDecl | BosDecl
    deriving (Eq, Read)

data Casus = Nominative | Genitive | Accusative | Ablative | Dative | Locative | Vocative
    deriving (Eq)

instance Show Casus where
    show Nominative = "nom."
    show Genitive   = "gen."
    show Accusative = "acc."
    show Ablative   = "abl."
    show Dative     = "dat."
    show Locative   = "loc."
    show Vocative   = "voc."

data Numerus = Singular | Plural
    deriving (Eq)

instance Show Numerus where
    show Singular = "sing."
    show Plural   = "plur."

data Verbum = Word { nomSing   :: String, -- nominative singular form
                    stem       :: String,
                    ending     :: String,
                   gender      :: Genus,
                   declension  :: Declension,
                    casus      :: Casus,
                    number     :: Numerus,
                   hasLocative :: Bool }
              deriving (Eq)

instance Show Declension where
    show First       = "first declension"
    show FirstAbus   = "first declension, dative and ablative plural -abus"
    show SecondR     = "second declension r-variant"
    show Second      = "second declension"
    show Third       = "third declension"
    show ThirdI      = "third declension i-stem"
    show ThirdIm     = "third declension i-stem im-variant"
    show Fourth      = "fourth declension"
    show Fifth       = "fifth declension"
    show FirstGreek  = "first declension, greek variant"
    show SecondGreek = "second declension, greek variant"
    show Undeclined  = "undeclined"
    show VisDecl     = "irregular"
    show JesusDecl   = "irregular"
    show DeusDecl    = "irregular"
    show BosDecl     = "irregular"

showDeclInfo :: Verbum -> String
showDeclInfo v = map toLower (show (gender v)) ++ ", " ++ show (declension v) ++
                 (if hasLocative v then ", with locative" else "")

instance Show Verbum where
    show v = case (number v, casus v, gender v, declension v) of
                (Singular, Vocative, Masculine, Second) | lastN 3 (nomSing v) == "ius" && last (stem v) == 'i'  -> stem v
                                                        | lastN 3 (nomSing v) == "ius" -> stem v ++ "i"
                                                        | otherwise -> stem v ++ ending v
                (Singular, Vocative, Feminine, Second) -> stem v ++ ending v
                (Singular, Vocative, _, FirstGreek) -> stem v ++ ending v
                (Singular, Vocative, _, First) -> stem v ++ ending v
                (Singular, Vocative, Masculine, SecondGreek) -> stem v ++ ending v
                (Singular, Vocative, Feminine, SecondGreek) -> stem v ++ ending v
                (Singular, Vocative, _, JesusDecl) -> stem v ++ ending v
                (Singular, Vocative, _, _) -> nomSing v
                (Singular, Accusative, Neuter, _) -> nomSing v
                (Singular, Nominative, _, _) -> nomSing v
                (_, _, _, _) -> stem v ++ ending v
             where lastN n = reverse . take n . reverse


nounDictEntry :: Verbum -> String
nounDictEntry n = case declension n of
                    Fifth -> nomSing n ++ ", " ++ stem n ++ "ei " ++ genderAbbr
                    Fourth -> nomSing n ++ ", " ++ stem n ++ "us " ++ genderAbbr
                    JesusDecl -> nomSing n ++ ", " ++ stem n ++ "u " ++ genderAbbr
                    Third -> nomSing n ++ ", " ++ stem n ++ "is " ++ genderAbbr
                    ThirdI -> nomSing n ++ ", " ++ stem n ++ "is " ++ genderAbbr
                    ThirdIm -> nomSing n ++ ", " ++ stem n ++ "is " ++ genderAbbr
                    VisDecl -> nomSing n ++ ", " ++ stem n ++ "is " ++ genderAbbr
                    BosDecl -> nomSing n ++ ", " ++ stem n ++ "ovis " ++ genderAbbr
                    Second -> nomSing n ++ ", " ++ stem n ++ "i " ++ genderAbbr
                    SecondR -> nomSing n ++ ", " ++ stem n ++ "i " ++ genderAbbr
                    DeusDecl -> nomSing n ++ ", " ++ stem n ++ "ei " ++ genderAbbr
                    SecondGreek -> nomSing n ++ ", " ++ stem n ++ "i " ++ genderAbbr
                    First -> nomSing n ++ ", " ++ stem n ++ "ae " ++ genderAbbr
                    FirstAbus -> nomSing n ++ ", " ++ stem n ++ "ae " ++ genderAbbr
                    FirstGreek | gender n == Feminine -> nomSing n ++ ", " ++ stem n ++ "es " ++ genderAbbr
                               | otherwise -> nomSing n ++ ", " ++ stem n ++ "ae " ++ genderAbbr
                    Undeclined -> nomSing n ++ ", undecl. " ++ genderAbbr

                  where genderAbbr = "(" ++ (take 1 . show) (gender n) ++ ".)"

data AdjDeclension = AdjFirstSecond | AdjFirstSecondRe | AdjFirstSecondR | AdjFirstSecondGreek3 |
                     AdjFirstSecondGreek2 | AdjThird1 | AdjThird2 | AdjThird3 | AdjPositiveOnly | AdjComparativeOnly | AdjIus
                     | AdjPresentParticipleFirst | AdjPresentParticipleSecond | AdjPresentParticipleThird | AdjPresentParticipleThirdI
                     | AdjPresentParticipleFourth | AdjPresentParticipleEo | AdjFutPasParticipleFirst | AdjFutPasParticipleSecond
                     | AdjFutPasParticipleThird | AdjFutPasParticipleThirdI | AdjFutPasParticipleFourth | AdjFutPasParticipleEo
                     | AdjFutActParticiple | AdjFutActParticipleEo
                     deriving (Eq, Read)

data Degree = Positive | Comparative | Superlative
                     deriving (Eq, Show)

data Adjective = Adjective {mascSing      :: String,
                            femSing       :: String,
                            neutSing      :: String,
                            adjDeclension :: AdjDeclension,
                            adjCasus      :: Casus,
                            adjNumber     :: Numerus,
                            adjStem       :: String,
                            adjEnding     :: String,
                            compMasc      :: String,
                            superlatStem  :: String,
                            compNeut      :: String,
                            adjGender     :: Genus,
                            adjDegree     :: Degree}
                 deriving (Eq)

showNomSingForMyGender :: Adjective -> String
showNomSingForMyGender adj = case (adjGender adj, adjDegree adj) of
    (Masculine, Positive)    -> mascSing adj
    (Feminine, Positive)     -> femSing adj
    (Neuter, Positive)       -> neutSing adj
    (Neuter, Comparative)    -> compNeut adj
    (_, Comparative)         -> compMasc adj
    (Masculine, Superlative) -> superlatStem adj ++ "us"
    (Feminine, Superlative)  -> superlatStem adj ++ "a"
    (Neuter, Superlative)    -> superlatStem adj ++ "um"

showMyStem :: Adjective -> String
showMyStem adj = case (adjGender adj, adjDegree adj) of
    (_, Positive)    -> adjStem adj
    (_, Comparative) -> compMasc adj
    (_, Superlative) -> superlatStem adj


instance Show Adjective where
    show v = case (adjNumber v, adjCasus v, adjGender v, adjDeclension v) of
        (Singular, Vocative, Masculine, AdjFirstSecond) -> showMyStem v ++ adjEnding v
        (Singular, Vocative, Masculine, AdjPositiveOnly) | mascSing v == "meus" -> "mi"
                                                         | otherwise -> showMyStem v ++ adjEnding v
        (Singular, Vocative, Masculine, AdjFirstSecondGreek2) -> showMyStem v ++ adjEnding v
        (Singular, Vocative, Feminine, AdjFirstSecondGreek2) -> showMyStem v ++ adjEnding v
        (Singular, Vocative, Masculine, AdjFirstSecondGreek3) -> showMyStem v ++ adjEnding v
        (Singular, Vocative, _, _) -> showNomSingForMyGender v
        (Singular, Accusative, Neuter, _) -> showNomSingForMyGender v
        (Singular, Nominative, _, _) -> showNomSingForMyGender v
        (_, _, _, _) -> showMyStem v ++ adjEnding v

instance Show AdjDeclension where
    show AdjFirstSecond       = "first / second declension"
    show AdjFirstSecondRe     = "first / second declension -er"
    show AdjFirstSecondR      = "first / second declension -(e)r"
    show AdjFirstSecondGreek2 = "second declension, greek variant"
    show AdjFirstSecondGreek3 = "first / second declension, greek variant"
    show AdjThird1            = "third declension, one ending"
    show AdjThird2            = "third declension, two endings"
    show AdjThird3            = "third declension, three endings"
    show AdjPositiveOnly      = "first / second declension"
    show AdjComparativeOnly   = "third declension, comparative variant"
    show AdjIus               = "first / second declension (-ius)"
    show _                    = "unknown declension"

adjDictEntry :: Adjective -> String
adjDictEntry adj = case adjDeclension adj of
                    AdjThird1 -> mascSing adj ++ ", (gen.) " ++ adjStem adj ++ "is"
                    AdjPresentParticipleFirst -> mascSing adj ++ ", (gen.) " ++ adjStem adj ++ "antis"
                    AdjPresentParticipleSecond -> mascSing adj ++ ", (gen.) " ++ adjStem adj ++ "ntis"
                    AdjPresentParticipleThird -> mascSing adj ++ ", (gen.) " ++ adjStem adj ++ "entis"
                    AdjPresentParticipleThirdI -> mascSing adj ++ ", (gen.) " ++ adjStem adj ++ "ientis"
                    AdjPresentParticipleFourth -> mascSing adj ++ ", (gen.) " ++ adjStem adj ++ "entis"
                    AdjPresentParticipleEo -> mascSing adj ++ ", (gen.) " ++ adjStem adj ++ "euntis"
                    AdjThird2 -> mascSing adj ++ ", -" ++ lastN 2 (mascSing adj) ++ ", -" ++ lastN 1 (neutSing adj)
                    AdjThird3 -> mascSing adj ++ ", " ++ femSing adj ++ ", " ++ neutSing adj
                    AdjComparativeOnly -> mascSing adj ++ ", " ++ femSing adj ++ ", " ++ neutSing adj
                    AdjFirstSecondR -> mascSing adj ++ ", -" ++ lastN 2 (femSing adj) ++ ", -" ++ lastN 3 (neutSing adj)
                    AdjFirstSecondGreek2 -> mascSing adj ++ ", -" ++ lastN 2 (mascSing adj) ++ ", -" ++ lastN 2 (neutSing adj)
                    _ -> mascSing adj ++ ", -" ++ lastN 1 (femSing adj) ++ ", -" ++ lastN 2 (neutSing adj)

                   where
                       lastN n = reverse . take n . reverse

data VerbClass = Ordinary | OrdinaryNoPass | Deponent | SemiDeponent | Impersonal
                 deriving (Eq, Read)

instance Show VerbClass where
    show Ordinary       = ""
    show OrdinaryNoPass = "active only"
    show Deponent       = "deponent"
    show SemiDeponent   = "semideponent"
    show Impersonal     = "impersonal"

data Defectiveness = NonDefective | Defective | DefectivePerf
                     deriving (Eq, Read)

instance Show Defectiveness where
    show NonDefective  = ""
    show Defective     = "defective"
    show DefectivePerf = "perfect tense expresses present"

data Conjugation = ConjFirst | ConjSecond | ConjThird | ConjThirdI | ConjFourth
                   | ConjSum | ConjPossum | ConjEo | ConjVolo | ConjNolo | ConjMalo
                   | ConjFero | ConjFio | ConjEdo | ConjAit | ConjMemini
                   deriving (Eq, Read)

instance Show Conjugation where
    show ConjFirst  = "first conjugation"
    show ConjSecond = "second conjugation"
    show ConjThird  = "third conjugation"
    show ConjThirdI = "third conjugation io-variant"
    show ConjFourth = "fourth conjugation"
    show _          = "irregular"

data VerbPerson = PersonFirst | PersonSecond | PersonThird
                   deriving (Eq)

instance Show VerbPerson where
    show PersonFirst  = "1st"
    show PersonSecond = "2nd"
    show PersonThird  = "3rd"

data VerbMood = Indicative | Subjunctive | Imperative
                deriving (Eq)

instance Show VerbMood where
    show Indicative  = "ind."
    show Subjunctive = "subj."
    show Imperative  = "imp."

data VerbVoice = Active | Passive
                 deriving (Eq)

instance Show VerbVoice where
    show Active  = "act."
    show Passive = "pass."

data VerbTense = Present | Imperfect | Future | Perfect | Pluperfect | FuturePerfect
                 deriving (Eq)

instance Show VerbTense where
    show Present       = "pres."
    show Imperfect     = "imperf."
    show Future        = "fut."
    show Perfect       = "perf."
    show Pluperfect    = "pluperf."
    show FuturePerfect = "fut. perf."

conjAbbr :: Conjugation -> String
conjAbbr ConjFirst  = "I"
conjAbbr ConjSecond = "II"
conjAbbr ConjThird  = "III"
conjAbbr ConjThirdI = "III -io"
conjAbbr ConjFourth = "IV"
conjAbbr _          = "irr."

data ParticipleType = PerfectPassive | PerfectActive | PresentActive | FutureActive | FuturePassive
                      deriving (Eq)

instance Show ParticipleType where
    show PerfectPassive = "perf. pass."
    show PerfectActive  = "perf. act"
    show PresentActive  = "pres. act."
    show FutureActive   = "fut. act."
    show FuturePassive  = "fut. pass."

data InfinitiveType = IPresentActive | IPerfectActive | IFutureActive | IPresentPassive | IPerfectPassive | IFuturePassive
                      deriving (Eq)

instance Show InfinitiveType where
    show IPresentActive  = "Present active"
    show IPerfectActive  = "Perfect active"
    show IFutureActive   = "Future active"
    show IPresentPassive = "Present passive"
    show IPerfectPassive = "Perfect passive"
    show IFuturePassive  = "Future passive"

type Infinitive = String

data Verb = Verb {  princ1        :: String,
                    princ2        :: String,
                    princ3        :: String,
                    princ4        :: String,
                    presentStem   :: String,
                    perfectStem   :: String,
                    partcpStem    :: String,
                    verbEnding    :: String,
                    fakePrinc2    :: String,
                    verbClass     :: VerbClass,
                    conjugation   :: Conjugation,
                    defectiveness :: Defectiveness,
                    person        :: VerbPerson,
                    mood          :: VerbMood,
                    verbNumber    :: Numerus,
                    voice         :: VerbVoice,
                    tense         :: VerbTense,
                    participles   :: [(Adjective, ParticipleType)],
                    infinitives   :: [(Infinitive, InfinitiveType)]}
            deriving (Eq)

instance Show Verb where
    show v = case (mood v, tense v) of
                (Imperative, _)          -> presentStem v ++ verbEnding v
                (_, Present)             -> presentStem v ++ verbEnding v
                (Indicative, Imperfect)  -> presentStem v ++ verbEnding v
                (Subjunctive, Imperfect) -> fakePrinc2 v ++ verbEnding v
                (_, Future)              -> presentStem v ++ verbEnding v
                (_, Perfect)             -> perfectStem v ++ verbEnding v
                (_, Pluperfect)          -> perfectStem v ++ verbEnding v
                (_, FuturePerfect)       -> perfectStem v ++ verbEnding v

verbDictEntry :: Verb -> String
verbDictEntry v = dispStr ++ " " ++ conjAbbr (conjugation v)
    where dispStr = case verbClass v of
                        Ordinary -> intercalate ", " [princ1 v, princ2 v, princ3 v, princ4 v]
                        OrdinaryNoPass -> intercalate ", " [princ1 v, princ2 v, princ3 v, princ4 v]
                        Impersonal -> intercalate ", " [princ1 v, princ2 v, princ3 v, princ4 v]
                        _ -> intercalate ", " [princ1 v, princ2 v, princ4 v ++ " sum"]

data IndeclType = Adverb | Conjunction | Interjection | Preposition
                  deriving (Eq, Read)

instance Show IndeclType where
    show i = case i of
                Adverb       -> "adv."
                Conjunction  -> "conj."
                Interjection -> "interj."
                Preposition  -> "prep."

data Indeclinable = Indeclinable {form :: String, indType :: IndeclType}

data AdverbType = AdverbType {positive    :: String,
                              comparative :: String,
                              superlative :: String}

firstDecData :: [(String, Casus, Numerus)]
firstDecData = [("", Nominative, Singular), ("ae", Genitive, Singular), ("am", Accusative, Singular),
                ("a", Ablative, Singular), ("ae", Dative, Singular), ("ae", Locative, Singular),
                 ("a", Vocative, Singular), ("ae", Nominative, Plural), ("arum", Genitive, Plural),
                 ("as", Accusative, Plural), ("is", Ablative, Plural), ("is", Dative, Plural),
                 ("is", Locative, Plural), ("ae", Vocative, Plural)]
firstGreekDecDataF :: [(String, Casus, Numerus)]
firstGreekDecDataF = [("", Nominative, Singular), ("es", Genitive, Singular), ("en", Accusative, Singular),
                ("e", Ablative, Singular), ("ae", Dative, Singular), ("ae", Locative, Singular),
                 ("e", Vocative, Singular), ("ae", Nominative, Plural), ("arum", Genitive, Plural),
                 ("as", Accusative, Plural), ("is", Ablative, Plural), ("is", Dative, Plural),
                 ("is", Locative, Plural), ("ae", Vocative, Plural)]
firstGreekDecDataM :: [(String, Casus, Numerus)]
firstGreekDecDataM = [("", Nominative, Singular), ("ae", Genitive, Singular), ("en", Accusative, Singular),
                ("e", Ablative, Singular), ("ae", Dative, Singular), ("ae", Locative, Singular),
                 ("e", Vocative, Singular), ("ae", Nominative, Plural), ("arum", Genitive, Plural),
                 ("as", Accusative, Plural), ("is", Ablative, Plural), ("is", Dative, Plural),
                 ("is", Locative, Plural), ("ae", Vocative, Plural)]
firstAbusDecData :: [(String, Casus, Numerus)]
firstAbusDecData = [("", Nominative, Singular), ("ae", Genitive, Singular), ("am", Accusative, Singular),
                ("a", Ablative, Singular), ("ae", Dative, Singular), ("ae", Locative, Singular),
                 ("a", Vocative, Singular), ("ae", Nominative, Plural), ("arum", Genitive, Plural),
                 ("as", Accusative, Plural), ("abus", Ablative, Plural), ("abus", Dative, Plural),
                 ("abus", Locative, Plural), ("ae", Vocative, Plural)]
secondDecDataM :: [(String, Casus, Numerus)]
secondDecDataM = [("", Nominative, Singular), ("i", Genitive, Singular), ("um", Accusative, Singular),
                ("o", Ablative, Singular), ("o", Dative, Singular), ("i", Locative, Singular),
                 ("e", Vocative, Singular), ("i", Nominative, Plural), ("orum", Genitive, Plural),
                 ("os", Accusative, Plural), ("is", Ablative, Plural), ("is", Dative, Plural),
                 ("is", Locative, Plural), ("i", Vocative, Plural)]
secondRDecData :: [(String, Casus, Numerus)]
secondRDecData = [("", Nominative, Singular), ("i", Genitive, Singular), ("um", Accusative, Singular),
                ("o", Ablative, Singular), ("o", Dative, Singular), ("i", Locative, Singular),
                 ("", Vocative, Singular), ("i", Nominative, Plural), ("orum", Genitive, Plural),
                 ("os", Accusative, Plural), ("is", Ablative, Plural), ("is", Dative, Plural),
                 ("is", Locative, Plural), ("i", Vocative, Plural)]
secondDecDataN :: [(String, Casus, Numerus)]
secondDecDataN = [("", Nominative, Singular), ("i", Genitive, Singular), ("um", Accusative, Singular),
                ("o", Ablative, Singular), ("o", Dative, Singular), ("i", Locative, Singular),
                 ("", Vocative, Singular), ("a", Nominative, Plural), ("orum", Genitive, Plural),
                 ("a", Accusative, Plural), ("is", Ablative, Plural), ("is", Dative, Plural),
                 ("is", Locative, Plural), ("a", Vocative, Plural)]
secondGreekDecDataN :: [(String, Casus, Numerus)]
secondGreekDecDataN = [("", Nominative, Singular), ("i", Genitive, Singular), ("on", Accusative, Singular),
                ("o", Ablative, Singular), ("o", Dative, Singular), ("i", Locative, Singular),
                 ("", Vocative, Singular), ("a", Nominative, Plural), ("orum", Genitive, Plural),
                 ("a", Accusative, Plural), ("is", Ablative, Plural), ("is", Dative, Plural),
                 ("is", Locative, Plural), ("a", Vocative, Plural)]
secondGreekDecDataM :: [(String, Casus, Numerus)]
secondGreekDecDataM = [("", Nominative, Singular), ("i", Genitive, Singular), ("on", Accusative, Singular),
                ("o", Ablative, Singular), ("o", Dative, Singular), ("i", Locative, Singular),
                 ("e", Vocative, Singular), ("i", Nominative, Plural), ("orum", Genitive, Plural),
                 ("os", Accusative, Plural), ("is", Ablative, Plural), ("is", Dative, Plural),
                 ("is", Locative, Plural), ("i", Vocative, Plural)]
thirdDecData :: [(String, Casus, Numerus)]
thirdDecData = [("", Nominative, Singular), ("is", Genitive, Singular), ("em", Accusative, Singular),
                ("e", Ablative, Singular), ("i", Dative, Singular), ("i", Locative, Singular),
                 ("", Vocative, Singular), ("es", Nominative, Plural), ("um", Genitive, Plural),
                 ("es", Accusative, Plural), ("ibus", Ablative, Plural), ("ibus", Dative, Plural),
                 ("ibus", Locative, Plural), ("es", Vocative, Plural)]
thirdDecDataN :: [(String, Casus, Numerus)]
thirdDecDataN = [("", Nominative, Singular), ("is", Genitive, Singular), ("", Accusative, Singular),
                ("e", Ablative, Singular), ("i", Dative, Singular), ("i", Locative, Singular),
                 ("", Vocative, Singular), ("a", Nominative, Plural), ("um", Genitive, Plural),
                 ("a", Accusative, Plural), ("ibus", Ablative, Plural), ("ibus", Dative, Plural),
                 ("ibus", Locative, Plural), ("a", Vocative, Plural)]
thirdIDecData :: [(String, Casus, Numerus)]
thirdIDecData = [("", Nominative, Singular), ("is", Genitive, Singular), ("em", Accusative, Singular),
                ("e", Ablative, Singular), ("i", Dative, Singular), ("i", Locative, Singular),
                 ("", Vocative, Singular), ("es", Nominative, Plural), ("ium", Genitive, Plural),
                 ("es", Accusative, Plural), ("ibus", Ablative, Plural), ("ibus", Dative, Plural),
                 ("ibus", Locative, Plural), ("es", Vocative, Plural)]
thirdIDecDataN :: [(String, Casus, Numerus)]
thirdIDecDataN = [("", Nominative, Singular), ("is", Genitive, Singular), ("", Accusative, Singular),
                ("i", Ablative, Singular), ("i", Dative, Singular), ("i", Locative, Singular),
                 ("", Vocative, Singular), ("ia", Nominative, Plural), ("ium", Genitive, Plural),
                 ("ia", Accusative, Plural), ("ibus", Ablative, Plural), ("ibus", Dative, Plural),
                 ("ibus", Locative, Plural), ("ia", Vocative, Plural)]
thirdImDecData :: [(String, Casus, Numerus)]
thirdImDecData = [("", Nominative, Singular), ("is", Genitive, Singular), ("im", Accusative, Singular),
                ("i", Ablative, Singular), ("i", Dative, Singular), ("i", Locative, Singular),
                 ("", Vocative, Singular), ("es", Nominative, Plural), ("ium", Genitive, Plural),
                 ("es", Accusative, Plural), ("ibus", Ablative, Plural), ("ibus", Dative, Plural),
                 ("ibus", Locative, Plural), ("es", Vocative, Plural)]
fourthDecData :: [(String, Casus, Numerus)]
fourthDecData = [("", Nominative, Singular), ("us", Genitive, Singular), ("um", Accusative, Singular),
                ("u", Ablative, Singular), ("ui", Dative, Singular), ("i", Locative, Singular),
                 ("", Vocative, Singular), ("us", Nominative, Plural), ("uum", Genitive, Plural),
                 ("us", Accusative, Plural), ("ibus", Ablative, Plural), ("ibus", Dative, Plural),
                 ("ibus", Locative, Plural), ("us", Vocative, Plural)]
fourthDecDataN :: [(String, Casus, Numerus)]
fourthDecDataN = [("", Nominative, Singular), ("us", Genitive, Singular), ("", Accusative, Singular),
                ("u", Ablative, Singular), ("u", Dative, Singular), ("i", Locative, Singular),
                 ("", Vocative, Singular), ("ua", Nominative, Plural), ("uum", Genitive, Plural),
                 ("ua", Accusative, Plural), ("ibus", Ablative, Plural), ("ibus", Dative, Plural),
                 ("ibus", Locative, Plural), ("ua", Vocative, Plural)]
fifthDecData :: [(String, Casus, Numerus)]
fifthDecData = [("", Nominative, Singular), ("ei", Genitive, Singular), ("em", Accusative, Singular),
                ("e", Ablative, Singular), ("ei", Dative, Singular), ("ei", Locative, Singular),
                 ("", Vocative, Singular), ("es", Nominative, Plural), ("erum", Genitive, Plural),
                 ("es", Accusative, Plural), ("ebus", Ablative, Plural), ("ebus", Dative, Plural),
                 ("ebus", Locative, Plural), ("es", Vocative, Plural)]
undeclinedData :: [(String, Casus, Numerus)]
undeclinedData = [("", Nominative, Singular), ("", Genitive, Singular), ("", Accusative, Singular),
                ("", Ablative, Singular), ("", Dative, Singular), ("", Locative, Singular),
                 ("", Vocative, Singular), ("", Nominative, Plural), ("", Genitive, Plural),
                 ("", Accusative, Plural), ("", Ablative, Plural), ("", Dative, Plural),
                 ("", Locative, Plural), ("", Vocative, Plural)]
visDecData :: [(String, Casus, Numerus)]
visDecData = [("", Nominative, Singular), ("is", Genitive, Singular), ("im", Accusative, Singular),
                ("i", Ablative, Singular), ("i", Dative, Singular), ("i", Locative, Singular),
                 ("", Vocative, Singular), ("ires", Nominative, Plural), ("irium", Genitive, Plural),
                 ("ires", Accusative, Plural), ("iribus", Ablative, Plural), ("iribus", Dative, Plural),
                 ("iribus", Locative, Plural), ("ires", Vocative, Plural)]
jesusDecData :: [(String, Casus, Numerus)]
jesusDecData = [("us", Nominative, Singular), ("u", Genitive, Singular), ("um", Accusative, Singular),
                ("u", Ablative, Singular), ("u", Dative, Singular), ("u", Locative, Singular),
                 ("u", Vocative, Singular), ("us", Nominative, Plural), ("uum", Genitive, Plural),
                 ("us", Accusative, Plural), ("ubus", Ablative, Plural), ("ubus", Dative, Plural),
                 ("ubus", Locative, Plural), ("us", Vocative, Plural)]
deusDecData :: [(String, Casus, Numerus)]
deusDecData = [("", Nominative, Singular), ("ei", Genitive, Singular), ("eum", Accusative, Singular),
                ("eo", Ablative, Singular), ("eo", Dative, Singular), ("ei", Locative, Singular),
                 ("", Vocative, Singular), ("i", Nominative, Plural), ("eorum", Genitive, Plural),
                 ("eos", Accusative, Plural), ("is", Ablative, Plural), ("is", Dative, Plural),
                 ("is", Locative, Plural), ("i", Vocative, Plural)]
bosDecData :: [(String, Casus, Numerus)]
bosDecData = [("", Nominative, Singular), ("ovis", Genitive, Singular), ("ovem", Accusative, Singular),
                ("ove", Ablative, Singular), ("ovi", Dative, Singular), ("ovi", Locative, Singular),
                 ("", Vocative, Singular), ("oves", Nominative, Plural), ("oum", Genitive, Plural),
                 ("oves", Accusative, Plural), ("obus", Ablative, Plural), ("obus", Dative, Plural),
                 ("obus", Locative, Plural), ("oves", Vocative, Plural)]

infFstDecData :: [(String, Casus, Numerus)]
infFstDecData = [("isse", Nominative, Singular), ("ari", Nominative, Singular)]
infSndDecData :: [(String, Casus, Numerus)]
infSndDecData = [("isse", Nominative, Singular), ("ri", Nominative, Singular)]
infThdDecData :: [(String, Casus, Numerus)]
infThdDecData = [("isse", Nominative, Singular), ("i", Nominative, Singular)]

first :: (a, b, c) -> a
first (a, _, _) = a
second :: (a, b, c) -> b
second (_, b, _) = b
third :: (a, b, c) -> c
third (_, _, c) = c

decline :: Verbum -> [Verbum]
decline v = case (gender v, declension v) of
            (_, First)              -> map createWord firstDecData
            (Feminine, FirstGreek)  -> map createWord firstGreekDecDataF
            (Masculine, FirstGreek) -> map createWord firstGreekDecDataM
            (_, FirstAbus)          -> map createWord firstAbusDecData
            (Neuter, Second)        -> map createWord secondDecDataN
            (_, Second)             -> map createWord secondDecDataM
            (_, SecondR)            -> map createWord secondRDecData
            (Neuter, SecondGreek)   -> map createWord secondGreekDecDataN
            (_, SecondGreek)        -> map createWord secondGreekDecDataM
            (Neuter, Third)         -> map createWord thirdDecDataN
            (_, Third)              -> map createWord thirdDecData
            (Neuter, ThirdI)        -> map createWord thirdIDecDataN
            (_, ThirdIm)            -> map createWord thirdImDecData
            (_, ThirdI)             -> map createWord thirdIDecData
            (Neuter, Fourth)        -> map createWord fourthDecDataN
            (_, Fourth)             -> map createWord fourthDecData
            (_, Fifth)              -> map createWord fifthDecData
            (_, Undeclined)         -> map createWord undeclinedData
            (_, VisDecl)            -> map createWord visDecData
            (_, JesusDecl)          -> map createWord jesusDecData
            (_, DeusDecl)           -> map createWord deusDecData
            (_, BosDecl)            -> map createWord bosDecData
            _                       -> error "bad word"
            where createWord d = v {ending = first d, casus = second d, number = third d}

adjFSData :: [(String, Casus, Numerus, Genus)]
adjFSData = [
                ("us", Nominative, Singular, Masculine), ("i", Genitive, Singular, Masculine), ("um", Accusative, Singular, Masculine),
                ("o", Ablative, Singular, Masculine), ("o", Dative, Singular, Masculine), ("o", Locative, Singular, Masculine),
                 ("e", Vocative, Singular, Masculine), ("i", Nominative, Plural, Masculine), ("orum", Genitive, Plural, Masculine),
                 ("os", Accusative, Plural, Masculine), ("is", Ablative, Plural, Masculine), ("is", Dative, Plural, Masculine),
                 ("is", Locative, Plural, Masculine), ("i", Vocative, Plural, Masculine),
                ("a", Nominative, Singular, Feminine), ("ae", Genitive, Singular, Feminine), ("am", Accusative, Singular, Feminine),
                ("a", Ablative, Singular, Feminine), ("ae", Dative, Singular, Feminine), ("a", Locative, Singular, Feminine),
                 ("", Vocative, Singular, Feminine), ("ae", Nominative, Plural, Feminine), ("arum", Genitive, Plural, Feminine),
                 ("as", Accusative, Plural, Feminine), ("is", Ablative, Plural, Feminine), ("is", Dative, Plural, Feminine),
                 ("is", Locative, Plural, Feminine), ("ae", Vocative, Plural, Feminine),
                ("um", Nominative, Singular, Neuter), ("i", Genitive, Singular, Neuter), ("um", Accusative, Singular, Neuter),
                ("o", Ablative, Singular, Neuter), ("o", Dative, Singular, Neuter), ("o", Locative, Singular, Neuter),
                 ("um", Vocative, Singular, Neuter), ("a", Nominative, Plural, Neuter), ("orum", Genitive, Plural, Neuter),
                 ("a", Accusative, Plural, Neuter), ("is", Ablative, Plural, Neuter), ("is", Dative, Plural, Neuter),
                 ("is", Locative, Plural, Neuter), ("a", Vocative, Plural, Neuter)
            ]
adjIusData :: [(String, Casus, Numerus, Genus)]
adjIusData = [
                ("us", Nominative, Singular, Masculine), ("ius", Genitive, Singular, Masculine), ("um", Accusative, Singular, Masculine),
                ("o", Ablative, Singular, Masculine), ("i", Dative, Singular, Masculine), ("us", Locative, Singular, Masculine),
                 ("e", Vocative, Singular, Masculine), ("i", Nominative, Plural, Masculine), ("orum", Genitive, Plural, Masculine),
                 ("os", Accusative, Plural, Masculine), ("is", Ablative, Plural, Masculine), ("is", Dative, Plural, Masculine),
                 ("is", Locative, Plural, Masculine), ("i", Vocative, Plural, Masculine),
                ("a", Nominative, Singular, Feminine), ("ius", Genitive, Singular, Feminine), ("am", Accusative, Singular, Feminine),
                ("a", Ablative, Singular, Feminine), ("i", Dative, Singular, Feminine), ("us", Locative, Singular, Feminine),
                 ("a", Vocative, Singular, Feminine), ("ae", Nominative, Plural, Feminine), ("arum", Genitive, Plural, Feminine),
                 ("as", Accusative, Plural, Feminine), ("is", Ablative, Plural, Feminine), ("is", Dative, Plural, Feminine),
                 ("is", Locative, Plural, Feminine), ("ae", Vocative, Plural, Feminine),
                ("um", Nominative, Singular, Neuter), ("ius", Genitive, Singular, Neuter), ("um", Accusative, Singular, Neuter),
                ("o", Ablative, Singular, Neuter), ("i", Dative, Singular, Neuter), ("us", Locative, Singular, Neuter),
                 ("um", Vocative, Singular, Neuter), ("a", Nominative, Plural, Neuter), ("orum", Genitive, Plural, Neuter),
                 ("a", Accusative, Plural, Neuter), ("is", Ablative, Plural, Neuter), ("is", Dative, Plural, Neuter),
                 ("is", Locative, Plural, Neuter), ("a", Vocative, Plural, Neuter)
            ]
adjIus2Data = map (\(s,c,n,g) -> (if s == "ius" then "us" else s, c, n, g)) adjIusData
adjFSGreek2Data :: [(String, Casus, Numerus, Genus)]
adjFSGreek2Data = [
                ("os", Nominative, Singular, Masculine), ("i", Genitive, Singular, Masculine), ("on", Accusative, Singular, Masculine),
                ("o", Ablative, Singular, Masculine), ("o", Dative, Singular, Masculine), ("i", Locative, Singular, Masculine),
                 ("e", Vocative, Singular, Masculine), ("i", Nominative, Plural, Masculine), ("orum", Genitive, Plural, Masculine),
                 ("os", Accusative, Plural, Masculine), ("is", Ablative, Plural, Masculine), ("is", Dative, Plural, Masculine),
                 ("is", Locative, Plural, Masculine), ("i", Vocative, Plural, Masculine),
                ("os", Nominative, Singular, Feminine), ("i", Genitive, Singular, Feminine), ("on", Accusative, Singular, Feminine),
                ("o", Ablative, Singular, Feminine), ("o", Dative, Singular, Feminine), ("i", Locative, Singular, Feminine),
                 ("e", Vocative, Singular, Feminine), ("i", Nominative, Plural, Feminine), ("orum", Genitive, Plural, Feminine),
                 ("os", Accusative, Plural, Feminine), ("is", Ablative, Plural, Feminine), ("is", Dative, Plural, Feminine),
                 ("is", Locative, Plural, Feminine), ("i", Vocative, Plural, Feminine),
                ("on", Nominative, Singular, Neuter), ("i", Genitive, Singular, Neuter), ("on", Accusative, Singular, Neuter),
                ("o", Ablative, Singular, Neuter), ("o", Dative, Singular, Neuter), ("i", Locative, Singular, Neuter),
                 ("on", Vocative, Singular, Neuter), ("a", Nominative, Plural, Neuter), ("orum", Genitive, Plural, Neuter),
                 ("a", Accusative, Plural, Neuter), ("is", Ablative, Plural, Neuter), ("is", Dative, Plural, Neuter),
                 ("is", Locative, Plural, Neuter), ("a", Vocative, Plural, Neuter)
            ]
adjFSGreek3Data :: [(String, Casus, Numerus, Genus)]
adjFSGreek3Data = [
                ("os", Nominative, Singular, Masculine), ("i", Genitive, Singular, Masculine), ("on", Accusative, Singular, Masculine),
                ("o", Ablative, Singular, Masculine), ("o", Dative, Singular, Masculine), ("i", Locative, Singular, Masculine),
                 ("e", Vocative, Singular, Masculine), ("i", Nominative, Plural, Masculine), ("orum", Genitive, Plural, Masculine),
                 ("os", Accusative, Plural, Masculine), ("is", Ablative, Plural, Masculine), ("is", Dative, Plural, Masculine),
                 ("is", Locative, Plural, Masculine), ("i", Vocative, Plural, Masculine),
                ("e", Nominative, Singular, Feminine), ("es", Genitive, Singular, Feminine), ("en", Accusative, Singular, Feminine),
                ("e", Ablative, Singular, Feminine), ("ae", Dative, Singular, Feminine), ("ae", Locative, Singular, Feminine),
                 ("e", Vocative, Singular, Feminine), ("ae", Nominative, Plural, Feminine), ("arum", Genitive, Plural, Feminine),
                 ("as", Accusative, Plural, Feminine), ("is", Ablative, Plural, Feminine), ("is", Dative, Plural, Feminine),
                 ("is", Locative, Plural, Feminine), ("ae", Vocative, Plural, Feminine),
                ("on", Nominative, Singular, Neuter), ("i", Genitive, Singular, Neuter), ("on", Accusative, Singular, Neuter),
                ("o", Ablative, Singular, Neuter), ("o", Dative, Singular, Neuter), ("i", Locative, Singular, Neuter),
                 ("on", Vocative, Singular, Neuter), ("a", Nominative, Plural, Neuter), ("orum", Genitive, Plural, Neuter),
                 ("a", Accusative, Plural, Neuter), ("is", Ablative, Plural, Neuter), ("is", Dative, Plural, Neuter),
                 ("is", Locative, Plural, Neuter), ("a", Vocative, Plural, Neuter)
            ]
adjFSThird1Data :: [(String, Casus, Numerus, Genus)]
adjFSThird1Data = [
                ("", Nominative, Singular, Masculine), ("is", Genitive, Singular, Masculine), ("em", Accusative, Singular, Masculine),
                ("i", Ablative, Singular, Masculine), ("i", Dative, Singular, Masculine), ("i", Locative, Singular, Masculine),
                 ("", Vocative, Singular, Masculine), ("es", Nominative, Plural, Masculine), ("ium", Genitive, Plural, Masculine),
                 ("es", Accusative, Plural, Masculine), ("ibus", Ablative, Plural, Masculine), ("ibus", Dative, Plural, Masculine),
                 ("ibus", Locative, Plural, Masculine), ("es", Vocative, Plural, Masculine),
                ("", Nominative, Singular, Feminine), ("is", Genitive, Singular, Feminine), ("em", Accusative, Singular, Feminine),
                ("i", Ablative, Singular, Feminine), ("i", Dative, Singular, Feminine), ("i", Locative, Singular, Feminine),
                 ("", Vocative, Singular, Feminine), ("es", Nominative, Plural, Feminine), ("ium", Genitive, Plural, Feminine),
                 ("es", Accusative, Plural, Feminine), ("ibus", Ablative, Plural, Feminine), ("ibus", Dative, Plural, Feminine),
                 ("ibus", Locative, Plural, Feminine), ("es", Vocative, Plural, Feminine),
                ("", Nominative, Singular, Neuter), ("is", Genitive, Singular, Neuter), ("", Accusative, Singular, Neuter),
                ("i", Ablative, Singular, Neuter), ("i", Dative, Singular, Neuter), ("i", Locative, Singular, Neuter),
                 ("", Vocative, Singular, Neuter), ("ia", Nominative, Plural, Neuter), ("ium", Genitive, Plural, Neuter),
                 ("ia", Accusative, Plural, Neuter), ("ibus", Ablative, Plural, Neuter), ("ibus", Dative, Plural, Neuter),
                 ("ibus", Locative, Plural, Neuter), ("ia", Vocative, Plural, Neuter)
            ]
adjCompData :: [(String, Casus, Numerus, Genus)]
adjCompData = [
                ("", Nominative, Singular, Masculine), ("is", Genitive, Singular, Masculine), ("em", Accusative, Singular, Masculine),
                ("e", Ablative, Singular, Masculine), ("i", Dative, Singular, Masculine), ("e", Locative, Singular, Masculine),
                 ("", Vocative, Singular, Masculine), ("es", Nominative, Plural, Masculine), ("um", Genitive, Plural, Masculine),
                 ("es", Accusative, Plural, Masculine), ("ibus", Ablative, Plural, Masculine), ("ibus", Dative, Plural, Masculine),
                 ("ibus", Locative, Plural, Masculine), ("es", Vocative, Plural, Masculine),
                ("", Nominative, Singular, Feminine), ("is", Genitive, Singular, Feminine), ("em", Accusative, Singular, Feminine),
                ("e", Ablative, Singular, Feminine), ("i", Dative, Singular, Feminine), ("e", Locative, Singular, Feminine),
                 ("", Vocative, Singular, Feminine), ("es", Nominative, Plural, Feminine), ("um", Genitive, Plural, Feminine),
                 ("es", Accusative, Plural, Feminine), ("ibus", Ablative, Plural, Feminine), ("ibus", Dative, Plural, Feminine),
                 ("ibus", Locative, Plural, Feminine), ("es", Vocative, Plural, Feminine),
                ("", Nominative, Singular, Neuter), ("is", Genitive, Singular, Neuter), ("", Accusative, Singular, Neuter),
                ("e", Ablative, Singular, Neuter), ("i", Dative, Singular, Neuter), ("e", Locative, Singular, Neuter),
                 ("", Vocative, Singular, Neuter), ("a", Nominative, Plural, Neuter), ("um", Genitive, Plural, Neuter),
                 ("a", Accusative, Plural, Neuter), ("ibus", Ablative, Plural, Neuter), ("ibus", Dative, Plural, Neuter),
                 ("ibus", Locative, Plural, Neuter), ("a", Vocative, Plural, Neuter)
            ]
adjPresentPartFirstData :: [(String, Casus, Numerus, Genus)]
adjPresentPartFirstData = [
                ("ans", Nominative, Singular, Masculine), ("antis", Genitive, Singular, Masculine), ("antem", Accusative, Singular, Masculine),
                ("ante", Ablative, Singular, Masculine), ("anti", Dative, Singular, Masculine), ("anti", Locative, Singular, Masculine),
                 ("ans", Vocative, Singular, Masculine), ("antes", Nominative, Plural, Masculine), ("antium", Genitive, Plural, Masculine),
                 ("antes", Accusative, Plural, Masculine), ("antibus", Ablative, Plural, Masculine), ("antibus", Dative, Plural, Masculine),
                 ("antibus", Locative, Plural, Masculine), ("antes", Vocative, Plural, Masculine),
                ("ans", Nominative, Singular, Feminine), ("antis", Genitive, Singular, Feminine), ("antem", Accusative, Singular, Feminine),
                ("ante", Ablative, Singular, Feminine), ("anti", Dative, Singular, Feminine), ("anti", Locative, Singular, Feminine),
                 ("ans", Vocative, Singular, Feminine), ("antes", Nominative, Plural, Feminine), ("antium", Genitive, Plural, Feminine),
                 ("antes", Accusative, Plural, Feminine), ("antibus", Ablative, Plural, Feminine), ("antibus", Dative, Plural, Feminine),
                 ("antibus", Locative, Plural, Feminine), ("antes", Vocative, Plural, Feminine),
                ("ans", Nominative, Singular, Neuter), ("antis", Genitive, Singular, Neuter), ("ans", Accusative, Singular, Neuter),
                ("ante", Ablative, Singular, Neuter), ("anti", Dative, Singular, Neuter), ("anti", Locative, Singular, Neuter),
                 ("ans", Vocative, Singular, Neuter), ("antia", Nominative, Plural, Neuter), ("antium", Genitive, Plural, Neuter),
                 ("antia", Accusative, Plural, Neuter), ("antibus", Ablative, Plural, Neuter), ("antibus", Dative, Plural, Neuter),
                 ("antibus", Locative, Plural, Neuter), ("antia", Vocative, Plural, Neuter)
            ]
adjPresentPartSecondData :: [(String, Casus, Numerus, Genus)]
adjPresentPartSecondData = [
                ("ns", Nominative, Singular, Masculine), ("ntis", Genitive, Singular, Masculine), ("ntem", Accusative, Singular, Masculine),
                ("nte", Ablative, Singular, Masculine), ("nti", Dative, Singular, Masculine), ("nti", Locative, Singular, Masculine),
                 ("ns", Vocative, Singular, Masculine), ("ntes", Nominative, Plural, Masculine), ("ntium", Genitive, Plural, Masculine),
                 ("ntes", Accusative, Plural, Masculine), ("ntibus", Ablative, Plural, Masculine), ("ntibus", Dative, Plural, Masculine),
                 ("ntibus", Locative, Plural, Masculine), ("ntes", Vocative, Plural, Masculine),
                ("ns", Nominative, Singular, Feminine), ("ntis", Genitive, Singular, Feminine), ("ntem", Accusative, Singular, Feminine),
                ("nte", Ablative, Singular, Feminine), ("nti", Dative, Singular, Feminine), ("nti", Locative, Singular, Feminine),
                 ("ns", Vocative, Singular, Feminine), ("ntes", Nominative, Plural, Feminine), ("ntium", Genitive, Plural, Feminine),
                 ("ntes", Accusative, Plural, Feminine), ("ntibus", Ablative, Plural, Feminine), ("ntibus", Dative, Plural, Feminine),
                 ("ntibus", Locative, Plural, Feminine), ("ntes", Vocative, Plural, Feminine),
                ("ns", Nominative, Singular, Neuter), ("ntis", Genitive, Singular, Neuter), ("ns", Accusative, Singular, Neuter),
                ("nte", Ablative, Singular, Neuter), ("nti", Dative, Singular, Neuter), ("nti", Locative, Singular, Neuter),
                 ("ns", Vocative, Singular, Neuter), ("ntia", Nominative, Plural, Neuter), ("ntium", Genitive, Plural, Neuter),
                 ("ntia", Accusative, Plural, Neuter), ("ntibus", Ablative, Plural, Neuter), ("ntibus", Dative, Plural, Neuter),
                 ("ntibus", Locative, Plural, Neuter), ("ntia", Vocative, Plural, Neuter)
            ]
adjPresentPartThirdData :: [(String, Casus, Numerus, Genus)]
adjPresentPartThirdData = [
                ("ens", Nominative, Singular, Masculine), ("entis", Genitive, Singular, Masculine), ("entem", Accusative, Singular, Masculine),
                ("ente", Ablative, Singular, Masculine), ("enti", Dative, Singular, Masculine), ("enti", Locative, Singular, Masculine),
                 ("ens", Vocative, Singular, Masculine), ("entes", Nominative, Plural, Masculine), ("entium", Genitive, Plural, Masculine),
                 ("entes", Accusative, Plural, Masculine), ("entibus", Ablative, Plural, Masculine), ("entibus", Dative, Plural, Masculine),
                 ("entibus", Locative, Plural, Masculine), ("entes", Vocative, Plural, Masculine),
                ("ens", Nominative, Singular, Feminine), ("entis", Genitive, Singular, Feminine), ("entem", Accusative, Singular, Feminine),
                ("ente", Ablative, Singular, Feminine), ("enti", Dative, Singular, Feminine), ("enti", Locative, Singular, Feminine),
                 ("ens", Vocative, Singular, Feminine), ("entes", Nominative, Plural, Feminine), ("entium", Genitive, Plural, Feminine),
                 ("entes", Accusative, Plural, Feminine), ("entibus", Ablative, Plural, Feminine), ("entibus", Dative, Plural, Feminine),
                 ("entibus", Locative, Plural, Feminine), ("entes", Vocative, Plural, Feminine),
                ("ens", Nominative, Singular, Neuter), ("entis", Genitive, Singular, Neuter), ("ens", Accusative, Singular, Neuter),
                ("ente", Ablative, Singular, Neuter), ("enti", Dative, Singular, Neuter), ("enti", Locative, Singular, Neuter),
                 ("ens", Vocative, Singular, Neuter), ("entia", Nominative, Plural, Neuter), ("entium", Genitive, Plural, Neuter),
                 ("entia", Accusative, Plural, Neuter), ("entibus", Ablative, Plural, Neuter), ("entibus", Dative, Plural, Neuter),
                 ("entibus", Locative, Plural, Neuter), ("entia", Vocative, Plural, Neuter)
            ]
adjPresentPartThirdIData :: [(String, Casus, Numerus, Genus)]
adjPresentPartThirdIData = [
                ("iens", Nominative, Singular, Masculine), ("ientis", Genitive, Singular, Masculine), ("entem", Accusative, Singular, Masculine),
                ("iente", Ablative, Singular, Masculine), ("ienti", Dative, Singular, Masculine), ("ienti", Locative, Singular, Masculine),
                 ("iens", Vocative, Singular, Masculine), ("ientes", Nominative, Plural, Masculine), ("ientium", Genitive, Plural, Masculine),
                 ("ientes", Accusative, Plural, Masculine), ("ientibus", Ablative, Plural, Masculine), ("ientibus", Dative, Plural, Masculine),
                 ("ientibus", Locative, Plural, Masculine), ("ientes", Vocative, Plural, Masculine),
                ("iens", Nominative, Singular, Feminine), ("ientis", Genitive, Singular, Feminine), ("ientem", Accusative, Singular, Feminine),
                ("iente", Ablative, Singular, Feminine), ("ienti", Dative, Singular, Feminine), ("ienti", Locative, Singular, Feminine),
                 ("iens", Vocative, Singular, Feminine), ("ientes", Nominative, Plural, Feminine), ("ientium", Genitive, Plural, Feminine),
                 ("ientes", Accusative, Plural, Feminine), ("ientibus", Ablative, Plural, Feminine), ("ientibus", Dative, Plural, Feminine),
                 ("ientibus", Locative, Plural, Feminine), ("ientes", Vocative, Plural, Feminine),
                ("iens", Nominative, Singular, Neuter), ("ientis", Genitive, Singular, Neuter), ("iens", Accusative, Singular, Neuter),
                ("iente", Ablative, Singular, Neuter), ("ienti", Dative, Singular, Neuter), ("ienti", Locative, Singular, Neuter),
                 ("iens", Vocative, Singular, Neuter), ("ientia", Nominative, Plural, Neuter), ("ientium", Genitive, Plural, Neuter),
                 ("ientia", Accusative, Plural, Neuter), ("ientibus", Ablative, Plural, Neuter), ("ientibus", Dative, Plural, Neuter),
                 ("ientibus", Locative, Plural, Neuter), ("ientia", Vocative, Plural, Neuter)
            ]
adjPresentPartEoData :: [(String, Casus, Numerus, Genus)]
adjPresentPartEoData = [
                ("iens", Nominative, Singular, Masculine), ("euntis", Genitive, Singular, Masculine), ("euntem", Accusative, Singular, Masculine),
                ("eunte", Ablative, Singular, Masculine), ("eunti", Dative, Singular, Masculine), ("eunti", Locative, Singular, Masculine),
                 ("iens", Vocative, Singular, Masculine), ("euntes", Nominative, Plural, Masculine), ("euntium", Genitive, Plural, Masculine),
                 ("euntes", Accusative, Plural, Masculine), ("euntibus", Ablative, Plural, Masculine), ("euntibus", Dative, Plural, Masculine),
                 ("euntibus", Locative, Plural, Masculine), ("euntes", Vocative, Plural, Masculine),
                ("iens", Nominative, Singular, Feminine), ("euntis", Genitive, Singular, Feminine), ("euntem", Accusative, Singular, Feminine),
                ("eunte", Ablative, Singular, Feminine), ("eunti", Dative, Singular, Feminine), ("eunti", Locative, Singular, Feminine),
                 ("iens", Vocative, Singular, Feminine), ("euntes", Nominative, Plural, Feminine), ("euntium", Genitive, Plural, Feminine),
                 ("euntes", Accusative, Plural, Feminine), ("euntibus", Ablative, Plural, Feminine), ("euntibus", Dative, Plural, Feminine),
                 ("euntibus", Locative, Plural, Feminine), ("euntes", Vocative, Plural, Feminine),
                ("iens", Nominative, Singular, Neuter), ("euntis", Genitive, Singular, Neuter), ("iens", Accusative, Singular, Neuter),
                ("eunte", Ablative, Singular, Neuter), ("eunti", Dative, Singular, Neuter), ("eunti", Locative, Singular, Neuter),
                 ("iens", Vocative, Singular, Neuter), ("euntia", Nominative, Plural, Neuter), ("euntium", Genitive, Plural, Neuter),
                 ("euntia", Accusative, Plural, Neuter), ("euntibus", Ablative, Plural, Neuter), ("euntibus", Dative, Plural, Neuter),
                 ("euntibus", Locative, Plural, Neuter), ("euntia", Vocative, Plural, Neuter)
            ]
adjFutPasSecondData :: [(String, Casus, Numerus, Genus)]
adjFutPasSecondData = [
                ("ndus", Nominative, Singular, Masculine), ("ndi", Genitive, Singular, Masculine), ("ndum", Accusative, Singular, Masculine),
                ("ndo", Ablative, Singular, Masculine), ("ndo", Dative, Singular, Masculine), ("ndi", Locative, Singular, Masculine),
                 ("nde", Vocative, Singular, Masculine), ("ndi", Nominative, Plural, Masculine), ("ndorum", Genitive, Plural, Masculine),
                 ("ndos", Accusative, Plural, Masculine), ("ndis", Ablative, Plural, Masculine), ("ndis", Dative, Plural, Masculine),
                 ("ndis", Locative, Plural, Masculine), ("ndi", Vocative, Plural, Masculine),
                ("nda", Nominative, Singular, Feminine), ("ndae", Genitive, Singular, Feminine), ("ndam", Accusative, Singular, Feminine),
                ("nda", Ablative, Singular, Feminine), ("ndae", Dative, Singular, Feminine), ("ndae", Locative, Singular, Feminine),
                 ("nda", Vocative, Singular, Feminine), ("ndae", Nominative, Plural, Feminine), ("ndarum", Genitive, Plural, Feminine),
                 ("ndas", Accusative, Plural, Feminine), ("ndis", Ablative, Plural, Feminine), ("ndis", Dative, Plural, Feminine),
                 ("ndis", Locative, Plural, Feminine), ("ndae", Vocative, Plural, Feminine),
                ("ndum", Nominative, Singular, Neuter), ("ndi", Genitive, Singular, Neuter), ("ndum", Accusative, Singular, Neuter),
                ("ndo", Ablative, Singular, Neuter), ("ndo", Dative, Singular, Neuter), ("ndi", Locative, Singular, Neuter),
                 ("ndum", Vocative, Singular, Neuter), ("nda", Nominative, Plural, Neuter), ("ndorum", Genitive, Plural, Neuter),
                 ("nda", Accusative, Plural, Neuter), ("ndis", Ablative, Plural, Neuter), ("ndis", Dative, Plural, Neuter),
                 ("ndis", Locative, Plural, Neuter), ("nda", Vocative, Plural, Neuter)
            ]
adjFutPasFirstData = map (\(s,c,n,g) -> ("a"++s, c, n, g)) adjFutPasSecondData
adjFutPasThirdData = map (\(s,c,n,g) -> ("e"++s, c, n, g)) adjFutPasSecondData
adjFutPasThirdIData = map (\(s,c,n,g) -> ("ie"++s, c, n, g)) adjFutPasSecondData
adjFutPasEoData = map (\(s,c,n,g) -> ("eu"++s, c, n, g)) adjFutPasSecondData
adjFutActEndings :: [(String, Casus, Numerus, Genus)]
adjFutActEndings = [
                    ("urus", Nominative, Singular, Masculine), ("uri", Genitive, Singular, Masculine), ("urum", Accusative, Singular, Masculine)
                    , ("uro", Ablative, Singular, Masculine), ("uro", Dative, Singular, Masculine), ("uri", Locative, Singular, Masculine)
                    , ("ure", Vocative, Singular, Masculine), ("uri", Nominative, Plural, Masculine), ("urorum", Genitive, Plural, Masculine)
                    , ("uros", Accusative, Plural, Masculine), ("uris", Ablative, Plural, Masculine), ("uris", Dative, Plural, Masculine)
                    , ("uris", Locative, Plural, Masculine), ("uri", Vocative, Plural, Masculine),
                    ("ura", Nominative, Singular, Feminine), ("urae", Genitive, Singular, Feminine), ("uram", Accusative, Singular, Feminine)
                    , ("ura", Ablative, Singular, Feminine), ("urae", Dative, Singular, Feminine), ("urae", Locative, Singular, Feminine)
                    , ("ura", Vocative, Singular, Feminine), ("urae", Nominative, Plural, Feminine), ("urarum", Genitive, Plural, Feminine)
                    , ("uras", Accusative, Plural, Feminine), ("uris", Ablative, Plural, Feminine), ("uris", Dative, Plural, Feminine)
                    , ("uris", Locative, Plural, Feminine), ("urae", Vocative, Plural, Feminine),
                    ("urum", Nominative, Singular, Neuter), ("uri", Genitive, Singular, Neuter), ("urum", Accusative, Singular, Neuter)
                    , ("uro", Ablative, Singular, Neuter), ("uro", Dative, Singular, Neuter), ("uri", Locative, Singular, Neuter)
                    , ("urum", Vocative, Singular, Neuter), ("ura", Nominative, Plural, Neuter), ("urorum", Genitive, Plural, Neuter)
                    , ("ura", Accusative, Plural, Neuter), ("uris", Ablative, Plural, Neuter), ("uris", Dative, Plural, Neuter)
                    , ("uris", Locative, Plural, Neuter), ("ura", Vocative, Plural, Neuter)
                    ]

first4 :: (a, b, c, d) -> a
first4 (a, _, _, _) = a
second4 :: (a, b, c, d) -> b
second4 (_, b, _, _) = b
third4 :: (a, b, c, d) -> c
third4 (_, _, c, _) = c
fourth4 :: (a, b, c, d) -> d
fourth4 (_, _, _, d) = d

positiveData = zip adjFSData (repeat Positive)
iusData = zip adjIusData (repeat Positive)
ius2Data = zip adjIus2Data (repeat Positive)
positiveThirdData = zip adjFSThird1Data (repeat Positive)
positiveGreek2Data = zip adjFSGreek2Data (repeat Positive)
positiveGreek3Data = zip adjFSGreek3Data (repeat Positive)
comparativeData = zip adjCompData (repeat Comparative)
superlativeData = zip adjFSData (repeat Superlative)
presentPartFirstData = zip adjPresentPartFirstData (repeat Positive)
presentPartSecondData = zip adjPresentPartSecondData (repeat Positive)
presentPartThirdData = zip adjPresentPartThirdData (repeat Positive)
presentPartThirdIData = zip adjPresentPartThirdIData (repeat Positive)
presentPartEoData = zip adjPresentPartEoData (repeat Positive)
futPasFirstData = zip adjFutPasFirstData (repeat Positive)
futPasSecondData = zip adjFutPasSecondData (repeat Positive)
futPasThirdData = zip adjFutPasThirdData (repeat Positive)
futPasThirdIData = zip adjFutPasThirdIData (repeat Positive)
futPasEoData = zip adjFutPasEoData (repeat Positive)
futActData = zip adjFutActEndings (repeat Positive)

declineAdj :: Adjective -> [Adjective]
declineAdj adj = case adjDeclension adj of
              AdjFirstSecond -> map createWord positiveData ++ map createWord comparativeData ++ map createWord superlativeData
              AdjFirstSecondRe -> map createWord positiveData ++ map createWord comparativeData ++ map createWord superlativeData
              AdjFirstSecondR -> map createWord positiveData ++ map createWord comparativeData ++ map createWord superlativeData
              AdjFirstSecondGreek2 -> map createWord positiveGreek2Data ++ map createWord comparativeData ++ map createWord superlativeData
              AdjFirstSecondGreek3 -> map createWord positiveGreek3Data ++ map createWord comparativeData ++ map createWord superlativeData
              AdjThird1 -> map createWord positiveThirdData ++ map createWord comparativeData ++ map createWord superlativeData
              AdjThird2 -> map createWord positiveThirdData ++ map createWord comparativeData ++ map createWord superlativeData
              AdjThird3 -> map createWord positiveThirdData ++ map createWord comparativeData ++ map createWord superlativeData
              AdjPositiveOnly -> map createWord positiveData
              AdjComparativeOnly -> map createWord comparativeData
              AdjPresentParticipleFirst -> map createWord presentPartFirstData
              AdjPresentParticipleSecond -> map createWord presentPartSecondData
              AdjPresentParticipleThird -> map createWord presentPartThirdData
              AdjPresentParticipleThirdI -> map createWord presentPartThirdIData
              AdjPresentParticipleFourth -> map createWord presentPartThirdData
              AdjPresentParticipleEo -> map createWord presentPartEoData
              AdjFutPasParticipleFirst -> map createWord futPasFirstData
              AdjFutPasParticipleSecond -> map createWord futPasSecondData
              AdjFutPasParticipleThird -> map createWord futPasThirdData
              AdjFutPasParticipleThirdI -> map createWord futPasThirdData
              AdjFutPasParticipleFourth -> map createWord futPasThirdData
              AdjFutPasParticipleEo -> map createWord futPasEoData
              AdjIus | mascSing adj == "alius" -> map createWord ius2Data
                     | otherwise -> map createWord iusData
              AdjFutActParticiple -> map createWord futActData
              where createWord (d, comp) = adj {adjEnding = first4 d, adjCasus = second4 d,
                                                adjNumber = third4 d, adjGender = fourth4 d, adjDegree = comp}

verbFirstConjData :: [(String, VerbPerson, Numerus)]
verbFirstConjData = [
                     ("o", PersonFirst, Singular), ("as", PersonSecond, Singular),
                     ("at", PersonThird, Singular), ("amus", PersonFirst, Plural),
                     ("atis", PersonSecond, Plural), ("ant", PersonThird, Plural)
                    ]
verbSecondConjData :: [(String, VerbPerson, Numerus)]
verbSecondConjData = [
                     ("o", PersonFirst, Singular), ("s", PersonSecond, Singular),
                     ("t", PersonThird, Singular), ("mus", PersonFirst, Plural),
                     ("tis", PersonSecond, Plural), ("nt", PersonThird, Plural)
                    ]
verbThirdConjData :: [(String, VerbPerson, Numerus)]
verbThirdConjData = [
                     ("o", PersonFirst, Singular), ("is", PersonSecond, Singular),
                     ("it", PersonThird, Singular), ("imus", PersonFirst, Plural),
                     ("itis", PersonSecond, Plural), ("unt", PersonThird, Plural)
                    ]
verbThirdIConjData :: [(String, VerbPerson, Numerus)]
verbThirdIConjData = [
                     ("io", PersonFirst, Singular), ("is", PersonSecond, Singular),
                     ("it", PersonThird, Singular), ("imus", PersonFirst, Plural),
                     ("itis", PersonSecond, Plural), ("iunt", PersonThird, Plural)
                    ]
verbFourthConjData :: [(String, VerbPerson, Numerus)]
verbFourthConjData = [
                     ("o", PersonFirst, Singular), ("s", PersonSecond, Singular),
                     ("t", PersonThird, Singular), ("mus", PersonFirst, Plural),
                     ("tis", PersonSecond, Plural), ("unt", PersonThird, Plural)
                    ]
verbFirstPassConjData :: [(String, VerbPerson, Numerus)]
verbFirstPassConjData = [
                     ("or", PersonFirst, Singular), ("aris", PersonSecond, Singular),
                     ("atur", PersonThird, Singular), ("amur", PersonFirst, Plural),
                     ("amini", PersonSecond, Plural), ("antur", PersonThird, Plural)
                    ]
verbSecondPassConjData :: [(String, VerbPerson, Numerus)]
verbSecondPassConjData = [
                     ("or", PersonFirst, Singular), ("ris", PersonSecond, Singular),
                     ("tur", PersonThird, Singular), ("mur", PersonFirst, Plural),
                     ("mini", PersonSecond, Plural), ("ntur", PersonThird, Plural)
                    ]
verbThirdPassConjData :: [(String, VerbPerson, Numerus)]
verbThirdPassConjData = [
                     ("or", PersonFirst, Singular), ("eris", PersonSecond, Singular),
                     ("itur", PersonThird, Singular), ("imur", PersonFirst, Plural),
                     ("imini", PersonSecond, Plural), ("untur", PersonThird, Plural)
                    ]
verbThirdIPassConjData :: [(String, VerbPerson, Numerus)]
verbThirdIPassConjData = [
                     ("ior", PersonFirst, Singular), ("eris", PersonSecond, Singular),
                     ("itur", PersonThird, Singular), ("imur", PersonFirst, Plural),
                     ("imini", PersonSecond, Plural), ("iuntur", PersonThird, Plural)
                    ]
verbFourthPassConjData :: [(String, VerbPerson, Numerus)]
verbFourthPassConjData = [
                     ("or", PersonFirst, Singular), ("ris", PersonSecond, Singular),
                     ("tur", PersonThird, Singular), ("mur", PersonFirst, Plural),
                     ("mini", PersonSecond, Plural), ("untur", PersonThird, Plural)
                    ]
verbFirstConjImperfData :: [(String, VerbPerson, Numerus)]
verbFirstConjImperfData = [
                     ("abam", PersonFirst, Singular), ("abas", PersonSecond, Singular),
                     ("abat", PersonThird, Singular), ("abamus", PersonFirst, Plural),
                     ("abatis", PersonSecond, Plural), ("abant", PersonThird, Plural)
                    ]
verbSecondConjImperfData :: [(String, VerbPerson, Numerus)]
verbSecondConjImperfData = [
                     ("bam", PersonFirst, Singular), ("bas", PersonSecond, Singular),
                     ("bat", PersonThird, Singular), ("bamus", PersonFirst, Plural),
                     ("batis", PersonSecond, Plural), ("bant", PersonThird, Plural)
                    ]
verbEoConjImperfData = map (\(s,p,n) -> ("i" ++ s, p, n)) verbSecondConjImperfData
verbThirdConjImperfData :: [(String, VerbPerson, Numerus)]
verbThirdConjImperfData = [
                     ("ebam", PersonFirst, Singular), ("ebas", PersonSecond, Singular),
                     ("ebat", PersonThird, Singular), ("ebamus", PersonFirst, Plural),
                     ("ebatis", PersonSecond, Plural), ("ebant", PersonThird, Plural)
                    ]
verbThirdIConjImperfData :: [(String, VerbPerson, Numerus)]
verbThirdIConjImperfData = [
                     ("iebam", PersonFirst, Singular), ("iebas", PersonSecond, Singular),
                     ("iebat", PersonThird, Singular), ("iebamus", PersonFirst, Plural),
                     ("iebatis", PersonSecond, Plural), ("iebant", PersonThird, Plural)
                    ]
verbFourthConjImperfData :: [(String, VerbPerson, Numerus)]
verbFourthConjImperfData = [
                     ("ebam", PersonFirst, Singular), ("ebas", PersonSecond, Singular),
                     ("ebat", PersonThird, Singular), ("ebamus", PersonFirst, Plural),
                     ("ebatis", PersonSecond, Plural), ("ebant", PersonThird, Plural)
                    ]
verbFirstConjImperfPasData :: [(String, VerbPerson, Numerus)]
verbFirstConjImperfPasData = [
                     ("abar", PersonFirst, Singular), ("abaris", PersonSecond, Singular),
                     ("abatur", PersonThird, Singular), ("abamur", PersonFirst, Plural),
                     ("abamini", PersonSecond, Plural), ("abantur", PersonThird, Plural)
                    ]
verbSecondConjImperfPasData :: [(String, VerbPerson, Numerus)]
verbSecondConjImperfPasData = [
                     ("bar", PersonFirst, Singular), ("baris", PersonSecond, Singular),
                     ("batur", PersonThird, Singular), ("bamur", PersonFirst, Plural),
                     ("bamini", PersonSecond, Plural), ("bantur", PersonThird, Plural)
                    ]
verbThirdConjImperfPasData :: [(String, VerbPerson, Numerus)]
verbThirdConjImperfPasData = [
                     ("ebar", PersonFirst, Singular), ("ebaris", PersonSecond, Singular),
                     ("ebatur", PersonThird, Singular), ("ebamur", PersonFirst, Plural),
                     ("ebamini", PersonSecond, Plural), ("ebantur", PersonThird, Plural)
                    ]
verbThirdIConjImperfPasData :: [(String, VerbPerson, Numerus)]
verbThirdIConjImperfPasData = [
                     ("iebar", PersonFirst, Singular), ("iebaris", PersonSecond, Singular),
                     ("iebatur", PersonThird, Singular), ("iebamur", PersonFirst, Plural),
                     ("iebamini", PersonSecond, Plural), ("iebantur", PersonThird, Plural)
                    ]
verbFourthConjImperfPasData :: [(String, VerbPerson, Numerus)]
verbFourthConjImperfPasData = [
                     ("ebar", PersonFirst, Singular), ("ebaris", PersonSecond, Singular),
                     ("ebatur", PersonThird, Singular), ("ebamur", PersonFirst, Plural),
                     ("ebamini", PersonSecond, Plural), ("ebantur", PersonThird, Plural)
                    ]
verbFirstConjFutData :: [(String, VerbPerson, Numerus)]
verbFirstConjFutData = [
                     ("abo", PersonFirst, Singular), ("abis", PersonSecond, Singular),
                     ("abit", PersonThird, Singular), ("abimus", PersonFirst, Plural),
                     ("abitis", PersonSecond, Plural), ("abunt", PersonThird, Plural)
                    ]
verbSecondConjFutData :: [(String, VerbPerson, Numerus)]
verbSecondConjFutData = [
                     ("bo", PersonFirst, Singular), ("bis", PersonSecond, Singular),
                     ("bit", PersonThird, Singular), ("bimus", PersonFirst, Plural),
                     ("bitis", PersonSecond, Plural), ("bunt", PersonThird, Plural)
                    ]
verbEoConjFutData = map (\(s,p,n) -> ("i" ++ s, p, n)) verbSecondConjFutData
verbThirdConjFutData :: [(String, VerbPerson, Numerus)]
verbThirdConjFutData = [
                     ("am", PersonFirst, Singular), ("es", PersonSecond, Singular),
                     ("et", PersonThird, Singular), ("emus", PersonFirst, Plural),
                     ("etis", PersonSecond, Plural), ("ent", PersonThird, Plural)
                    ]
verbThirdIConjFutData :: [(String, VerbPerson, Numerus)]
verbThirdIConjFutData = [
                     ("iam", PersonFirst, Singular), ("ies", PersonSecond, Singular),
                     ("iet", PersonThird, Singular), ("iemus", PersonFirst, Plural),
                     ("ietis", PersonSecond, Plural), ("ient", PersonThird, Plural)
                    ]
verbFourthConjFutData :: [(String, VerbPerson, Numerus)]
verbFourthConjFutData = [
                     ("am", PersonFirst, Singular), ("es", PersonSecond, Singular),
                     ("et", PersonThird, Singular), ("emus", PersonFirst, Plural),
                     ("etis", PersonSecond, Plural), ("ent", PersonThird, Plural)
                    ]
verbFirstConjFutPasData :: [(String, VerbPerson, Numerus)]
verbFirstConjFutPasData = [
                     ("abor", PersonFirst, Singular), ("aberis", PersonSecond, Singular),
                     ("abitur", PersonThird, Singular), ("abimur", PersonFirst, Plural),
                     ("abimini", PersonSecond, Plural), ("abuntur", PersonThird, Plural)
                    ]
verbSecondConjFutPasData :: [(String, VerbPerson, Numerus)]
verbSecondConjFutPasData = [
                     ("bor", PersonFirst, Singular), ("beris", PersonSecond, Singular),
                     ("bitur", PersonThird, Singular), ("bimur", PersonFirst, Plural),
                     ("bimini", PersonSecond, Plural), ("buntur", PersonThird, Plural)
                    ]
verbThirdConjFutPasData :: [(String, VerbPerson, Numerus)]
verbThirdConjFutPasData = [
                     ("ar", PersonFirst, Singular), ("eris", PersonSecond, Singular),
                     ("etur", PersonThird, Singular), ("emur", PersonFirst, Plural),
                     ("emini", PersonSecond, Plural), ("entur", PersonThird, Plural)
                    ]
verbThirdIConjFutPasData :: [(String, VerbPerson, Numerus)]
verbThirdIConjFutPasData = [
                     ("iar", PersonFirst, Singular), ("ieris", PersonSecond, Singular),
                     ("ietur", PersonThird, Singular), ("iemur", PersonFirst, Plural),
                     ("iemini", PersonSecond, Plural), ("ientur", PersonThird, Plural)
                    ]
verbFourthConjFutPasData :: [(String, VerbPerson, Numerus)]
verbFourthConjFutPasData = [
                     ("ar", PersonFirst, Singular), ("eris", PersonSecond, Singular),
                     ("etur", PersonThird, Singular), ("emur", PersonFirst, Plural),
                     ("emini", PersonSecond, Plural), ("entur", PersonThird, Plural)
                    ]
verbPerfectData :: [(String, VerbPerson, Numerus)]
verbPerfectData = [
                    ("i", PersonFirst, Singular), ("isti", PersonSecond, Singular),
                    ("it", PersonThird, Singular), ("imus", PersonFirst, Plural),
                    ("istis", PersonSecond, Plural), ("erunt", PersonThird, Plural)
                  ]
verbEoConjData :: [(String, VerbPerson, Numerus)]
verbEoConjData = [
                    ("eo", PersonFirst, Singular), ("is", PersonSecond, Singular),
                    ("it", PersonThird, Singular), ("imus", PersonFirst, Plural),
                    ("itis", PersonSecond, Plural), ("eunt", PersonThird, Plural)
                 ]
verbFeroConjData :: [(String, VerbPerson, Numerus)]
verbFeroConjData = [
                    ("o", PersonFirst, Singular), ("s", PersonSecond, Singular),
                    ("t", PersonThird, Singular), ("imus", PersonFirst, Plural),
                    ("tis", PersonSecond, Plural), ("unt", PersonThird, Plural)
                 ]
verbFeroPassConjData :: [(String, VerbPerson, Numerus)]
verbFeroPassConjData = [
                    ("or", PersonFirst, Singular), ("ris", PersonSecond, Singular),
                    ("tur", PersonThird, Singular), ("imur", PersonFirst, Plural),
                    ("imini", PersonSecond, Plural), ("untur", PersonThird, Plural)
                 ]
verbPluPerfectData :: [(String, VerbPerson, Numerus)]
verbPluPerfectData = [
                    ("eram", PersonFirst, Singular), ("eras", PersonSecond, Singular),
                    ("erat", PersonThird, Singular), ("eramus", PersonFirst, Plural),
                    ("eratis", PersonSecond, Plural), ("erant", PersonThird, Plural)
                  ]
verbFutPerfectData :: [(String, VerbPerson, Numerus)]
verbFutPerfectData = [
                    ("ero", PersonFirst, Singular), ("eris", PersonSecond, Singular),
                    ("erit", PersonThird, Singular), ("erimus", PersonFirst, Plural),
                    ("eritis", PersonSecond, Plural), ("erint", PersonThird, Plural)
                  ]
verbSubjunctiveStubData :: [(String, VerbPerson, Numerus)]
verbSubjunctiveStubData = [
                     ("m", PersonFirst, Singular), ("s", PersonSecond, Singular),
                     ("t", PersonThird, Singular), ("mus", PersonFirst, Plural),
                     ("tis", PersonSecond, Plural), ("nt", PersonThird, Plural)
                    ]
verbSubjunctiveFirstData = map (\(s,p,n) -> ("e" ++ s, p, n)) verbSubjunctiveStubData
verbSubjunctiveSecondData = map (\(s,p,n) -> ("a" ++ s, p, n)) verbSubjunctiveStubData
verbSubjunctiveThirdData = map (\(s,p,n) -> ("a" ++ s, p, n)) verbSubjunctiveStubData
verbSubjunctiveThirdIData = map (\(s,p,n) -> ("ia" ++ s, p, n)) verbSubjunctiveStubData
verbSubjunctiveFourthData = map (\(s,p,n) -> ("a" ++ s, p, n)) verbSubjunctiveStubData
verbSubjunctiveEoData = map (\(s,p,n) -> ("ea" ++ s, p, n)) verbSubjunctiveStubData
verbSubjunctiveImperfPasStubData :: [(String, VerbPerson, Numerus)]
verbSubjunctiveImperfPasStubData = [
                     ("r", PersonFirst, Singular), ("ris", PersonSecond, Singular),
                     ("tur", PersonThird, Singular), ("mur", PersonFirst, Plural),
                     ("mini", PersonSecond, Plural), ("ntur", PersonThird, Plural)
                    ]
verbSubjunctiveFirstPasData = map (\(s,p,n) -> ("e" ++ s, p, n)) verbSubjunctiveImperfPasStubData
verbSubjunctiveSecondPasData = map (\(s,p,n) -> ("a" ++ s, p, n)) verbSubjunctiveImperfPasStubData
verbSubjunctiveThirdPasData = map (\(s,p,n) -> ("a" ++ s, p, n)) verbSubjunctiveImperfPasStubData
verbSubjunctiveThirdIPasData = map (\(s,p,n) -> ("ia" ++ s, p, n)) verbSubjunctiveImperfPasStubData
verbSubjunctiveFourthPasData = map (\(s,p,n) -> ("a" ++ s, p, n)) verbSubjunctiveImperfPasStubData
verbSubjunctivePerfectData :: [(String, VerbPerson, Numerus)]
verbSubjunctivePerfectData = [
                    ("erim", PersonFirst, Singular), ("eris", PersonSecond, Singular),
                    ("erit", PersonThird, Singular), ("erimus", PersonFirst, Plural),
                    ("eritis", PersonSecond, Plural), ("erint", PersonThird, Plural)
                  ]
verbSubjunctivePluPerfectData :: [(String, VerbPerson, Numerus)]
verbSubjunctivePluPerfectData = [
                    ("issem", PersonFirst, Singular), ("isses", PersonSecond, Singular),
                    ("isset", PersonThird, Singular), ("issemus", PersonFirst, Plural),
                    ("issetis", PersonSecond, Plural), ("issent", PersonThird, Plural)
                  ]
verbSumPresIndEnds = [
                    ("sum", PersonFirst, Singular), ("es", PersonSecond, Singular), ("est", PersonThird, Singular),
                    ("sumus", PersonFirst, Plural), ("estis", PersonSecond, Plural), ("sunt", PersonThird, Plural)
                    ]
verbSumImperfIndEnds = [
                    ("eram", PersonFirst, Singular), ("eras", PersonSecond, Singular), ("erat", PersonThird, Singular),
                    ("eramus", PersonFirst, Plural), ("eratis", PersonSecond, Plural), ("erant", PersonThird, Plural)
                    ]
verbSumFutIndEnds = [
                    ("ero", PersonFirst, Singular), ("eris", PersonSecond, Singular), ("erit", PersonThird, Singular),
                    ("erimus", PersonFirst, Plural), ("eritis", PersonSecond, Plural), ("erunt", PersonThird, Plural)
                    ]
verbSumPresSubjEnds = [
                    ("sim", PersonFirst, Singular), ("sis", PersonSecond, Singular), ("sit", PersonThird, Singular),
                    ("simus", PersonFirst, Plural), ("sitis", PersonSecond, Plural), ("sint", PersonThird, Plural)
                    ]
verbImpPresFirstEnds = [
                    ("a", PersonSecond, Singular), ("ate", PersonSecond, Plural)
                   ]
verbImpPresSecondEnds = [
                        ("", PersonSecond, Singular), ("te", PersonSecond, Plural)
                        ]
verbImpPresThirdEnds = [
                        ("e", PersonSecond, Singular), ("ite", PersonSecond, Plural)
                       ]
verbImpPresThirdIEnds = [
                        ("e", PersonSecond, Singular), ("ite", PersonSecond, Plural)
                        ]
verbImpPresFourthEnds = [
                        ("", PersonSecond, Singular), ("te", PersonSecond, Plural)
                        ]
verbImpFutFirstEnds = [
                      ("ato", PersonSecond, Singular), ("ato", PersonThird, Singular),
                      ("atote", PersonSecond, Plural), ("anto", PersonThird, Plural)
                      ]
verbImpFutSecondEnds = [
                      ("to", PersonSecond, Singular), ("to", PersonThird, Singular),
                      ("tote", PersonSecond, Plural), ("nto", PersonThird, Plural)
                      ]
verbImpFutThirdEnds = [
                      ("ito", PersonSecond, Singular), ("ito", PersonThird, Singular),
                      ("itote", PersonSecond, Plural), ("unto", PersonThird, Plural)
                      ]
verbImpFutThirdIEnds = [
                      ("ito", PersonSecond, Singular), ("ito", PersonThird, Singular),
                      ("itote", PersonSecond, Plural), ("iunto", PersonThird, Plural)
                      ]
verbImpFutFourthEnds = [
                      ("to", PersonSecond, Singular), ("to", PersonThird, Singular),
                      ("tote", PersonSecond, Plural), ("unto", PersonThird, Plural)
                      ]
verbImpPresFeroEnds = [
                    ("", PersonSecond, Singular), ("te", PersonSecond, Plural)
                   ]
verbImpFutFeroEnds = [
                      ("to", PersonSecond, Singular), ("to", PersonThird, Singular),
                      ("tote", PersonSecond, Plural), ("unto", PersonThird, Plural)
                      ]
verbImpPresSumEnds = [
                    ("es", PersonSecond, Singular), ("este", PersonSecond, Plural)
                   ]
verbImpFutSumEnds = [
                      ("esto", PersonSecond, Singular), ("esto", PersonThird, Singular),
                      ("estote", PersonSecond, Plural), ("sunto", PersonThird, Plural)
                      ]
verbImpPresEoEnds = [
                    ("i", PersonSecond, Singular), ("ite", PersonSecond, Plural)
                   ]
verbImpFutEoEnds = [
                      ("ito", PersonSecond, Singular), ("ito", PersonThird, Singular),
                      ("itote", PersonSecond, Plural), ("eunto", PersonThird, Plural)
                      ]

verbFirstConjActIndData :: [((String, VerbPerson, Numerus), VerbMood, VerbVoice, VerbTense)]
verbFirstConjActIndData = zip4 verbFirstConjData (repeat Indicative) (repeat Active) (repeat Present)
verbSecondConjActIndData :: [((String, VerbPerson, Numerus), VerbMood, VerbVoice, VerbTense)]
verbSecondConjActIndData = zip4 verbSecondConjData (repeat Indicative) (repeat Active) (repeat Present)
verbThirdConjActIndData :: [((String, VerbPerson, Numerus), VerbMood, VerbVoice, VerbTense)]
verbThirdConjActIndData = zip4 verbThirdConjData (repeat Indicative) (repeat Active) (repeat Present)
verbThirdIConjActIndData :: [((String, VerbPerson, Numerus), VerbMood, VerbVoice, VerbTense)]
verbThirdIConjActIndData = zip4 verbThirdIConjData (repeat Indicative) (repeat Active) (repeat Present)
verbFourthConjActIndData :: [((String, VerbPerson, Numerus), VerbMood, VerbVoice, VerbTense)]
verbFourthConjActIndData = zip4 verbFourthConjData (repeat Indicative) (repeat Active) (repeat Present)
verbFirstConjPassIndData :: [((String, VerbPerson, Numerus), VerbMood, VerbVoice, VerbTense)]
verbFirstConjPassIndData = zip4 verbFirstPassConjData (repeat Indicative) (repeat Passive) (repeat Present)
verbSecondConjPassIndData :: [((String, VerbPerson, Numerus), VerbMood, VerbVoice, VerbTense)]
verbSecondConjPassIndData = zip4 verbSecondPassConjData (repeat Indicative) (repeat Passive) (repeat Present)
verbThirdConjPassIndData :: [((String, VerbPerson, Numerus), VerbMood, VerbVoice, VerbTense)]
verbThirdConjPassIndData = zip4 verbThirdPassConjData (repeat Indicative) (repeat Passive) (repeat Present)
verbThirdIConjPassIndData :: [((String, VerbPerson, Numerus), VerbMood, VerbVoice, VerbTense)]
verbThirdIConjPassIndData = zip4 verbThirdIPassConjData (repeat Indicative) (repeat Passive) (repeat Present)
verbFourthConjPassIndData :: [((String, VerbPerson, Numerus), VerbMood, VerbVoice, VerbTense)]
verbFourthConjPassIndData = zip4 verbFourthPassConjData (repeat Indicative) (repeat Passive) (repeat Present)

verbFirstConjImperfActIndData :: [((String, VerbPerson, Numerus), VerbMood, VerbVoice, VerbTense)]
verbFirstConjImperfActIndData = zip4 verbFirstConjImperfData (repeat Indicative) (repeat Active) (repeat Imperfect)
verbSecondConjImperfActIndData :: [((String, VerbPerson, Numerus), VerbMood, VerbVoice, VerbTense)]
verbSecondConjImperfActIndData = zip4 verbSecondConjImperfData (repeat Indicative) (repeat Active) (repeat Imperfect)
verbThirdConjImperfActIndData :: [((String, VerbPerson, Numerus), VerbMood, VerbVoice, VerbTense)]
verbThirdConjImperfActIndData = zip4 verbThirdConjImperfData (repeat Indicative) (repeat Active) (repeat Imperfect)
verbThirdIConjImperfActIndData :: [((String, VerbPerson, Numerus), VerbMood, VerbVoice, VerbTense)]
verbThirdIConjImperfActIndData = zip4 verbThirdIConjImperfData (repeat Indicative) (repeat Active) (repeat Imperfect)
verbFourthConjImperfActIndData :: [((String, VerbPerson, Numerus), VerbMood, VerbVoice, VerbTense)]
verbFourthConjImperfActIndData = zip4 verbFourthConjImperfData (repeat Indicative) (repeat Active) (repeat Imperfect)

verbFirstConjImperfPasIndData :: [((String, VerbPerson, Numerus), VerbMood, VerbVoice, VerbTense)]
verbFirstConjImperfPasIndData = zip4 verbFirstConjImperfPasData (repeat Indicative) (repeat Passive) (repeat Imperfect)
verbSecondConjImperfPasIndData :: [((String, VerbPerson, Numerus), VerbMood, VerbVoice, VerbTense)]
verbSecondConjImperfPasIndData = zip4 verbSecondConjImperfPasData (repeat Indicative) (repeat Passive) (repeat Imperfect)
verbThirdConjImperfPasIndData :: [((String, VerbPerson, Numerus), VerbMood, VerbVoice, VerbTense)]
verbThirdConjImperfPasIndData = zip4 verbThirdConjImperfPasData (repeat Indicative) (repeat Passive) (repeat Imperfect)
verbThirdIConjImperfPasIndData :: [((String, VerbPerson, Numerus), VerbMood, VerbVoice, VerbTense)]
verbThirdIConjImperfPasIndData = zip4 verbThirdIConjImperfPasData (repeat Indicative) (repeat Passive) (repeat Imperfect)
verbFourthConjImperfPasIndData :: [((String, VerbPerson, Numerus), VerbMood, VerbVoice, VerbTense)]
verbFourthConjImperfPasIndData = zip4 verbFourthConjImperfPasData (repeat Indicative) (repeat Passive) (repeat Imperfect)

verbFirstConjFutActIndData :: [((String, VerbPerson, Numerus), VerbMood, VerbVoice, VerbTense)]
verbFirstConjFutActIndData = zip4 verbFirstConjFutData (repeat Indicative) (repeat Active) (repeat Future)
verbSecondConjFutActIndData :: [((String, VerbPerson, Numerus), VerbMood, VerbVoice, VerbTense)]
verbSecondConjFutActIndData = zip4 verbSecondConjFutData (repeat Indicative) (repeat Active) (repeat Future)
verbThirdConjFutActIndData :: [((String, VerbPerson, Numerus), VerbMood, VerbVoice, VerbTense)]
verbThirdConjFutActIndData = zip4 verbThirdConjFutData (repeat Indicative) (repeat Active) (repeat Future)
verbThirdIConjFutActIndData :: [((String, VerbPerson, Numerus), VerbMood, VerbVoice, VerbTense)]
verbThirdIConjFutActIndData = zip4 verbThirdIConjFutData (repeat Indicative) (repeat Active) (repeat Future)
verbFourthConjFutActIndData :: [((String, VerbPerson, Numerus), VerbMood, VerbVoice, VerbTense)]
verbFourthConjFutActIndData = zip4 verbFourthConjFutData (repeat Indicative) (repeat Active) (repeat Future)

verbFirstConjFutPasIndData :: [((String, VerbPerson, Numerus), VerbMood, VerbVoice, VerbTense)]
verbFirstConjFutPasIndData = zip4 verbFirstConjFutPasData (repeat Indicative) (repeat Passive) (repeat Future)
verbSecondConjFutPasIndData :: [((String, VerbPerson, Numerus), VerbMood, VerbVoice, VerbTense)]
verbSecondConjFutPasIndData = zip4 verbSecondConjFutPasData (repeat Indicative) (repeat Passive) (repeat Future)
verbThirdConjFutPasIndData :: [((String, VerbPerson, Numerus), VerbMood, VerbVoice, VerbTense)]
verbThirdConjFutPasIndData = zip4 verbThirdConjFutPasData (repeat Indicative) (repeat Passive) (repeat Future)
verbThirdIConjFutPasIndData :: [((String, VerbPerson, Numerus), VerbMood, VerbVoice, VerbTense)]
verbThirdIConjFutPasIndData = zip4 verbThirdIConjFutPasData (repeat Indicative) (repeat Passive) (repeat Future)
verbFourthConjFutPasIndData :: [((String, VerbPerson, Numerus), VerbMood, VerbVoice, VerbTense)]
verbFourthConjFutPasIndData = zip4 verbFourthConjFutPasData (repeat Indicative) (repeat Passive) (repeat Future)

verbFirstConjDepIndData :: [((String, VerbPerson, Numerus), VerbMood, VerbVoice, VerbTense)]
verbFirstConjDepIndData = zip4 verbFirstPassConjData (repeat Indicative) (repeat Active) (repeat Present)
verbSecondConjDepIndData :: [((String, VerbPerson, Numerus), VerbMood, VerbVoice, VerbTense)]
verbSecondConjDepIndData = zip4 verbSecondPassConjData (repeat Indicative) (repeat Active) (repeat Present)
verbThirdConjDepIndData :: [((String, VerbPerson, Numerus), VerbMood, VerbVoice, VerbTense)]
verbThirdConjDepIndData = zip4 verbThirdPassConjData (repeat Indicative) (repeat Active) (repeat Present)
verbThirdIConjDepIndData :: [((String, VerbPerson, Numerus), VerbMood, VerbVoice, VerbTense)]
verbThirdIConjDepIndData = zip4 verbThirdIPassConjData (repeat Indicative) (repeat Active) (repeat Present)
verbFourthConjDepIndData :: [((String, VerbPerson, Numerus), VerbMood, VerbVoice, VerbTense)]
verbFourthConjDepIndData = zip4 verbFourthPassConjData (repeat Indicative) (repeat Active) (repeat Present)

verbFirstConjDepImperfIndData :: [((String, VerbPerson, Numerus), VerbMood, VerbVoice, VerbTense)]
verbFirstConjDepImperfIndData = zip4 verbFirstConjImperfPasData (repeat Indicative) (repeat Active) (repeat Imperfect)
verbSecondConjDepImperfIndData :: [((String, VerbPerson, Numerus), VerbMood, VerbVoice, VerbTense)]
verbSecondConjDepImperfIndData = zip4 verbSecondConjImperfPasData (repeat Indicative) (repeat Active) (repeat Imperfect)
verbThirdConjDepImperfIndData :: [((String, VerbPerson, Numerus), VerbMood, VerbVoice, VerbTense)]
verbThirdConjDepImperfIndData = zip4 verbThirdConjImperfPasData (repeat Indicative) (repeat Active) (repeat Imperfect)
verbThirdIConjDepImperfIndData :: [((String, VerbPerson, Numerus), VerbMood, VerbVoice, VerbTense)]
verbThirdIConjDepImperfIndData = zip4 verbThirdIConjImperfPasData (repeat Indicative) (repeat Active) (repeat Imperfect)
verbFourthConjDepImperfIndData :: [((String, VerbPerson, Numerus), VerbMood, VerbVoice, VerbTense)]
verbFourthConjDepImperfIndData = zip4 verbFourthConjImperfPasData (repeat Indicative) (repeat Active) (repeat Imperfect)

verbFirstConjDepFutIndData :: [((String, VerbPerson, Numerus), VerbMood, VerbVoice, VerbTense)]
verbFirstConjDepFutIndData = zip4 verbFirstConjFutPasData (repeat Indicative) (repeat Active) (repeat Future)
verbSecondConjDepFutIndData :: [((String, VerbPerson, Numerus), VerbMood, VerbVoice, VerbTense)]
verbSecondConjDepFutIndData = zip4 verbSecondConjFutPasData (repeat Indicative) (repeat Active) (repeat Future)
verbThirdConjDepFutIndData :: [((String, VerbPerson, Numerus), VerbMood, VerbVoice, VerbTense)]
verbThirdConjDepFutIndData = zip4 verbThirdConjFutPasData (repeat Indicative) (repeat Active) (repeat Future)
verbThirdIConjDepFutIndData :: [((String, VerbPerson, Numerus), VerbMood, VerbVoice, VerbTense)]
verbThirdIConjDepFutIndData = zip4 verbThirdIConjFutPasData (repeat Indicative) (repeat Active) (repeat Future)
verbFourthConjDepFutIndData :: [((String, VerbPerson, Numerus), VerbMood, VerbVoice, VerbTense)]
verbFourthConjDepFutIndData = zip4 verbFourthConjFutPasData (repeat Indicative) (repeat Active) (repeat Future)

verbPerfectActIndData :: [((String, VerbPerson, Numerus), VerbMood, VerbVoice, VerbTense)]
verbPerfectActIndData = zip4 verbPerfectData (repeat Indicative) (repeat Active) (repeat Perfect)

verbEoConjActIndData :: [((String, VerbPerson, Numerus), VerbMood, VerbVoice, VerbTense)]
verbEoConjActIndData = zip4 verbEoConjData (repeat Indicative) (repeat Active) (repeat Present)
verbEoConjImperfActIndData = zip4 verbEoConjImperfData (repeat Indicative) (repeat Active) (repeat Imperfect)
verbEoConjFutActIndData = zip4 verbEoConjFutData (repeat Indicative) (repeat Active) (repeat Future)
verbEoConjActSubjData = zip4 verbSubjunctiveEoData (repeat Subjunctive) (repeat Active) (repeat Present)
verbFeroConjActIndData :: [((String, VerbPerson, Numerus), VerbMood, VerbVoice, VerbTense)]
verbFeroConjActIndData = zip4 verbFeroConjData (repeat Indicative) (repeat Active) (repeat Present)
verbFeroConjPassIndData :: [((String, VerbPerson, Numerus), VerbMood, VerbVoice, VerbTense)]
verbFeroConjPassIndData = zip4 verbFeroPassConjData (repeat Indicative) (repeat Passive) (repeat Present)

verbPluPerfectActIndData = zip4 verbPluPerfectData (repeat Indicative) (repeat Active) (repeat Pluperfect)
verbFutPerfectActIndData = zip4 verbFutPerfectData (repeat Indicative) (repeat Active) (repeat FuturePerfect)

verbFirstConjActSubjData = zip4 verbSubjunctiveFirstData (repeat Subjunctive) (repeat Active) (repeat Present)
verbSecondConjActSubjData = zip4 verbSubjunctiveSecondData (repeat Subjunctive) (repeat Active) (repeat Present)
verbThirdConjActSubjData = zip4 verbSubjunctiveThirdData (repeat Subjunctive) (repeat Active) (repeat Present)
verbThirdIConjActSubjData = zip4 verbSubjunctiveThirdIData (repeat Subjunctive) (repeat Active) (repeat Present)
verbFourthConjActSubjData = zip4 verbSubjunctiveFourthData (repeat Subjunctive) (repeat Active) (repeat Present)

verbFirstConjPasSubjData = zip4 verbSubjunctiveFirstPasData (repeat Subjunctive) (repeat Passive) (repeat Present)
verbSecondConjPasSubjData = zip4 verbSubjunctiveSecondPasData (repeat Subjunctive) (repeat Passive) (repeat Present)
verbThirdConjPasSubjData = zip4 verbSubjunctiveThirdPasData (repeat Subjunctive) (repeat Passive) (repeat Present)
verbThirdIConjPasSubjData = zip4 verbSubjunctiveThirdIPasData (repeat Subjunctive) (repeat Passive) (repeat Present)
verbFourthConjPasSubjData = zip4 verbSubjunctiveFourthPasData (repeat Subjunctive) (repeat Passive) (repeat Present)

verbFirstConjDepSubjData = zip4 verbSubjunctiveFirstPasData (repeat Subjunctive) (repeat Active) (repeat Present)
verbSecondConjDepSubjData = zip4 verbSubjunctiveSecondPasData (repeat Subjunctive) (repeat Active) (repeat Present)
verbThirdConjDepSubjData = zip4 verbSubjunctiveThirdPasData (repeat Subjunctive) (repeat Active) (repeat Present)
verbThirdIConjDepSubjData = zip4 verbSubjunctiveThirdIPasData (repeat Subjunctive) (repeat Active) (repeat Present)
verbFourthConjDepSubjData = zip4 verbSubjunctiveFourthPasData (repeat Subjunctive) (repeat Active) (repeat Present)

verbAllConjActSubjImperfData = zip4 verbSubjunctiveStubData (repeat Subjunctive) (repeat Active) (repeat Imperfect)
verbAllConjPasSubjImperfData = zip4 verbSubjunctiveImperfPasStubData (repeat Subjunctive) (repeat Passive) (repeat Imperfect)
verbAllConjDepSubjImperfData = zip4 verbSubjunctiveImperfPasStubData (repeat Subjunctive) (repeat Active) (repeat Imperfect)
verbAllConjActSubjPerfData = zip4 verbSubjunctivePerfectData (repeat Subjunctive) (repeat Active) (repeat Perfect)
verbAllConjActSubjPluPerfData = zip4 verbSubjunctivePluPerfectData (repeat Subjunctive) (repeat Active) (repeat Pluperfect)

verbSumConjActIndData = zip4 verbSumPresIndEnds (repeat Indicative) (repeat Active) (repeat Present)
verbSumConjImperfActIndData = zip4 verbSumImperfIndEnds (repeat Indicative) (repeat Active) (repeat Imperfect)
verbSumConjFutActIndData = zip4 verbSumFutIndEnds (repeat Indicative) (repeat Active) (repeat Future)
verbSumConjActSubjData = zip4 verbSumPresSubjEnds (repeat Subjunctive) (repeat Active) (repeat Present)

verbFirstConjImpPresData = zip4 verbImpPresFirstEnds (repeat Imperative) (repeat Active) (repeat Present)
verbSecondConjImpPresData = zip4 verbImpPresSecondEnds (repeat Imperative) (repeat Active) (repeat Present)
verbThirdConjImpPresData = zip4 verbImpPresThirdEnds (repeat Imperative) (repeat Active) (repeat Present)
verbThirdIConjImpPresData = zip4 verbImpPresThirdIEnds (repeat Imperative) (repeat Active) (repeat Present)
verbFourthConjImpPresData = zip4 verbImpPresFourthEnds (repeat Imperative) (repeat Active) (repeat Present)

verbFirstConjImpFutData = zip4 verbImpFutFirstEnds (repeat Imperative) (repeat Active) (repeat Future)
verbSecondConjImpFutData = zip4 verbImpFutSecondEnds (repeat Imperative) (repeat Active) (repeat Future)
verbThirdConjImpFutData = zip4 verbImpFutThirdEnds (repeat Imperative) (repeat Active) (repeat Future)
verbThirdIConjImpFutData = zip4 verbImpFutThirdIEnds (repeat Imperative) (repeat Active) (repeat Future)
verbFourthConjImpFutData = zip4 verbImpFutFourthEnds (repeat Imperative) (repeat Active) (repeat Future)

verbFeroConjImpPresData = zip4 verbImpPresFeroEnds (repeat Imperative) (repeat Active) (repeat Present)
verbFeroConjImpFutData = zip4 verbImpFutFeroEnds (repeat Imperative) (repeat Active) (repeat Future)
verbSumConjImpPresData = zip4 verbImpPresSumEnds (repeat Imperative) (repeat Active) (repeat Present)
verbSumConjImpFutData = zip4 verbImpFutSumEnds (repeat Imperative) (repeat Active) (repeat Future)
verbEoConjImpPresData = zip4 verbImpPresEoEnds (repeat Imperative) (repeat Active) (repeat Present)
verbEoConjImpFutData = zip4 verbImpFutEoEnds (repeat Imperative) (repeat Active) (repeat Future)

-- TODO: Eo has passive
conjugate :: Verb -> [Verb]
conjugate verb = case (conjugation verb, verbClass verb) of
                 (ConjFirst, Ordinary) -> concatConjs [verbFirstConjActIndData, verbFirstConjPassIndData, verbFirstConjImperfActIndData, verbFirstConjImperfPasIndData, verbFirstConjFutActIndData, verbFirstConjFutPasIndData, verbPerfectActIndData, verbPluPerfectActIndData, verbFutPerfectActIndData, verbFirstConjActSubjData, verbFirstConjPasSubjData, verbAllConjActSubjImperfData, verbAllConjPasSubjImperfData, verbAllConjActSubjPerfData, verbAllConjActSubjPluPerfData, verbFirstConjImpPresData, verbFirstConjImpFutData]
                 (ConjSecond, Ordinary) -> concatConjs [verbSecondConjActIndData, verbSecondConjPassIndData, verbSecondConjImperfActIndData, verbSecondConjImperfPasIndData, verbSecondConjFutActIndData, verbSecondConjFutPasIndData, verbPerfectActIndData, verbPluPerfectActIndData, verbFutPerfectActIndData, verbSecondConjActSubjData, verbSecondConjPasSubjData, verbAllConjActSubjImperfData, verbAllConjPasSubjImperfData, verbAllConjActSubjPerfData, verbAllConjActSubjPluPerfData, verbSecondConjImpPresData, verbSecondConjImpFutData]
                 (ConjThird, Ordinary) -> concatConjs [verbThirdConjActIndData, verbThirdConjPassIndData, verbThirdConjImperfActIndData, verbThirdConjImperfPasIndData, verbThirdConjFutActIndData, verbThirdConjFutPasIndData, verbPerfectActIndData, verbPluPerfectActIndData, verbFutPerfectActIndData, verbThirdConjActSubjData, verbThirdConjPasSubjData, verbAllConjActSubjImperfData, verbAllConjPasSubjImperfData, verbAllConjActSubjPerfData, verbAllConjActSubjPluPerfData, verbThirdConjImpPresData, verbThirdConjImpFutData]
                 (ConjThirdI, Ordinary) -> concatConjs [verbThirdIConjActIndData, verbThirdIConjPassIndData, verbThirdIConjImperfActIndData, verbThirdIConjImperfPasIndData, verbThirdIConjFutActIndData, verbThirdIConjFutPasIndData, verbPerfectActIndData, verbPluPerfectActIndData, verbFutPerfectActIndData, verbThirdIConjActSubjData, verbThirdIConjPasSubjData, verbAllConjActSubjImperfData, verbAllConjPasSubjImperfData, verbAllConjActSubjPerfData, verbAllConjActSubjPluPerfData, verbThirdIConjImpPresData, verbThirdIConjImpFutData]
                 (ConjFourth, Ordinary) -> concatConjs [verbFourthConjActIndData, verbFourthConjPassIndData, verbFourthConjImperfActIndData, verbFourthConjImperfPasIndData, verbFourthConjFutActIndData, verbFourthConjFutPasIndData, verbPerfectActIndData, verbPluPerfectActIndData, verbFutPerfectActIndData, verbFourthConjActSubjData, verbFourthConjPasSubjData, verbAllConjActSubjImperfData, verbAllConjPasSubjImperfData, verbAllConjActSubjPerfData, verbAllConjActSubjPluPerfData, verbFourthConjImpPresData, verbFourthConjImpFutData]
                 (ConjFirst, OrdinaryNoPass) -> concatConjs [verbFirstConjActIndData, verbFirstConjImperfActIndData, verbFirstConjFutActIndData, verbPerfectActIndData, verbPluPerfectActIndData, verbFutPerfectActIndData, verbFirstConjActSubjData, verbAllConjActSubjImperfData, verbAllConjActSubjPerfData, verbAllConjActSubjPluPerfData, verbFirstConjImpPresData, verbFirstConjImpFutData]
                 (ConjSecond, OrdinaryNoPass) -> concatConjs [verbSecondConjActIndData, verbSecondConjImperfActIndData, verbSecondConjFutActIndData, verbPerfectActIndData, verbPluPerfectActIndData, verbFutPerfectActIndData, verbSecondConjActSubjData, verbAllConjActSubjImperfData, verbAllConjActSubjPerfData, verbAllConjActSubjPluPerfData, verbSecondConjImpPresData, verbSecondConjImpFutData]
                 (ConjThird, OrdinaryNoPass) -> concatConjs [verbThirdConjActIndData, verbThirdConjImperfActIndData, verbThirdConjFutActIndData, verbPerfectActIndData, verbPluPerfectActIndData, verbFutPerfectActIndData, verbThirdConjActSubjData, verbAllConjActSubjImperfData, verbAllConjActSubjPerfData, verbAllConjActSubjPluPerfData, verbThirdConjImpPresData, verbThirdConjImpFutData]
                 (ConjThirdI, OrdinaryNoPass) -> concatConjs [verbThirdIConjActIndData, verbThirdIConjImperfActIndData, verbThirdIConjFutActIndData, verbPerfectActIndData, verbPluPerfectActIndData, verbFutPerfectActIndData, verbThirdIConjActSubjData, verbAllConjActSubjImperfData, verbAllConjActSubjPerfData, verbAllConjActSubjPluPerfData, verbThirdIConjImpPresData, verbThirdIConjImpFutData]
                 (ConjFourth, OrdinaryNoPass) -> concatConjs [verbFourthConjActIndData, verbFourthConjImperfActIndData, verbFourthConjFutActIndData, verbPerfectActIndData, verbPluPerfectActIndData, verbFutPerfectActIndData, verbFourthConjActSubjData, verbAllConjActSubjImperfData, verbAllConjActSubjPerfData, verbAllConjActSubjPluPerfData, verbFourthConjImpPresData, verbFourthConjImpFutData]
                 (ConjFirst, SemiDeponent) -> concatConjs [verbFirstConjActIndData, verbFirstConjImperfActIndData, verbFirstConjFutActIndData, verbFirstConjActSubjData, verbAllConjActSubjImperfData, verbFirstConjImpPresData, verbFirstConjImpFutData]
                 (ConjSecond, SemiDeponent) -> concatConjs [verbSecondConjActIndData, verbSecondConjImperfActIndData, verbSecondConjFutActIndData, verbSecondConjActSubjData, verbAllConjActSubjImperfData, verbSecondConjImpPresData, verbSecondConjImpFutData]
                 (ConjThird, SemiDeponent) -> concatConjs [verbThirdConjActIndData, verbThirdConjImperfActIndData, verbThirdConjFutActIndData, verbThirdConjActSubjData, verbAllConjActSubjImperfData, verbThirdConjImpPresData, verbThirdConjImpFutData]
                 (ConjThirdI, SemiDeponent) -> concatConjs [verbThirdIConjActIndData, verbThirdIConjImperfActIndData, verbThirdIConjFutActIndData, verbThirdIConjActSubjData, verbAllConjActSubjImperfData, verbThirdIConjImpPresData, verbThirdIConjImpFutData]
                 (ConjFourth, SemiDeponent) -> concatConjs [verbFourthConjActIndData, verbFourthConjImperfActIndData, verbFourthConjFutActIndData, verbFourthConjActSubjData, verbAllConjActSubjImperfData, verbFourthConjImpPresData, verbFourthConjImpFutData]
                 (ConjFirst, Deponent) -> concatConjs [verbFirstConjDepIndData, verbFirstConjDepImperfIndData, verbFirstConjDepFutIndData, verbFirstConjDepSubjData, verbAllConjDepSubjImperfData]
                 (ConjSecond, Deponent) -> concatConjs [verbSecondConjDepIndData, verbSecondConjDepImperfIndData, verbSecondConjDepFutIndData, verbSecondConjDepSubjData, verbAllConjDepSubjImperfData]
                 (ConjThird, Deponent) -> concatConjs [verbThirdConjDepIndData, verbThirdConjDepImperfIndData, verbThirdConjDepFutIndData, verbThirdConjDepSubjData, verbAllConjDepSubjImperfData]
                 (ConjThirdI, Deponent) -> concatConjs [verbThirdIConjDepIndData, verbThirdIConjDepImperfIndData, verbThirdIConjDepFutIndData, verbThirdIConjDepSubjData, verbAllConjDepSubjImperfData]
                 (ConjFourth, Deponent) -> concatConjs [verbFourthConjDepIndData, verbFourthConjDepImperfIndData, verbFourthConjDepFutIndData, verbFourthConjDepSubjData, verbAllConjDepSubjImperfData]
                 (ConjEo, Ordinary) -> concatConjs [verbEoConjActIndData, verbEoConjFutActIndData, verbEoConjImperfActIndData, verbPerfectActIndData, verbPluPerfectActIndData, verbFutPerfectActIndData, verbEoConjActSubjData, verbAllConjActSubjImperfData, verbAllConjActSubjPerfData, verbAllConjActSubjPluPerfData, verbEoConjImpPresData, verbEoConjImpFutData]
                 (ConjFero, Ordinary) -> concatConjs [verbFeroConjActIndData, verbFeroConjPassIndData, verbFourthConjImperfActIndData, verbFourthConjImperfPasIndData, verbFourthConjFutActIndData, verbFourthConjFutPasIndData, verbPerfectActIndData, verbPluPerfectActIndData, verbFutPerfectActIndData, verbThirdConjActSubjData, verbThirdConjPasSubjData, verbAllConjActSubjImperfData, verbAllConjPasSubjImperfData, verbAllConjActSubjPerfData, verbAllConjActSubjPluPerfData, verbFeroConjImpPresData, verbFeroConjImpFutData]
                 (ConjFio, SemiDeponent) -> concatConjs [verbThirdIConjActIndData, verbThirdIConjFutActIndData, verbThirdIConjImperfActIndData, verbThirdIConjActSubjData, verbAllConjActSubjImperfData]
                 (ConjSum, _) -> concatConjs [verbSumConjActIndData, verbSumConjFutActIndData, verbSumConjImperfActIndData, verbPerfectActIndData, verbPluPerfectActIndData, verbFutPerfectActIndData, verbSumConjActSubjData, verbAllConjActSubjImperfData, verbAllConjActSubjPerfData, verbAllConjActSubjPluPerfData, verbSumConjImpPresData, verbSumConjImpFutData]
                 -- TODO: fix memini
                 _ -> error $ "error for " ++ princ1 verb
                 where createWord (d, m, v, t) = verb {verbEnding = first d, person = second d,
                                                    verbNumber = third d, mood = m, voice = v, tense = t}
                       concatConjs = concatMap (map createWord)

green, yellow, magenta, cyan, red, blue, italic :: SGR
green = SetColor Foreground Vivid Green
yellow = SetColor Foreground Vivid Yellow
magenta = SetColor Foreground Vivid Magenta
cyan = SetColor Foreground Vivid Cyan
red = SetColor Foreground Vivid Red
blue = SetColor Foreground Vivid Blue
italic = SetItalicized True

printC :: Maybe Handle -> SGR -> String -> IO ()
printC (Just h) sgr s = hPutStr h s
printC Nothing sgr s  = setSGR [sgr] >> putStr s >> setSGR []

printDictC :: Maybe Handle -> Color -> String -> IO ()
printDictC (Just h) col s = hPutStr h s
printDictC Nothing col s = setSGR [c, italic] >> putStr s >> setSGR [] where c = SetColor Foreground Vivid col

forceFind :: Casus -> Numerus -> [Verbum] -> Verbum
forceFind k1 k2 l = forceMaybe $ find (\w -> casus w == k1 && number w == k2) l

forceFindAdj :: Casus -> Numerus -> [Adjective] -> Adjective
forceFindAdj k1 k2 l = forceMaybe $ find (\adj -> adjCasus adj == k1 && adjNumber adj == k2) l

dummyVerb = Verb {presentStem = "-", verbEnding = "", tense = Present, mood = Imperative}

forceFindVerb :: VerbPerson -> Numerus -> [Verb] -> Verb
forceFindVerb k1 k2 l = fromMaybe dummyVerb $ find (\verb -> person verb == k1 && verbNumber verb == k2) l

printDeclination :: Maybe Handle -> [Verbum] -> IO ()
printDeclination op vs = do
    let hasLoc = hasLocative $ head vs
    printC op red "\t\tSingular\tPlural\n"
    printC op green "Nominative:\t"
    masterPrintLn op $ show (forceFind Nominative Singular vs) ++ "\t\t" ++ show (forceFind Nominative Plural vs)
    printC op green "Genitive:\t"
    masterPrintLn op $ show (forceFind Genitive Singular vs) ++ "\t\t" ++ show (forceFind Genitive Plural vs)
    printC op green "Accusative\t"
    masterPrintLn op $ show (forceFind Accusative Singular vs) ++ "\t\t" ++ show (forceFind Accusative Plural vs)
    printC op green "Ablative:\t"
    masterPrintLn op $ show (forceFind Ablative Singular vs) ++ "\t\t" ++ show (forceFind Ablative Plural vs)
    printC op green "Dative:\t\t"
    masterPrintLn op $ show (forceFind Dative Singular vs) ++ "\t\t" ++ show (forceFind Dative Plural vs)
    when hasLoc $ do
        printC op green "Locative:\t"
        masterPrintLn op $ show (forceFind Locative Singular vs) ++ "\t\t" ++ show (forceFind Locative Plural vs)
    printC op green "Vocative:\t"
    masterPrintLn op $ show (forceFind Vocative Singular vs) ++ "\t\t" ++ show (forceFind Vocative Plural vs)

printAdjDeclination :: Maybe Handle -> [Adjective] -> IO ()
printAdjDeclination op adjs = do
    let positives = filter (\adj -> adjDegree adj == Positive) adjs
    let comparatives = filter (\adj -> adjDegree adj == Comparative) adjs
    let superlatives = filter (\adj -> adjDegree adj == Superlative) adjs
    let degrees = [positives, comparatives, superlatives]
    forM_ (filter (not.null) degrees) $ \ds -> do
        let mascs = filter (\adj -> adjGender adj == Masculine) ds
        let fems = filter (\adj -> adjGender adj == Feminine) ds
        let neuts = filter (\adj -> adjGender adj == Neuter) ds
        printC op red $ show (adjDegree (head ds)) ++ "\n"
        masterPrintLn op "\t\tSingular\tPlural"
        let casi = [Nominative, Genitive, Accusative, Ablative, Dative, Vocative]
        printC op blue "\t\tMasculine\n"
        forM_ casi $ \c -> do
            let padding = if c == Dative then "\t" else ""
            printC op green $ show c ++ ":\t" ++ padding
            masterPrintLn op $ show (forceFindAdj c Singular mascs) ++ "\t\t" ++ show (forceFindAdj c Plural mascs)
        printC op magenta "\t\tFeminine\n"
        forM_ casi $ \c -> do
            let padding = if c == Dative then "\t" else ""
            printC op green $ show c ++ ":\t" ++ padding
            masterPrintLn op $ show (forceFindAdj c Singular fems) ++ "\t\t" ++ show (forceFindAdj c Plural fems)
        printC op cyan "\t\tNeuter\n"
        forM_ casi $ \c -> do
            let padding = if c == Dative then "\t" else ""
            printC op green $ show c ++ ":\t" ++ padding
            masterPrintLn op $ show (forceFindAdj c Singular neuts) ++ "\t\t" ++ show (forceFindAdj c Plural neuts)

printConjugation :: Maybe Handle -> [Verb] -> IO ()
printConjugation op verbs = do
    let conj = conjugation (head verbs)
    let classi = verbClass (head verbs)
    let defn = defectiveness (head verbs)
    masterPrintLn op $ intercalate ", " $ filter (not.null) [show conj, show classi, show defn]
    let indicatives = filter (\verb -> mood verb == Indicative) verbs
    let subjunctives = filter (\verb -> mood verb == Subjunctive) verbs
    let imperatives = filter (\verb -> mood verb == Imperative) verbs
    let moods = filter (not.null) [indicatives, subjunctives, imperatives]
    forM_ moods $ \modus -> do
        let currMood = mood (head modus)
        printC op cyan $ show currMood ++ "\n"
        masterPrintLn op "\t\tSingular\t\tPlural"
        printC op green "\tFirst\tSecond\tThird\tFirst\tSecond\tThird\n"
        let actives = filter (\verb -> voice verb == Active) modus
        let passives = filter (\verb -> voice verb == Passive) modus
        let voices = filter (not.null) [actives, passives]
        forM_ voices $ \voces -> do
            let currVoic = voice (head voces)
            printC op yellow $ show currVoic ++ "\n"
            let presents = filter (\verb -> tense verb == Present) voces
            let perfects = filter (\verb -> tense verb == Perfect) voces
            let imperfects = filter (\verb -> tense verb == Imperfect) voces
            let futures = filter (\verb -> tense verb == Future) voces
            let pluperfects = filter (\verb -> tense verb == Pluperfect) voces
            let futureperfects = filter (\verb -> tense verb == FuturePerfect) voces
            let tenses = filter (not.null) [presents, futures, imperfects, perfects, futureperfects, pluperfects]
            forM_ tenses $ \tempora -> do
                let currTemp = tense (head tempora)
                let personsNums = [(PersonFirst, Singular), (PersonSecond, Singular), (PersonThird, Singular), (PersonFirst, Plural), (PersonSecond, Plural), (PersonThird, Plural)]
                let fetched = map (\(p,n) -> show (forceFindVerb p n tempora)) personsNums
                printC op red $ show currTemp ++ " "
                masterPrintLn op $ intercalate ",\t" fetched
    printC op cyan "Infinitives\t"
    let actInfs = filter(\(_,t) -> t `elem` [IPresentActive, IPerfectActive, IFutureActive]) $ infinitives (head verbs)
    let pasInfs = infinitives (head verbs) \\ actInfs
    case [(not.null) actInfs, (not.null) pasInfs] of
        [True, True]  -> masterPrintLn op "active\t\tpassive"
        [True, False] -> masterPrintLn op "active"
        [False, True] -> masterPrintLn op "passive"
        _             -> masterPrintLn op ""
    forM_ (map getInfTense (actInfs++pasInfs)) $ \s -> printC op green (s ++ " ")
    masterPrintLn op ""
    masterPrintLn op $ intercalate ", " (map fst (actInfs ++ pasInfs))
    let perfParts = filter (\(_,t) -> t == PerfectPassive || t == PerfectActive) $ participles (head verbs)
    let presParts = filter (\(_,t) -> t == PresentActive) $ participles (head verbs)
    let futPasParts = filter (\(_,t) -> t == FuturePassive) $ participles (head verbs)
    let futActParts = filter (\(_,t) -> t == FutureActive) $ participles (head verbs)
    when (any (not.null) [perfParts, presParts, futPasParts, futActParts]) $ printC op cyan "Participles\n"
    unless (null perfParts) $ do
        printC op green $ show (snd.head $ perfParts) ++ " "
        masterPrintLn op $ adjDictEntry (fst.head $ perfParts)
    unless (null presParts) $ do
        printC op green $ show (snd.head $ presParts) ++ " "
        masterPrintLn op $ adjDictEntry (fst.head $ presParts)
    unless (null futPasParts) $ do
        printC op green $ show (snd.head $ futPasParts) ++ " "
        masterPrintLn op $ adjDictEntry (fst.head $ futPasParts)
    unless (null futActParts) $ do
        printC op green $ show (snd.head $ futActParts) ++ " "
        masterPrintLn op $ adjDictEntry (fst.head $ futActParts)
    where getInfTense (_,t) = head.words.show $ t

fromString :: String -> String -> String -> String -> String -> Verbum
fromString nomsing declension gender stem hasLoc = Word {nomSing = nomsing, stem = stem, ending = "",
                                         gender = read gender, declension = read declension,
                                         casus = Nominative, number = Singular, hasLocative = read hasLoc}

adjFromString :: String -> String -> String -> String -> String -> String -> String -> Adjective
adjFromString masc fem neut decl stem compMasc superlatStem =
        Adjective {mascSing = masc, femSing = fem, neutSing = neut,
                   adjDeclension = read decl, adjStem = stem, adjEnding = "",
                   adjCasus = Nominative, adjGender = Masculine, adjDegree = Positive,
                   adjNumber = Singular, compMasc = compMasc, superlatStem = superlatStem,
                   compNeut = take (length compMasc - 2) compMasc ++ "us"}

verbFromString :: String -> String -> String -> String -> String -> String -> String -> String -> String -> String -> Verb
verbFromString princ1 princ2 princ3 princ4 verbClassA defness conj presStem perfStem partStem =
    Verb {princ1 = princ1, princ2 = princ2,
          princ3 = princ3, verbEnding = "",
          princ4 = princ4, conjugation = redConj,
          verbClass = redClass, defectiveness = read defness,
          presentStem = presStem, perfectStem = perfStem, partcpStem = partStem,
          person = PersonFirst, mood = Indicative, tense = Present, verbNumber = Singular, voice = Active,
          participles = perfParts ++ presParts ++ futPasParts ++ futActParts, fakePrinc2 = if redClass == Deponent then constrFakePrinc2 else princ2,
          infinitives = inftives}
    where redClass = read verbClassA
          redConj = read conj
          takeMinusN n s = take (length s - n) s
          constrFakePrinc2 | redConj == ConjThird = init princ2 ++ "ere"
                           | redConj == ConjThirdI = init princ2 ++ "ere"
                           | otherwise = takeMinusN 2 princ2 ++ "re"
          presPartDecl = case redConj of
                         ConjFirst -> AdjPresentParticipleFirst
                         ConjSecond -> AdjPresentParticipleSecond
                         ConjThird -> AdjPresentParticipleThird
                         ConjThirdI -> AdjPresentParticipleThirdI
                         ConjFourth -> AdjPresentParticipleFourth
                         ConjFero -> AdjPresentParticipleThird
                         ConjEo -> AdjPresentParticipleEo
                         _ -> error "unimplemented present participle"
          thematicVowel = case redConj of
                          ConjFirst  -> "a"
                          ConjSecond -> ""
                          ConjThird  -> "e"
                          ConjThirdI -> "ie"
                          ConjFourth -> "e"
                          ConjEo     -> ""
                          ConjFero   -> "e"
                          ConjFio    -> ""
                          ConjSum    -> ""
          futPasPartDecl = case redConj of
                           ConjFirst -> AdjFutPasParticipleFirst
                           ConjSecond -> AdjFutPasParticipleSecond
                           ConjThird -> AdjFutPasParticipleThird
                           ConjThirdI -> AdjFutPasParticipleThirdI
                           ConjFourth -> AdjFutPasParticipleFourth
                           ConjFero -> AdjFutPasParticipleThird
                           ConjEo -> AdjFutPasParticipleEo
                           _ -> error "unimplemented future passive participle"
          perfPartPrototype = Adjective {mascSing = partStem ++ "us", femSing = partStem ++ "a", neutSing = partStem ++ "um",
                                         adjDeclension = AdjPositiveOnly, adjStem = partStem, adjEnding = "",
                                         adjCasus = Nominative, adjGender = Masculine, adjDegree = Positive,
                                         adjNumber = Singular, compMasc = "", superlatStem = "", compNeut = ""}
          perfParts | redConj == ConjSum = []
                    | otherwise = zip (declineAdj perfPartPrototype) (repeat (if redClass `notElem` [Deponent, SemiDeponent] then PerfectPassive else PerfectActive))
          presActPrototype  | redConj == ConjEo = Adjective {mascSing = presStem ++ "iens", femSing = presStem ++ "iens",
                                                    neutSing = presStem ++ "iens",
                                                    adjDeclension = presPartDecl, adjStem = presStem,
                                                    adjEnding = "", adjCasus = Nominative, adjGender = Masculine, adjDegree = Positive,
                                                    adjNumber = Singular, compMasc = "", superlatStem = "", compNeut = ""}
                            | otherwise = Adjective {mascSing = presStem ++ thematicVowel ++ "ns", femSing = presStem ++ thematicVowel ++ "ns",
                                                neutSing = presStem ++ thematicVowel ++ "ns",
                                                adjDeclension = presPartDecl, adjStem = presStem,
                                                adjEnding = "", adjCasus = Nominative, adjGender = Masculine, adjDegree = Positive,
                                                adjNumber = Singular, compMasc = "", superlatStem = "", compNeut = ""}
          presParts | redConj == ConjFio = []
                    | redConj == ConjSum = []
                    | redConj == ConjPossum = []
                    | otherwise = zip (declineAdj presActPrototype) (repeat PresentActive)
          futPasPrototype | redConj == ConjEo = Adjective {mascSing = presStem ++ "eundus", femSing = presStem ++ "eunda", neutSing = presStem ++ "eundum",
                                                            adjDeclension = futPasPartDecl, adjStem = presStem,
                                                            adjEnding = "", adjCasus = Nominative, adjGender = Masculine, adjDegree = Positive,
                                                            adjNumber = Singular, compMasc = "", superlatStem = "", compNeut = ""}
                          | otherwise = Adjective {mascSing = presStem ++ thematicVowel ++ "ndus", femSing = presStem ++ thematicVowel ++ "nda",
                                                            neutSing = presStem ++ thematicVowel ++ "ndum",
                                                            adjDeclension = futPasPartDecl, adjStem = presStem,
                                                            adjEnding = "", adjCasus = Nominative, adjGender = Masculine, adjDegree = Positive,
                                                            adjNumber = Singular, compMasc = "", superlatStem = "", compNeut = ""}
          futPasParts   | redConj == ConjFio = []
                        | redConj == ConjSum = []
                        | redConj == ConjPossum = []
                        | otherwise = zip (declineAdj futPasPrototype) (repeat FuturePassive)
          futActPrototype | redConj == ConjSum = Adjective {mascSing = princ4, femSing = takeMinusN 2 princ4 ++ "a", neutSing = takeMinusN 2 princ4 ++ "um",
                                                            adjDeclension = AdjFutActParticiple, adjStem = takeMinusN 2 princ4, adjEnding = "", adjCasus = Nominative,
                                                            adjGender = Masculine, adjDegree = Positive, adjNumber = Singular, compMasc = "", superlatStem = "", compNeut = ""}
                          | otherwise = Adjective {mascSing = partStem ++ "urus", femSing = partStem ++ "ura", neutSing = partStem ++ "urum",
                                                            adjDeclension = AdjFutActParticiple, adjStem = partStem, adjEnding = "", adjCasus = Nominative,
                                                            adjGender = Masculine, adjDegree = Positive, adjNumber = Singular, compMasc = "", superlatStem = "", compNeut = ""}
          futActParts | redConj == ConjFio = []
                      | redConj == ConjPossum = []
                      | otherwise = zip (declineAdj futActPrototype) (repeat FutureActive)
          inftives | redClass == Deponent = zip [princ2, mascSing perfPartPrototype++" esse", mascSing futActPrototype++" esse"] inftypes2
                   | redConj == ConjThird || redConj == ConjThirdI = zip [princ2, perfStem++"isse", mascSing futActPrototype++" esse", presStem++"i", mascSing perfPartPrototype++" esse", neutSing perfPartPrototype++" iri"] inftypes
                   | otherwise = zip [princ2, perfStem++"isse", mascSing futActPrototype++" esse", takeMinusN 1 princ2 ++ "i", mascSing perfPartPrototype++" esse", neutSing perfPartPrototype++" iri"] inftypes
          inftypes = [IPresentActive, IPerfectActive, IFutureActive, IPresentPassive, IPerfectPassive, IFuturePassive]
          inftypes2 = [IPresentActive, IPerfectActive, IFutureActive]

wordMapper :: String -> String -> String -> String -> String -> String -> (Verbum, String)
wordMapper ns d g s hl t = (fromString ns d g s hl, t)

adjMapper :: String -> String -> String -> String -> String -> String -> String -> String -> (Adjective, String)
adjMapper m f n d s cmp sprl t = (adjFromString m f n d s cmp sprl, t)

verbMapper :: String -> String -> String -> String -> String -> String -> String -> String -> String -> String -> String -> (Verb, String)
verbMapper p1 p2 p3 p4 c dfn conj pst pfst pcst t = (verbFromString p1 p2 p3 p4 c dfn conj pst pfst pcst, t)

forceLookup :: Eq a => a -> [(a, b)] -> b
forceLookup k = forceMaybe . (lookup k)

errorFor :: String -> IO ()
errorFor s = do
    hPutStrLn stderr $ "latin: no results for " ++ s

replaceFstI :: String -> String
replaceFstI s = case elemIndex 'i' s of
  Just x -> map (snd.replF) (zip [0..] s)
    where replF (i,c) = if (i == x) then (i,'j') else (i,c)
  Nothing -> s

openTmp :: String -> IO (Maybe Handle)
openTmp ""   = return Nothing
openTmp path = Just <$> openFile path WriteMode

masterPrint :: Maybe Handle -> String -> IO ()
masterPrint Nothing  = putStr
masterPrint (Just h) = hPutStr h

masterPrintLn :: Maybe Handle -> String -> IO ()
masterPrintLn Nothing  = putStrLn
masterPrintLn (Just h) = hPutStrLn h

closeTmp :: Maybe Handle -> IO ()
closeTmp Nothing  = return ()
closeTmp (Just h) = hClose h

runApp :: CmdLine -> IO ()
runApp cmd = do
    cfgFile <- (</>) <$> getHomeDirectory <*> return ".latin.conf"
    cfgExists <- doesFileExist cfgFile
    dbName <- case cfgExists of
                  True  -> strip <$> readFile cfgFile
                  False -> return "latin.db"
    when ("-" `elem` (query cmd)) $ error "invalid query"
    let wordQuery' = strip.head $ query cmd
    let (wordQuery, wordQuery2) = case (caseInv cmd, itoj cmd) of
          (True, True) -> (map toLower wordQuery', replaceFstI (map toLower wordQuery'))
          (True, False) -> (map toLower wordQuery', map toLower wordQuery')
          (False, True) -> (wordQuery', replaceFstI wordQuery')
          (False, False) -> (wordQuery', wordQuery')
    let col = color cmd
    let lnBreak = if html cmd then "<br>" else ""
    dbExists <- doesFileExist dbName
    unless dbExists $ error ("database file " ++ dbName ++ " doesn't exist.")
    outputT <- openTmp $ output cmd
    handle <- openReadonlyConnection dbName
    when (translate cmd) $ findTranslation col handle wordQuery
    res1 <- execStatement handle $ printf "select * from nounTable where word in ('%s','%s');" wordQuery wordQuery2
    adjRes <- execStatement handle $ printf "select * from adjectiveTable where mascSing in ('%s','%s') or femSing in ('%s','%s') or neutSing in ('%s','%s');" wordQuery wordQuery2 wordQuery wordQuery2 wordQuery wordQuery2
    verbRes <- execStatement handle $ printf "select * from verbTable where princ1 in ('%s','%s') or princ2 in ('%s','%s') or princ3 in ('%s','%s') or princ4 in ('%s','%s');" wordQuery wordQuery2 wordQuery wordQuery2 wordQuery wordQuery2 wordQuery wordQuery2
    indRes <- execStatement handle $ printf "select * from indeclinableTable where form in ('%s','%s');" wordQuery wordQuery2
    advRes <- execStatement handle $ printf "select * from adverbTable where positive in ('%s','%s') or comparative in ('%s','%s') or superlative in ('%s','%s');" wordQuery wordQuery2 wordQuery wordQuery2 wordQuery wordQuery2
    let resultList = case res1 of
                        Left s  -> error s
                        Right l -> head l
    let adjResultList = case adjRes of
                         Left s  -> error s
                         Right l -> head l
    let verbResultList = case verbRes of
                         Left s  -> error s
                         Right l -> head l
    let indResultList = case indRes of
                         Left s  -> error s
                         Right l -> head l
    let advResultList = case advRes of
                         Left s  -> error s
                         Right l -> head l
    let mapped = map (\row -> wordMapper (forceLookup "word" row) (forceLookup "declension" row)
                    (forceLookup "gender" row) (forceLookup "stem" row) (forceLookup "hasLocative" row) (forceLookup "trans" row)) resultList
    let mappedAdj = map (\row -> adjMapper (forceLookup "mascSing" row) (forceLookup "femSing" row)
                    (forceLookup "neutSing" row) (forceLookup "declension" row) (forceLookup "stem" row)
                    (forceLookup "compMasc" row) (forceLookup "superlatStem" row) (forceLookup "trans" row))  adjResultList
    let mappedVerbs = map (\row -> verbMapper (forceLookup "princ1" row) (forceLookup "princ2" row)
                    (forceLookup "princ3" row) (forceLookup "princ4" row) (forceLookup "verbClass" row) (forceLookup "defectiveness" row) (forceLookup "conjugation" row) (forceLookup "presentStem" row) (forceLookup "perfectStem" row) (forceLookup "partcpStem" row) (forceLookup "trans" row)) verbResultList
    let mappedInds = map (\row -> (Indeclinable {form = forceLookup "form" row,
                                   indType = read (forceLookup "type" row)}, (forceLookup "trans" row))) indResultList
    let mappedAdvs = map (\row -> (AdverbType {positive = forceLookup "positive" row, comparative = forceLookup "comparative" row,
                                   superlative = forceLookup "superlative" row}, (forceLookup "trans" row))) advResultList
    forM_ mappedInds $ \(v, t) -> do
        printDictC outputT col (form v)
        masterPrintLn outputT $ " (" ++ show (indType v) ++ "): " ++ t
        putStr lnBreak
    forM_ mappedAdvs $ \(v, t) -> do
        printDictC outputT col $ intercalate ", " [positive v, comparative v, superlative v]
        masterPrintLn outputT $ " (adv.): " ++ t
        putStr lnBreak
    forM_ mapped $ \(v, t) -> do
        printDictC outputT col (nounDictEntry v)
        masterPrintLn outputT $ ": " ++ t
        when (verbose cmd) $ do
            masterPrintLn outputT $ showDeclInfo v
            printDeclination outputT $ decline v
        putStr lnBreak
    forM_ mappedAdj $ \(v, t) -> do
        printDictC outputT col (adjDictEntry v)
        masterPrintLn outputT $ ": " ++ t
        when (verbose cmd) $ do
            masterPrintLn outputT $ "\t" ++ show (adjDeclension v)
            printAdjDeclination outputT $ declineAdj v
        putStr lnBreak
    forM_ mappedVerbs $ \(v, t) -> do
        printDictC outputT col (verbDictEntry v)
        masterPrintLn outputT $ ": " ++ t
        when (verbose cmd) $ do
            printConjugation outputT $ conjugate v
        putStr lnBreak
    foundInfl <- findInflected cmd handle wordQuery (verbose cmd) (map fst mapped) (map fst mappedAdj) (map fst mappedVerbs) outputT
    foundInfl2 <- if (not foundInfl) then findInflected cmd handle wordQuery2 (verbose cmd) (map fst mapped) (map fst mappedAdj) (map fst mappedVerbs) outputT else return True
    closeConnection handle
    closeTmp outputT
    when (null mapped && null mappedAdj && null mappedVerbs && null mappedInds && null mappedAdvs && not foundInfl && not foundInfl2) $ errorFor wordQuery >> exitFailure

findInflected :: CmdLine -> SQLiteHandle -> String -> Bool -> [Verbum] -> [Adjective] -> [Verb] -> Maybe Handle -> IO Bool
findInflected cmd handle query verbos foundW foundA foundV outputT = do
    let lnBreak = if html cmd then "<br>" else ""
    let col = color cmd
    let nounEndings = nub $ concatMap (map first) [firstDecData, firstAbusDecData, firstGreekDecDataF, firstGreekDecDataM, secondDecDataM
                                , secondDecDataN, secondRDecData, secondGreekDecDataN, secondGreekDecDataM, thirdDecData, thirdIDecData
                                , thirdIDecDataN, thirdImDecData, thirdDecDataN, fourthDecData, fourthDecDataN, fifthDecData, deusDecData
                                , jesusDecData, bosDecData, visDecData]
    let adjectiveEndings = nub (concatMap (map first4) [adjFSData, adjFSGreek2Data, adjFSThird1Data, adjCompData, adjPresentPartFirstData, adjPresentPartSecondData
                                                        , adjPresentPartThirdData, adjPresentPartThirdIData, adjPresentPartEoData, adjFutPasFirstData, adjFutPasSecondData
                                                        , adjFutPasThirdData, adjFutPasThirdIData, adjFutPasEoData, adjIusData, adjIus2Data, adjFutActEndings])
    let verbEndings = nub $ concatMap (map first) [verbFirstConjData, verbFirstConjFutData, verbFirstPassConjData, verbFirstConjImperfData, verbFirstConjImperfPasData, verbFirstConjFutPasData
                                , verbSecondConjData, verbSecondConjFutData, verbSecondPassConjData, verbSecondConjImperfData, verbSecondConjFutPasData, verbSecondConjImperfPasData
                                , verbThirdConjData, verbThirdConjFutData, verbThirdPassConjData, verbThirdConjFutPasData, verbThirdConjImperfData, verbThirdConjImperfPasData
                                , verbThirdIConjData, verbThirdIConjFutData, verbThirdIPassConjData, verbThirdIConjFutPasData, verbThirdIConjImperfData, verbThirdIConjImperfPasData
                                , verbFourthConjData, verbFourthConjFutData, verbFourthPassConjData, verbFourthConjFutPasData, verbFourthConjImperfData, verbFourthConjImperfPasData
                                , verbFeroConjData, verbFeroPassConjData, verbEoConjData, verbPerfectData, verbPluPerfectData, verbFutPerfectData, verbSubjunctiveFirstData, verbSubjunctiveSecondData
                                , verbSubjunctiveThirdData, verbSubjunctiveThirdIData, verbSubjunctiveFourthData, verbSubjunctiveFirstPasData, verbSubjunctiveSecondPasData, verbSubjunctiveThirdPasData
                                , verbSubjunctiveThirdIPasData, verbSubjunctiveFourthPasData, verbSubjunctiveStubData, verbSubjunctiveImperfPasStubData, verbSubjunctivePerfectData, verbSubjunctivePluPerfectData
                                , verbEoConjImperfData, verbEoConjFutData, verbSubjunctiveEoData, verbSumPresIndEnds, verbSumImperfIndEnds, verbSumFutIndEnds, verbSumPresSubjEnds, verbImpPresFirstEnds
                                , verbImpPresSecondEnds, verbImpPresThirdEnds, verbImpPresThirdIEnds, verbImpPresFourthEnds, verbImpFutFirstEnds, verbImpFutSecondEnds, verbImpFutThirdEnds, verbImpFutThirdIEnds
                                , verbImpFutFourthEnds, verbImpPresFeroEnds, verbImpFutFeroEnds, verbImpPresSumEnds, verbImpFutSumEnds, verbImpPresEoEnds, verbImpFutEoEnds]
    let infinitiveEndings = nub $ concatMap (map first) [infFstDecData, infSndDecData, infThdDecData]
    let firstPhase = sortOn (length.head) . groupBy ((==) `on` length)
    let secondPhase = groupBy ((==) `on` (length.head)) . firstPhase
    let sortByLength = map concat . secondPhase
    let nounEndingsSorted = sortByLength nounEndings
    let adjectiveEndingsSorted = sortByLength adjectiveEndings
    let verbEndingsSorted = sortByLength verbEndings
    let infEndingsSorted = sortByLength infinitiveEndings
    let nounEndingLs = map (length.head) nounEndingsSorted
    let adjectiveEndingLs = map (length.head) adjectiveEndingsSorted
    let verbEndingLs = map (length.head) verbEndingsSorted
    let infEndingLs = map (length.head) infEndingsSorted
    let foundNounEndings = map (\(ends, l) -> elemIndex (lastN l query) ends) $ zip nounEndingsSorted nounEndingLs
    let foundAdjectiveEndings = map (\(ends, l) -> elemIndex (lastN l query) ends) $ zip adjectiveEndingsSorted adjectiveEndingLs
    let foundVerbEndings = map (\(ends, l) -> elemIndex (lastN l query) ends) $ zip verbEndingsSorted verbEndingLs
    let foundInfEndings = map (\(ends, l) -> elemIndex (lastN l query) ends) $ zip infEndingsSorted infEndingLs
    let dropQuestNoun (idx,sidx) = case sidx of
                                   Just x -> ((nounEndingsSorted !! idx) !! x) `dropFrom` query
                                   Nothing -> ""
    let dropQuestAdjective (idx,sidx) = case sidx of
                                        Just x -> ((adjectiveEndingsSorted !! idx) !! x) `dropFrom` query
                                        Nothing -> ""
    let dropQuestVerb (idx,sidx) = case sidx of
                                        Just x -> ((verbEndingsSorted !! idx) !! x) `dropFrom` query
                                        Nothing -> ""
    let dropQuestInf (idx,sidx) = case sidx of
                                        Just x -> ((infEndingsSorted !! idx) !! x) `dropFrom` query
                                        Nothing -> ""
    let nounStems = map dropQuestNoun $ zip [0..] foundNounEndings
    let adjectiveStems = map dropQuestAdjective $ zip [0..] foundAdjectiveEndings
    let verbStems = map dropQuestVerb $ zip [0..] foundVerbEndings
    let infStems = map dropQuestInf $ zip [0..] foundInfEndings

    let adjectiveStupid = if lastN 3 query == "ius" then "ius" `dropFrom` query ++ "ior" else ""
    let adjectiveStupid2 = if query == "plus" then "plur" else ""
    let adjectiveStupid3 = if lastN 3 query == "jus" then "jus" `dropFrom` query ++ "jor" else ""
    res <- forM nounStems $ \st -> execStatement handle $ printf "select * from nounTable where stem in ('%s','%s');" st (modi st)
    adjRes <- forM (filter (not.null) [adjectiveStupid, adjectiveStupid2, adjectiveStupid3] ++ adjectiveStems) $ \st -> do
        execStatement handle $ printf "select * from adjectiveTable where stem in ('%s','%s') or compMasc in ('%s','%s') or superlatStem in ('%s','%s');" st (modi st) st (modi st) st (modi st)
    verbRes <- forM (filter (not.null) $ verbStems ++ adjectiveStems ++ infStems) $ \st -> do
        execStatement handle $ printf "select * from verbTable where presentStem in ('%s','%s') or perfectStem in ('%s','%s') or partcpStem in ('%s','%s') or fakePrinc2 in ('%s','%s');" st (modi st) st (modi st) st (modi st) st (modi st)
    let extract resA = case resA of
                        Left s  -> error s
                        Right l -> head l
    let resultsList = filter (not.null) $ map extract res
    let adjResultsList = filter (not.null) $ map extract adjRes
    let verbResultsList = filter (not.null) $ map extract verbRes
    --when (null resultList) $ closeConnection handle >> error "no results"
    let mapped = concatMap (map (\row -> wordMapper (forceLookup "word" row) (forceLookup "declension" row)
                    (forceLookup "gender" row) (forceLookup "stem" row) (forceLookup "hasLocative" row) (forceLookup "trans" row))) resultsList
    let mapped' = filter (\(v,_) -> v `notElem` foundW) mapped
    matchedWords <- liftM catMaybes $ forM mapped' $ \(v, t) -> do
        let decls = decline v
        let falseLocative w = case casus w of
                                Locative | show w `elem` [query, modi query] -> hasLocative w
                                         | otherwise -> True
                                _   -> True
        let match = find (\w -> show w `elem` [query, modi query] && falseLocative w) decls
        case match of
            Just w  -> return $ Just (w, t)
            Nothing -> return Nothing
    let adjMapped = concatMap (map (\row -> adjMapper (forceLookup "mascSing" row) (forceLookup "femSing" row)
                    (forceLookup "neutSing" row) (forceLookup "declension" row) (forceLookup "stem" row)
                    (forceLookup "compMasc" row) (forceLookup "superlatStem" row) (forceLookup "trans" row))) adjResultsList
    let adjMapped' = filter (\(v,_) -> v `notElem` foundA) adjMapped
    matchedAdjectives <- liftM catMaybes $ forM adjMapped' $ \(v, t) -> do
        let decls = declineAdj v
        let match = find (\w -> show w `elem` [query, modi query]) decls
        case match of
            Just w  -> return $ Just (w, t)
            Nothing -> return Nothing
    let verbMapped = concatMap (map (\row -> verbMapper (forceLookup "princ1" row) (forceLookup "princ2" row)
                     (forceLookup "princ3" row) (forceLookup "princ4" row) (forceLookup "verbClass" row) (forceLookup "defectiveness" row)
                     (forceLookup "conjugation" row) (forceLookup "presentStem" row) (forceLookup "perfectStem" row) (forceLookup "partcpStem" row) (forceLookup "trans" row))) verbResultsList
    let verbMapped' = filter (\(v,_) -> v `notElem` foundV) verbMapped
    matchedVerbs <- liftM catMaybes $ forM verbMapped' $ \(v, t) -> do
        let conjs = conjugate v
        let match = find (\w -> show w `elem` [query, modi query]) conjs
        case match of
            Just w  -> return $ Just (w, t)
            Nothing -> return Nothing
    let differentParticiples = concatMap (\(v,t) -> zip3 (repeat $ verbDictEntry v) (participles v) (repeat t)) verbMapped'
    matchedParticiples <- liftM catMaybes $ forM differentParticiples $ \(de, (part,typ), t) -> do
        if show part `elem` [query, modi query] then return (Just (part,typ,t,de)) else return Nothing
    let differentInfinitives = concatMap (\(v,t) -> zip3 (repeat $ verbDictEntry v) (infinitives v) (repeat t)) verbMapped'
    matchedInfinitives <- liftM catMaybes $ forM differentInfinitives $ \(de, (inf,typ), t) -> do
        if inf `elem` [query, modi query] then return (Just (inf,typ,t,de)) else return Nothing
    --nubBy ((==) `on` first)
    forM_ (nubBy ((==) `on` fst) matchedWords) $ \(v, t) -> do
        printC outputT red query
        masterPrint outputT $ ": " ++ map toLower (show (casus v)) ++ " " ++ map toLower (show (number v)) ++ " of "
        printDictC outputT col (nounDictEntry v)
        masterPrintLn outputT $ ": " ++ t
        masterPrint outputT lnBreak
    forM_ (nubBy ((==) `on` fst) matchedAdjectives) $ \(v, t) -> do
        printC outputT red query
        masterPrint outputT $ ": " ++ putToLow (show (adjGender v)) ++ " " ++ putToLow (show (adjCasus v)) ++ " " ++ putToLow (show (adjNumber v)) ++ " " ++ putToLow (show (adjDegree v)) ++ " of "
        printDictC outputT col (adjDictEntry v)
        masterPrintLn outputT $ ": " ++ t
        masterPrint outputT lnBreak
    forM_ (nubBy ((==) `on` fst) matchedVerbs) $ \(v, t) -> do
        printC outputT red query
        masterPrint outputT $ ": " ++ show (person v) ++ " " ++ putToLow (show (verbNumber v)) ++ " " ++ putToLow (show (tense v)) ++ " " ++ putToLow (show (voice v)) ++ " " ++ putToLow (show (mood v)) ++ " of "
        printDictC outputT col (verbDictEntry v)
        masterPrintLn outputT $ ": " ++ t
        masterPrint outputT lnBreak
    forM_ (nubBy ((==) `on` fourth4) matchedParticiples) $ \(v, typ, t, de) -> do
        printC outputT red query
        let typeStr = if typ == FuturePassive then show typ ++ " part. / gerundive of " else show typ ++ " participle of "
        masterPrint outputT $ ": " ++ putToLow (show (adjGender v)) ++ " " ++ putToLow (show (adjCasus v)) ++ " " ++ putToLow (show (adjNumber v)) ++ " of " ++ typeStr
        printDictC outputT col de
        masterPrintLn outputT $ ": " ++ t
        masterPrint outputT lnBreak
    forM_ (nubBy ((==) `on` fourth4) matchedInfinitives) $ \(_, typ, t, de) -> do
        printC outputT red query
        let typeStr = putToLow (show typ) ++ " infinitive of "
        masterPrint outputT $ ": " ++ typeStr
        printDictC outputT col de
        masterPrintLn outputT $ ": " ++ t
        masterPrint outputT lnBreak
    let checks = [mkCheck matchedWords, mkCheck matchedAdjectives, mkCheck matchedVerbs
                 , mkCheck matchedParticiples, mkCheck matchedInfinitives]
    return $ not (all runCheck checks)

    where
           lastN n = reverse . take n . reverse
           dropFrom s1 s2 = take (length s2 - (length s1)) s2
           modi st = if caseInv cmd then map toLower st else st
           putToLow = map toLower

data Check = forall a. Check a (a -> Bool)

mkCheck :: [a] -> Check
mkCheck lst = Check lst null

runCheck :: Check -> Bool
runCheck (Check a f) = f a

findTranslation :: Color -> SQLiteHandle -> String -> IO ()
findTranslation col handle query = do
    let extract resA = case resA of
                        Left s  -> error s
                        Right l -> head l
    nounRes <- fmap extract $ execStatement handle $ printf "select * from nounTable where trans like '%%%s%%';" query
    adjRes <- fmap extract $ execStatement handle $ printf "select * from adjectiveTable where trans like '%%%s%%';" query
    verbRes <- fmap extract $ execStatement handle $ printf "select * from verbTable where trans like '%%%s%%';" query
    indRes <- fmap extract $ execStatement handle $ printf "select * from indeclinableTable where trans like '%%%s%%';" query
    advRes <- fmap extract $ execStatement handle $ printf "select * from adverbTable where trans like '%%%s%%';" query
    let justDoIt sub str = maybe (False, -1) (\idx -> (True, idx)) (subIndex sub str)
    let boundIndices sub str = (success, startAt - 1, startAt + length sub) where (success, startAt) = justDoIt sub str
    let properWord s = success && (idx1 < 0 || (s !! idx1) `elem` " ,;/") && (idx2 >= length s || (s !! idx2) `elem` " ,;/") where (success, idx1, idx2) = boundIndices query s
    --forM_ nounRes $ \row -> do
    --    masterPrintLn (forceLookup "trans" row)
    let nounRes' = filter (\row -> properWord (forceLookup "trans" row)) nounRes
    let adjRes' = filter (\row -> properWord (forceLookup "trans" row)) adjRes
    let verbRes' = filter (\row -> properWord (forceLookup "trans" row)) verbRes
    let indRes' = filter (\row -> properWord (forceLookup "trans" row)) indRes
    let advRes' = filter (\row -> properWord (forceLookup "trans" row)) advRes
    let mappedNouns = map (\row -> wordMapper (forceLookup "word" row) (forceLookup "declension" row)
                    (forceLookup "gender" row) (forceLookup "stem" row) (forceLookup "hasLocative" row) (forceLookup "trans" row)) nounRes'
    let mappedAdjs = map (\row -> adjMapper (forceLookup "mascSing" row) (forceLookup "femSing" row)
                    (forceLookup "neutSing" row) (forceLookup "declension" row) (forceLookup "stem" row)
                    (forceLookup "compMasc" row) (forceLookup "superlatStem" row) (forceLookup "trans" row))  adjRes'
    let mappedVerbs = map (\row -> verbMapper (forceLookup "princ1" row) (forceLookup "princ2" row)
                    (forceLookup "princ3" row) (forceLookup "princ4" row) (forceLookup "verbClass" row) (forceLookup "defectiveness" row) (forceLookup "conjugation" row) (forceLookup "presentStem" row) (forceLookup "perfectStem" row) (forceLookup "partcpStem" row) (forceLookup "trans" row)) verbRes'
    let mappedInds = map (\row -> (Indeclinable {form = forceLookup "form" row,
                                   indType = read (forceLookup "type" row)}, (forceLookup "trans" row))) indRes'
    let mappedAdvs = map (\row -> (AdverbType {positive = forceLookup "positive" row, comparative = forceLookup "comparative" row,
                                   superlative = forceLookup "superlative" row}, (forceLookup "trans" row))) advRes'
    unless (null mappedInds) $ printC Nothing red "===Indeclinables===\n"
    forM_ mappedInds $ \(v, t) -> do
        printDictC Nothing col (form v)
        putStrLn $ " (" ++ show (indType v) ++ "): " ++ t
    unless (null mappedAdvs) $ printC Nothing red "===Adverbs===\n"
    forM_ mappedAdvs $ \(v, t) -> do
        printDictC Nothing col $ intercalate ", " [positive v, comparative v, superlative v]
        putStrLn $ " (adv.): " ++ t
    unless (null mappedNouns) $ printC Nothing red "===Nouns===\n"
    forM_ mappedNouns $ \(v, t) -> do
        printDictC Nothing col (nounDictEntry v)
        putStrLn $ ": " ++ t
    unless (null mappedAdjs) $ printC Nothing red "===Adjectives===\n"
    forM_ mappedAdjs $ \(v, t) -> do
        printDictC Nothing col (adjDictEntry v)
        putStrLn $ ": " ++ t
    unless (null mappedVerbs) $ printC Nothing red "===Verbs===\n"
    forM_ mappedVerbs $ \(v, t) -> do
        printDictC Nothing col (verbDictEntry v)
        putStrLn $ ": " ++ t
    closeConnection handle
    when (and [null mappedNouns, null mappedAdjs, null mappedVerbs, null mappedInds, null mappedAdvs]) $ errorFor query >> exitFailure
    exitSuccess


{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

#include "shim.h"
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

module Text.Regex.Onigmo.Internal (
  -- * Regexes
  Regex
, compile
, captureCount
, namedCaptureCount
, captureNames

  -- * Search 
, Result(..)
, search
, match

  -- * Working with Regions
, Region
, newRegion
, clearRegion
, resizeRegion
, captures
, captureByName
, captureByNumber
, captureNameToNumber

  -- * Compile-time options
, CompileOption
, noCompileOption
, ignoreCase
, extend
, multiline
, dotAll
, singleLine
, findLongest
, findNotEmpty
, negateSingleLine
, dontCaptureGroup
, captureGroup
, posixRegion
, asciiRange
, posixBracketAllRange
, wordBoundAllRange
, newlineCRLF

  -- * Exec-time options
, SearchOption
, noSearchOption
, notBOS
, notEOS
, notBOL
, notEOL

  -- * Case folding
, CaseFold
, caseFoldTurkishAzeri
, caseFoldMultiChar

  -- * Syntaxes
, Syntax
, syntaxASIS
, syntaxPosixBasic
, syntaxPosixExtended
, syntaxEmacs
, syntaxGrep
, syntaxGnuRegex
, syntaxJava
, syntaxPerl58
, syntaxPerl58NG
, syntaxPerl
, syntaxRuby
, syntaxPython

  -- * Encodings
, Encoding
, encodingASCII
, encodingISO8859_1
, encodingISO8859_2
, encodingISO8859_3
, encodingISO8859_4
, encodingISO8859_5
, encodingISO8859_6
, encodingISO8859_7
, encodingISO8859_8
, encodingISO8859_9
, encodingISO8859_10
, encodingISO8859_11
, encodingISO8859_13
, encodingISO8859_14
, encodingISO8859_15
, encodingISO8859_16
, encodingUTF8
, encodingUTF16BE
, encodingUTF16LE
, encodingUTF32BE
, encodingUTF32LE
, encodingEUCJP
, encodingEUCTW
, encodingEUCKR
, encodingEUCCN
, encodingSJIS
, encodingCP932
, encodingKOI8R
, encodingCP1251
, encodingBIG5
, encodingGB18030 
) where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad (void)
import           Data.Monoid
import           Data.Bits ((.|.))
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Unsafe as BS
import           Foreign hiding (void)
import           Foreign.C
import           System.IO.Unsafe (unsafePerformIO)


type OnigPosition = CPtrdiff

-- higher level data types
data Regex = Regex (ForeignPtr OnigRegex)
data Region = Region (ForeignPtr OnigRegion)
data CaptureTree = CaptureTree Int BS.ByteString [ CaptureTree ] -- include group name?
                   deriving Show

-- higher level API
withRegex :: Regex -> (OnigRegex -> IO a) -> IO a
withRegex (Regex ptr) f = withForeignPtr ptr $ \x -> f (OnigRegex x)

withRegion :: Maybe Region -> (Ptr OnigRegion -> IO a) -> IO a
withRegion (Just (Region ptr)) f = withForeignPtr ptr f
withRegion Nothing             f = f nullPtr

compile :: Syntax
        -> CaseFold
        -> CompileOption
        -> BS.ByteString
        -> Encoding
        -> Encoding
        -> IO (Either String Regex)
compile syntax caseFold option pattern patternEncoding targetEncoding = do
    let oci = OnigCompileInfo 5 patternEncoding targetEncoding syntax option caseFold
    BS.unsafeUseAsCStringLen pattern $ \(patternPtr, patternLen) ->
        alloca $ \ociPtr ->
            alloca $ \regexPtr ->
                allocaBytesAligned onigErrorInfoSize onigErrorInfoAlignment $ \errInfoPtr -> do
                    let patternPtrEnd = plusPtr patternPtr patternLen
                    poke ociPtr oci
                    status <- onig_new_deluxe regexPtr patternPtr patternPtrEnd ociPtr errInfoPtr
                    if status /= onigNormal
                    then allocaBytes onigMaxErrorMessageLen $ \msgPtr -> do
                        len <- hs_onig_error_code_to_str_with_err_info msgPtr status errInfoPtr
                        message <- peekCAStringLen (msgPtr, fromIntegral len)
                        return $ Left message
                    else do
                        (OnigRegex regex) <- peek regexPtr
                        regex' <- newForeignPtr onigFinalizer regex
                        return $ Right $ Regex regex'

newRegion :: IO Region
newRegion = do
    region <- onig_region_new
    region' <- newForeignPtr regionWithSelfFinalizer region
    return $ Region region'

clearRegion :: Region -> IO ()
clearRegion region =
    withRegion (Just region) $ \regionPtr -> do
        onig_region_clear regionPtr

-- TODO: should I throw if ONIGERR_MEMORY?
resizeRegion :: Region -> Int -> IO ()
resizeRegion region size =
    withRegion (Just region) $ \regionPtr -> do
        onig_region_resize regionPtr (fromIntegral size)
        return ()

data Result = Match Int
            | NoMatch
            | Failure Int String
            deriving (Show)

search :: Regex -> BS.ByteString -> Int -> Int -> Int -> Maybe Region -> SearchOption -> IO Result
search regex string globalOffset startOffset endOffset region option =
    BS.unsafeUseAsCStringLen string $ \(stringPtr, stringLen) ->
        withRegex regex $ \regexPtr ->
            withRegion region $ \regionPtr -> do
                let stringPtrEnd = plusPtr stringPtr stringLen
                let globalPos = plusPtr stringPtr globalOffset
                let startPos = plusPtr stringPtr startOffset
                let endPos = plusPtr stringPtr endOffset
                pos <- onig_search_gpos regexPtr stringPtr stringPtrEnd globalPos startPos endPos regionPtr option
                toResult pos

match :: Regex -> BS.ByteString -> Int -> Maybe Region -> SearchOption -> IO Result
match regex string atOffset region option =
    BS.unsafeUseAsCStringLen string $ \(stringPtr, stringLen) ->
        withRegex regex $ \regexPtr ->
            withRegion region $ \regionPtr -> do
                let stringPtrEnd = plusPtr stringPtr stringLen
                let atPos = plusPtr stringPtr atOffset
                pos <- onig_match regexPtr stringPtr stringPtrEnd atPos regionPtr option
                toResult pos

toResult :: OnigPosition -> IO Result
toResult pos | pos == onigMismatch = return NoMatch
             | pos < 0 = Failure (fromIntegral pos) <$> errorMessage (fromIntegral pos)
             | otherwise = return $ Match (fromIntegral pos)

errorMessage :: CInt -> IO String
errorMessage err =
    allocaBytes onigMaxErrorMessageLen $ \msgPtr -> do
        len <- hs_onig_error_code_to_str msgPtr (fromIntegral err)
        message <- peekCAStringLen (msgPtr, fromIntegral len)
        return message

-- the number of captured regions
numberOfRegions :: Region -> IO Int
numberOfRegions region =
    withRegion (Just region) $ \regionPtr -> do
        numRegs <- (#peek OnigRegion, num_regs) regionPtr :: IO CInt
        return (fromIntegral numRegs)

captureNameToNumber :: Regex -> Region -> BS.ByteString -> IO (Maybe Int)
captureNameToNumber regex region name =
    BS.unsafeUseAsCStringLen name $ \(namePtr, nameLen) ->
        withRegex regex $ \regexPtr ->
            withRegion (Just region) $ \regionPtr -> do
                let namePtrEnd = plusPtr namePtr nameLen
                ref <- onig_name_to_backref_number regexPtr namePtr namePtrEnd regionPtr
                if ref < 0
                then return $ Nothing
                else return $ Just $ fromIntegral ref

captureByName :: Regex -> Region -> BS.ByteString -> IO (Maybe (Int, Int)) -- IO (Either String (Int, Int))
captureByName regex region name = do
    ref <- captureNameToNumber regex region name
    case ref of
        Nothing   -> return Nothing
        Just ref' -> Just <$> capture' region (fromIntegral ref')

captures :: Region -> IO [(Int, Int)]
captures region = do
    num <- numberOfRegions region 
    mapM (capture' region) [0..num-1]

captureByNumber :: Region -> Int -> IO (Maybe (Int, Int))
captureByNumber region ref = do
    num <- numberOfRegions region 
    if ref >= num
    then return Nothing
    else Just <$> capture' region ref

capture' :: Region -> Int -> IO (Int, Int)
capture' region ref =
    withRegion (Just region) $ \regionPtr -> do
        beg <- (#peek OnigRegion, beg) regionPtr
        end <- (#peek OnigRegion, end) regionPtr
        beg' <- peekElemOff beg ref :: IO OnigPosition
        end' <- peekElemOff end ref :: IO OnigPosition
        return (fromIntegral beg', fromIntegral end')

-- TOOD: test this.
captureTree :: Region -> IO (Maybe CaptureTree)
captureTree region =
    withRegion (Just region) $ \(regionPtr) -> do
        historyRoot <- (#peek OnigRegion, history_root) regionPtr
        if historyRoot == nullPtr
        then return Nothing
        else fmap Just $ peekCaptureTree historyRoot
  where
    peekCaptureTree :: Ptr () -> IO CaptureTree
    peekCaptureTree ptr = do
        -- get the tree fields
        group <- (#peek OnigCaptureTreeNode, group) ptr
        beg <- (#peek OnigCaptureTreeNode, beg) ptr :: IO (Ptr CChar)
        end <- (#peek OnigCaptureTreeNode, end) ptr :: IO (Ptr CChar)
        str <- BS.packCStringLen (beg, minusPtr end beg)
        numChilds <- (#peek OnigCaptureTreeNode, num_childs) ptr :: IO CInt
        childs <- (#peek OnigCaptureTreeNode, childs) ptr
        -- traverse the children
        children <- peekArray (fromIntegral numChilds) childs
        children' <- mapM peekCaptureTree children
        return $ CaptureTree group str children'

-- | The total number of captures.
captureCount :: Regex -> IO Int
captureCount regex =
    withRegex regex $ \(OnigRegex regexPtr) -> do
        num <- (#peek regex_t, num_mem) regexPtr
        return num

-- | The number of named captures.
namedCaptureCount :: Regex -> IO Int
namedCaptureCount regex =
    withRegex regex $ \regexPtr -> do
        numNames <- onig_number_of_names regexPtr
        return $ fromIntegral numNames

-- | The named captures and their corresponding numbers.
captureNames :: Regex -> IO [(BS.ByteString, [Int])]
captureNames regex =
    withRegex regex $ \regexPtr -> do
        numNames <- namedCaptureCount regex
        if numNames == 0
        then return [ ]
        else allocaBytesAligned (numNames * matchInfoSize) matchInfoAlignment $ \infoPtr -> do
            hs_onig_match_info regexPtr infoPtr
            peekMatchInfoArray numNames infoPtr

peekMatchInfoArray :: Int -> Ptr MatchInfo -> IO [(BS.ByteString, [Int])]
peekMatchInfoArray size ptr | size <= 0 = return []
                            | otherwise = f (size-1) []
  where
    f 0 acc = do e <- peekMatchInfo ptr; return (e:acc)
    f n acc = do e <- peekMatchInfo $ plusPtr ptr $ n * matchInfoSize; f (n-1) (e:acc)

peekMatchInfo :: Ptr MatchInfo -> IO (BS.ByteString, [Int])
peekMatchInfo ptr = do
    -- fields
    numGroups <- (#peek MatchInfo, num_groups) ptr :: IO CInt
    groups    <- (#peek MatchInfo, groups) ptr     :: IO (Ptr CInt)
    name      <- (#peek MatchInfo, name) ptr       :: IO (Ptr CChar)
    nameLen   <- (#peek MatchInfo, name_len) ptr   :: IO CInt
    str <- BS.packCStringLen (name, fromIntegral nameLen)
    -- traverse groups
    groups' <- map fromIntegral <$> peekArray (fromIntegral numGroups) groups
    return (str, groups')

matchInfoSize      = #{size MatchInfo}      :: Int
matchInfoAlignment = #{alignment MatchInfo} :: Int

-- constants
onigNormal             = #{const ONIG_NORMAL} :: CInt
onigMaxErrorMessageLen = #{const ONIG_MAX_ERROR_MESSAGE_LEN} :: Int
onigErrorInfoSize      = #{size OnigErrorInfo} :: Int
onigErrorInfoAlignment = #{alignment OnigErrorInfo} :: Int
onigMismatch           = #{const ONIG_MISMATCH} :: OnigPosition
onigErrMemory          = #{const ONIGERR_MEMORY} :: CInt

-- opaque pointers
newtype OnigRegex      = OnigRegex (Ptr OnigRegex) deriving Storable
newtype Encoding   = Encoding (Ptr Encoding) deriving Storable
newtype OnigErrorInfo  = OnigErrorInfo (Ptr OnigErrorInfo) deriving Storable
newtype Syntax = Syntax (Ptr Syntax) deriving Storable

-- case fold
newtype CaseFold = CaseFold CUInt deriving (Eq, Show, Ord, Storable)
caseFoldTurkishAzeri = CaseFold #{const ONIGENC_CASE_FOLD_TURKISH_AZERI}
caseFoldMultiChar    = CaseFold #{const INTERNAL_ONIGENC_CASE_FOLD_MULTI_CHAR}

-- structs
data MatchInfo = MatchInfo

data OnigRegion = OnigRegion

data OnigCompileInfo =
    OnigCompileInfo { ociNumOfElements :: CInt
                    , ociPatternEnc :: Encoding
                    , ociTargetEnc :: Encoding
                    , ociSyntax :: Syntax
                    , ociOption :: CompileOption
                    , ociCaseFoldFlag :: CaseFold
                    }

instance Storable OnigCompileInfo where
    sizeOf _ = (#size OnigCompileInfo)
    alignment _ = (#alignment OnigCompileInfo)
    peek ptr = do
        numOfElements <- (#peek OnigCompileInfo, num_of_elements) ptr
        patternEnc    <- (#peek OnigCompileInfo, pattern_enc) ptr
        targetEnc     <- (#peek OnigCompileInfo, target_enc) ptr
        syntax        <- (#peek OnigCompileInfo, syntax) ptr
        option        <- (#peek OnigCompileInfo, option) ptr
        caseFoldFlag  <- (#peek OnigCompileInfo, case_fold_flag) ptr
        return $ OnigCompileInfo numOfElements patternEnc targetEnc syntax option caseFoldFlag
    poke ptr (OnigCompileInfo numOfElements patternEnc targetEnc syntax option caseFoldFlag) = do
        (#poke OnigCompileInfo, num_of_elements) ptr numOfElements
        (#poke OnigCompileInfo, pattern_enc) ptr patternEnc
        (#poke OnigCompileInfo, target_enc) ptr targetEnc
        (#poke OnigCompileInfo, syntax) ptr syntax
        (#poke OnigCompileInfo, option) ptr option
        (#poke OnigCompileInfo, case_fold_flag) ptr caseFoldFlag

-- options
newtype CompileOption = CompileOption CUInt deriving (Eq, Show, Ord, Storable)
instance Monoid CompileOption where
    mempty = noCompileOption
    mappend (CompileOption opa) (CompileOption opb) =
        CompileOption (opa .|. opb)

noCompileOption      = CompileOption #{const ONIG_OPTION_NONE}
ignoreCase           = CompileOption #{const ONIG_OPTION_IGNORECASE}
extend               = CompileOption #{const ONIG_OPTION_EXTEND}
multiline            = CompileOption #{const ONIG_OPTION_MULTILINE}
dotAll               = CompileOption #{const ONIG_OPTION_DOTALL}
singleLine           = CompileOption #{const ONIG_OPTION_SINGLELINE}
findLongest          = CompileOption #{const ONIG_OPTION_FIND_LONGEST}
findNotEmpty         = CompileOption #{const ONIG_OPTION_FIND_NOT_EMPTY}
negateSingleLine     = CompileOption #{const ONIG_OPTION_NEGATE_SINGLELINE}
dontCaptureGroup     = CompileOption #{const ONIG_OPTION_DONT_CAPTURE_GROUP}
captureGroup         = CompileOption #{const ONIG_OPTION_CAPTURE_GROUP}
posixRegion          = CompileOption #{const ONIG_OPTION_POSIX_REGION}
asciiRange           = CompileOption #{const ONIG_OPTION_ASCII_RANGE}
posixBracketAllRange = CompileOption #{const ONIG_OPTION_POSIX_BRACKET_ALL_RANGE}
wordBoundAllRange    = CompileOption #{const ONIG_OPTION_WORD_BOUND_ALL_RANGE}
newlineCRLF          = CompileOption #{const ONIG_OPTION_NEWLINE_CRLF}

newtype SearchOption = SearchOption CUInt deriving (Eq, Show, Ord, Storable)
instance Monoid SearchOption where
    mempty = noSearchOption
    mappend (SearchOption opa) (SearchOption opb) =
        SearchOption (opa .|. opb)

noSearchOption = SearchOption #{const ONIG_OPTION_NONE}
notBOS         = SearchOption #{const ONIG_OPTION_NOTBOS}
notEOS         = SearchOption #{const ONIG_OPTION_NOTEOS}
notBOL         = SearchOption #{const ONIG_OPTION_NOTBOL}
notEOL         = SearchOption #{const ONIG_OPTION_NOTEOL}

data OperatorOption = OperatorOption CUInt CUInt deriving (Eq, Show)
instance Monoid OperatorOption where
    mempty = OperatorOption 0 0
    mappend (OperatorOption op1a op2a) (OperatorOption op1b op2b) =
        OperatorOption (op1a .|. op1b) (op2a .|. op2b)

synOpVariableMetaCharacters             = OperatorOption #{const ONIG_SYN_OP_VARIABLE_META_CHARACTERS}    0
synOpDotanychar                         = OperatorOption #{const ONIG_SYN_OP_DOT_ANYCHAR}                 0
synOpAsteriskZeroInf                    = OperatorOption #{const ONIG_SYN_OP_ASTERISK_ZERO_INF}           0
synOpEscAsteriskZeroInf                 = OperatorOption #{const ONIG_SYN_OP_ESC_ASTERISK_ZERO_INF}       0
synOpPlusOneInf                         = OperatorOption #{const ONIG_SYN_OP_PLUS_ONE_INF}                0
synOpEscPlusOneInf                      = OperatorOption #{const ONIG_SYN_OP_ESC_PLUS_ONE_INF}            0
synOpQMarkZeroOne                       = OperatorOption #{const ONIG_SYN_OP_QMARK_ZERO_ONE}              0
synOpEscQMarkZeroOne                    = OperatorOption #{const ONIG_SYN_OP_ESC_QMARK_ZERO_ONE}          0
synOpBraceInterval                      = OperatorOption #{const ONIG_SYN_OP_BRACE_INTERVAL}              0
synOpEscBraceInterval                   = OperatorOption #{const ONIG_SYN_OP_ESC_BRACE_INTERVAL}          0
synOpVBarAlt                            = OperatorOption #{const ONIG_SYN_OP_VBAR_ALT}                    0
synOpEscVBarAlt                         = OperatorOption #{const ONIG_SYN_OP_ESC_VBAR_ALT}                0
synOpLParenSubexp                       = OperatorOption #{const ONIG_SYN_OP_LPAREN_SUBEXP}               0
synOpEscLParenSubexp                    = OperatorOption #{const ONIG_SYN_OP_ESC_LPAREN_SUBEXP}           0
synOpEscAzBufAnchor                     = OperatorOption #{const ONIG_SYN_OP_ESC_AZ_BUF_ANCHOR}           0
synOpEscCapitalGBeginAnchO              = OperatorOption #{const ONIG_SYN_OP_ESC_CAPITAL_G_BEGIN_ANCHOR}  0
synOpDecimalBackref                     = OperatorOption #{const ONIG_SYN_OP_DECIMAL_BACKREF}             0
synOpBracketCc                          = OperatorOption #{const ONIG_SYN_OP_BRACKET_CC}                  0
synOpEscWWord                           = OperatorOption #{const ONIG_SYN_OP_ESC_W_WORD}                  0
synOpEscLtGtWordBeginEnd                = OperatorOption #{const ONIG_SYN_OP_ESC_LTGT_WORD_BEGIN_END}     0
synOpEscBWordBound                      = OperatorOption #{const ONIG_SYN_OP_ESC_B_WORD_BOUND}            0
synOpEscSWhiteSpace                     = OperatorOption #{const ONIG_SYN_OP_ESC_S_WHITE_SPACE}           0
synOpEscDDigit                          = OperatorOption #{const ONIG_SYN_OP_ESC_D_DIGIT}                 0
synOpLineAnchor                         = OperatorOption #{const ONIG_SYN_OP_LINE_ANCHOR}                 0
synOpPosixBracket                       = OperatorOption #{const ONIG_SYN_OP_POSIX_BRACKET}               0
synOpQMarkNonGreedy                     = OperatorOption #{const ONIG_SYN_OP_QMARK_NON_GREEDY}            0
synOpEscControlChars                    = OperatorOption #{const ONIG_SYN_OP_ESC_CONTROL_CHARS}           0
synOpEscCControl                        = OperatorOption #{const ONIG_SYN_OP_ESC_C_CONTROL}               0
synOpEscOctal3                          = OperatorOption #{const ONIG_SYN_OP_ESC_OCTAL3}                  0
synOpEscXHex2                           = OperatorOption #{const ONIG_SYN_OP_ESC_X_HEX2}                  0
synOpEscXBraceHex8                      = OperatorOption #{const ONIG_SYN_OP_ESC_X_BRACE_HEX8}            0
synOpEscOBraceOctal                     = OperatorOption #{const ONIG_SYN_OP_ESC_O_BRACE_OCTAL}           0
synOpEscCapitalQQuote                   = OperatorOption 0 #{const ONIG_SYN_OP2_ESC_CAPITAL_Q_QUOTE}
synOpQMarkGroupEffect                   = OperatorOption 0 #{const ONIG_SYN_OP2_QMARK_GROUP_EFFECT}
synOpOptionPerl                         = OperatorOption 0 #{const ONIG_SYN_OP2_OPTION_PERL}
synOpOptionRuby                         = OperatorOption 0 #{const ONIG_SYN_OP2_OPTION_RUBY}
synOpPlusPossessiveRepeat               = OperatorOption 0 #{const ONIG_SYN_OP2_PLUS_POSSESSIVE_REPEAT}
synOpPlusPossessiveInterval             = OperatorOption 0 #{const ONIG_SYN_OP2_PLUS_POSSESSIVE_INTERVAL}
synOpCClassSetOp                        = OperatorOption 0 #{const ONIG_SYN_OP2_CCLASS_SET_OP}
synOpQMarkLtNamedGroup                  = OperatorOption 0 #{const ONIG_SYN_OP2_QMARK_LT_NAMED_GROUP}
synOpEscKNamedBackref                   = OperatorOption 0 #{const ONIG_SYN_OP2_ESC_K_NAMED_BACKREF}
synOpEscGSubexpCall                     = OperatorOption 0 #{const ONIG_SYN_OP2_ESC_G_SUBEXP_CALL}
synOpAtMarkCaptureHistory               = OperatorOption 0 #{const ONIG_SYN_OP2_ATMARK_CAPTURE_HISTORY}
synOpEscCapitalCBarControl              = OperatorOption 0 #{const ONIG_SYN_OP2_ESC_CAPITAL_C_BAR_CONTROL}
synOpEscCapitalMBarMeta                 = OperatorOption 0 #{const ONIG_SYN_OP2_ESC_CAPITAL_M_BAR_META}
synOpEscVVTab                           = OperatorOption 0 #{const ONIG_SYN_OP2_ESC_V_VTAB}
synOpEscUHex4                           = OperatorOption 0 #{const ONIG_SYN_OP2_ESC_U_HEX4}
synOpEscGnuBufAnchor                    = OperatorOption 0 #{const ONIG_SYN_OP2_ESC_GNU_BUF_ANCHOR}
synOpEscPBraceCharProperty              = OperatorOption 0 #{const ONIG_SYN_OP2_ESC_P_BRACE_CHAR_PROPERTY}
synOpEscPBraceCircumflexNot             = OperatorOption 0 #{const ONIG_SYN_OP2_ESC_P_BRACE_CIRCUMFLEX_NOT}
synOpEscHXDigit                         = OperatorOption 0 #{const ONIG_SYN_OP2_ESC_H_XDIGIT}
synOpIneffectiveEscape                  = OperatorOption 0 #{const ONIG_SYN_OP2_INEFFECTIVE_ESCAPE}
synOpEscCapitalRLinebreak               = OperatorOption 0 #{const ONIG_SYN_OP2_ESC_CAPITAL_R_LINEBREAK}
synOpEscCapitalXExtendedGraphemeCluster = OperatorOption 0 #{const ONIG_SYN_OP2_ESC_CAPITAL_X_EXTENDED_GRAPHEME_CLUSTER }
synOpEscVVerticalWhitespace             = OperatorOption 0 #{const ONIG_SYN_OP2_ESC_V_VERTICAL_WHITESPACE}
synOpEscHHorizontalWhitespace           = OperatorOption 0 #{const ONIG_SYN_OP2_ESC_H_HORIZONTAL_WHITESPACE}
synOpEscCapitalKKeep                    = OperatorOption 0 #{const ONIG_SYN_OP2_ESC_CAPITAL_K_KEEP}
synOpEscGBraceBackref                   = OperatorOption 0 #{const ONIG_SYN_OP2_ESC_G_BRACE_BACKREF}
synOpQMarkSubexpcall                    = OperatorOption 0 #{const ONIG_SYN_OP2_QMARK_SUBEXP_CALL}
synOpQMarkVBarBranchReset               = OperatorOption 0 #{const ONIG_SYN_OP2_QMARK_VBAR_BRANCH_RESET}
synOpQMarkLParenCondition               = OperatorOption 0 #{const ONIG_SYN_OP2_QMARK_LPAREN_CONDITION}
synOpQMarkCapitalPNamedGroup            = OperatorOption 0 #{const ONIG_SYN_OP2_QMARK_CAPITAL_P_NAMED_GROUP}
synOpOptionJava                         = OperatorOption 0 #{const ONIG_SYN_OP2_OPTION_JAVA}

-- syntax
foreign import ccall "&OnigSyntaxASIS"          syntaxASIS          :: Syntax
foreign import ccall "&OnigSyntaxPosixBasic"    syntaxPosixBasic    :: Syntax
foreign import ccall "&OnigSyntaxPosixExtended" syntaxPosixExtended :: Syntax
foreign import ccall "&OnigSyntaxEmacs"         syntaxEmacs         :: Syntax
foreign import ccall "&OnigSyntaxGrep"          syntaxGrep          :: Syntax
foreign import ccall "&OnigSyntaxGnuRegex"      syntaxGnuRegex      :: Syntax
foreign import ccall "&OnigSyntaxJava"          syntaxJava          :: Syntax
foreign import ccall "&OnigSyntaxPerl58"        syntaxPerl58        :: Syntax
foreign import ccall "&OnigSyntaxPerl58_NG"     syntaxPerl58NG      :: Syntax
foreign import ccall "&OnigSyntaxPerl"          syntaxPerl          :: Syntax
foreign import ccall "&OnigSyntaxRuby"          syntaxRuby          :: Syntax
foreign import ccall "&OnigSyntaxPython"        syntaxPython        :: Syntax

-- encodings
foreign import ccall "&EncodingASCII"       encodingASCII       :: Encoding
foreign import ccall "&EncodingISO_8859_1"  encodingISO8859_1   :: Encoding
foreign import ccall "&EncodingISO_8859_2"  encodingISO8859_2   :: Encoding
foreign import ccall "&EncodingISO_8859_3"  encodingISO8859_3   :: Encoding
foreign import ccall "&EncodingISO_8859_4"  encodingISO8859_4   :: Encoding
foreign import ccall "&EncodingISO_8859_5"  encodingISO8859_5   :: Encoding
foreign import ccall "&EncodingISO_8859_6"  encodingISO8859_6   :: Encoding
foreign import ccall "&EncodingISO_8859_7"  encodingISO8859_7   :: Encoding
foreign import ccall "&EncodingISO_8859_8"  encodingISO8859_8   :: Encoding
foreign import ccall "&EncodingISO_8859_9"  encodingISO8859_9   :: Encoding
foreign import ccall "&EncodingISO_8859_10" encodingISO8859_10  :: Encoding
foreign import ccall "&EncodingISO_8859_11" encodingISO8859_11  :: Encoding
foreign import ccall "&EncodingISO_8859_13" encodingISO8859_13  :: Encoding
foreign import ccall "&EncodingISO_8859_14" encodingISO8859_14  :: Encoding
foreign import ccall "&EncodingISO_8859_15" encodingISO8859_15  :: Encoding
foreign import ccall "&EncodingISO_8859_16" encodingISO8859_16  :: Encoding
foreign import ccall "&EncodingUTF8"        encodingUTF8        :: Encoding
foreign import ccall "&EncodingUTF16_BE"    encodingUTF16BE     :: Encoding
foreign import ccall "&EncodingUTF16_LE"    encodingUTF16LE     :: Encoding
foreign import ccall "&EncodingUTF32_BE"    encodingUTF32BE     :: Encoding
foreign import ccall "&EncodingUTF32_LE"    encodingUTF32LE     :: Encoding
foreign import ccall "&EncodingEUC_JP"      encodingEUCJP       :: Encoding
foreign import ccall "&EncodingEUC_TW"      encodingEUCTW       :: Encoding
foreign import ccall "&EncodingEUC_KR"      encodingEUCKR       :: Encoding
foreign import ccall "&EncodingEUC_CN"      encodingEUCCN       :: Encoding
foreign import ccall "&EncodingSJIS"        encodingSJIS        :: Encoding
foreign import ccall "&EncodingCP932"       encodingCP932       :: Encoding
--foreign import ccall "&EncodingKOI8"        encodingKOI8        :: Encoding
foreign import ccall "&EncodingKOI8_R"      encodingKOI8R       :: Encoding
foreign import ccall "&EncodingCP1251"      encodingCP1251      :: Encoding
foreign import ccall "&EncodingBIG5"        encodingBIG5        :: Encoding
foreign import ccall "&EncodingGB18030"     encodingGB18030     :: Encoding

-- low-level functions
foreign import ccall safe "onig_number_of_names" onig_number_of_names :: OnigRegex -> IO CInt

foreign import ccall safe "onig_new_deluxe" onig_new_deluxe :: Ptr OnigRegex -> CString -> CString -> Ptr OnigCompileInfo -> Ptr OnigErrorInfo -> IO CInt

foreign import ccall safe "onig_region_new" onig_region_new :: IO (Ptr OnigRegion)

foreign import ccall safe "onig_search_gpos" onig_search_gpos :: OnigRegex -> CString -> CString -> CString -> CString -> CString -> Ptr OnigRegion -> SearchOption -> IO OnigPosition
foreign import ccall safe "onig_match" onig_match :: OnigRegex -> CString -> CString -> CString -> Ptr OnigRegion -> SearchOption -> IO OnigPosition

foreign import ccall safe "&onig_free" onigFinalizer :: FunPtr (Ptr OnigRegex -> IO ())
foreign import ccall safe "&hs_onig_region_free_with_self" regionWithSelfFinalizer :: FunPtr (Ptr OnigRegion -> IO ())

foreign import ccall safe "hs_onig_error_code_to_str" hs_onig_error_code_to_str :: CString -> CInt -> IO (CInt)
foreign import ccall safe "hs_onig_error_code_to_str_with_err_info" hs_onig_error_code_to_str_with_err_info :: CString -> CInt -> Ptr OnigErrorInfo -> IO (CInt)

foreign import ccall safe "hs_onig_match_info" hs_onig_match_info :: OnigRegex -> Ptr MatchInfo -> IO ()

foreign import ccall safe "onig_region_clear" onig_region_clear :: Ptr OnigRegion -> IO ()

foreign import ccall safe "onig_region_resize" onig_region_resize :: Ptr OnigRegion -> CInt -> IO CInt

foreign import ccall safe "onig_name_to_backref_number" onig_name_to_backref_number
  :: OnigRegex -> CString -> CString -> Ptr OnigRegion -> IO CInt

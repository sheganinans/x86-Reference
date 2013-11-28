module ReferenceParser where

import Text.XML.HXT.Core hiding (attr)
import Control.Lens

parse :: IO Reference
parse = do
  r <- runX (xunpickleDocument
              xpx86reference parseXML
              "x86reference.xml")
  return (head r)

parseXML :: [SysConfig]
parseXML =
  [ withValidate no
  , withRemoveWS yes
  ]

xpx86reference :: PU Reference
xpx86reference
  = xpElem "x86reference" $
    xpWrap ( \ (v,o,t,g,r) -> Reference v o t g r
           , \ r -> ( _version r, _oneByte r, _twoByte r
                   , _genNotes r, _ringNotes r)) $
    xp5Tuple (xpAttr "version" xpText)
             xpOneByte
             xpTwoByte
             xpGenNotes
             xpRingNotes

data Reference = Reference
  { _version   :: Version
  , _oneByte   :: OneByte
  , _twoByte   :: TwoByte
  , _genNotes  :: GenNotes
  , _ringNotes :: RingNotes
  } deriving (Show, Eq)


type Version   = String


xpOneByte  :: PU OneByte
xpOneByte = xpElem "one-byte" xpOpList

type OneByte = OpList


xpTwoByte  :: PU TwoByte
xpTwoByte = xpElem "two-byte" xpOpList

type TwoByte = OpList


xpOpList :: PU OpList
xpOpList
  = xpWrap ( OpList, _opList) $
    xpList xpOp

data OpList = OpList
  { _opList :: [Op] } deriving (Show, Eq)


xpGenNotes :: PU GenNotes
xpGenNotes
  = xpElem "gen_notes"
    (xpList (xpElem "gen_note"
             (getAttrText "id")))

type GenNotes  = [String]


xpRingNotes :: PU RingNotes
xpRingNotes
  = xpElem "ring_notes"
    (xpList (xpElem "ring_note"
             (getAttrText "id")))

type RingNotes = [String]


xpOp :: PU Op
xpOp
  = xpElem "pri_opcd" $
    xpWrap ( \ (v,opps,oppe,e) -> Op v opps oppe e
           , \ o -> (_value o, _oProcStart o
                    , _oProcEnd o, _entries o)) $
    xp4Tuple (getAttrText "value")
             (mGetElemText "proc_start")
             (mGetElemText "proc_end")
             (xpList xpEntry)

data Op = Op
  { _value      :: Value
  , _oProcStart :: OProcStart
  , _oProcEnd   :: OProcEnd
  , _entries    :: [Entry]
  } deriving (Show, Eq)

type Value      =       String
type OProcStart = Maybe String
type OProcEnd   = Maybe String


xpEntry :: PU Entry
xpEntry
  = xpElem "entry" $
    xpWrap ( \ (o,p,o2,s,eps,epe,sy,g,f,n,eh)
             -> Entry o p o2 s eps epe sy g f n eh
           , \ e -> (_opcdExt e, _pref e, _opcdExt2 e
                    , _secOpcd e, _eProcStart e, _eProcEnd e
                    , _syntax e, _groups e, _flags e
                    , _note e, _entryHelper e)) $
    xp11Tuple (mGetElemText "opcd_ext")
              (mGetElemText "pref")
              (mGetElemText "opcd_ext")
              (xpOption         xpSecOpcd)
              (xpOption         xpProcStart)
              (xpOption         xpProcEnd)
              (xpList xpSyntax)
              (xpOption         xpGroups)
              (xpOption         xpFlags)
              (xpOption         xpNotes)
              xpEntryHelper

data Entry = Entry
  { _opcdExt     :: OpcdExt
  , _pref        :: Pref
  , _opcdExt2    :: OpcdExt
  , _secOpcd     :: Maybe SecOpcd
  , _eProcStart  :: Maybe EProcStart
  , _eProcEnd    :: Maybe EProcEnd
  , _syntax      :: [Syntax]
  , _groups      :: Maybe Groups
  , _flags       :: Maybe Flags
  , _note        :: Maybe Note
  , _entryHelper :: EntryHelper
  } deriving (Show, Eq)

type OpcdExt = Maybe String
type Pref    = Maybe String


xpSecOpcd :: PU SecOpcd
xpSecOpcd
  = xpElem "sec_opcd" $
    xpWrap ( uncurry SecOpcd
           , _unSecOpcd &&& _escape) $
    xpPair mText
           (mGetAttrText "escape")

data SecOpcd = SecOpcd
  { _unSecOpcd :: SecOpcdT
  , _escape    :: Escape
  } deriving (Show, Eq)

type SecOpcdT = Maybe String
type Escape   = Maybe String



xpEntryHelper :: PU EntryHelper
xpEntryHelper
  = xpWrap ( \ (d,s,o,t,m,r,mo,rf,a,pa,dpar,l,att,dc,newd)
             -> EntryHelper d s o t m r mo rf a
                           pa dpar l att dc newd
           , \ e -> ( _direction e, _signExt e
                   , _opSize e, _tttn e, _memFormat e
                   , _entryR e, _emod e, _ref e, _alias e
                   , _partAlias e, _dpAliasRef e
                   , _lock e, _attr e, _doc e, _newDoc e)) $
    xp15Tuple (mGetAttrText "direction")
              (mGetAttrText "sign-ext")
              (mGetAttrText "op_size")
              (mGetAttrText "tttn")
              (mGetAttrText "mem_format")
              (mGetAttrText "r")
              (mGetAttrText "mod")
              (mGetAttrText "ref")
              (mGetAttrText "alias")
              (mGetAttrText "part_alias")
              (mGetAttrText "doc_part_alias_ref")
              (mGetAttrText "lock")
              (mGetAttrText "attr")
              (mGetAttrText "doc")
              xpNewDoc

data EntryHelper = EntryHelper
  { _direction   :: Direction
  , _signExt     :: SignExt
  , _opSize      :: OpSize
  , _tttn        :: Tttn
  , _memFormat   :: MemFormat
  , _entryR      :: R
  , _emod        :: Mod
  , _ref         :: EntryRef
  , _alias       :: Alias
  , _partAlias   :: PartAlias
  , _dpAliasRef  :: DpAliasRef
  , _lock        :: Lock
  , _attr        :: Attr
  , _doc         :: Doc
  , _newDoc      :: NewDoc
  } deriving (Show, Eq)

type Direction  = Maybe String
type SignExt    = Maybe String
type OpSize     = Maybe String
type Tttn       = Maybe String
type MemFormat  = Maybe String
type R          = Maybe String
type Mod        = Maybe String
type EntryRef   = Maybe String
type Alias      = Maybe String
type PartAlias  = Maybe String
type DpAliasRef = Maybe String
type Lock       = Maybe String
type Attr       = Maybe String
type Doc        = Maybe String


xpNewDoc :: PU NewDoc
xpNewDoc
  = xpWrap ( \ (d,u,dr,d16,d64,m,r,rr,pop,push,p)
             -> NewDoc d u dr d16 d64 m r rr pop push p
           , \ n -> ( _isDoc n, _isUndoc n, _docRef n
                   , _doc1632Ref n, _doc64Ref n, _mode n
                   , _ring n, _ringRef n, _fpop n, _fpush n
                   , _particular n)) $
    xp11Tuple (mGetAttrText "is_doc")
              (mGetAttrText "is_undoc")
              (mGetAttrText "doc_ref")
              (mGetAttrText "doc1632_ref")
              (mGetAttrText "doc64_ref")
              (mGetAttrText "mode")
              (mGetAttrText "ring")
              (mGetAttrText "ring_ref")
              (mGetAttrText "fpop")
              (mGetAttrText "fpush")
              (mGetAttrText "particular")

data NewDoc = NewDoc
  { _isDoc       :: IsDoc
  , _isUndoc     :: IsUndoc
  , _docRef      :: DocRef
  , _doc1632Ref  :: Doc1632Ref
  , _doc64Ref    :: Doc64Ref
  , _ring        :: Ring
  , _ringRef     :: RingRef
  , _mode        :: Mode
  , _fpop        :: Fpop
  , _fpush       :: Fpush
  , _particular  :: Particular
  } deriving (Show, Eq)

type IsDoc      = Maybe String
type IsUndoc    = Maybe String
type DocRef     = Maybe String
type Doc1632Ref = Maybe String
type Doc64Ref   = Maybe String
type Ring       = Maybe String
type RingRef    = Maybe String
type Mode       = Maybe String
type Fpop       = Maybe String
type Fpush      = Maybe String
type Particular = Maybe String


xpProcStart :: PU EProcStart
xpProcStart
  = xpElem "proc_start" $
    xpWrap ( \ (s,o,l) -> EProcStart s o l
           , \ s -> (_ps s, _post s, _latStep s)) $
    xpTriple xpickle
             (mGetAttrText "post")
             (mGetAttrText "lat_step")

data EProcStart = EProcStart
  { _ps      :: EProcStartT
  , _post    :: EPost
  , _latStep :: ElatStep
  } deriving (Show, Eq)

type EProcStartT = Maybe Integer
type EPost       = Maybe String
type ElatStep    = Maybe String


xpProcEnd :: PU EProcEnd
xpProcEnd
  = xpElem "proc_end" $
    xpWrap ( EProcEnd
           , _pe)
    xpickle

data EProcEnd = EProcEnd
  { _pe :: EProcEndT
  } deriving (Show, Eq)

type EProcEndT = Maybe Integer


xpSyntax :: PU Syntax
xpSyntax
  = xpElem "syntax" $
    xpWrap ( \ (m,d,s,mo)
             -> Syntax m d s mo
           , \ s -> (_mnem s, _dsts s, _srcs s, _smod s)) $
    xp4Tuple (xpOption         xpMnem)
             (xpOption (xpList xpDst))
             (xpOption (xpList xpSrc))
             (mGetAttrText "mod")

data Syntax = Syntax
  { _mnem :: Maybe Mnem
  , _dsts :: Maybe [Dst]
  , _srcs :: Maybe [Src]
  , _smod :: SMod
  } deriving (Show, Eq)


xpMnem :: PU Mnem
xpMnem
  = xpElem "mnem" $
    xpWrap ( uncurry Mnem
            , _unmnem &&& _sug) $
    xpPair mText
           (mGetAttrText "sug")

data Mnem = Mnem
  { _unmnem :: MnemT
  , _sug    :: Sug
  } deriving (Show, Eq)

type MnemT = Maybe String
type Sug   = Maybe String


xpDst :: PU Dst
xpDst
  = xpElem "dst" $
    xpWrap ( \ (n,g,t,a,d,dd,aa,tt,ddd)
             -> Oper n g t a d dd aa tt ddd
           , \ d -> ( _nr d, _group d
                   , _otype d, _address d
                   , _display d, _depend d, _oa d
                   , _ot d, _oper d)) operTuple

xpSrc :: PU Src
xpSrc
  = xpElem "src" $
    xpWrap( \ (n,g,t,a,d,dd,aa,tt,ddd)
            -> Oper n g t a d dd aa tt ddd
          , \ s -> ( _nr s, _group s
                   , _otype s, _address s
                   , _display s, _depend s, _oa s
                   , _ot s, _oper s)) operTuple

type Dst = Oper
type Src = Oper

data Oper = Oper
  { _nr      :: Nr
  , _group   :: OperGroup
  , _otype   :: OperType
  , _address :: Address
  , _display :: Display
  , _depend  :: Depend
  , _oa      :: Oa
  , _ot      :: Ot
  , _oper    :: OperT
  } deriving (Show, Eq)

type Nr        = Maybe String
type OperGroup = Maybe String
type OperType  = Maybe String
type Address   = Maybe String
type Display   = Maybe String
type Depend    = Maybe String
type Oa        = Maybe String
type Ot        = Maybe String
type OperT     = Maybe String


operTuple :: PU ( Maybe String
                , Maybe String
                , Maybe String
                , Maybe String
                , Maybe String
                , Maybe String
                , Maybe String
                , Maybe String
                , Maybe String)
operTuple = xp9Tuple (mGetAttrText "nr")
                     (mGetAttrText "group")
                     (mGetAttrText "type")
                     (mGetAttrText "address")
                     (mGetAttrText "displayed")
                     (mGetAttrText "depend")
                     (mGetElemText "a")
                     (mGetElemText "t")
                     mText

type SMod = Maybe String


xpGroups :: PU Groups
xpGroups
  = xpWrap ( \ (i,o,t,tt,h) -> Groups i o t tt h
           , \ g -> ( _iext g, _grp1 g
                   , _grp2 g, _grp22 g, _grp3 g)) $
    xp5Tuple (xpOption (xpElem "instr_ext" xpText))
             (xpOption (xpElem "grp1"      xpText))
             (xpOption (xpElem "grp2"      xpText))
             (xpOption (xpElem "grp2"      xpText))
             (xpOption (xpElem "grp3"      xpText))

data Groups = Groups
  { _iext  :: Iext
  , _grp1  :: Grp1
  , _grp2  :: Grp2
  , _grp22 :: Grp22
  , _grp3  :: Grp3
  } deriving (Show, Eq)

type Iext  = Maybe String
type Grp1  = Maybe String
type Grp2  = Maybe String
type Grp22 = Maybe String
type Grp3  = Maybe String


xpFlags :: PU Flags
xpFlags
  = xpWrap ( \ (t,m,d,u,f,tf,mf,df,uf,ff)
             -> Flags t m d u f tf mf df uf ff
           , \ f -> (_testf f, _modiff f, _deff f
                    , _undeff f, _fvals f, _testff f
                    , _modifff f, _defff f, _undefff f
                    , _fvalsf f)) $
    xp10Tuple (xpOption xpTestf)
              (xpOption xpModiff)
              (xpOption xpDeff)
              (xpOption xpUndeff)
              (xpOption xpFvals)
              (xpOption xpTestff)
              (xpOption xpModifff)
              (xpOption xpDefff)
              (xpOption xpUndefff)
              (xpOption xpFvalsf)

newFlag :: String -> PU Flag
newFlag = flip xpElem $
    xpWrap ( uncurry Flag, _flag &&& _cond) $
    xpPair mText
           (mGetAttrText "cond")

xpTestf :: PU Testf
xpTestf = newFlag "test_f"

xpModiff :: PU Modiff
xpModiff = newFlag "modif_f"

xpDeff :: PU Deff
xpDeff = newFlag "def_f"

xpUndeff :: PU Undeff
xpUndeff = newFlag "undef_f"

xpFvals :: PU Fvals
xpFvals = newFlag "f_vals"

xpTestff :: PU Testff
xpTestff = newFlag "test_f_fpu"

xpModifff :: PU Modifff
xpModifff = newFlag "modif_f_fpu"

xpDefff :: PU Defff
xpDefff = newFlag "def_f_fpu"

xpUndefff :: PU Undefff
xpUndefff = newFlag "undef_f_fpu"

xpFvalsf :: PU Fvalsf
xpFvalsf = newFlag "f_vals_fpu"


data Flags = Flags
  { _testf   :: Maybe Testf
  , _modiff  :: Maybe Modiff
  , _deff    :: Maybe Deff
  , _undeff  :: Maybe Undeff
  , _fvals   :: Maybe Fvals
  , _testff  :: Maybe Testff
  , _modifff :: Maybe Modifff
  , _defff   :: Maybe Defff
  , _undefff :: Maybe Undefff
  , _fvalsf  :: Maybe Fvalsf
  } deriving (Show, Eq)

type Testf   = Flag
type Modiff  = Flag
type Deff    = Flag
type Undeff  = Flag
type Fvals   = Flag
type Testff  = Flag
type Modifff = Flag
type Defff   = Flag
type Undefff = Flag
type Fvalsf  = Flag

data Flag = Flag
  { _flag :: FlagT
  , _cond :: Cond
  } deriving (Show, Eq)

type FlagT = Maybe String
type Cond  = Maybe String

xpNotes :: PU Note
xpNotes
  = xpElem "note" $
    xpWrap ( \ (n,b,d) -> Note n b d
           , \ n -> (_ns n, _brief n, _det n)) $
    xpTriple (mGetElemText "note")
             xpBrief
             (mGetElemText "det")

data Note = Note
  { _ns    :: Ns
  , _brief :: Brief
  , _det   :: Det
  } deriving (Show, Eq)

type Ns = Maybe String
type Det = Maybe String

xpBrief :: PU Brief
xpBrief
  = xpElem "brief" $
    xpWrap ( \ (v,p,b,e) -> Brief v p b e
           , \ b -> (_val b, _sup b, _sub b, _ext b)) $
    xp4Tuple xpText
             (mGetElemText "sup")
             (mGetElemText "sub")
             mText

data Brief = Brief
  { _val :: Val
  , _sup :: Sup
  , _sub :: Sub
  , _ext :: Ext
  } deriving (Show, Eq)

type Val = String
type Sup = Maybe String
type Sub = Maybe String
type Ext = Maybe String

mText :: PU (Maybe String)
mText = xpOption xpText

mGetAttrText :: String -> PU (Maybe String)
mGetAttrText = xpOption . getAttrText

getAttrText :: String -> PU String
getAttrText = flip xpAttr xpText

mGetElemText :: String -> PU (Maybe String)
mGetElemText = xpOption . getElemText

getElemText :: String -> PU String
getElemText = flip xpElem xpText

makeLenses ''Reference
makeLenses ''Op
makeLenses ''OpList
makeLenses ''Entry
makeLenses ''SecOpcd
makeLenses ''EntryHelper
makeLenses ''NewDoc
makeLenses ''EProcStart
makeLenses ''EProcEnd
makeLenses ''Syntax
makeLenses ''Mnem
makeLenses ''Oper
makeLenses ''Groups
makeLenses ''Flags
makeLenses ''Note
  makeLenses ''Brief

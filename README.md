Takes the [x86 reference](http://ref.x86asm.net/) and hoovers it up into a nice Haskell ADT.

Ripped out of a personal project currently under development and uploaded for posterity.

```haskell

data Reference = Reference
  { _version   :: Version
  , _oneByte   :: OneByte
  , _twoByte   :: TwoByte
  , _genNotes  :: GenNotes
  , _ringNotes :: RingNotes
  } deriving (Show, Eq)

type Version  = String

type OneByte = OpList

type TwoByte = OpList

data OpList = OpList
  { _opList :: [Op] } deriving (Show, Eq)

type GenNotes  = [String]

type RingNotes = [String]

data Op = Op
  { _value      :: Value
  , _oProcStart :: OProcStart
  , _oProcEnd   :: OProcEnd
  , _entries    :: [Entry]
  } deriving (Show, Eq)

type Value      =       String
type OProcStart = Maybe String
type OProcEnd   = Maybe String

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

data SecOpcd = SecOpcd
  { _unSecOpcd :: SecOpcdT
  , _escape    :: Escape
  } deriving (Show, Eq)

type SecOpcdT = Maybe String
type Escape   = Maybe String

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

data EProcStart = EProcStart
  { _ps      :: EProcStartT
  , _post    :: EPost
  , _latStep :: ElatStep
  } deriving (Show, Eq)

type EProcStartT = Maybe Integer
type EPost       = Maybe String
type ElatStep    = Maybe String

data EProcEnd = EProcEnd
  { _pe :: EProcEndT
  } deriving (Show, Eq)

type EProcEndT = Maybe Integer

data Syntax = Syntax
  { _mnem :: Maybe Mnem
  , _dsts :: Maybe [Dst]
  , _srcs :: Maybe [Src]
  , _smod :: SMod
  } deriving (Show, Eq)

data Mnem = Mnem
  { _unmnem :: MnemT
  , _sug    :: Sug
  } deriving (Show, Eq)

type MnemT = Maybe String
type Sug   = Maybe String

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

type SMod = Maybe String

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

data Note = Note
  { _ns    :: Ns
  , _brief :: Brief
  , _det   :: Det
  } deriving (Show, Eq)

type Ns = Maybe String
type Det = Maybe String

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

```

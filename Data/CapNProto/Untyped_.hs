

-- This is a WIP, still thinking about the design of this.
-- The idea is to have a series of types that abstract out
-- the "cursor".

data Struct l c m = Struct (m (l m Word64, l m (Ptr l c m))))

data Ptr l c m
    = StructPtr (m (Struct l c m))
    | CapPtr (m c)
--    | FarPtr (m (Ptr l c m))
    | ListPtr (m (ListPtr l c m))

data ListPtr l c m
    | List0  (l m ())
    | List1  (l m Bool)
    | List8  (l m Word8)
    | List16 (l m Word16)
    | List32 (l m Word32)
    | List64 (l m Word64)
    | ListPtr
        Bool -- true iff this is really a composite
        (l m (Ptr l c m))

class List l m e where
    length :: l m e -> m Int
    lookup :: Int -> l m e -> m e
    slice  :: Int -> Int -> l m e -> l m e

class (List l m e) => MList l m e where
    write :: l m e -> Int -> e -> m ()


data Address = Address
    { segIndex  :: Int
    , wordIndex :: Int
    } deriving(Show, Eq)


data PhysPtr
    = PhysStructPtr
        !Int32 -- ^ Offset in words from the end of the pointer
               -- to the start of the data section.
        !Word16 -- ^ Size of data section in words
        !Word16 -- ^ Size of pointer section in words.
    | PhysListPtr
        !Int32 -- ^ Offset in words from the end of the pointer
               -- to the start of the first element.
        !ElementSize -- ^ Size of each element
        !Int32 -- ^ Number of elements in the list, or size in words
               -- if size is SzComposite.
    | PhysFarPtr
        !Bool -- ^ True iff landing pad is two words
        !Word32 -- ^ Words from the start of the targement segment
                -- to the start of the landing pad.
        !Word32 -- ID of the target segment.
    | PhysCapPtr !Word32
    deriving(Show, Eq)


data ElementSize
    = Sz0
    | Sz1
    | Sz8
    | Sz16
    | Sz32
    | Sz64
    | SzPtr
    | SzComposite
    deriving(Show, Eq

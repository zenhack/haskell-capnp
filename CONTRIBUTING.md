Firstly, thanks!

# Style

Generally, do what the rest of the code does. Much of this section is
bikeshed, but consistency is worth a bit of that.

## Formatting/Layout

* [stylish-haskell][1] takes care of most formatting issues. If it wants
  to change your code, let it; this solves a lot of consistency issues
  without us needing to remember anything, and it gets run as part of
  our CI, so failing to use it will likely cause failures. Running `make
  format` will apply it to the entire source tree, using this project's
  rules, but we recommend you configure your editor to use it
  automatically.
* Use a tabstop of 4 spaces in Haskell source code, 2 spaces in the
  cabal file.
* Don't align things unless stylish-haskell wants to. It will get basic
  things like:

```haskell
data MyRecord = MyRecord
    { short      :: Int
    , longerName :: Bool
    }
```

Where it doesn't contradict you, use more regular indentation.

Bad:

```haskell
data MyVariant = Apples Int
               | Oranges Bool
```

Good:

```haskell
data MyVariant
    = Apples Int
    | Oranges Bool
```

Bad:

```haskell
myAction val = do print val
                  c <- getChar
                  putChar (toUpper c)
```

Good:

```haskell
myAction Val = do
    print val
    c <- getChar
    putChar (toUpper c)
```

This goes for cabal files as well. Bad:

```haskell
    build-depends:   base       >= 4.8  && < 5.0
                   , text       >= 1.2  && < 2.0
                   , bytestring >= 0.10 && < 0.11
                   , array      >= 0.5  && < 0.6
                   ...
```

Good:

```haskell
    build-depends:
        base       >= 4.8  && < 5.0
      , text       >= 1.2  && < 2.0
      , bytestring >= 0.10 && < 0.11
      , array      >= 0.5  && < 0.6
      ...
```

The same rule applies for other constructs.

## Imports

Most of this section is bikeshed, but here so we at least agree on
*something*. There is one bit that I(@zenhack) think is a bit more
objective though:

Favor qualified imports or importing specific items. Unqualified
imports are acceptable in a few cases, where you're using a ton of
stuff from a single module, but try to avoid them, especially with
libraries whose API is not very very stable.

On to the bikeshed:

* Group imports into four distinct sections, separated by a single blank
  line (some of these may be absent):

```haskell
-- "negative" imports:
import Prelude hiding (length)

-- unqualified imports; try to avoid these, but sometimes if you've got
-- a module that's doing nothing but bitwhacking, it can make sense:
import Data.Bits
import Data.Word

-- imports of specific values
import Control.Monad(when, void)
import Control.Monad.Catch(throwM)

-- qualified module imports:
import Data.ByteString as BS
```

The formatter will take care of formatting the sections correctly, as
long as you keep the line-breaks right.

* Don't mix different "kinds" of imports:

```haskell
-- Bad:

import qualified Data.ByteString as BS (ByteString)

-- Good:

import Data.ByteString (ByteString)

import qualified Data.ByteString as BS
```


[1]: https://github.com/jaspervdj/stylish-haskell

Firstly, thanks!

# Style

We try to keep the style rules that need memorizing for this project to
a minimum. Basically:

* [stylish-haskell][1] is the final arbiter of all formatting issues.
  Running `make format` will apply it to the entire source tree, using
  this project's rules.
* Generally, do what the rest of the code does.

There are a couple things that are worth spelling out though

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

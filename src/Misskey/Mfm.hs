module Misskey.Mfm
    -- * MFM: Misskey Formatting Markup
    ( Mfm
    
    -- * Embed links and special characters
    , mention
    , hashtag
    , urlLink
    , emoji

    -- * Text formatting

    -- ** Text size
    , small
    , big
    , large
    , huge
    , gigantic

    -- ** Styling
    , bold
    , italic
    , quote
    , center
    , codeSnip
    , codeBlock
    , codeBlockLang
    , fontStyle
    , TypeFace(..)
    , blur

    -- ** Special
    , flip
    , FlipType(..)
    --, tada
    --, jelly
    --, twitch
    --, shake
    --, spin
    --, jump
    --, bounce
    --, sparkle

    -- * Unformat
    , plain
) where

import RIO
    ( IsString, Semigroup(..), Maybe(..), Text, (<$>), maybe )
import Misskey.Types ( Url(..), User(username, host) )
import Data.Text ( replace )

-- |Alias for `Text` as convenience type
type Mfm = Text

-- |Larger font size than normal
big :: Mfm -> Mfm
big = wrap "***"

-- |Larger font size than `big`
large :: Mfm -> Mfm
large = mfmBrackets "x2"

-- |Larger font size than `large`
huge :: Mfm -> Mfm
huge = mfmBrackets "x3"

-- |Largest font size. Larger than `huge`
gigantic :: Mfm -> Mfm
gigantic = mfmBrackets "x4"


-- |Creates a mention for a user of form \@user\@host where \@host is optional
mention :: User -> Mfm
mention user = "@" <> user.username <> maybe "" ("@" <>) hostName
    where hostName = urlToText <$> user.host
          urlToText (Url t) = t

-- |Remove whitespaces and add '#' at start. No capitalization or checking for invalid characters
hashtag :: Text -> Mfm
hashtag t = "#" <> replace " " "" t

-- |A link to an web resource. Optional text to be displayed in place of the raw url. Url is not checked for correctnes.
--  like [display text](https://misskey.io/)
urlLink :: Url -> Maybe Text -> Mfm
urlLink linkVal (Just displayTxt) = "[" <> displayTxt <> "]" <> linkTxt linkVal.toText
urlLink linkVal Nothing = linkTxt linkVal.toText

linkTxt :: (Semigroup a, IsString a) => a -> a
linkTxt l = "(" <> l <> ")"

-- |Embed an emoji value. Can be unicode or server specific. Emoji value is not checked for correctness.
emoji :: Text -> Mfm
emoji = wrap ":"

-- |Embolden like **bold**
bold :: Mfm -> Mfm
bold = wrap "**"

-- |Italisize like *italics*
italic :: Mfm -> Mfm
italic = wrap "*"

-- |Makes font size smaller than normal
small :: Mfm -> Mfm
small t = "<small>" <> t <> "</small>"

-- |Quotes a section of Mfm. Handles nested quotes and newlines.
quote :: Mfm -> Mfm
quote t = ">" <> replace "\n" "\n>" t

-- |Centers content
center :: Mfm -> Mfm
center t = "<center>" <> t <> "</center>"

-- |Colors an inline section of code
codeSnip :: Mfm -> Mfm
codeSnip = wrap "`"

-- |Colors a multiline section of code
codeBlock :: Mfm -> Mfm
codeBlock c = "```\n" <> c <> "\n```"

-- |Colors a multiline section of code with language type specified. Lanaugage not checked for correctness.
codeBlockLang :: Text -> Mfm -> Mfm
codeBlockLang lang c = "```" <> lang <> "\n" <> c <> "\n```"

-- |Mirrors content in some direction
flip :: FlipType -> Mfm -> Mfm
flip Horizontal = mfmBrackets "flip"
flip Vertical = mfmBrackets "flip.v"
flip Both = mfmBrackets "flip.h,v"

data FlipType = Horizontal | Vertical | Both

-- |Set the typeface to a specific style
fontStyle :: TypeFace -> Mfm -> Mfm
fontStyle Serif = mfmBrackets "font.serif"
fontStyle Monospace = mfmBrackets "font.monospace"
fontStyle Cursive = mfmBrackets "font.cursive"
fontStyle Fantasy = mfmBrackets "font.fantasy"

data TypeFace = Serif | Monospace | Cursive | Fantasy

-- |Blur the contained content. Becomes clear on hover.
blur :: Mfm -> Mfm
blur = mfmBrackets "blur"

-- |Remove all MFM formatting contained within
plain :: Mfm -> Mfm
plain mfm = "<plain>" <> mfm <> "</plain>"

mfmBrackets :: (Semigroup a, IsString a) => a -> a -> a
mfmBrackets args txt = "$[" <> args <> " " <> txt <> "]"

wrap :: Semigroup a => a -> a -> a
wrap wrapper inside = wrapper <> inside <> wrapper
module Editor.Theme
  ( solarizedLight
  , solarizedDark
  ) where

import Data.Semigroup ((<>))
import Yi
  ( Attributes(..)
  , Color(RGB)
  , Theme
  , UIStyle(..)
  , defaultTheme
  , emptyAttributes
  , override
  , withBg
  , withFg
  )

data Mode = Mode
  { base03 :: Color
  , base02 :: Color
  , base01 :: Color
  , base00 :: Color
  , base0 :: Color
  , base1 :: Color
  , base2 :: Color
  , base3 :: Color
  , yellow :: Color
  , orange :: Color
  , red :: Color
  , magenta :: Color
  , violet :: Color
  , blue :: Color
  , cyan :: Color
  , green :: Color
  }

dark :: Mode
dark =
  Mode
    { base03 = RGB 0 0 0
    , base02 = RGB 128 128 128
    , base01 = RGB 0 128 0
    , base00 = RGB 255 255 0
    , base0 = RGB 0 0 255
    , base1 = RGB 0 255 255
    , base2 = RGB 165 165 165
    , base3 = RGB 255 255 255
    , red = RGB 139 0 0
    , orange = RGB 255 0 0
    , yellow = RGB 165 42 42
    , magenta = RGB 128 0 128
    , violet = RGB 255 0 255
    , blue = RGB 0 0 139
    , cyan = RGB 0 139 139
    , green = RGB 0 100 0
    }

light :: Mode
light =
  dark
    { base03 = base3 dark
    , base3 = base03 dark
    , base02 = base2 dark
    , base2 = base02 dark
    , base01 = base1 dark
    , base1 = base01 dark
    , base00 = base0 dark
    , base0 = base00 dark
    }

solarized :: Mode -> Theme
solarized mode =
  defaultTheme `override` \super ->
    const
      super
        { baseAttributes =
            emptyAttributes {foreground = base0 mode, background = base03 mode}
        , selectedStyle = withBg (base01 mode) <> withFg (base03 mode)
        , tabBarAttributes =
            emptyAttributes {foreground = base0 mode, background = base03 mode}
        , tabInFocusStyle = withFg (base01 mode) <> withBg (base2 mode)
        , modelineAttributes =
            emptyAttributes {foreground = base1 mode, background = base02 mode}
        , modelineFocusStyle = withFg (base00 mode)
        , hintStyle = withFg (base03 mode) <> withBg (orange mode)
        , strongHintStyle = withFg (base03 mode) <> withBg (yellow mode)
        , commentStyle = withFg (base01 mode)
        , blockCommentStyle = withFg (base01 mode)
        , keywordStyle = withFg (yellow mode)
        , preprocessorStyle = withFg (orange mode)
        , stringStyle = withFg (cyan mode)
        , longStringStyle = withFg (cyan mode)
        , typeStyle = withFg (yellow mode)
        , importStyle = withFg (orange mode)
        , dataConstructorStyle = withFg (yellow mode)
        , operatorStyle = withFg (green mode)
        , builtinStyle = withFg (red mode)
        , numberStyle = withFg (cyan mode)
        , regexStyle = withFg (cyan mode)
        , variableStyle = withFg (base1 mode)
        , quoteStyle = withFg (cyan mode)
        }

solarizedLight :: Theme
solarizedLight = solarized light

solarizedDark :: Theme
solarizedDark = solarized dark

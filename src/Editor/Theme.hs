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
    { base03 = RGB 0 43 54
    , base02 = RGB 7 54 66
    , base01 = RGB 88 110 117
    , base00 = RGB 101 123 131
    , base0 = RGB 131 148 150
    , base1 = RGB 147 161 161
    , base2 = RGB 238 232 213
    , base3 = RGB 253 246 227
    , yellow = RGB 181 137 0
    , orange = RGB 203 75 22
    , red = RGB 220 50 47
    , magenta = RGB 211 54 130
    , violet = RGB 108 113 196
    , blue = RGB 38 139 210
    , cyan = RGB 42 161 152
    , green = RGB 133 153 0
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
        , importStyle = withFg (red mode)
        , dataConstructorStyle = withFg (yellow mode)
        , operatorStyle = withFg (green mode)
        , builtinStyle = withFg (red mode)
        , numberStyle = withFg (cyan mode)
        , regexStyle = withFg (cyan mode)
        , variableStyle = withFg (base0 mode)
        , quoteStyle = withFg (cyan mode)
        }

solarizedLight :: Theme
solarizedLight = solarized light

solarizedDark :: Theme
solarizedDark = solarized dark
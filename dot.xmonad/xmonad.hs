{-------------------------------------------------------------------------------
  Stolen from Chris P.
-------------------------------------------------------------------------------}
import XMonad
import XMonad.Actions.WindowGo
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Gaps
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.Scratchpad
import System.IO

{-------------------------------------------------------------------------------
  Main
-------------------------------------------------------------------------------}

main = do
    xmonad conf
      {
        startupHook = startupHook conf >> setWMName "LG3D"
      }

conf = withUrgencyHook NoUrgencyHook $ ewmh defaultConfig
        { terminal = "urxvt"
        , modMask = mod4Mask
        , focusedBorderColor = "gray80"
        , normalBorderColor = "gray20"
        -- Shouldn't need gaps, but xmonad or tint2 are not respecting struts
        , layoutHook = avoidStruts . smartBorders . gaps [(U,25)] $ layoutHook defaultConfig
        , manageHook = manageHook defaultConfig <+> manageDocks
        }
        `removeKeysP` keysToRemove
        `additionalKeysP` keysToAdd

{-------------------------------------------------------------------------------
  Key Bindings
-------------------------------------------------------------------------------}

-- Additional key bindings to add to the default configuration
keysToAdd =
    [ ("M-<Space>", spawn dmenuCmd)
    , ("M-\\", sendMessage NextLayout)
    , ("M-s", scratchpadSpawnActionTerminal "urxvt")
    , ("M-f", runOrRaise "firefox" (className =? "Namoroka"))
    , ("M-i", raiseMaybe (runInTerm "-title irssi" "sh -c 'irssi'") (title =? "irssi"))
    , ("M-C-l", spawn "xscreensaver-command -lock")
    , ("<XF86AudioMute>", spawn "amixer -q set Master toggle")
    , ("<XF86AudioLowerVolume>", spawn "amixer -q set Master 2dB- unmute")
    , ("<XF86AudioRaiseVolume>", spawn "amixer -q set Master 2dB+ unmute")
    , ("<XF86AudioStop>", spawn "mpc stop")
    , ("<XF86AudioPlay>", spawn "mpc toggle")
    , ("<XF86AudioPrev>", spawn "mpc prev")
    , ("<XF86AudioNext>", spawn "mpc next")
    , ("<XF86ScreenSaver>", spawn "xscreensaver-command -l")
    ]

-- Key bindings to remove from the default configuration
keysToRemove =
    [ "M-p"
    , "M-<Space>"
    ]


{-------------------------------------------------------------------------------
  External Processes
-------------------------------------------------------------------------------}

dmenuCmd = "dmenu_run -fn " ++ quoteStr defaultFont


{-------------------------------------------------------------------------------
  Fonts
-------------------------------------------------------------------------------}

--defaultFont = terminusFont
defaultFont =  helveticaFont
helveticaFont = "-*-helvetica-medium-r-*-*-12-*-*-*-*-*-*-*"
--terminusFont = "-*-terminus-*-*-*-*-12-*-*-*-*-*-*-*"


{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

quoteStr = wrap "'" "'"

-- My second haskell program: A window manager.
--
-- With a little help from my personal gods, the xmonad dev team!
import XMonad
import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Config.Xfce
import XMonad.Config.Desktop
import XMonad.Hooks.EwmhDesktops
--import XMonad.Hooks.SetWMName
import XMonad.Actions.CycleWS
import XMonad.Layout.SimpleDecoration
import XMonad.Layout.Magnifier
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.Grid
import XMonad.Layout.FixedColumn
import XMonad.Layout.IM
import XMonad.Layout.Reflect
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ResizableTile
import XMonad.Layout.Dishes
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.LimitWindows
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Spiral
import XMonad.Actions.UpdateFocus
import XMonad.Actions.CopyWindow
import XMonad.Util.Themes
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Actions.RotSlaves
import XMonad.Actions.PerWorkspaceKeys
import qualified XMonad.Actions.FlexibleResize as Flex
import XMonad.Hooks.ManageHelpers
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import Control.Monad
import Data.Ratio ((%))
import Data.Maybe
import Data.List

myMod = mod4Mask -- windows key
myTerminal = "x-terminal-emulator"

myWorkSpaces = ["logs", "main", "web", "chat", "misc", "book", "m2", "f1", "f2", "f3" ]

myTheme = defaultTheme
	{ activeColor         = blue
	, inactiveColor       = grey
	, activeBorderColor   = blue
	, inactiveBorderColor = grey
	, activeTextColor     = "white"
	, inactiveTextColor   = "black"
	, decoHeight          = 12
	}
	where
		blue = "#4a708b" -- same color used by pager
		grey = "#cccccc"

myXPConfig = defaultXPConfig
	{ fgColor  = "white"
	, bgColor  = "black"
	, promptBorderWidth = 0
	, position = Bottom
	, height   = 25
	}

myLayout = toggleLayouts Full perWS
	where
		-- Per workspace layout selection.
		perWS =
			onWorkspace "logs" (withTitles $ myLogs dishFirst) $
			onWorkspace "web"  (noTitles   $ (mySplit ||| myWide)) $
			onWorkspace "chat" (noTitles   $ myChat gridFirst) $
			onWorkspace "book" (noTitles   $ myBook) $
                        onWorkspace "f1"   (withTitles $ myFloat) $
                        onWorkspace "f2"   (withTitles $ myFloat) $
                        onWorkspace "f3"   (noTitles   $ myFloat) $
			                   ((withTitles $ codeFirst) ||| (noTitles $ codeFirst))

		-- modifies a layout to be desktop friendly with title bars
		-- and avoid the panel.
		withTitles l = noFrillsDeco shrinkText myTheme $ desktopLayoutModifiers l
		--withTitles l = desktopLayoutModifiers l

		-- Modifies a layout to be desktop friendly, but with no title bars
		-- and avoid the panel.
		noTitles l = desktopLayoutModifiers l

		-- Each of these allows toggling through a set of layouts
		-- in the same logical order, but from a different starting
		-- point.
		codeFirst = myCode ||| myWide ||| mySpiral ||| myGrid ||| myDish
		dishFirst = myDish ||| myCode ||| myWide ||| mySpiral ||| myGrid
		gridFirst = myGrid ||| myDish ||| myCode ||| myWide ||| mySpiral

		-- This is a tall-like layout.
		-- The master window is fixed at 100 columns wide, making
		-- this good for coding. Limited to 5 visible windows at
		-- a time to ensure all are a good size.
		myCode = limitWindows 5 $
                         --magnifiercz 1.4 $
			FixedColumn 1 20 100 10

		-- Stack with one large master window.
		-- It's easy to overflow a stack to the point that windows
		-- are too small, so only show first 5.
		myDish = limitWindows 5 $ Dishes nmaster ratio
			where
				-- default number of windows in the master pane
				nmaster = 1
				-- proportion of screen occupied by other panes
				ratio = 1/5

		-- Wide layout with subwindows at the bottom.
		myWide = Mirror $ Tall nmaster delta ratio
			where
				-- default number of windows in the master pane
				nmaster = 1
				-- Percent of screen to increment by when resizing panes
				delta   = 3/100
				-- proportion of screen occupied by master pane
				ratio   = 80/100

		-- Split screen, optimized for web browsing.
		mySplit = Tall nmaster delta ratio
			where
				-- default number of windows in the master pane
				nmaster = 1
				-- Percent of screen to increment by when resizing panes
				delta   = 3/100
				-- proportion of screen occupied by master pane
				ratio   = 60/100

		-- Standard grid.
		myGrid = Grid

		-- Determined experimentally
		mySpiral = spiral (6/7)

		-- The chat workspace has a roster on the right.
		myChat base = mirror base $ withIM size roster
			where
                          -- Ratio of screen roster will
                          size = 1%5
			  -- Match roster window
		          roster = Title "Buddy List"

		-- The logs workspace has space for procmeter.
		myLogs base = mirror base $ withIM procmeterSize procmeter
			where
				-- Ratio of screen procmeter will occupy
				procmeterSize = 1%7
				                -- Match procmeter
				procmeter = ClassName "ProcMeter3"

		-- For reading books, I typically want borders on
		-- the margin of the screen.
		myBook = ThreeColMid nmaster delta ratio
			where
				-- default number of windows in the master pane
				nmaster = 1
				-- Percent of screen to increment by when resizing panes
				delta   = 3/100
				-- proportion of screen occupied by master pane
				ratio   = 2/3

                myFloat = simpleFloat ||| Full

		-- Applies a layout mirrored.
		mirror base a = reflectHoriz $ a $ reflectHoriz base

myKeys =
	[ ((myMod, xK_x), spawn myTerminal)
	, ((myMod, xK_c), kill)
	, ((myMod, xK_Left), prevWS)
	, ((myMod, xK_Right), nextWS)
	, ((myMod, xK_a), toggleWS)
	, ((myMod, xK_Up), prevWS)
	, ((myMod, xK_Down), nextWS)
	, ((myMod, xK_z), shellPrompt myXPConfig)
 	, ((myMod, xK_Tab), bindOn [("chat", rotSlavesDown), ("", rotAllDown)])
        , ((myMod .|. controlMask, xK_l), spawn "xscreensaver-command -lock")
	, ((myMod .|. shiftMask, xK_Tab), bindOn [("chat", rotSlavesUp), ("", rotAllUp)])
	, ((myMod, xK_d), sendMessage $ NextLayout)
	, ((myMod, xK_space), sendMessage $ ToggleLayout)
	, ((myMod, xK_p), spawn "assword gui")
	]


intellijTweaks = [
  --ignore IntelliJ autocomplete
  appName =? "sun-awt-X11-XWindowPeer" <&&> className =? "jetbrains-idea" --> doIgnore
  ]

myManageHook =
	-- comes first to partially override default gimp floating behavior
	[ gimp "toolbox" --> nofloat
	, gimp "image-window" --> nofloat
	, manageHook xfceConfig
	, doF avoidMaster
	, resource =? "floatterm" --> doFloat
	, className =? "mplayer2" --> doFloat
	-- workaround for <http://code.google.com/p/xmonad/issues/detail?id=228>
	, composeOne [ isFullscreen -?> doFullFloat ]
	-- display notifications on all workspaces,
	-- and avoid focus stealing
        , className =? "Xfce4-notifyd" --> doF W.focusDown <+> doIgnore <+> doF copyToAll
	] ++ intellijTweaks
  where
	gimp win = className =? "Gimp" <&&> fmap (win `isSuffixOf`) role
	role = stringProperty "WM_WINDOW_ROLE"
	nofloat = ask >>= doF . W.sink

-- Modified to only operate on floating windows, since I seem to do this by
-- accident to non-floating.
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList
	-- mod-button1, Move by dragging
	[ ((modMask, button1), (\w -> focus w >> ifFloating w mouseMoveWindow))
	-- mod-button2, Raise the window to the top of the stack
	, ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
	-- mod-button3, Resize by dragging
	, ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))
	]
	where
		ifFloating w f = withWindowSet $ \ws ->
			when (M.member w $ W.floating ws) (f w)

myConfig = xfceConfig
	{ manageHook = manageHook xfceConfig <+> composeAll myManageHook
	, layoutHook = myLayout
	, modMask = myMod
	, workspaces = myWorkSpaces
	, mouseBindings = myMouseBindings
	, terminal = myTerminal
	, borderWidth = 0
	, normalBorderColor  = inactiveBorderColor myTheme
	, focusedBorderColor = activeBorderColor myTheme
	--, startupHook = adjustEventInput
	--, handleEventHook = focusOnMouseMove
	} `additionalKeys` myKeys

main = xmonad myConfig

-- Avoid the master window, but otherwise manage new windows normally.
avoidMaster :: W.StackSet i l a s sd -> W.StackSet i l a s sd
avoidMaster = W.modify' $ \c ->
	case c of
		W.Stack t [] (r:rs) -> W.Stack t [r] rs
		_ -> c

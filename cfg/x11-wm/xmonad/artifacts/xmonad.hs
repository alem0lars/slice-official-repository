{-# LANGUAGE NoMonomorphismRestriction #-}


import XMonad
import Control.Monad

import qualified XMonad.StackSet as W
import qualified XMonad.Actions.Search as S
import qualified XMonad.Actions.Submap as SM

import qualified Data.Map as M
import Data.Ratio
import Graphics.X11.Xinerama

import XMonad.Hooks.ICCCMFocus

import XMonad.Actions.FloatSnap
import XMonad.Actions.GridSelect
import XMonad.Actions.WithAll
import XMonad.Actions.SwapWorkspaces

import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Reflect
import XMonad.Layout.PerWorkspace(onWorkspace)
import XMonad.Layout.Grid
import XMonad.Layout.Tabbed
import XMonad.Layout.Mosaic
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Cross
import XMonad.Layout.WindowArranger
import XMonad.Layout.Accordion
import XMonad.Layout.Spiral
import XMonad.Layout.WindowNavigation
import XMonad.Layout.Named
import XMonad.Layout.Combo
import XMonad.Layout.DragPane
import XMonad.Layout.TwoPane
import XMonad.Layout.WorkspaceDir
import qualified XMonad.Layout.Magnifier as Mag
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.OneBig

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.SetWMName

import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Man
import XMonad.Prompt.Workspace
import XMonad.Prompt.AppendFile
import XMonad.Prompt.Ssh

import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys, additionalKeysP)
import XMonad.Util.NamedScratchpad
import XMonad.Util.WindowProperties(getProp32s)

import System.IO


-- Colors
defWhite = "#ffffff"
defGrey_125 = "#d6d6d6"
defGrey_100 = "#ababab"
defGrey_75 = "#838383"
defGrey_50 = "#5d5d5d"
defGrey_25 = "#3a3a3a"
defGrey_0 = "#1a1a1a"
defRed_100 = "#fc8c72"
defCyan_100 = "#3bbdb6"
defCyan_75 = "#2b918c"
defBlack = "#000000"

defBgColor = defGrey_0
defFgColor = defGrey_100

defInactiveTextColor = defGrey_75
defActiveTextColor = defGrey_100
defUrgentTextColor = defRed_100

defInactiveBorderColor = defGrey_25
defActiveBorderColor = defGrey_100
defUrgentBorderColor = defGrey_125

defCurCols = (defBgColor, defCyan_100)
defVisCols = (defBgColor, defGrey_100)
defHidCols = (defCyan_75, defBgColor)
defEmpCols = (defInactiveTextColor, defBgColor)
defUrgCols = (defUrgentTextColor, defBgColor)
defLayCols = (defBgColor, defActiveTextColor)

-- Font
defTerminal = "urxvtc"

-- Terminal
defFont = "Inconsolata-11"

-- Notes
defNotesFile = "~/tmp/notes"

-- Search engines
searchEngineMap method = M.fromList
  [ ((0, xK_g), method S.google)
  , ((0, xK_c), method S.codesearch)
  , ((0, xK_t), method S.mathworld)
  , ((0, xK_y), method S.youtube)
  , ((0, xK_m), method S.maps)
  , ((0, xK_d), method S.dictionary)
  , ((0, xK_w), method S.wikipedia)
  , ((0, xK_a), method S.alpha)
  ]

-- Scratchpads
defScratchpadSize = W.RationalRect (1/4) (1/4) (1/2) (1/2)
defSPFloat = customFloating defScratchpadSize
defScratchpads =
  [ NS "term" "urxvtc -title term" (title =? "term") defSPFloat
  , NS "irb" "urxvtc -e irb" (title =? "irb") defSPFloat
  , NS "top"  "urxvtc -e htop" (title =? "htop") defSPFloat
  , NS "ghci" "urxvtc -e ghci" (title =? "ghci") defSPFloat
  ]

-- Workspaces
defWsLbls = zipWith ((++) . show) [1..] (map (defWsLblSep ++) defWsNames)
defWsLblSep = " "
defWsNames = [ "ref"
             , "wflow"
             , "wflow-envs"
             , "wflow-supp"
             , "notes"
             , "elab"
             , "social"
             , "mgmt"
             , "tmp"
             ]
defCurW = (" [", "] ")
defVisW = (" |", "| ")
defHidW = (" +", "  ")
defEmpW = ("  ", "  ")
defUrgW = ("=!", "!=")
defLayW = (" # ", " # ")

-- Layouts
defLayoutHook = smartBorders . avoidStruts $
    workspaceDir "~" $
    mkToggle1 NBFULL $
    mkToggle1 REFLECTX $
    mkToggle1 REFLECTY $
    mkToggle1 NOBORDERS $
    mkToggle1 MIRROR $
    onWorkspace (head defWsLbls) defRefLayouts $ onWorkspace (defWsLbls !! 1) defWflowLayouts $ onWorkspace (defWsLbls !! 2) defWflowenvsLayouts $ onWorkspace (defWsLbls !! 3) defWflowsuppLayouts $ onWorkspace (defWsLbls !! 4) defNotesLayouts $ onWorkspace (defWsLbls !! 5) defElabLayouts $ onWorkspace (defWsLbls !! 6) defSocialLayouts $ onWorkspace (defWsLbls !! 7) defMgmtLayouts $ onWorkspace (defWsLbls !! 8) defTmpLayouts $ defStdLayouts
defTiled = ResizableTall 1 (2/100) (1/2) []
defTabbed = tabbed shrinkText defTabbedTheme
  where
    defTabbedTheme = defaultTheme { inactiveBorderColor = defInactiveBorderColor
                                  , activeBorderColor = defActiveBorderColor
                                  , activeTextColor = defActiveTextColor
                                  , urgentBorderColor = defUrgentBorderColor
                                  , urgentTextColor = defUrgentTextColor
                                  }

-- Layout definitions
defCross = simpleCross
defFull = Full
defAccordion = Accordion
defGrid = Mag.magnifiercz (11%10) Grid
defSpiral = Mag.magnifiercz (11%10) $ spiral (1/2)
defMosaic = mosaic 2 [3, 2] -- mosaic 1.5 [7,5,2]
defFloat = simpleFloat
defMstSlvs = windowNavigation (named "MstSlvs" (reflectVert (combineTwo (dragPane Horizontal 0.7 0.3) defGrid defFull)))
defThreeFocus = windowNavigation (named "ThreeFocus" (combineTwo (TwoPane 0.03 0.45) defFull (combineTwo (Mirror (TwoPane 0.03 0.85)) defFull defFull)))
defOneBig = OneBig (0.6) (0.74)

-- Layout groups
defRefLayouts = defTabbed ||| defTiled ||| defOneBig
defWflowLayouts = defOneBig ||| defTiled
defWflowenvsLayouts = defTabbed ||| defOneBig ||| defTiled
defWflowsuppLayouts = defWflowLayouts
defNotesLayouts = defAccordion ||| defGrid ||| defTiled
defElabLayouts = defOneBig ||| defTiled ||| defTabbed
defSocialLayouts = defGrid ||| defAccordion ||| defOneBig
defMgmtLayouts = defGrid ||| defTiled ||| defAccordion ||| defTabbed
defTmpLayouts = defGrid ||| defOneBig ||| defTabbed ||| defAccordion
defStdLayouts = defOneBig ||| defTabbed ||| defAccordion ||| defGrid ||| defTiled

 -- Keybindings
defScratchpadKeymap =
  [ ("M-a t", namedScratchpadAction defScratchpads "term")
  , ("M-a r", namedScratchpadAction defScratchpads "irb")
  , ("M-a i", namedScratchpadAction defScratchpads "top")
  , ("M-a h", namedScratchpadAction defScratchpads "ghci")
  , ("M-a w", workspacePrompt defXPConfig (windows . W.greedyView))
  , ("M-a M-w", workspacePrompt defXPConfig (windows . W.shift))
  , ("M-a m", manPrompt defXPConfig)
  , ("M-a d", changeDir defXPConfig)
  , ("M-a n", appendFilePrompt defXPConfig defNotesFile)
  , ("M-a s", sshPrompt defXPConfig)
  ]

-- Mouse bindings
defMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList
  [ ((mod4Mask, button1), \w -> focus w >> mouseMoveWindow w >> snapMagicMove (Just 50) (Just 50) w)
  , ((mod4Mask .|. shiftMask, button1), \w -> focus w >> mouseMoveWindow w >> snapMagicResize [L,R,U,D] (Just 50) (Just 50) w)
  , ((mod4Mask, button3), \w -> focus w >> mouseResizeWindow w >> snapMagicResize [R,D] (Just 50) (Just 50) w)
  ]

-- Statusbars
defStatusBarPP h = defaultPP
  { ppCurrent = uncurry dzenColor defCurCols . uncurry wrap defCurW
  , ppVisible = uncurry dzenColor defVisCols . uncurry wrap defVisW
  , ppHidden = uncurry dzenColor defHidCols . uncurry wrap defHidW
  , ppHiddenNoWindows = uncurry dzenColor defEmpCols . uncurry wrap defEmpW
  , ppUrgent = uncurry dzenColor defUrgCols . uncurry wrap defUrgW
  , ppSep = "  "
  , ppWsSep = " "
  , ppTitle = take 0
  , ppLayout = uncurry dzenColor defLayCols . uncurry wrap defLayW . take 16 . (++ repeat ' ')
  , ppOutput = hPutStrLn h
  }
defStatusBarCmd screenWidth = "dzen2 -bg '" ++ defBgColor ++ "' -x 0 -y 0 -h 20 -w " ++ (show $ screenWidth * 0.8)

-- XP options
defXPConfig = defaultXPConfig
  { font = "xft:" ++ defFont
  , fgColor = defFgColor
  , bgColor = defBgColor
  , fgHLight = defGrey_125
  , bgHLight = defBgColor
  , borderColor = defFgColor
  , position = Top
  }

-- Manage hooks
defManageHook = basicManageHook <+> manageHook defaultConfig
basicManageHook = manageDocks <+> composeAll
  [ isFullscreen --> doFullFloat
  , className =? "Arandr" --> doFloat
  , className =? "Pavucontrol" --> doFloat
  , className =? "Nm-connection-editor" --> doFloat
  , namedScratchpadManageHook defScratchpads
  ]

-- Startup hook
defStartupHook = startupHook defaultConfig >> setWMName "LG3D"

-- Log hook
defLogHook h = takeTopFocus >> (dynamicLogWithPP $ defStatusBarPP h)

-- Xmonad params
defXmonadParams = defaultConfig{ layoutHook = Layout (layoutHook defaultConfig) }

-- Define the screen size
--screenWidth :: Int -> IO Double
getScreenWidth s = do
  dsp <- openDisplay ""
  mss <- xineramaQueryScreens dsp
  return $ case mss of
    Nothing -> 0
    Just [] -> 0
    Just ss -> if s >= 0 && s < length ss -- prevent bad index
      then fromIntegral . xsi_width $ ss !! s else 0
getScreenNumber = 0

-- Main
main = do

  screenWidth <- getScreenWidth getScreenNumber

  defStatusBarHandle <- spawnPipe (defStatusBarCmd screenWidth)

  xmonad $ withUrgencyHook NoUrgencyHook $ defXmonadParams
    { manageHook = defManageHook
    , layoutHook = defLayoutHook
    , logHook = defLogHook defStatusBarHandle
    , modMask = mod4Mask
    , focusFollowsMouse = True
    , terminal = defTerminal
    , workspaces = defWsLbls
    , borderWidth = 2
    , normalBorderColor = defInactiveBorderColor
    , focusedBorderColor = defActiveBorderColor
    , startupHook = defStartupHook
    , mouseBindings = defMouseBindings
    }
    `additionalKeys`
    [ ((mod4Mask,                               xK_grave), spawn "gmrun")
    , ((mod4Mask                 .|. mod1Mask,  xK_v), spawn "pavucontrol")
    , ((mod4Mask                 .|. mod1Mask,  xK_m), spawn "urxvtc -e ncmpcpp")
    , ((mod4Mask,                               xK_s), SM.submap $ searchEngineMap $ S.promptSearch defXPConfig)
    , ((mod4Mask                 .|. shiftMask, xK_s), SM.submap $ searchEngineMap S.selectSearch)
    , ((mod4Mask,                               xK_Left), withFocused $ snapMove L Nothing)
    , ((mod4Mask,                               xK_Right), withFocused $ snapMove R Nothing)
    , ((mod4Mask,                               xK_Up), withFocused $ snapMove U Nothing)
    , ((mod4Mask,                               xK_Down), withFocused $ snapMove D Nothing)
    , ((mod4Mask                 .|. mod1Mask,  xK_t), sinkAll)
    , ((mod4Mask                 .|. mod1Mask,  xK_Left), withFocused $ snapShrink R Nothing)
    , ((mod4Mask                 .|. mod1Mask,  xK_Right), withFocused $ snapGrow R Nothing)
    , ((mod4Mask                 .|. mod1Mask,  xK_Up), withFocused $ snapShrink D Nothing)
    , ((mod4Mask                 .|. mod1Mask,  xK_Down), withFocused $ snapGrow D Nothing)
    , ((mod4Mask .|. controlMask              , xK_s    ), sendMessage  Arrange)
    , ((mod4Mask .|. controlMask .|. shiftMask, xK_s    ), sendMessage  DeArrange)
    , ((mod4Mask .|. controlMask              , xK_Left ), sendMessage (MoveLeft      1))
    , ((mod4Mask .|. controlMask              , xK_Right), sendMessage (MoveRight     1))
    , ((mod4Mask .|. controlMask              , xK_Down ), sendMessage (MoveDown      1))
    , ((mod4Mask .|. controlMask              , xK_Up   ), sendMessage (MoveUp        1))
    , ((mod4Mask                 .|. shiftMask, xK_Left ), sendMessage (IncreaseLeft  1))
    , ((mod4Mask                 .|. shiftMask, xK_Right), sendMessage (IncreaseRight 1))
    , ((mod4Mask                 .|. shiftMask, xK_Down ), sendMessage (IncreaseDown  1))
    , ((mod4Mask                 .|. shiftMask, xK_Up   ), sendMessage (IncreaseUp    1))
    , ((mod4Mask .|. controlMask .|. shiftMask, xK_Left ), sendMessage (DecreaseLeft  1))
    , ((mod4Mask .|. controlMask .|. shiftMask, xK_Right), sendMessage (DecreaseRight 1))
    , ((mod4Mask .|. controlMask .|. shiftMask, xK_Down ), sendMessage (DecreaseDown  1))
    , ((mod4Mask .|. controlMask .|. shiftMask, xK_Up   ), sendMessage (DecreaseUp    1))
    , ((mod4Mask .|. controlMask, xK_plus), sendMessage Mag.MagnifyMore)
    , ((mod4Mask .|. controlMask, xK_minus), sendMessage Mag.MagnifyLess)
    , ((mod4Mask .|. controlMask, xK_o), sendMessage Mag.Toggle)
    , ((mod4Mask, xK_w), goToSelected defaultGSConfig)
    , ((mod4Mask .|. controlMask, xK_space), sendMessage $ Toggle NBFULL)
    , ((mod4Mask .|. controlMask, xK_x), sendMessage $ Toggle REFLECTX)
    , ((mod4Mask .|. controlMask, xK_y), sendMessage $ Toggle REFLECTY)
    , ((mod4Mask .|. controlMask, xK_m), sendMessage $ Toggle MIRROR)
    , ((mod4Mask .|. controlMask, xK_b), sendMessage $ Toggle NOBORDERS)
    ]
    `additionalKeys`
    [((mod4Mask .|. controlMask, k), windows $ swapWithCurrent i) | (i, k) <- zip defWsLbls [xK_1 ..]]
    `additionalKeysP`
    defScratchpadKeymap

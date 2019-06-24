import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP, removeKeysP)
import System.IO
import Data.Maybe (isJust)
import qualified XMonad.StackSet as W
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.CycleWS (moveTo, shiftTo, WSType(..), shiftNextScreen, shiftPrevScreen)
import XMonad.Layout.ZoomRow (zoomRow, zoomIn, zoomOut, zoomReset, ZoomMessage(ZoomFullToggle))
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run (safeSpawn, unsafeSpawn, runInTerm, spawnPipe)
import XMonad.Util.SpawnOnce
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers


      -- Layouts modifiers
import XMonad.Layout.PerWorkspace (onWorkspace) 
import XMonad.Layout.Renamed (renamed, Rename(CutWordsLeft, Replace))
import XMonad.Layout.WorkspaceDir
import XMonad.Layout.Spacing (spacing) 
import XMonad.Layout.NoBorders
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import XMonad.Layout.Reflect (reflectVert, reflectHoriz, REFLECTX(..), REFLECTY(..))
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
-- import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
-- import XMonad.Layout.Fullscreen
import XMonad.Hooks.SetWMName (setWMName)

main = do
    xmproc <- spawnPipe "xmobar ~/.xmobar/.xmobarrc"
    xmonad $ docks defaultConfig  
        { borderWidth = 3
        , terminal = "st"
        , normalBorderColor = "#1E2127"
        , focusedBorderColor = "#bbc2cf"
        , focusFollowsMouse  = False
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        --, workspaces         = ["1","2","3","4","5","6"]
        , workspaces = myWorkspaces
        , startupHook = myStartupHook
        , manageHook = manageDocks <+> (isFullscreen --> doFullFloat) <+> manageHook defaultConfig
        , handleEventHook = XMonad.Hooks.EwmhDesktops.fullscreenEventHook
        , layoutHook = myLayoutHook
        , logHook = dynamicLogWithPP xmobarPP
          { ppOutput = hPutStrLn xmproc 
          , ppCurrent = xmobarColor "#ffffff" ""  -- Current workspace in xmobar
                    , ppVisible = xmobarColor "#c3e88d" ""                -- Visible but not current workspace
                    , ppHidden = xmobarColor "#FE3A3A" ""    -- Hidden workspaces in xmobar
                    , ppHiddenNoWindows = xmobarColor "#ABB2BF" ""        -- Hidden workspaces (no windows)
                    , ppTitle = xmobarColor "#d0d0d0" "" . shorten 40     -- Title of active window in xmobar
                    -- , ppSep =  "<fc=#9AEDFE> : </fc>"                     -- Separators in xmobar
                    , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"  -- Urgent workspace

                    -- ppOutput = hPutStrLn xmproc
                    -- ppTitle = xmobarColor "white" "" . shorten 50
                        }
        } `additionalKeysP` myKeys `removeKeysP` remKeys



xmobarEscape = concatMap doubleLts
  where
        doubleLts '<' = "<<"
        doubleLts x   = [x]
        
myWorkspaces :: [String]   
myWorkspaces = clickable . (map xmobarEscape) 
               $ ["\xf2dc", "\xf239", "\xf7b6", "\xf219", "\xf4d3", "\xf4c7"]
  where                                                                      
        clickable l = [ "<action=xdotool key super+" ++ show (n) ++ ">" ++ ws ++ "</action>" |
                      (i,ws) <- zip [1..6] l,                                        
                      let n = i ] 
myKeys =
  [
    ("M-S-q", kill)
  , ("M-C-<Space>", sendMessage NextLayout)
  , ("M-j", windows W.focusDown)
  , ("M-k", windows W.focusUp)
  , ("M-h", windows W.focusMaster)
  , ("M-l", windows W.focusDown)
  , ("M-S-j", windows W.swapDown)
  , ("M-S-k", windows W.swapUp)
  , ("M-S-h", sendMessage Shrink)
  , ("M-S-l", sendMessage Expand)
  , ("M-S-t", withFocused $ windows . W.sink)
  , ("M-.", sendMessage (IncMasterN (-1)))
  , ("M-,", sendMessage (IncMasterN 1))
  , ("M1-S-<Tab>", rotSlavesDown)
  , ("M1-S-<Tab>", rotAllDown)
  , ("M-C-r", spawn "xmonad --recompile; xmonad --restart")
  , ("M-C-l", moveTo Next nonNSP)
  , ("M-C-h", moveTo Prev nonNSP)
  , ("M-m", sendMessage $ Toggle FULL)
  ] where nonNSP = WSIs (return (\ws -> W.tag ws /= "nsp"))
          nonEmptyNonNSP  = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "nsp"))

remKeys = ["M-p", "M-S-p", "M-S-c", "M-<Return>", "M-n", "M-e", "M-w", "M-r"]
  
myLayoutHook = avoidStruts
               $ smartBorders
               $ mkToggle (NOBORDERS ?? FULL ?? EOT)
               $ myLayout
myLayout = tiled ||| Mirror tiled ||| Full
  where

      -- default tiling algorithm partitions the screen into two panes
      tiled = spacing 6 $ Tall nmaster delta ratio
  
    -- The default number of windows in the master pane
      nmaster = 1
  
    -- Default proportion of screen occupied by master pane
      ratio   = 1/2
  
    -- Percent of screen to increment by when resizing panes
      delta   = 3/100
  
myStartupHook = do
    spawnOnce "sxhkd &"                                      
    spawnOnce "nitrogen --restore &"                       
    spawnOnce "compton -b --config ~/.config/compton.conf &"
    spawnOnce "xfce4-power-manager &"
    spawnOnce "pcloud &"
    spawnOnce "copyq &"
    spawnOnce "conky -c ~/conkys/reloj-texto &"
    spawnOnce "emacs --daemon &"
    spawnOnce "/usr/lib/kdeconnectd &"
    spawnOnce "redshift &"
    spawnOnce "xsetroot -cursor_name left_ptr &"
    setWMName "LG3D"


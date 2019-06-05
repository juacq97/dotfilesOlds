import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP, removeKeysP)
import System.IO
import qualified XMonad.StackSet as W
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)

main = do
    xmproc <- spawnPipe "xmobar"

    xmonad $ defaultConfig
  -- Configuraciones rice-tier
        { borderWidth = 3
        , terminal = "st"
        , normalBorderColor = "#1E2127"
        , focusedBorderColor = "#bbc2cf"
        , focusFollowsMouse  = False
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        , workspaces         = ["1","2","3","4","5","6"]

        , manageHook = manageDocks <+> manageHook defaultConfig
        , layoutHook = avoidStruts  $  layoutHook defaultConfig
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "white" "" . shorten 50
                        }
        } `additionalKeysP` myKeys `removeKeysP` remKeys

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
  ]
      
remKeys = ["M-p", "M-S-p", "M-S-c", "M-<Return>", "M-n"]

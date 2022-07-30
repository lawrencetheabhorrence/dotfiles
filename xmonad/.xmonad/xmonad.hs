import qualified Codec.Binary.UTF8.String    as UTF8
import           Control.Monad               (forM_, join)
--import qualified DBus                        as D
--import qualified DBus.Client                 as D
import           Data.Function               (on)
import           Data.List                   (sortBy)
import qualified Data.Map                    as M
import           Data.Maybe                  (isJust)
import           System.IO
import           XMonad
import           XMonad.Actions.CycleWS
import XMonad.Config.Desktop
import           XMonad.Actions.SpawnOn
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.Script
import           XMonad.Hooks.SetWMName
import           XMonad.Layout.Gaps
import           XMonad.Layout.Magnifier
import           XMonad.Layout.NoBorders
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.Spacing
import           XMonad.Layout.Tabbed
import           XMonad.Layout.ThreeColumns
import qualified XMonad.StackSet             as W
import           XMonad.Util.EZConfig        (additionalKeys)
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.NamedWindows    (getName)
import           XMonad.Util.Run             (safeSpawn, spawnPipe)
import           XMonad.Util.Scratchpad
import           XMonad.Util.SpawnOnce
main =  do
        xmonad $ ewmh $ ewmhFullscreen $ docks $ desktopConfig
              { modMask = mod1Mask
              , manageHook = manageDocks <+> myManageHook
              , terminal = myTerminal
              , borderWidth = 0
              , normalBorderColor = "#e6c363"
              , focusedBorderColor = "#448756"
              , layoutHook = gapLayout $ spacingLayout $ smartBorders $ avoidStruts $ ResizableTall 1 (3/100) (1/2) [] ||| Mirror (ResizableTall 1 (3/100) (1/2) []) ||| ThreeColMid 1 (1/4) (1/3) ||| Full
              , workspaces = wsNamesAlt
              , startupHook = do
                  spawn "~/.config/polybar/launch.sh"
                  spawnOnce "feh --bg-fill ~/.xmonad/61f73c1fc85e2.png"
                  spawnOnce "picom --experimental-backend &"
                  spawnOnce "redshift &"
                  spawnOnce "firefox-developer-edition &"
                  spawnOnce "discord &"
                  spawnOnce "fcitx5"
                  spawnOnce "udiskie"
                  setWMName "LG3D"
              } `additionalKeys` myKeyBindings

myTerminal = "alacritty"

ppLayoutFormat ('S':'p':'a':'c':'i':'n':'g':' ':s) = s
ppLayoutFormat s                                   = s

wsNames = ["web", "dev", "chat", "mus", "term", "office", "misc", "game", "pdf"]

wsNamesAlt = ["W", "C"] ++ map show [3..5]

myCurrentWs = xmobarColor "#eb4934" "#ebbd34" . wrap " { " " } "
myHiddenWs = xmobarColor "#668a4c" "#b5ae45" . wrap " [" "] "
myHiddenNoWindowsWs = xmobarColor "#695b43" "#54372d" .wrap "[" "]"

myKeyBindings =
        [ ((mod1Mask, xK_f), spawn "rofi -show drun")
        , ((mod1Mask, xK_p), spawn "rofi-pass")
        , ((mod1Mask, xK_s), spawn "slock")
        , ((mod1Mask, xK_c), scratchpadSpawnActionTerminal "alacritty -t nvim ~/PriorityChecklist.org")
        , ((mod1Mask, xK_Tab), moveTo Next (Not emptyWS))
        , ((mod1Mask .|. shiftMask, xK_Tab), moveTo Prev (Not emptyWS))
        , ((mod1Mask .|. controlMask, xK_Tab), shiftTo Next emptyWS)
        , ((mod1Mask .|. controlMask, xK_g), sendMessage ToggleGaps)
        , ((mod1Mask .|. shiftMask, xK_n), moveTo Next emptyWS)
        , ((mod1Mask, xK_Print), spawn "spectacle")
        , ((mod1Mask, xK_i), sendMessage MirrorShrink)
        , ((mod1Mask, xK_u), sendMessage MirrorExpand)
        , ((0, 0x1008ff12), spawn "amixer set Master toggle")
        , ((0, 0x1008ff11), spawn "amixer -q sset Master 2%-")
        , ((0, 0x1008ff13), spawn "amixer -q sset Master 2%+")
        , ((mod1Mask .|. controlMask              , xK_0 ), sendMessage MagnifyMore)
        , ((mod1Mask .|. controlMask              , xK_minus), sendMessage MagnifyLess)
        , ((mod1Mask .|. controlMask              , xK_o    ), sendMessage ToggleOff  )
        , ((mod1Mask .|. controlMask .|. shiftMask, xK_o    ), sendMessage ToggleOn   )
        , ((mod1Mask .|. controlMask              , xK_m    ), sendMessage Toggle     )
        , ((mod1Mask .|. controlMask, xK_g), sendMessage ToggleGaps )
        , ((mod1Mask .|. shiftMask, xK_t), namedScratchpadAction myScratchPads "terminal")
        , ((mod1Mask .|. shiftMask, xK_p), namedScratchpadAction myScratchPads "priority checklist")
        ]
          where nonNSP          = WSIs (return (\ws -> W.tag ws /= "NSP"))
                nonEmptyNonNSP  = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "NSP"))


spacingLayout = spacingRaw True (Border 0 5 5 5) True (Border 0 5 5 5) True

gapLayout = gaps [(U, 40), (R, 5), (L, 5), (D, 5)]

myManageHook = (composeAll . concat $
        [ [ className   =? c --> doFloat           | c <- myFloats]
        , [ className   =? c --> doF (W.shift (head wsNamesAlt)) | c <- ["firefox-developer-edition"]]
        , [ className   =? c --> doF (W.shift (wsNamesAlt !! 1)) | c <- ["discord"]]
        , [ className   =? c --> doF (W.shift (wsNamesAlt !! 4)) | c <- ["spotify", "vlc", "mpv"]]
        , [ className   =? c --> doF (W.shift (wsNamesAlt !! 9)) | c <- ["zathura"]]
        ]) <+> namedScratchpadManageHook myScratchPads
        where
                myFloats = ["confirm", "file_progress", "dialog", "download", "error", "notification", "splash"]

myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm
                , NS "priority checklist" spawnChecklist findChecklist manageChecklist
                ]
                  where
                    spawnTerm = myTerminal ++ " -t scratchpad"
                    findTerm = title =? "scratchpad"
                    manageTerm = customFloating $ W.RationalRect l t w h
                      where
                        h = 0.9
                        w = 0.9
                        t = 0.95 -h
                        l = 0.95 -w
                    spawnChecklist = myTerminal ++ " -t PriorityChecklist -e nvim ~/PriorityChecklist.org"
                    findChecklist = title =? "PriorityChecklist"
                    manageChecklist = customFloating $ W.RationalRect l t w h
                      where
                        h = 0.9
                        w = 0.9
                        t = 0.95 -h
                        l = 0.95 -w

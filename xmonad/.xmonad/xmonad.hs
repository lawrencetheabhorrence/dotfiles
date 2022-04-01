import qualified Codec.Binary.UTF8.String    as UTF8
import           Control.Monad               (forM_, join)
import qualified DBus                        as D
import qualified DBus.Client                 as D
import           Data.Function               (on)
import           Data.List                   (sortBy)
import qualified Data.Map                    as M
import           Data.Maybe                  (isJust)
import           System.IO
import           XMonad
import           XMonad.Actions.CycleWS
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
        dbus <- D.connectSession
        D.requestName dbus (D.busName_ "org.xmonad.Log") [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
        xmonad $ ewmh $ defaultConfig
              { modMask = mod1Mask
              , manageHook = manageDocks <+> myManageHook
              , terminal = myTerminal
              , borderWidth = 0
              , normalBorderColor = "#e6c363"
              , focusedBorderColor = "#448756"
              , layoutHook = gapLayout $ spacingLayout $ smartBorders $ avoidStruts $ ResizableTall 1 (3/100) (1/2) [] ||| Mirror (ResizableTall 1 (3/100) (1/2) []) ||| ThreeColMid 1 (1/4) (1/3) ||| Full
              , workspaces = wsNamesAlt
              , handleEventHook = docksEventHook <+> fullscreenEventHook
              , startupHook = do
                  spawn "~/.config/polybar/launch.sh"
                  spawnOnce "feh --bg-fill ~/.xmonad/61f73c1fc85e2.png"
                  spawnOnce "picom --experimental-backend &"
                  spawnOnce "redshift &"
                  spawnOnce "vivaldi-stable &"
                  spawnOnce "discord &"
                  spawnOnce "fcitx5"
                  spawnOnce "udiskie"
                  setWMName "LG3D"
              , logHook = dynamicLogWithPP (myLogHook dbus)
              } `additionalKeys` myKeyBindings

myTerminal = "alacritty"

ppLayoutFormat ('S':'p':'a':'c':'i':'n':'g':' ':s) = s
ppLayoutFormat s                                   = s

myLogHook dbus = def { ppOutput = dbusOutput dbus
                     , ppOrder = \(ws:l:_:_) -> [l, ws]
                     , ppCurrent = myCurrentWs
                     , ppHidden = myHiddenWs
                     , ppHiddenNoWindows = myHiddenNoWindowsWs
                     , ppWsSep = " "
                     , ppSep = " "
                     }

dbusOutput :: D.Client -> String -> IO()
dbusOutput dbus str = do
  let signal = (D.signal objectPath interfaceName memberName) {D.signalBody = [D.toVariant $ UTF8.decodeString str]}
  D.emit dbus signal
    where
      objectPath = D.objectPath_ "org/xmonad/Log"
      interfaceName = D.interfaceName_ "org.xmonad.Log"
      memberName = D.memberName_ "Update"

eventLogHook = do
   winset <- gets windowset
   title <- maybe (return "") (fmap show. getName) . W.peek $ winset
   let currWs = W.currentTag winset
   let wss = map W.tag $ W.workspaces winset
   let wsStr = join $ map (fmt currWs) $ sort' wss

   io $ appendFile "/tmp/.xmonad-title-log" (title ++ "\n")
   io $ appendFile "/tmp/.xmonad-workspace-log" (wsStr ++ "\n")

   where fmt currWs ws
           | currWs == ws = "[" ++ ws ++ "]"
           | otherwise = " " ++ ws ++ " "
         sort' = sortBy (compare `on` (!! 0))

wsNames = ["web", "dev", "chat", "mus", "term", "office", "misc", "game", "pdf"]

wsNamesAlt = ["W", "C"] ++ map show [3..5]

dmenuCommand = "dmenu -i -nb '#54372d' -nf '#ede0ca' -sb '#c2452f' -sf '#dec5c1' -fn 'Iosevka NF-10'"

myCurrentWs = xmobarColor "#eb4934" "#ebbd34" . wrap " { " " } "
myHiddenWs = xmobarColor "#668a4c" "#b5ae45" . wrap " [" "] "
myHiddenNoWindowsWs = xmobarColor "#695b43" "#54372d" .wrap "[" "]"

myKeyBindings =
        [ ((mod1Mask, xK_f), spawn "rofi -show drun")
        , ((mod1Mask, xK_p), spawn "rofi-pass")
        , ((mod1Mask, xK_s), spawn "slock")
        , ((mod1Mask, xK_c), scratchpadSpawnActionTerminal "alacritty -t nvim ~/PriorityChecklist.org")
        , ((mod1Mask, xK_Tab), moveTo Next NonEmptyWS)
        , ((mod1Mask .|. shiftMask, xK_Tab), moveTo Prev NonEmptyWS)
        , ((mod1Mask .|. controlMask, xK_Tab), shiftTo Next EmptyWS)
        , ((mod1Mask .|. controlMask, xK_g), sendMessage ToggleGaps)
        , ((mod1Mask .|. shiftMask, xK_n), moveTo Next EmptyWS)
        , ((mod1Mask, xK_Print), spawn "spectacle")
        , ((mod1Mask, xK_i), sendMessage MirrorShrink)
        , ((mod1Mask, xK_u), sendMessage MirrorExpand)
        , ((mod1Mask .|. controlMask, xK_Print), spawn "scrot window_%Y-%m-%d-%H-%M-%S.png -d 1-u")
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

gapLayout = gaps [(U, 40), (R, 15), (L, 15), (D, 5)]

myManageHook = (composeAll . concat $
        [ [ className   =? c --> doFloat           | c <- myFloats]
        , [ className   =? c --> doF (W.shift (head wsNamesAlt)) | c <- ["vivaldi-stable"]]
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

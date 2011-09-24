-------------------------------------------------------------------------------
-- |
-- Module      :  xmonad.hs
-- Copyright   :  (c) Patrick Brisbin 2010 
-- License     :  as-is
--
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- Up to date [testing], using -darcs, last modified 25 August 2010.
--
-- vim:foldmethod=marker foldmarker={{{,}}}
-------------------------------------------------------------------------------

-- Imports {{{

-- my lib
import Dzen           -- http://pbrisbin.com/xmonad/docs/Dzen.html
import RssReader      -- http://pbrisbin.com/xmonad/docs/RssReader.html
import ScratchPadKeys -- http://pbrisbin.com/xmonad/docs/ScratchPadKeys.html

-- xmonad
import XMonad hiding ((|||))
import qualified XMonad.StackSet as W

-- xmonad-contrib
import XMonad.Actions.CycleWS            (toggleWS)
import XMonad.Actions.FindEmptyWorkspace (tagToEmptyWorkspace, viewEmptyWorkspace)
import XMonad.Actions.WithAll            (killAll)
import XMonad.Actions.UpdatePointer      (PointerPosition(..), updatePointer)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops         (ewmh)
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Fullscreen
import XMonad.Layout.IM                  (Property(..), withIM)
import XMonad.Layout.LayoutCombinators   ((|||), JumpToLayout(..))
import XMonad.Layout.LayoutHints         (layoutHintsWithPlacement)
import XMonad.Layout.NoBorders           (Ambiguity(..), With(..), lessBorders)
import XMonad.Layout.PerWorkspace        (onWorkspace)
import XMonad.Layout.ResizableTile       (ResizableTall(..), MirrorResize(..))
import XMonad.Util.EZConfig              (additionalKeysP)
import XMonad.Util.Loggers               (Logger, maildirNew, dzenColorL, wrapL, shortenL)
import XMonad.Util.Run                   (spawnPipe)
import XMonad.Util.WindowProperties      (getProp32s)
import XMonad.Util.WorkspaceCompare      (getSortByXineramaRule)
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Layout.LimitWindows
import XMonad.Actions.RotSlaves
import XMonad.Actions.PerWorkspaceKeys

-- haskell stuff
import Data.List      (isPrefixOf)
import System.IO      (Handle, hPutStrLn, hGetContents)
import System.Process (runInteractiveCommand)

-- }}}

-- Main {{{
main :: IO ()
main = do
    d <- spawnDzen myLeftBar

    spawn "conky"
    spawn $ "conky -c ~/.dzen_conkyrc | " ++ show myRightBar
    spawnDzen myRssBar >>= spawnReader myReaderConf

    -- ewmh just makes wmctrl work
    xmonad $ ewmh $ withUrgencyHookC myUrgencyHook myUrgencyConfig $ defaultConfig
        { 
          modMask = mod4Mask
        , terminal           = myTerminal
        , workspaces         = myWorkspaces
        , borderWidth        = myBorderWidth
        , normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , layoutHook         = myLayout
        , manageHook         = myManageHook
        , handleEventHook    = fullscreenEventHook
        , logHook            = myLogHook d
        } `additionalKeysP` myKeys

-- }}}

-- Options {{{
myTerminal           = "urxvtc"
myBorderWidth        = 1
myNormalBorderColor  = colorFG
myFocusedBorderColor = colorFG4
-- if you change workspace names, be sure to update them throughout
myWorkspaces = ["1-main","2-code","3-web","4-chat"] ++ map show [5..9]

-- aur/dzen2-svn is required for an xft font
myFont = "Verdana-8"

-- background/foreground and various levels of emphasis
colorBG  = "#303030"
colorFG  = "#606060"
colorFG2 = "#909090"
colorFG3 = "#c4df90"
colorFG4 = "#cc896d"
colorFG5 = "#c4df90"
colorFG6 = "#ffffba"

-- status bar sizes
leftBarWidth  = 520
rssBarWidth   = 1
rightBarWidth = 2940

myXPConfig = defaultXPConfig {
    fgColor  = "white"
  , bgColor  = "black"
  , promptBorderWidth = 0
  , position = Bottom
--  , height   = 25
  }


-- }}}

-- Layouts {{{
--
-- See http://pbrisbin.com:8080/pages/im-layout.html#update
--
myLayout = avoidStruts $ onWorkspace "4-chat" imLayout $ standardLayouts

    where
        -- gajim's roster on left tenth, standardLayouts in the rest
        imLayout = withIM (1/5) (Role "buddy_list") standardLayouts

        standardLayouts = smart $ tiled ||| Mirror tiled ||| full

        tiled = hinted $ ResizableTall 1 (1/100) golden []
        full  = hinted $ Full

        -- golden ratio
        golden = toRational $ 2/(1 + sqrt 5 :: Double)

        -- custom smartBorders for xinerama
        smart = lessBorders $ Combine Union OnlyFloat OtherIndicated

        -- custom hintedTile
        hinted l = layoutHintsWithPlacement (0,0) l

-- }}}

-- ManageHook {{{
myManageHook :: ManageHook
myManageHook = mainManageHook <+> manageDocks <+> manageFullScreen <+> manageScratchPads scratchPadList

    where
        -- the main managehook
        mainManageHook = composeAll $ concat
            [ [ resource  =? r     --> doIgnore         |  r    <- myIgnores ]
            , [ className =? c     --> doShift "3-web"  |  c    <- myWebs    ]
            , [ title     =? t     --> doShift "4-chat" |  t    <- myChats   ]
            , [ className =? c     --> doShift "4-chat" | (c,_) <- myIMs     ]
            , [ className =? c     --> doFloat          |  c    <- myFloats  ]
            , [ className =? c     --> doCenterFloat    |  c    <- myCFloats ]
            , [ name      =? n     --> doCenterFloat    |  n    <- myCNames  ]
            , [ classNotRole (c,r) --> doFloat          | (c,r) <- myIMs     ]
            , [ isDialog           --> doCenterFloat                         ]
            ]

        -- fullscreen but still allow focusing of other WSs
        manageFullScreen = isFullscreen --> doFullFloat

        -- a special query to find an im window that's not my buddy list
        classNotRole :: (String,String) -> Query Bool
        classNotRole (c,r) = className =? c <&&> role /=? r

        role = stringProperty "WM_WINDOW_ROLE"
        name = stringProperty "WM_NAME"

        myIMs     = [("Pidgin","buddy_list")]
        myIgnores = ["desktop","desktop_window"]
        myChats   = ["irssi","mutt","Pidgin","Buddy List","buddy_list"]
        myWebs    = ["Google-chrome","Chrome","Uzbl","Uzbl-core","Jumanji","firefox","Chromium"]
        myFloats  = ["MPlayer","Zenity","VirtualBox","Gimp","gimp","simple.py","rdesktop"]
        myCFloats = ["Xmessage","Save As...","XFontSel"]
        myCNames  = ["bashrun"]

-- }}}

-- StatusBars {{{
--
-- See http://pbrisbin.com:8080/xmonad/docs/Dzen.html
--
myLeftBar :: DzenConf
myLeftBar = defaultDzen
    { width       = leftBarWidth
    , Dzen.font        = myFont
    , fg_color    = colorFG
    , bg_color    = colorBG
    }

myRssBar :: DzenConf
myRssBar = myLeftBar
    { x_position = leftBarWidth
    , width      = rssBarWidth
    }

myRightBar :: DzenConf
myRightBar = myLeftBar
    { x_position = leftBarWidth + rssBarWidth
    , width      = rightBarWidth
    , alignment  = RightAlign
    }

-- }}}

-- RssReader {{{
--
-- See http://pbrisbin.com:8080/xmonad/docs/RssReader.html
--
myReaderConf :: ReaderConf
myReaderConf = defaultReaderConf
    -- stealing some dynamicLog functions here:
    { titleFormat = wrap "" ":" . dzenColor "#909090" "" . dzenEscape
    , descrFormat = dzenEscape . shorten 200
    }

-- }}}

-- LogHook {{{
--
-- todo: refactor this
--
myLogHook :: Handle -> X ()
myLogHook h = do 
    dynamicLogWithPP $ defaultPP
        { ppCurrent         = dzenFG colorFG5 . pad
        , ppVisible         = dzenFG colorFG2 . pad
        , ppHidden          = dzenFG colorFG2 . noScratchPad
        , ppHiddenNoWindows = namedOnly
        , ppUrgent          = dzenFG colorFG4 . pad . dzenStrip
        , ppSep             = replicate 4 ' '
        , ppWsSep           = []
        , ppTitle           = shorten 100 
        , ppLayout          = dzenFG colorFG2 . renameLayouts . stripIM
        , ppSort            = getSortByXineramaRule
        -- myUpdates is too slow. when webkit starts throwing X events
        -- like it's on crack, the whole WM becomes unusable for a few
        -- seconds. feel free to use it if you don't run one of them
        -- new-fangled minimal webkit browsers :)
        --, ppExtras          = [myMail] --, myUpdates]
        , ppOutput          = hPutStrLn h
        }

    updatePointer (Relative 0.95 0.95)

    where

        namedOnly    ws = if any (`elem` ws) ['a'..'z'] then pad ws else ""
        noScratchPad ws = if ws /= "NSP"                then pad ws else ""

        -- L needed for loggers
        dzenFG  c = dzenColor  c ""
        dzenFGL c = dzenColorL c "" 

        -- custom loggers
        --myMail    = wrapL "Mail: "    "" . dzenFGL colorFG6 $ maildirNew "/home/pankajm/Mail/GMail/INBOX"
        myUpdates = wrapL "Updates: " "" . dzenFGL colorFG6 $ countOutputLines "pacman -Qu"
        
        countOutputLines :: String -> Logger
        countOutputLines c = io $ do
            (_, out, _, _) <- runInteractiveCommand c
            (doCount out) `catch` (const $ return Nothing)
        
            where
                -- 0 lines returns Nothing
                doCount h = hGetContents h >>= \c ->
                    case length $ lines c of
                        0 -> return Nothing
                        n -> return $ Just $ show n

        renameLayouts s = case s of
            "Hinted ResizableTall"          -> "/ /-/"
            "Mirror Hinted ResizableTall"   -> "/-,-/"
            "Hinted Full"                   -> "/   /"
            _                               -> s

        stripIM s = if "IM " `isPrefixOf` s then drop (length "IM ") s else s


-- }}}

-- SpawnHook {{{
--
-- Spawn any arbitrary command on urgent
--
data MySpawnHook = MySpawnHook String deriving (Read, Show)

instance UrgencyHook MySpawnHook where
    urgencyHook (MySpawnHook s) w = spawn s

myUrgencyHook :: MySpawnHook
myUrgencyHook = MySpawnHook "aplay -q /usr/share/gajim/data/sounds/message2.wav" 

myUrgencyConfig :: UrgencyConfig
myUrgencyConfig = UrgencyConfig OnScreen (Repeatedly 1 30)

-- }}}

-- KeyBindings {{{
myKeys :: [(String, X())]
myKeys = [ ("M-d"                   , spawn "python2 /home/pankajm/dmenu-python/launch.py"   ) -- dmenu app launcher
         , ("M-S-p"                 , spawn "bashrun"    ) -- gmrun replacement
		 , ("M-S-n"                 , spawn "mpd"        )
		 , ("M-S-k"                 , spawn "killall mpd")
		 , ("M-x"					, spawn "urxvtc"     )
		 , ("M-S-d"                 , spawn "eiskaltdcpp")
         , ("M-z"                   , shellPrompt myXPConfig)
         , ("M-w"                   , sendMessage ToggleStruts)
         -- opening apps with Win
         , ("M--"                  , spawn "sudo ~/bin/brightness-down.sh")
         , ("M-="                  , spawn "sudo ~/bin/brightness-up.sh")
--         , ("M-m"                  , myMail             ) -- open mail client
         , ("M-b"                  , myBrowser          ) -- open web client
--         , ("<XF86HomePage>"       , myBroswer          )
         , ("M-e"                  , myEject            ) -- open/close tray 
         , ("<XF86ScreenSaver>"      , spawn "i3lock -d -c 000000") -- W-l to lock screen
         , ("M-l"                    , spawn "xlock -mode matrix") -- W-l to lock screen
         , ("M-i"                  , myIRC              ) -- open/attach IRC client in screen
--         , ("M-r"                  , myTorrents         ) -- open/attach rtorrent in screen 
--		 , ("M-f"					, spawn "pcmanfm"	 )
         -- some custom hotkeys
         , ("M-g"                   , spawn "~/bin/goodsong.sh"   ) -- note current song as 'good'
         , ("M-S-g"                 , spawn "~/bin/goodsong.sh -p") -- play a random 'good' song
--         , ("<Print>"               , spawn "scrot"      ) -- take a screenshot
         
         -- extended workspace navigatio
         , ("M-`"                   , toggleWS           ) -- switch to the most recently viewed ws
         , ("M-<Backspace>"         , focusUrgent        ) -- focus most recently urgent window
         , ("M-S-<Backspace>"       , clearUrgents       ) -- make urgents go away
         , ("M-0"                   , viewEmptyWorkspace ) -- go to next empty workspace
         , ("M-S-0"                 , tagToEmptyWorkspace) -- send window to empty workspace and view it

         -- extended window movements
         , ("M-o"                   , mirrorShrink       ) -- shink slave panes vertically
         , ("M-i"                   , mirrorExpand       ) -- expand slave panes vertically
         , ("M-f"                   , jumpToFull         ) -- jump to full layout

         -- non-standard navigation
         , ("M-<Tab>"               , bindOn [("chat", rotSlavesDown), ("", rotAllDown)]) -- focus left screen
         , ("M-S-<Tab>"             , bindOn [("chat", rotSlavesUp), ("", rotAllUp)]) -- focus rght screen
         , ("M-S-h"                 , shrink             ) -- shrink master (was M-h)
         , ("M-S-l"                 , expand             ) -- expand master (was M-l)

         -- mpd and oss volume
         , ("<XF86AudioPlay>"       , spawn "ncmpcpp toggle" ) -- play/pause mpd
         , ("<XF86AudioStop>"       , spawn "ncmpcpp stop"   ) -- stop mpd
         , ("<XF86AudioPrev>"       , spawn "ncmpcpp prev"   ) -- prev song
         , ("<XF86AudioNext>"       , spawn "ncmpcpp next"   ) -- next song
         , ("<XF86AudioMute>"       , spawn "amixer -q set Master toggle"  ) -- toggle mute
         , ("<XF86AudioLowerVolume>", spawn "amixer -q set Master 1- unmute") -- volume down 
         , ("<XF86AudioRaiseVolume>", spawn "amixer -q set Master 1+ unmute") -- volume up

         -- Mod+ to control MPlayer
         , ("M-<XF86AudioPlay>"     , mplayer "pause"    ) -- play/pause mplayer
         , ("M-<XF86AudioStop>"     , mplayer "stop"     ) -- stop mplayer
         , ("M-<XF86AudioPrev>"     , mplayer "seek -10" ) -- seek back 10s
         , ("M-<XF86AudioNext>"     , mplayer "seek 10"  ) -- seek forward 10s

         -- kill, reconfigure, exit commands
         , ("M-S-c"                 , killAll            ) -- close all windows on this ws
	     , ("M-c"                   , kill               )
         , ("M-q"                   , myRestart          ) -- restart xmonad
         , ("M-S-q"                 , spawn "leave"      ) -- logout menu
        
         -- See http://pbrisbin.com:8080/xmonad/docs/ScratchPadKeys.html
         ] ++ scratchPadKeys scratchPadList

    where

        shrink = sendMessage Shrink
        expand = sendMessage Expand

        mirrorShrink = sendMessage MirrorShrink
        mirrorExpand = sendMessage MirrorExpand

        focusScreen n = screenWorkspace n >>= flip whenJust (windows . W.view)
        jumpToFull    = sendMessage $ JumpToLayout "Hinted Full"

        myBrowser  = spawn "google-chrome"
        myEject    = spawn "eject -T"
--        myMail     = spawn "mutt"

        -- see http://pbrisbin.com:8080/pages/screen_tricks.html
        myIRC      = spawnInScreen "irssi"
--        myTorrents = spawnInScreen "rtorrent"

        spawnInScreen s = spawn $ unwords [ myTerminal, "-title", s, "-e bash -cl", command s ]

            where
                -- a quoted command to pass off to bash -cl
                command s = ("\""++) . (++"\"") $ unwords ["SCREEN_CONF=" ++ s, "screen -S", s, "-R -D", s]

        -- see http://pbrisbin.com:8080/pages/mplayer-control.html
        mplayer s = spawn $ unwords [ "echo", s, "> $HOME/.mplayer_fifo" ]

        -- kill all conky/dzen2 before executing default restart command
        myRestart = spawn $ "for pid in `pgrep conky`; do kill -9 $pid; done && " ++
                            "for pid in `pgrep dzen2`; do kill -9 $pid; done && " ++
                            "xmonad --recompile && xmonad --restart"

-- }}}

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Scratchpad (scratchpadSpawnAction, scratchpadManageHook, scratchpadFilterOutWorkspace)
import qualified XMonad.StackSet as W 
import qualified Data.Map as M
import System.Exit
import Graphics.X11.Xlib
import Graphics.X11.ExtraTypes.XF86
-- actions
import XMonad.Actions.CycleWS
import XMonad.Actions.WindowGo
import qualified XMonad.Actions.Submap as SM
import XMonad.Actions.GridSelect
import XMonad.Util.Run(spawnPipe)
import qualified XMonad.Prompt 		as P
import XMonad.Prompt.Shell
import XMonad.Prompt
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Reflect
import XMonad.Layout.IM
import XMonad.Layout.Tabbed
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Grid
import System.IO

        --TODO:
        -- automatically generated cheatsheet
        -- fix fullscreen games (atm everything has to be borderless fullscreen)
        -- fix environment related bugs (default browser; locale)
        -- kwin style shutdown & restart hotkeys
        -- "Copying (100% of 343.4 MiB) — Dolphin" > doFloat


main = do
    xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
        { manageHook = myManageHook
        , startupHook = myStartupHook
        , handleEventHook = handleEventHook defaultConfig <+> fullscreenEventHook
        , keys = myKeys
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        , terminal = myTerminal
        , workspaces = myWorkspaces
        , focusFollowsMouse = False
        , clickJustFocuses = False  -- "False" makes it also be passed to window
        }

myStartupHook = do
	startupHook defaultConfig
	spawn "feh --bg-fill Pictures/bg/vlcsnap-2015-04-17-03h18m39s0.png"
	spawn "redshift -l 0:26.7"
        
-------------------------------------------------------------------------------
---- Terminal --
myTerminal :: String
myTerminal = "konsole"--"urxvt"
 
-------------------------------------------------------------------------------
-- Keys/Button bindings --
-- modmask
myModMask :: KeyMask
myModMask = mod4Mask        
        
--Workspaces
myWorkspaces :: [WorkspaceId]
myWorkspaces = ["0", "1:notes", "2:chat", "3:browser", "4", "5", "6" ,"7", "8", "9"] 
--

myManageHook :: ManageHook
myManageHook = scratchpadManageHook (W.RationalRect 0.25 0.375 0.5 0.35) <+> ( composeAll . concat $
--myManageHook = composeAll
                [[ isFullscreen --> (doF W.focusDown <+> doFullFloat)
                , className =? "Evernote.exe" --> doShift "1:notes" --doesnt work atm
                , className =? "Firefox" --> doShift "3:browser"
                , className =? "Skype" --> doShift "2:chat"
                , className =? "Pidgin" --> doShift "2:chat"
                ]]
                        )  <+> manageDocks


-- keys
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
        [ ((mod1Mask, xK_Print), spawn "ksnapshot -c")
        , ((0, xK_Print), spawn "ksnapshot")
        --xkill imitating KWin one
        , ((controlMask .|. mod1Mask, xK_Escape), spawn "xkill")
        --, ((mod4Mask, xK_l), spawn "xscreensaver-command -lock")
        -- volume controls
        , ((0, xF86XK_AudioRaiseVolume ), spawn "amixer -q set Master 2dB+")
        , ((0, xF86XK_AudioLowerVolume ), spawn "amixer -q set Master 2dB-")
        , ((0, xF86XK_AudioMute ), spawn "amixer -q -D pulse set Master toggle")
        -- microphone volume controls TODO: testing
        , ((mod5Mask, xK_F12 ), spawn "amixer -q set Capture 2dB+")
        , ((mod5Mask, xK_F11 ), spawn "amixer -q set Capture 2dB-")
        , ((mod5Mask, xF86XK_AudioMute ), spawn "amixer -q -D pulse set Capture toggle")
        --scripts
        , ((controlMask .|. shiftMask, xK_d), spawn "python3 /home/k/Scripts/date_iso_8601.py")
        , ((controlMask .|. shiftMask, xK_f), spawn "python3 /home/k/Scripts/date_personal_logs.py")
        --brightness scripts
        , ((controlMask .|. shiftMask, xK_F4), spawn "/home/k/Scripts/brightness.sh down")
        , ((controlMask .|. shiftMask, xK_F5), spawn "/home/k/Scripts/brightness.sh up")
        --starting programs
        , ((modMask, xK_e), spawn "dolphin")
        , ((modMask .|. mod1Mask, xK_3), runOrRaise "firefox" (className =? "Firefox"))
        , ((modMask .|. mod1Mask, xK_2), runOrRaise "pidgin" (className =? "Pidgin"))
        , ((modMask .|. mod1Mask, xK_1), spawn "playonlinux --run Evernote")
        --the following one didnt work because of className matching
        --, ((modMask .|. mod1Mask, xK_1), runOrRaise "playonlinux --run Evernote" (className =? "Evernote.exe"))
        , ((modMask .|. mod1Mask, xK_4), spawn "ksysguard & konsole -e watch sensors")
        
        
        
        --standard functionality (+ jelly(12gen)'s_xmonad.hs)
        
        , ((modMask, xK_Return), spawn $ XMonad.terminal conf)
        , ((modMask .|. shiftMask, xK_c ), kill)
        , ((mod1Mask, xK_F4 ), kill)
        
        , ((modMask , xK_r), shellPrompt myXPConfig)
 
 
        -- GridSelect
        , ((modMask, xK_s), goToSelected defaultGSConfig)
    
        -- layouts
        , ((modMask, xK_space ), sendMessage NextLayout)
        , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
        , ((modMask, xK_b ), sendMessage ToggleStruts)
    
        -- floating layer stuff
        , ((modMask, xK_t ), withFocused $ windows . W.sink)
    
        -- refresh'
        , ((modMask, xK_n ), refresh)
    
        -- focus
        , ((mod1Mask, xK_Tab ), windows W.focusDown)
        , ((mod1Mask .|. shiftMask, xK_Tab ), windows W.focusUp)
        , ((modMask, xK_j ), windows W.focusDown)
        , ((modMask, xK_k ), windows W.focusUp)
        , ((modMask, xK_m ), windows W.focusMaster)
    
    
        -- swapping
        , ((modMask .|. shiftMask, xK_Return), windows W.swapMaster)
        , ((modMask .|. shiftMask, xK_j ), windows W.swapDown )
        , ((modMask .|. shiftMask, xK_k ), windows W.swapUp )
    
        -- increase or decrease number of windows in the master area
        , ((modMask , xK_comma ), sendMessage (IncMasterN 1))
        , ((modMask , xK_period), sendMessage (IncMasterN (-1)))
    
        -- resizing
        , ((modMask, xK_h ), sendMessage Shrink)
        , ((modMask, xK_l ), sendMessage Expand)
        , ((modMask .|. shiftMask, xK_h ), sendMessage MirrorShrink)
        , ((modMask .|. shiftMask, xK_l ), sendMessage MirrorExpand)
        
        -- scratchpad
        , ((modMask , xK_v), scratchpadSpawnAction defaultConfig  {terminal = myTerminal}) 
        
        -- quit, or restart
        , ((modMask .|. shiftMask, xK_q ), io (exitWith ExitSuccess))
        , ((modMask , xK_q ), restart "xmonad" True)
        ]
        ++
        -- mod-[1..9] %! Switch to workspace N
        -- mod-shift-[1..9] %! Move client to workspace N
        [((m .|. modMask, k), windows $ f i)
            | (i, k) <- zip (XMonad.workspaces conf) [xK_0 .. xK_9]
            , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
        
        --idk what the following part is
--         ++
--         -- mod-[w,e] %! switch to twinview screen 1/2
--         -- mod-shift-[w,e] %! move window to screen 1/2
--         [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
--             | (key, sc) <- zip [xK_e, xK_w] [0..]
--             , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
            

-- some nice colors for the prompt windows to match the dzen status bar.
myXPConfig = defaultXPConfig                                    
    { 
	font  = "-*-terminus-*-*-*-*-12-*-*-*-*-*-*-u" 
	,fgColor = "#18f018"
	, bgColor = "#000000"
	, bgHLight    = "#000000"
	, fgHLight    = "#FF0000"
	, position = Top
    }
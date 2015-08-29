open Core.Std
open Xlib
open Keysym_match

let windowTest ~display ~screen ~root ~x ~y =
  let white = xWhitePixel display screen
  and black = xBlackPixel display screen in
  let w = xCreateSimpleWindow display root x y 100 100 1
                              white black in
  begin
    xMapWindow display w;
    xFlush display;
  end
;;

let () =
  let display = xOpenDisplay "" in
  let screen = xDefaultScreen display in
  let root = xRootWindow display screen in
  let event_masks = [KeyPressMask; ButtonPressMask; SubstructureNotifyMask;
                     StructureNotifyMask; EnterWindowMask; LeaveWindowMask;
                     ButtonReleaseMask; PointerMotionMask] in
  xSelectInput display root event_masks;
  let ev = new_xEvent() in
  let win_none = (Obj.magic () : window) in
  let none = (Obj.magic 0 : xButtonEvent_contents) in
  let attr = ref (Obj.magic 0 : xWindowAttributes) in
  let start = ref none in
  while true do
    xNextEvent display ev;
    match xEventKind ev with
    | XKeyPressedEvent ev -> let xkey = xKeyEvent_datas ev in
                             let keysym = xLookupKeysym ev 0 in
                             (* TODO this compare is fishy *)
                             if win_none <> xkey.key_subwindow &&
                                (List.exists xkey.key_state (fun m -> m = Mod1Mask)) then
                               ( match keysym_var keysym with
                                 | XK_Escape -> exit 0;
                                 | XK_m -> windowTest display screen root 600 320;
                                 | XK_r -> xRaiseWindow display xkey.key_subwindow;
                                 | XK_n -> xSelectInput display root
                                             (SubstructureRedirectMask :: event_masks);
                                 | _ -> () )
    | XButtonPressedEvent ev -> let xbutton = xButtonEvent_datas ev in
                                if win_none <> xbutton.button_subwindow then
                                  begin
                                    attr := xGetWindowAttributes display
                                              xbutton.button_subwindow;
                                    start := xbutton;
                                  end;
    | XMotionEvent ev -> let xmotion = xMotionEvent_datas ev in
                         if !start <> none &&
                            win_none <> !start.button_subwindow &&
                            (List.exists xmotion.motion_state (fun m -> m = Mod1Mask)) then
                           let xdiff = xmotion.motion_x_root - !start.button_x_root in
                           let ydiff = xmotion.motion_y_root - !start.button_y_root in
                           ( match !start.button with
                             | Button1 -> xMoveWindow display !start.button_subwindow
                                             ((xWindowAttributes_x !attr) + xdiff)
                                             ((xWindowAttributes_y !attr) + ydiff);
                             | Button3 -> xResizeWindow display !start.button_subwindow
                                             (Int.max 1 ((xWindowAttributes_width !attr) + xdiff))
                                             (Int.max 1 ((xWindowAttributes_height !attr) + ydiff));
                             | _ -> () )
    | XButtonReleasedEvent ev -> start := none;
    | _ -> ()
  done;
  xCloseDisplay display;
;;

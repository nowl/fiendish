-- simple event handler that just sets up the keyboard state

function handle_event(etype, keycode, keymod)
   if etype == core.SDL_KEYDOWN then
      Keyboard:press(keycode, keymod)
   elseif etype == core.SDL_KEYUP then
      Keyboard:release(keycode)
   end

   new_events(Keyboard:get_new_unhandled())
end

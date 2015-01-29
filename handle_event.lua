-- simple event handler that just sets up the keyboard state

function handle_event(etype, keycode, keymod)
   if etype == core.SDL_KEYDOWN then
      Keyboard:press(keycode, keymod)
   elseif etype == core.SDL_KEYUP then
      Keyboard:release(keycode)
   end

   if keycode == core.SDLK_u and bit.band(keymod, core.KMOD_CTRL) ~= 0 then
      print(core.rand_double())
   end

   if etype == core.SDL_KEYDOWN and keycode == core.SDLK_ESCAPE then
      core.end_game()
   end

   if etype == core.SDL_KEYDOWN and keycode == core.SDLK_k and bit.band(keymod, core.KMOD_CTRL) ~= 0 then
      Keyboard:print()

      print (Keyboard:get_new_unhandled())
      pretty.dump(Keyboard:get_pressed())

      putchar(0, 0, '@', 0, core.rand_double(), 1.0, 0, 0, 0)
   end
end


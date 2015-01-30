dofile 'colors.lua'
dofile 'keyboard.lua'
dofile 'handle_event.lua'
dofile 'draw.lua'
dofile 'text.lua'

function update()
   --print 'update'
   --require 'pl.pretty'.dump(result)
   local result = compile_text('Testing {[chsv 240 .8 .8]}hello resetting color now{[creset]}. Let\'s see if we can get this to go over a single line!')
   putstring(result, 10, 10, 40, 15)

   new_events(Keyboard:get_pressed())
end

core.register_event_handler(handle_event)
core.register_update_handler(update)

function new_events(keys)
   for _, k in ipairs(keys) do
      if k == core.SDLK_u and Keyboard:control_pressed(k) then
         print(core.rand_double())
      end

      if k == core.SDLK_ESCAPE then
         core.end_game()
      end

      if k == core.SDLK_k and Keyboard:control_pressed(k) then
         --Keyboard:print()
         
         --print (Keyboard:get_new_unhandled())
         --pretty.dump(Keyboard:get_pressed())
         
         putchar(0, 0, '@', 0, core.rand_double(), 1.0, 0, 0, 0)
      end
   end
end

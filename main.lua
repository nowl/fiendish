dofile 'keyboard.lua'
dofile 'handle_event.lua'
dofile 'draw.lua'
dofile 'text.lua'

local result = compile_text('Testing {[chsv 120 .75 .8]}hello{[creset]} resetting color. Let\'s see if we can get this to go over a single line!')

function update()
   --print 'update'
   --require 'pl.pretty'.dump(result)
   putstring(result, 10, 10, 30, 15)
end

core.register_event_handler(handle_event)
core.register_update_handler(update)

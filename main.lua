dofile 'keyboard.lua'
dofile 'handle_event.lua'
local draw = require 'draw'
local text = require 'text'

local result = text.compile_text('Testing {[chsv 120 .75 .8]}hello{[creset]} resetting color. Let\'s see if we can get this to go over a single line!!!!')

function update()
   --print 'update'
   --require 'pl.pretty'.dump(result)
   draw.putstring(result, 10, 20, 40, 15)
   draw.hex_chart()
end

core.register_event_handler(handle_event)
core.register_update_handler(update)

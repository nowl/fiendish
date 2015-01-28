dofile 'keyboard.lua'
dofile 'handle_event.lua'

function update()
   print 'update'
end

core.register_event_handler(handle_event)
core.register_update_handler(update)

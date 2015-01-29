-- keyboard state

local INITIAL_KEY_DELAY_MS = 150
local REPEAT_DELAY_MS = 60

Keyboard = {
   keys = {},

   -- sets up a pressed key in the keyboard state
   press = function (self, key)
      -- TODO this probably shouldn't allocate a new table each time
      self.keys[key] = {
         pressed = true,
         time_pressed = core.getticks(),
         handled = false,
      }
   end,

   -- when the user releases a key this controls the key state
   release = function (self, key)
      self.keys[key].pressed = false
   end,

   -- get new key presses to be handled
   get_new_unhandled = function (self)
      local unhandled_keys = {}
      for k,v in pairs(self.keys) do
         if v.handled == false then
            table.insert(unhandled_keys, k)
            v.handled = true
            v.time_handled = core.getticks()
         end
      end
      return unhandled_keys
   end,

   -- get keys that are pressed
   get_pressed = function (self)
      local pressed_keys = {}
      for k,v in pairs(self.keys) do
         if v.pressed then
            local time = core.getticks()
            if v.in_repeat then
               if time - v.time_handled > REPEAT_DELAY_MS then
                  table.insert(pressed_keys, k)
                  v.time_handled = time
               end
            else
               if time - v.time_handled > INITIAL_KEY_DELAY_MS then
                  table.insert(pressed_keys, k)
                  v.in_repeat = true
                  v.time_handled = time
               end
            end
         end
      end
      return pressed_keys
   end,

   -- debugging funtion to display the keystate
   print = function (self)
      --[[
      io.write ('key map', '\n')
      for k,v in pairs(self.keys) do
         io.write ('key: ', k, ' ')
         for k2,v2 in pairs(v) do
            io.write(k2, ':', tostring(v2), ' ')
         end
         print ()
      end
      ]]--
      require 'pl.pretty'.dump(self.keys)
      
   end,                       
}

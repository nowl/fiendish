-- keyboard state

local INITIAL_KEY_DELAY_MS = 200
local REPEAT_DELAY_MS = 60

Keyboard = {
   keys = {},

   -- sets up a pressed key in the keyboard state
   press = function (self, key, keymod)
      if self.keys[key] ~= nil then
         if self.keys[key].pressed == false then
            self.keys[key].pressed = true
            self.keys[key].time_pressed = core.getticks()
            self.keys[key].handled = false
            self.keys[key].keymod = keymod
         end
      else
         self.keys[key] = {
            pressed = true,
            time_pressed = core.getticks(),
            handled = false,
            keymod = keymod,
         }
      end
   end,

   -- when the user releases a key this controls the key state
   release = function (self, key)
      self.keys[key].pressed = false
      self.keys[key].in_repeat = false
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

   -- get keys that are pressed based on initial repeat and continuous
   -- repeat
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

   -- debugging function to display the keystate
   print = function (self)
      require 'pl.pretty'.dump(self.keys)
   end,

   -- is control pressed for the given key?
   control_pressed = function (self, key)
      return bit.band(self.keys[key].keymod, core.KMOD_CTRL) ~= 0
   end,
}

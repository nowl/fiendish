local inotify = require 'inotify'

for k,v in pairs(package.loaded) do
   print(k, v)
end

for k,v in pairs(happy) do
   print(k, v)
end

print(happy.cmwc())
--print(cmwc2())

local object
object = {
   test = function (self)
      print(self.a)
   end,

   set = function(self, a)
      self.a = a
   end,
   
   init = function ()
      local a = {}
      setmetatable(a, {__index = object})
      return a
   end
}

a = object.init()
a:set(6)
b = object.init()
b:set(7)

a:test()
b:test()

local handle = inotify.init({blocking = true})

local wd = handle:addwatch('./', inotify.IN_CREATE)

local events = handle:read()

for n, ev in ipairs(events) do
    print(n .. ' ' .. ev.name .. ' was created or renamed')
end

-- Done automatically on close, I think, but kept to be thorough
handle:rmwatch(wd)

handle:close()

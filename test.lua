for k,v in pairs(package.loaded) do
   print(k, v)
end

function handle_event(etype, keycode, keymod)
   print (etype .. ' ' .. keycode .. ' ' .. keymod)
   print (core.SDL_KEYUP)

   if keycode == core.SDLK_u and bit.band(keymod, core.KMOD_CTRL) ~= 0 then
      print 'pressed u'
   end
end

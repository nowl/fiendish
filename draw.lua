-- basic draw function

function putchar(x, y, c, fgr, fgg, fgb, bgr, bgg, bgb)
   if type(c) == "string" then
      c = string.byte(c)
   end
   core.putchar(x, y, c, fgr, fgg, fgb, bgr, bgg, bgb)
end

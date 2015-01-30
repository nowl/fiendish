function color_from_hsv(h, s, v)
   local c = v * s
   local x = c * (1 - math.abs(math.mod(h / 60, 2) - 1))
   local m = v - c
   
   local hseg = math.floor(h/60)

   if hseg == 0 then return {r=c+m, g=x+m, b=m}
   elseif hseg == 1 then return {r=x+m, g=c+m, b=m}
   elseif hseg == 2 then return {r=m, g=c+m, b=x+m}
   elseif hseg == 3 then return {r=m, g=x+m, b=c+m}
   elseif hseg == 4 then return {r=x+m, g=m, b=c+m}
   elseif hseg == 5 then return {r=c+m, g=m, b=x+m}
   end
end

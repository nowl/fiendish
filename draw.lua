-- basic draw function
function putchar(x, y, c, fgr, fgg, fgb, bgr, bgg, bgb)
   if type(c) == "string" then
      c = string.byte(c)
   end
   core.putchar(x, y, c, fgr, fgg, fgb, bgr, bgg, bgb)
end

local fg_color = {r = 1,
                  g = 1,
                  b = 1}

local bg_color = {r = 1,
                  g = 0,
                  b = 0}

-- draws a line of text or a compiled line of text to the screen
function putstring(str, x, y, xmax, ymax)
   if type(str) == "string" then
      str = compile_text(str)
   end

   local row, col = y, x
   
   for _, token in ipairs(str.tokens) do
      local space_remaining = xmax - col
      if token.type == 'char' then
         local char = string.sub(str.string, token.start, token.stop)
         
         if space_remaining >= token.to_next_break then
            putchar(col, row, char, fg_color.r, fg_color.g, fg_color.b,
                    bg_color.r, bg_color.g, bg_color.b)
            col = col + 1
         elseif is_whitespace(char) then
            row = row + 1
            col = x
         end
      end
   end
end

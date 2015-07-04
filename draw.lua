local _M = {}

local text = require 'text'

-- basic draw function
function _M.putchar(x, y, c, fgr, fgg, fgb, bgr, bgg, bgb)
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
function _M.putstring(str, x, y, xmax, ymax)
   if type(str) == "string" then
      str = compile_text(str)
   end

   local row, col = y, x
   
   for _, token in ipairs(str.tokens) do
      local space_remaining = xmax - col
      if token.type == 'char' then
         local char = string.sub(str.string, token.start, token.stop)
         
         if space_remaining >= token.to_next_break then
            _M.putchar(col, row, char, fg_color.r, fg_color.g, fg_color.b,
                    bg_color.r, bg_color.g, bg_color.b)
            col = col + 1
         elseif text.is_whitespace(char) then
            row = row + 1
            col = x
         end
      end
   end
end

function _M.hex_chart()
   local total = 0
   for y = 0, 3 do
      for i = 0, 63 do	    
	 local hex_val = string.format('%.2X', total)
	 _M.putchar(i, 4*y, string.byte(string.sub(hex_val, 1, -1)), 0,
		 1.0, 1.0, 0, 0, 0)
	 _M.putchar(i, 4*y+1, string.byte(string.sub(hex_val, 2, -1)), 0,
		 1.0, 1.0, 0, 0, 0)
	 _M.putchar(i, 4*y+2, 64*y+i, 0, core.rand_double(), 1.0, 0, 0, 0)
	 total = total + 1
      end
   end
end

return _M

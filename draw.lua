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

local function set_fg_color(r, g, b)
   fg_color.r = r
   fg_color.g = g
   fg_color.b = b
end

local function set_bg_color(r, g, b)
   bg_color.r = r
   bg_color.g = g
   bg_color.b = b
end


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
      elseif token.type == 'command' then
         if token.command_meta.type == 'creset' then
            set_fg_color(1, 1, 1)
            set_bg_color(0, 0, 0)
         elseif token.command_meta.type == 'chsv' then
            local c = color_from_hsv(token.command_meta.h,
                                     token.command_meta.s,
                                     token.command_meta.v)
            set_fg_color(c.r, c.g, c.b)
         end
      end
   end
end

-- move in reverse order and fill in the distance to next break for
-- each character
local function fill_breaks(tokens, init_break_count)
   local break_count = init_break_count
   for i = 1,#tokens do
      local j = #tokens - i + 1
      if tokens[j].to_next_break ~= nil then
         break
      end
      
      tokens[j].to_next_break = break_count

      if tokens[j].type == "char" then
         break_count = break_count + 1
      end
   end
end

-- parses command tokens and compiles each one
local function compile_commands(tokens, str)
   for _, token in ipairs(tokens) do
      if token.type == 'command' then
         command_str = string.sub(str, token.start, token.stop)
         command_tokens = {}
         for t in string.gmatch(command_str, "%S+") do
            table.insert(command_tokens, t)
         end
         
         command_tokens[1] = string.sub(command_tokens[1], 3)
         command_tokens[#command_tokens] = string.sub(command_tokens[#command_tokens], 1, -3)
         
         if command_tokens[1] == 'creset' then
            assert(#command_tokens == 1)
            token.command_meta = {type = 'creset'}
         elseif command_tokens[1] == 'chsv' then
            assert(#command_tokens == 4)
            token.command_meta = {type = 'chsv',
                                  h = tonumber(command_tokens[2]),
                                  s = tonumber(command_tokens[3]),
                                  v = tonumber(command_tokens[4])}
         else
            error(string.format('unknown command "%s" in string "%s"', command_str, str))
         end
      end
   end
end

function is_whitespace(char)
   if char == ' ' or char == '\n' or char == '\t' then
      return true
   else
      return false
   end
end

-- compile text string into text commands
function compile_text(s)
   local command = {
      string = s,
      tokens = {}
   }

   local in_command = false
   local cmd_start = 0
   local i=1
   while i <= #s do
      if not in_command then
         if string.sub(s, i, i) ~= '{' and string.sub(s, i+1, i+1) ~= '[' then
            local token = {
               type = "char",
               start = i,
               stop = i,
            }
            table.insert(command.tokens, token)
            
            local this_char = string.sub(s, i, i)
            if is_whitespace(this_char) then
               fill_breaks(command.tokens, 0)
            end
         else
            in_command = true
            cmd_start = i
         end
      else
         if string.sub(s, i, i) == ']' and string.sub(s, i+1, i+1) == '}' then
            local token = {
               type = "command",
               start = cmd_start,
               stop = i+1,
            }
            table.insert(command.tokens, token)

            in_command = false
            i = i + 2-1
         end
      end

      i = i + 1
   end

   fill_breaks(command.tokens, 1)

   compile_commands(command.tokens, s)

   return command
end

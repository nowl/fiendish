import fiend

class Rendering ():
    def __enter__(self):
        fiend.sdl_init()

    def __exit__(self, type, value, traceback):
        fiend.sdl_destroy()

def get_ticks():
    return fiend.sdl_getticks()

def get_input():
    input, key, mod = fiend.sdl_pollevent()
    return input != 0, chr(key), mod

def draw():
    fiend.sdl_draw()

class Color ():
    r, g, b = 0, 0, 0

    def to_native(self):
        c = fiend.color()
        c.r, c.g, c.b = self.r, self.g, self.b
        return c

def putchar(x, y, char, fg_color, bg_color):
    fiend.sdl_putchar(x, y, ord(char), fg.to_native(), bg.to_native())

# generate dungeon
# gameloop
#   get input
#   process input
#   draw world

x, y = 10, 10
running = True
frame = 0
start_time = get_ticks()
with Rendering():
    while running:
        is_input, key, key_mod = get_input()
        fg = Color()
        bg = Color()
        fg.r, fg.g, fg.b = 255, 255, 255
        bg.r, bg.g, bg.b = 0, 0, 0
        putchar(x, y, '@', fg, bg)
        if is_input:
            if key == fiend.SDLK_ESCAPE:
                running = False
            elif key == fiend.SDLK_l:
                x += 1
            elif key == fiend.SDLK_h:
                x -= 1
            elif key == fiend.SDLK_j:
                y += 1
            elif key == fiend.SDLK_k:
                y -= 1
        draw()
        frame += 1
end_time = get_ticks()    

elapsed_time_s = (end_time-start_time)/1000.0
print 'total frames = %d' % frame
print 'elapsed time = %f' % elapsed_time_s
print 'avg. fps = %f' % (frame/elapsed_time_s)

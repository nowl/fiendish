#include "fiendish.h"

#define GL_GLEXT_PROTOTYPES
#include <GL/gl.h>

#include "data.h"
#include "codepage-437-hex.h"

#define SDL_FLAGS   (SDL_WINDOW_OPENGL | SDL_WINDOW_SHOWN | SDL_WINDOW_MAXIMIZED | SDL_WINDOW_RESIZABLE | SDL_WINDOW_BORDERLESS)
//#define SDL_FLAGS   (SDL_WINDOW_OPENGL | SDL_WINDOW_SHOWN | SDL_WINDOW_RESIZABLE)

static SDL_Window* Window;
static SDL_Renderer* Ren;
static SDL_Texture* Tex;
static SDL_Event Event;
static int Dirty = 1;
static SDL_Rect SrcRect = {0, 0, 9, 16};
static SDL_Rect DstRect = {0, 0, 9, 16};

/* The texture offset for each character to draw on the screen. */
static int SymbolOffsetX[CELLS_HORIZ*CELLS_VERT];
static int SymbolOffsetY[CELLS_HORIZ*CELLS_VERT];

/* The foreground and background colors for each character to draw on
 * the screen. */
static struct color FGColor[CELLS_HORIZ*CELLS_VERT];
static struct color BGColor[CELLS_HORIZ*CELLS_VERT];

#define SOLID_BLOCK_X_OFFSET   ( (219 % 32) * 9 + 8 )
#define SOLID_BLOCK_Y_OFFSET   ( (219 / 32) * 16 + 8 )

static void sdldie(const char *msg)
{
    printf("%s: %s\n", msg, SDL_GetError());
    SDL_Quit();
    exit(1);
}

void sdl_init(void)
{
    // init sdl
    if (SDL_Init(SDL_INIT_VIDEO) < 0)
        sdldie("Unable to initialize SDL");
    
    Window = SDL_CreateWindow(PROGRAM_NAME, SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED, SCREENWIDTH, SCREENHEIGHT, SDL_FLAGS);
    if (!Window)
        sdldie("Unable to create window");

    Ren = SDL_CreateRenderer(Window, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
    if (!Ren)
        sdldie("Unable to create renderer");

    Tex = SDL_CreateTexture(Ren, SDL_PIXELFORMAT_ARGB8888, SDL_TEXTUREACCESS_STATIC, TEXTURE_WIDTH, TEXTURE_HEIGHT);
    if (!Tex)
        sdldie("Unable to create texture");

    /* read in the texture data */
    int i;
    uint8_t *texture_data = malloc(4 * sizeof(uint8_t) * TEXTURE_HEIGHT * TEXTURE_WIDTH);
    for(i=0; i<TEXTURE_WIDTH*TEXTURE_HEIGHT; i++) {
        int index = i / 8;
        int bit_num = 7 - (i % 8);
        int data_block = codepage437_image.pixel_data[index];
        int bit_val = (data_block >> bit_num) & 1;
        int assignment = (bit_val == 1) ? 255 : 0;
        int j;

        for(j=0; j<4; j++)
            texture_data[i*4 + j] = assignment;
    }

    int result = SDL_UpdateTexture(Tex, NULL, texture_data, TEXTURE_WIDTH * 4);
    free(texture_data);

    if(result)
        sdldie("Unable to fill texture data");

    SDL_RenderSetLogicalSize(Ren, WIDTH, HEIGHT);

    SDL_SetTextureAlphaMod(Tex, 255);
    SDL_SetTextureBlendMode(Tex, SDL_BLENDMODE_BLEND);
}

void sdl_destroy(void) {
	SDL_DestroyWindow(Window);
	SDL_Quit();
}

int sdl_getticks(void) {
    return SDL_GetTicks();
}

void sdl_draw(void) {
    if (Dirty) {
        Dirty = 0;

        SDL_RenderClear(Ren);
        
        int x, y;
        for(y=0; y<CELLS_VERT; y++) {
            for(x=0; x<CELLS_HORIZ; x++) {                
                DstRect.x = x * 9;
                DstRect.y = y * 16;

                SrcRect.x = SOLID_BLOCK_X_OFFSET;
                SrcRect.y = SOLID_BLOCK_Y_OFFSET;
                struct color c = BGColor[y*CELLS_HORIZ+x];
                SDL_SetTextureColorMod(Tex, c.r, c.g, c.b);
                SDL_RenderCopy(Ren, Tex, &SrcRect, &DstRect);
                
                SrcRect.x = SymbolOffsetX[y*CELLS_HORIZ+x];
                SrcRect.y = SymbolOffsetY[y*CELLS_HORIZ+x];
                c = FGColor[y*CELLS_HORIZ+x];
                SDL_SetTextureColorMod(Tex, c.r, c.g, c.b);
                SDL_RenderCopy(Ren, Tex, &SrcRect, &DstRect);
            }
        }

        SDL_RenderPresent(Ren);
    }
}

int
sdl_pollevent(int32_t *keycode,
              uint16_t *keymod)
{
    int result = SDL_PollEvent(&Event);
    if (result) {
        if (Event.type != SDL_KEYDOWN && Event.type != SDL_KEYUP)
            result = 0;
        else {
            *keycode = Event.key.keysym.sym;
            *keymod = Event.key.keysym.mod;
            result = Event.type;
        }            
    }
    return result;
}

void
sdl_putchar(int x,
            int y,
            unsigned char c,
            struct color fg,
            struct color bg)
{
    Dirty = 1;
    
    int col = c % 32;
    int row = c / 32;
    SymbolOffsetX[y*CELLS_HORIZ + x] = 9 * col + 8;
    SymbolOffsetY[y*CELLS_HORIZ + x] = 16 * row + 8;
    FGColor[y*CELLS_HORIZ + x] = fg;
    BGColor[y*CELLS_HORIZ + x] = bg;
}

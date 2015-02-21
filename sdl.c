#include <SDL.h>
#include <SDL_image.h>
#include <SDL_ttf.h>

#define SDL_FLAGS   (SDL_WINDOW_OPENGL | SDL_WINDOW_SHOWN | SDL_WINDOW_MAXIMIZED | SDL_WINDOW_RESIZABLE | SDL_WINDOW_BORDERLESS)
//#define SDL_FLAGS   (SDL_WINDOW_OPENGL | SDL_WINDOW_SHOWN | SDL_WINDOW_RESIZABLE)

static SDL_Window* Window;
static SDL_Renderer* Ren;
static SDL_Event Event;
//static int Dirty = 1;
static SDL_Rect SrcRect = {0, 0, 9, 16};
static SDL_Rect DstRect = {0, 0, 9, 16};
static SDL_Texture *Tex;

static void sdldie(const char *msg)
{
    printf("%s: %s\n", msg, SDL_GetError());
    SDL_Quit();
    exit(1);
}

void sdl_init(char *program_name, int width, int height, int logic_width, int logic_height)
{
    // init sdl
    if (SDL_Init(SDL_INIT_VIDEO) < 0)
        sdldie("Unable to initialize SDL");
    
    Window = SDL_CreateWindow(program_name, SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED, width, height, SDL_FLAGS);
    if (!Window)
        sdldie("Unable to create window");

    Ren = SDL_CreateRenderer(Window, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
    if (!Ren)
        sdldie("Unable to create renderer");

    SDL_RenderSetLogicalSize(Ren, logic_width, logic_height);

/*
    int flags = IMG_INIT_PNG;
    int result = IMG_Init(flags);
    if ( (result&flags) != flags)
        sdldie("Problem initializing SDL_image.");        
*/

    if (TTF_Init() == -1)
        sdldie("problem initializing fonts");
}

void sdl_destroy(void) {
    IMG_Quit();
    TTF_Quit();
	SDL_DestroyWindow(Window);
	SDL_Quit();
}

int sdl_getticks(void) {
    return SDL_GetTicks();
}

void sdl_clear(void) {
    //if (Dirty)
    SDL_RenderClear(Ren);
}

void sdl_draw(void) {
    //if (Dirty) {
    //    Dirty = 0;        
    SDL_RenderPresent(Ren);
    //}
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

static void blit_image() {
    SDL_RenderCopy(Ren, Tex, &SrcRect, &DstRect);
}

void set_texture_src(char *imagename) {
    SDL_Surface *image = IMG_Load(imagename);
    if(!image)
        sdldie("problem loading image");
    Tex = SDL_CreateTextureFromSurface(Ren, image);
    SDL_FreeSurface(image);
}

void
sdl_blit(int sx, int sy, int sw, int sh,
         int dx, int dy, int dw, int dh)
{
    //Dirty = 1;
    
    SrcRect.x = sx;
    SrcRect.y = sy;
    SrcRect.w = sw;
    SrcRect.h = sh;
    DstRect.x = dx;
    DstRect.y = dy;
    DstRect.w = dw;
    DstRect.h = dh;

    blit_image();
}

TTF_Font *open_font(char *file, int ptsize) {
    TTF_Font *font = TTF_OpenFont(file, ptsize);
    if(!font)
        sdldie("problem opening font");
    return font;
}

void draw_text(TTF_Font *font,
               char *text,
               int x,
               int y,
               unsigned char r,
               unsigned char g,
               unsigned char b,
               unsigned char a)
{
    //Dirty = 1;

    SDL_Color color = {r, g, b, a};
    
    SDL_Surface *image = TTF_RenderText_Solid(font, text, color);
    //SDL_Surface *image = TTF_RenderText_Blended(font, text, color);
    if(!image)
        sdldie("problem loading image");
    SDL_Texture *texture = SDL_CreateTextureFromSurface(Ren, image);

    DstRect.x = x;
    DstRect.y = y;
    DstRect.w = image->w;
    DstRect.h = image->h;
    SDL_RenderCopy(Ren, texture, NULL, &DstRect);

    SDL_FreeSurface(image);
    SDL_DestroyTexture(texture);
}

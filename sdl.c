#include "fiendish.h"

#define GL_GLEXT_PROTOTYPES
#include <GL/gl.h>

#include "data.h"
#include "codepage-437-hex.h"

#define SDL_FLAGS   (SDL_WINDOW_OPENGL | SDL_WINDOW_SHOWN | SDL_WINDOW_MAXIMIZED | SDL_WINDOW_RESIZABLE | SDL_WINDOW_BORDERLESS)
//#define SDL_FLAGS   (SDL_WINDOW_OPENGL | SDL_WINDOW_SHOWN | SDL_WINDOW_RESIZABLE)

static SDL_Window* Window;
static SDL_GLContext GLContext;
static SDL_Texture* Tex;
static SDL_Event Event;
static int Dirty = 1;
static GLuint Vao, Vbo[4];
static GLuint Vertexshader, Fragmentshader;
static GLuint Shaderprogram;

static GLfloat FGColor[CELLS_HORIZ*CELLS_VERT*3];
static GLfloat BGColor[CELLS_HORIZ*CELLS_VERT*3];
static GLubyte DisplayChar[CELLS_HORIZ*CELLS_VERT];

static void sdldie(const char *msg)
{
    printf("%s: %s\n", msg, SDL_GetError());
    SDL_Quit();
    exit(1);
}

static void setupwindow(void)
{
	// init sdl
    if (SDL_Init(SDL_INIT_VIDEO) < 0)
        sdldie("Unable to initialize SDL");

    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 3);
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 3);

    SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);
    SDL_GL_SetAttribute(SDL_GL_DEPTH_SIZE, 24);

    Window = SDL_CreateWindow(PROGRAM_NAME, SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED, SCREENWIDTH, SCREENHEIGHT, SDL_FLAGS);
    if (!Window)
        sdldie("Unable to create window");

    GLContext = SDL_GL_CreateContext(Window);

    SDL_GL_SetSwapInterval(1);
}

static void setupGL(void)
{
    int i;
    int IsCompiled_VS, IsCompiled_FS;
    int IsLinked;
    int maxLength;
    char *vertexInfoLog;
    char *fragmentInfoLog;
    char *shaderProgramInfoLog;

    GLuint texture;

    GLint major_version, minor_version;
    glGetIntegerv(GL_MAJOR_VERSION, &major_version);
    glGetIntegerv(GL_MINOR_VERSION, &minor_version);
    printf("major GL version: %d\n", major_version);
    printf("minor GL version: %d\n", minor_version);
    if (!(major_version > 3 || (major_version == 3 && minor_version >= 3))) {
        printf("minimum GL version required 3.3\n");
        exit(-1);
    }

    glActiveTexture(GL_TEXTURE0);
    glGenTextures( 1, &texture );
    glBindTexture( GL_TEXTURE_2D, texture );
    glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);

    unsigned char alpha_vals[TEXTURE_WIDTH * TEXTURE_HEIGHT];
    for(i=0; i<codepage437_image.width * codepage437_image.height / 8; i++) {
        unsigned char v = codepage437_image.pixel_data[i];
        
        alpha_vals[i*8] = ((v >> 7) & 1) * 0xff;
        alpha_vals[i*8+1] = ((v >> 6) & 1) * 0xff;
        alpha_vals[i*8+2] = ((v >> 5) & 1) * 0xff;
        alpha_vals[i*8+3] = ((v >> 4) & 1) * 0xff;
        alpha_vals[i*8+4] = ((v >> 3) & 1) * 0xff;
        alpha_vals[i*8+5] = ((v >> 2) & 1) * 0xff;
        alpha_vals[i*8+6] = ((v >> 1) & 1) * 0xff;
        alpha_vals[i*8+7] = (v & 1) * 0xff;
    }

    glTexImage2D(GL_TEXTURE_2D, 0, GL_RED, codepage437_image.width,
                 codepage437_image.height, 0, GL_RED, GL_UNSIGNED_BYTE,
                 alpha_vals);

    const GLfloat block[] = {
        0, 16,
        9, 16,
        9, 0,
        0, 0,
        0, 16,
        9, 0};

    for(i=0; i<CELLS_HORIZ*CELLS_VERT*3; i++) {
        FGColor[i] = 1;
        BGColor[i] = 0;
    }

    for(i=0; i<CELLS_HORIZ*CELLS_VERT; i++) {
        DisplayChar[i] = 0;
    }

    glGenVertexArrays(1, &Vao);
    glBindVertexArray(Vao);
    glGenBuffers(4, Vbo);
    glBindBuffer(GL_ARRAY_BUFFER, Vbo[0]);
    glBufferData(GL_ARRAY_BUFFER, 12 * sizeof(GLfloat), block, GL_STATIC_DRAW);
    glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 0, 0);
    glEnableVertexAttribArray(0);

    glBindBuffer(GL_ARRAY_BUFFER, Vbo[1]);
    glBufferData(GL_ARRAY_BUFFER, CELLS_HORIZ*CELLS_VERT*3 * sizeof(GLfloat), FGColor, GL_DYNAMIC_DRAW);
    glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 0, 0);
    glVertexAttribDivisor(1, 1);
    glEnableVertexAttribArray(1);

    glBindBuffer(GL_ARRAY_BUFFER, Vbo[2]);
    glBufferData(GL_ARRAY_BUFFER, CELLS_HORIZ*CELLS_VERT*3 * sizeof(GLfloat), BGColor, GL_DYNAMIC_DRAW);
    glVertexAttribPointer(2, 3, GL_FLOAT, GL_FALSE, 0, 0);
    glVertexAttribDivisor(2, 1);
    glEnableVertexAttribArray(2);

    glBindBuffer(GL_ARRAY_BUFFER, Vbo[3]);
    glBufferData(GL_ARRAY_BUFFER, CELLS_HORIZ*CELLS_VERT, DisplayChar, GL_DYNAMIC_DRAW);
    glVertexAttribPointer(3, 1, GL_UNSIGNED_BYTE, GL_FALSE, 0, 0);
    glVertexAttribDivisor(3, 1);
    glEnableVertexAttribArray(3);

    Vertexshader = glCreateShader(GL_VERTEX_SHADER);
    GLchar *vertexsourcePtr = vertexsource;
    glShaderSource(Vertexshader, 1, (const GLchar**)&vertexsourcePtr, 0);
    glCompileShader(Vertexshader);
    glGetShaderiv(Vertexshader, GL_COMPILE_STATUS, &IsCompiled_VS);
    if(IsCompiled_VS == FALSE)
    {
       glGetShaderiv(Vertexshader, GL_INFO_LOG_LENGTH, &maxLength);
       vertexInfoLog = (char *)malloc(maxLength);
       glGetShaderInfoLog(Vertexshader, maxLength, &maxLength, vertexInfoLog);
       printf("vertex shader error: %s\n", vertexInfoLog);

       free(vertexInfoLog);
       return;
    }

    Fragmentshader = glCreateShader(GL_FRAGMENT_SHADER);
    GLchar *fragmentsourcePtr = fragmentsource;
    glShaderSource(Fragmentshader, 1, (const GLchar**)&fragmentsourcePtr, 0);
    glCompileShader(Fragmentshader);
    glGetShaderiv(Fragmentshader, GL_COMPILE_STATUS, &IsCompiled_FS);
    if(IsCompiled_FS == FALSE)
    {
       glGetShaderiv(Fragmentshader, GL_INFO_LOG_LENGTH, &maxLength);
       fragmentInfoLog = (char *)malloc(maxLength);
       glGetShaderInfoLog(Fragmentshader, maxLength, &maxLength, fragmentInfoLog);
       printf("fragment shader error: %s\n", fragmentInfoLog);

       free(fragmentInfoLog);
       return;
    }

    Shaderprogram = glCreateProgram();
    glAttachShader(Shaderprogram, Vertexshader);
    glAttachShader(Shaderprogram, Fragmentshader);
    glBindAttribLocation(Shaderprogram, 0, "in_Position");
    glBindAttribLocation(Shaderprogram, 1, "inFG_Color");
    glBindAttribLocation(Shaderprogram, 2, "inBG_Color");
    glBindAttribLocation(Shaderprogram, 3, "in_Char");
    glLinkProgram(Shaderprogram);

    glGetProgramiv(Shaderprogram, GL_LINK_STATUS, (int *)&IsLinked);
    if(IsLinked == FALSE)
    {
       glGetProgramiv(Shaderprogram, GL_INFO_LOG_LENGTH, &maxLength);
       shaderProgramInfoLog = (char *)malloc(maxLength);
       glGetProgramInfoLog(Shaderprogram, maxLength, &maxLength, shaderProgramInfoLog);

       free(shaderProgramInfoLog);
       return;
    }

    glUseProgram(Shaderprogram);

    GLint mvp = glGetUniformLocation(Shaderprogram, "MVP" );

    float r = WIDTH;
    float l = 0;
    float t = HEIGHT;
    float b = 0;
    float f = -1;
    float n = 1;
    float mvp_ptr[] = {2/(r-l), 0, 0, 0,
                       0, 2/(t-b), 0, 0,
                       0, 0, -2/(f-n), 0,
                       -(r+l)/(r-l), -(t+b)/(t-b), -(f+n)/(f-n), 1};

    glUniformMatrix4fv(mvp, 1, GL_FALSE, mvp_ptr);

    GLint height_uf = glGetUniformLocation(Shaderprogram, "CELLS_VERTICAL" );
    glUniform1i(height_uf, CELLS_VERT);

    glActiveTexture(GL_TEXTURE0);
    GLint texture_location = glGetUniformLocation(Shaderprogram, "in_Texture");
    glUniform1i(texture_location, 0);
    glBindTexture(GL_TEXTURE_2D, texture);

    glClearColor(0.0, 0.0, 0.0, 1.0);
    glClear(GL_COLOR_BUFFER_BIT);

    glFrontFace(GL_CW);
    glCullFace(GL_BACK);
    glEnable(GL_CULL_FACE);
}

void sdl_init(void)
{
    setupwindow();
    setupGL();

    SDL_SetTextureAlphaMod(Tex, 255);
    SDL_SetTextureBlendMode(Tex, SDL_BLENDMODE_BLEND);
}

void sdl_destroy(void) {
    glUseProgram(0);
    glDisableVertexAttribArray(0);
    glDisableVertexAttribArray(1);
    glDisableVertexAttribArray(2);
    glDetachShader(Shaderprogram, Vertexshader);
    glDetachShader(Shaderprogram, Fragmentshader);
    glDeleteProgram(Shaderprogram);
    glDeleteShader(Vertexshader);
    glDeleteShader(Fragmentshader);
    glDeleteBuffers(3, Vbo);
    glDeleteVertexArrays(1, &Vao);

	SDL_GL_DeleteContext(GLContext);
	SDL_DestroyWindow(Window);
	SDL_Quit();
}

int sdl_getticks(void) {
    return SDL_GetTicks();
}

void sdl_draw(void) {
    if (Dirty) {
        Dirty = 0;

        glBindBuffer(GL_ARRAY_BUFFER, Vbo[1]);
        glBufferSubData(GL_ARRAY_BUFFER,
                        0,
                        CELLS_HORIZ*CELLS_VERT*3*sizeof(GLfloat),
                        FGColor);
        glBindBuffer(GL_ARRAY_BUFFER, Vbo[2]);
        glBufferSubData(GL_ARRAY_BUFFER,
                        0,
                        CELLS_HORIZ*CELLS_VERT*3*sizeof(GLfloat),
                        BGColor);
        glBindBuffer(GL_ARRAY_BUFFER, Vbo[3]);
        glBufferSubData(GL_ARRAY_BUFFER,
                        0,
                        CELLS_HORIZ*CELLS_VERT,
                        DisplayChar);

        glDrawArraysInstanced(GL_TRIANGLES, 0, 6, CELLS_HORIZ*CELLS_VERT);
        SDL_GL_SwapWindow(Window);
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

    int index = (CELLS_VERT-y-1) + x * CELLS_VERT;
    DisplayChar[index] = c;
    FGColor[index*3] = fg.r;
    FGColor[index*3+1] = fg.g;
    FGColor[index*3+2] = fg.b;
    BGColor[index*3] = bg.r;
    BGColor[index*3+1] = bg.g;
    BGColor[index*3+2] = bg.b;
}

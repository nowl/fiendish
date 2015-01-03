#include <stdlib.h>
#include <stdio.h>
#include <limits.h>

#include "sdl.hpp"

#define GL_GLEXT_PROTOTYPES
#include <GL/gl.h>

#include "data.h"
#include "codepage-437-hex.h"

struct SDL::impl {
	SDL_Window* window;
	SDL_GLContext context;
    GLuint vao, vbo[4];
    GLuint vertexshader, fragmentshader;
    GLuint shaderprogram;
    GLfloat colorFG[CELLS_HORIZ*CELLS_VERT*3];
    GLfloat colorBG[CELLS_HORIZ*CELLS_VERT*3];
    GLubyte displayChar[CELLS_HORIZ*CELLS_VERT];
    bool dirty;
    SDL_Event currentSDLEvent;
};

static void sdldie(const char *msg)
{
    printf("%s: %s\n", msg, SDL_GetError());
    SDL_Quit();
    exit(1);
}

void SDL::setupwindow()
{
	// init sdl
    if (SDL_Init(SDL_INIT_VIDEO) < 0)
        sdldie("Unable to initialize SDL");

    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 3);
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 3);

    SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);
    SDL_GL_SetAttribute(SDL_GL_DEPTH_SIZE, 24);

    p->window = SDL_CreateWindow(PROGRAM_NAME, SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
                                 SCREENWIDTH, SCREENHEIGHT,
                                 SDL_WINDOW_OPENGL | SDL_WINDOW_SHOWN | SDL_WINDOW_MAXIMIZED | SDL_WINDOW_RESIZABLE |SDL_WINDOW_BORDERLESS);
    if (!p->window)
        sdldie("Unable to create window");

    p->context = SDL_GL_CreateContext(p->window);

    SDL_GL_SetSwapInterval(0);
}

void SDL::setupGL()
{
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
    for(int i=0; i<codepage437_image.width * codepage437_image.height / 8; i++) {
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

    for(int i=0; i<CELLS_HORIZ*CELLS_VERT*3; i++) {
        p->colorFG[i] = 1;
        p->colorBG[i] = 0;
    }

    for(int i=0; i<CELLS_HORIZ*CELLS_VERT; i++) {
        p->displayChar[i] = 0;
    }

    glGenVertexArrays(1, &(p->vao));
    glBindVertexArray(p->vao);
    glGenBuffers(4, p->vbo);
    glBindBuffer(GL_ARRAY_BUFFER, p->vbo[0]);
    glBufferData(GL_ARRAY_BUFFER, 12 * sizeof(GLfloat), block, GL_STATIC_DRAW);
    glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 0, 0);
    glEnableVertexAttribArray(0);

    glBindBuffer(GL_ARRAY_BUFFER, p->vbo[1]);
    glBufferData(GL_ARRAY_BUFFER, CELLS_HORIZ*CELLS_VERT*3 * sizeof(GLfloat), p->colorFG, GL_DYNAMIC_DRAW);
    glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 0, 0);
    glVertexAttribDivisor(1, 1);
    glEnableVertexAttribArray(1);

    glBindBuffer(GL_ARRAY_BUFFER, p->vbo[2]);
    glBufferData(GL_ARRAY_BUFFER, CELLS_HORIZ*CELLS_VERT*3 * sizeof(GLfloat), p->colorBG, GL_DYNAMIC_DRAW);
    glVertexAttribPointer(2, 3, GL_FLOAT, GL_FALSE, 0, 0);
    glVertexAttribDivisor(2, 1);
    glEnableVertexAttribArray(2);

    glBindBuffer(GL_ARRAY_BUFFER, p->vbo[3]);
    glBufferData(GL_ARRAY_BUFFER, CELLS_HORIZ*CELLS_VERT, p->displayChar, GL_DYNAMIC_DRAW);
    glVertexAttribPointer(3, 1, GL_UNSIGNED_BYTE, GL_FALSE, 0, 0);
    glVertexAttribDivisor(3, 1);
    glEnableVertexAttribArray(3);

    p->vertexshader = glCreateShader(GL_VERTEX_SHADER);
    GLchar *vertexsourcePtr = vertexsource;
    glShaderSource(p->vertexshader, 1, (const GLchar**)&vertexsourcePtr, 0);
    glCompileShader(p->vertexshader);
    glGetShaderiv(p->vertexshader, GL_COMPILE_STATUS, &IsCompiled_VS);
    if(IsCompiled_VS == FALSE)
    {
       glGetShaderiv(p->vertexshader, GL_INFO_LOG_LENGTH, &maxLength);
       vertexInfoLog = (char *)malloc(maxLength);
       glGetShaderInfoLog(p->vertexshader, maxLength, &maxLength, vertexInfoLog);
       printf("vertex shader error: %s\n", vertexInfoLog);

       free(vertexInfoLog);
       return;
    }

    p->fragmentshader = glCreateShader(GL_FRAGMENT_SHADER);
    GLchar *fragmentsourcePtr = fragmentsource;
    glShaderSource(p->fragmentshader, 1, (const GLchar**)&fragmentsourcePtr, 0);
    glCompileShader(p->fragmentshader);
    glGetShaderiv(p->fragmentshader, GL_COMPILE_STATUS, &IsCompiled_FS);
    if(IsCompiled_FS == FALSE)
    {
       glGetShaderiv(p->fragmentshader, GL_INFO_LOG_LENGTH, &maxLength);
       fragmentInfoLog = (char *)malloc(maxLength);
       glGetShaderInfoLog(p->fragmentshader, maxLength, &maxLength, fragmentInfoLog);
       printf("fragment shader error: %s\n", fragmentInfoLog);

       free(fragmentInfoLog);
       return;
    }

    p->shaderprogram = glCreateProgram();
    glAttachShader(p->shaderprogram, p->vertexshader);
    glAttachShader(p->shaderprogram, p->fragmentshader);
    glBindAttribLocation(p->shaderprogram, 0, "in_Position");
    glBindAttribLocation(p->shaderprogram, 1, "inFG_Color");
    glBindAttribLocation(p->shaderprogram, 2, "inBG_Color");
    glBindAttribLocation(p->shaderprogram, 3, "in_Char");
    glLinkProgram(p->shaderprogram);

    glGetProgramiv(p->shaderprogram, GL_LINK_STATUS, (int *)&IsLinked);
    if(IsLinked == FALSE)
    {
       glGetProgramiv(p->shaderprogram, GL_INFO_LOG_LENGTH, &maxLength);
       shaderProgramInfoLog = (char *)malloc(maxLength);
       glGetProgramInfoLog(p->shaderprogram, maxLength, &maxLength, shaderProgramInfoLog);

       free(shaderProgramInfoLog);
       return;
    }

    glUseProgram(p->shaderprogram);

    GLint mvp = glGetUniformLocation(p->shaderprogram, "MVP" );

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

    GLint height_uf = glGetUniformLocation(p->shaderprogram, "CELLS_VERTICAL" );
    glUniform1i(height_uf, CELLS_VERT);

    glActiveTexture(GL_TEXTURE0);
    GLint texture_location = glGetUniformLocation(p->shaderprogram, "in_Texture");
    glUniform1i(texture_location, 0);
    glBindTexture(GL_TEXTURE_2D, texture);

    glClearColor(0.0, 0.0, 0.0, 1.0);
    glClear(GL_COLOR_BUFFER_BIT);

    glFrontFace(GL_CW);
    glCullFace(GL_BACK);
    glEnable(GL_CULL_FACE);
}

void SDL::draw() {
    if(p->dirty) {
        p->dirty = false;
        glBindBuffer(GL_ARRAY_BUFFER, p->vbo[1]);
        glBufferSubData(GL_ARRAY_BUFFER,
                        0,
                        CELLS_HORIZ*CELLS_VERT*3*sizeof(GLfloat),
                        p->colorFG);
        glBindBuffer(GL_ARRAY_BUFFER, p->vbo[2]);
        glBufferSubData(GL_ARRAY_BUFFER,
                        0,
                        CELLS_HORIZ*CELLS_VERT*3*sizeof(GLfloat),
                        p->colorBG);
        glBindBuffer(GL_ARRAY_BUFFER, p->vbo[3]);
        glBufferSubData(GL_ARRAY_BUFFER,
                        0,
                        CELLS_HORIZ*CELLS_VERT,
                        p->displayChar);

        glDrawArraysInstanced(GL_TRIANGLES, 0, 6, CELLS_HORIZ*CELLS_VERT);
        SDL_GL_SwapWindow(p->window);
    }
}

SDL::SDL()
    : p(new impl)
{
    p->dirty = true;

    setupwindow();
    setupGL();
}

SDL::~SDL() {
    glUseProgram(0);
    glDisableVertexAttribArray(0);
    glDisableVertexAttribArray(1);
    glDisableVertexAttribArray(2);
    glDetachShader(p->shaderprogram, p->vertexshader);
    glDetachShader(p->shaderprogram, p->fragmentshader);
    glDeleteProgram(p->shaderprogram);
    glDeleteShader(p->vertexshader);
    glDeleteShader(p->fragmentshader);
    glDeleteBuffers(3, p->vbo);
    glDeleteVertexArrays(1, &p->vao);

	SDL_GL_DeleteContext(p->context);
	SDL_DestroyWindow(p->window);
	SDL_Quit();
}

void SDL::putChar(int x, int y, char c, const Color& fg, const Color& bg)
{
    int index = (CELLS_VERT-y-1) + x * CELLS_VERT;
    p->displayChar[index] = c;
    p->colorFG[index*3] = fg.r;
    p->colorFG[index*3+1] = fg.g;
    p->colorFG[index*3+2] = fg.b;
    p->colorBG[index*3] = bg.r;
    p->colorBG[index*3+1] = bg.g;
    p->colorBG[index*3+2] = bg.b;

    p->dirty = true;
}

bool SDL::pollEvent() {
    return SDL_PollEvent(&p->currentSDLEvent);
}

SDL_Event& SDL::getCurrentEvent() {
    return p->currentSDLEvent;
}

int SDL::getTicks() {
    return SDL_GetTicks();
}

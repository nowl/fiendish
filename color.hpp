#pragma once

#include <unordered_map>

struct Color {
    float r, g, b;

    Color() : r(0), g(0), b(0) {}
    Color(float r, float g, float b) : r(r), g(g), b(b) {}
};

extern std::unordered_map<std::string, Color> ColorByName;

// initializes named colors
void init_colors();

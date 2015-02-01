#pragma once

struct Color {
    float r, g, b;

    Color() : r(0), g(0), b(0) {}
    Color(float r, float g, float b) : r(r), g(g), b(b) {}

    // h from 0 to 360, s from 0 to 1, v from 0 to 1
    static Color fromHSV(float h, float s, float v);
};

extern std::unordered_map<std::string, Color> ColorByName;

// initializes named colors
void init_colors();

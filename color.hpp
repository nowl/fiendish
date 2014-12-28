#pragma once

struct Color {
    float r, g, b;

    Color() : r(0), g(0), b(0) {}
    Color(float r, float g, float b) : r(r), g(g), b(b) {}

    static Color BLUE;
    static Color BLACK;
    static Color WHITE;
    static Color GRAY3;
    static Color GRAY2;
    static Color GRAY1;
};

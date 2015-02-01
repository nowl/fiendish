#include "fiendish.hpp"

std::unordered_map<std::string, Color> ColorByName;

void init_colors() {
    ColorByName["BLACK"] = Color(0, 0, 0);
    ColorByName["WHITE"] = Color(1, 1, 1);
    ColorByName["BLUE"] = Color(0, 0, 1);
    ColorByName["GRAY3"] = Color(0.3, 0.3, 0.3);
    ColorByName["GRAY2"] = Color(0.2, 0.2, 0.2);
    ColorByName["GRAY1"] = Color(0.1, 0.1, 0.1);
};

Color Color::fromHSV(float h, float s, float v) {
    float c = v * s;
    float x = c * (1 - fabs(fmod(h / 60, 2) - 1));
    float m = v - c;

    switch( (int)(h / 60) ) {
    case 0: return Color{c+m,x+m,m};
    case 1: return Color{x+m,c+m,m};
    case 2: return Color{m,c+m,x+m};
    case 3: return Color{m,x+m,c+m};
    case 4: return Color{x+m,m,c+m};
    case 5: return Color{c+m,m,x+m};
    }

    return Color{0,0,0};
}

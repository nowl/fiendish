#include "color.hpp"

std::unordered_map<std::string, Color> ColorByName;

void init_colors() {
    ColorByName["BLACK"] = Color(0, 0, 0);
    ColorByName["WHITE"] = Color(1, 1, 1);
    ColorByName["BLUE"] = Color(0, 0, 1);
    ColorByName["GRAY3"] = Color(0.3, 0.3, 0.3);
    ColorByName["GRAY2"] = Color(0.2, 0.2, 0.2);
    ColorByName["GRAY1"] = Color(0.1, 0.1, 0.1);
};

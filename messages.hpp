#pragma once

#include <string>
#include "text.hpp"

void add_message(const std::string& str);
void add_message(const TextCommand& tcmd);
void draw_messages(int row_min, int row_max, int col_min, int col_max);


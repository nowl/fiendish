#pragma once

#include <algorithm>
#include <vector>
#include <iostream>

class FOVResponse {
public:
    virtual void visitTile(int x, int y, float intensity) = 0;
    virtual bool isBlocked(int x, int y) = 0;
    virtual bool outOfBounds(int x, int y) = 0;
    virtual float radiusOf(int x, int y) = 0;
};

void ShadowFOV(int startx, int starty, int radius, FOVResponse& fovResponse);

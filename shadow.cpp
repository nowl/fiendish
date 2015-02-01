#include "fiendish.hpp"

static void Scan(int depth, int radius,
                 int startx, int starty,
                 float startSlope, float endSlope, 
                 int m1, int m2, int m3, int m4,
                 FOVResponse& fovResponse)
{
    if(startSlope < endSlope)
        return;
    bool blocked = false;
    float newStart = 0;
    for(int distance = depth; distance <= radius && !blocked; distance++) {
        int dy = -distance;
        for(int dx = -distance; dx <= 0; dx++) {
            int x = dx * m1 + dy * m2;
            int y = dy * m4 + dx * m3;
            int mapX = x + startx;
            int mapY = y + starty;
            float leftSlope = (dx - 0.5) / (dy + 0.5);
            float rightSlope = (dx + 0.5) / (dy - 0.5);
            if ( fovResponse.outOfBounds(mapX, mapY) ||
                 (startSlope < rightSlope) )
                continue;
            if (endSlope > leftSlope)
                break;
            float rad = fovResponse.radiusOf(dx, dy);
            if (rad < radius)
                fovResponse.visitTile(mapX, mapY, 1 - rad/radius);
            bool tileBlocked = fovResponse.isBlocked(mapX, mapY);
            if (blocked) {
                if (tileBlocked) {
                    newStart = rightSlope;
                    continue;
                } else {
                    blocked = false;
                    startSlope = newStart;
                }
            } else if (tileBlocked && (distance < radius)) {
                blocked = true;
                Scan(distance+1, radius, startx, starty,
                     startSlope, leftSlope,
                     m1, m2, m3, m4,
                     fovResponse);
                newStart = rightSlope;
            }
        }
    }
}

static struct {
    int m1, m2, m3, m4;
} shadowFOVConfigurations[] = {
    {0,-1, 1, 0},
    {0, 1,-1, 0},
    {0,-1,-1, 0},
    {0, 1, 1, 0},
    {1, 0, 0, 1},
    {1, 0, 0,-1},
    {-1, 0, 0, 1},
    {-1, 0, 0,-1},
};

#define NUM_CONFIGS ( (int)(sizeof(shadowFOVConfigurations)/sizeof(shadowFOVConfigurations[0])) )

void ShadowFOV(int startx, int starty, int radius, FOVResponse& fovResponse) {
    for(int i=0; i<NUM_CONFIGS; i++)
            Scan(1, radius, startx, starty, 1, 0,
                 shadowFOVConfigurations[i].m1,
                 shadowFOVConfigurations[i].m2,
                 shadowFOVConfigurations[i].m3,
                 shadowFOVConfigurations[i].m4,
                 fovResponse);
}

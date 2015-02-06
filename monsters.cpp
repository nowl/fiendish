#include "monsters.hpp"
#include "dungeon.hpp"
#include "rng.hpp"

void populateMonsters(Dungeon *dungeon, std::vector<Monster>& monsters)
{
    for (int y=0; y<dungeon->height; y++) {
        for (int x=0; x<dungeon->width; x++) {
            MapCell *cell = dungeon->getCell(x, y);
            if (cell->cellType == CellType::HALL ||
                cell->cellType == CellType::ROOM)
            {
                if (rng::f() < 0.02) {
                    monsters.push_back(Monster{x, y});
                }
            }
        }
    }
}

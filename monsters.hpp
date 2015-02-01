#pragma once

struct MonsterCatalog {
    int symbol;    
};

class Monster {
public:
    int x, y;
    
};

void populateMonsters(Dungeon *dungeon, std::vector<Monster>& monsters);

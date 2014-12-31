#include <cstdio>
#include "playstate.hpp"
#include "globals.hpp"
#include "rng.hpp"
#include "keyboard.hpp"
#include "text.hpp"
#include "messages.hpp"

class DungeonFOVResponse : public FOVResponse {
public:
    DungeonFOVResponse(Dungeon& d,
                       std::unordered_map<Point, float, PointHash>& map,
                       std::unordered_map<Point, bool, PointHash>& exploredMap)
        : d(d), map(map), exploredMap(exploredMap) {}

    virtual void visitTile(int x, int y, float intensity) {
        const Point p = {x, y};
        map[p] = intensity;
        exploredMap[p] = true;
    }

    virtual bool isBlocked(int x, int y) {
        auto cell = d.getCell(x, y);
        if (!cell || cell->cellType == CellType::BEDROCK)
            return true;
        return false;
    }

    virtual bool outOfBounds(int x, int y) {
        auto cell = d.getCell(x, y);
        if (!cell)
            return true;

        return false;
    }

    virtual float radiusOf(int x, int y) {
        const Point p = {x, y};
        if (radiusMap.find(p) == radiusMap.end())
            radiusMap[p] = std::sqrt(x*x + y*y);

        return radiusMap[p];
    }

private:
    Dungeon& d;
    std::unordered_map<Point, float, PointHash>& map;
    std::unordered_map<Point, bool, PointHash>& exploredMap;
    std::unordered_map<Point, float, PointHash> radiusMap;
};

PlayState::PlayState() {
    dungeon = makeRecursiveSplitDungeon(300, 300, 500, 5, 15);

    dView.setDungeon(dungeon.get());
    
    fovResponse = std::unique_ptr<FOVResponse>(new DungeonFOVResponse(*dungeon.get(),
                                                                      shadowMap,
                                                                      exploredMap));

    auto randomPoint = dungeon->getRandomRoom().randomPointInRoom();
    p->x = randomPoint.x;
    p->y = randomPoint.y;
}

static void player_move(Direction dir, Dungeon *dungeon) {
    Point desired_point = {0,0};
    switch (dir) {
    case Direction::NORTH:
        desired_point = {p->x, p->y-1};
        break;
    case Direction::SOUTH:
        desired_point = {p->x, p->y+1};
        break;
    case Direction::EAST:
        desired_point = {p->x+1, p->y};
        break;
    case Direction::WEST:
        desired_point = {p->x-1, p->y};
        break;
    }

    auto cell = dungeon->getCell(desired_point.x, desired_point.y);
    if (cell &&
        CellTypeInfo[static_cast<int>(cell->cellType)].enterable)
    {
        p->x = desired_point.x;
        p->y = desired_point.y;
    } else if (cell) {
        char *text;
        int result = asprintf(&text, STRING_BLOCKED,
                              DirectionInfo[static_cast<int>(dir)].desc,
                              CellTypeInfo[static_cast<int>(cell->cellType)].desc);
        assert(result != -1);
        add_message(parse_text_command(text));
        free(text);
    }
}

void PlayState::handle_events()
{
    key_event key;
    while (unhandled_key(key)) {

        if (key.scancode == SDL_SCANCODE_ESCAPE || key.scancode == SDL_SCANCODE_Q)
            g->set_running(false);

        if (key.scancode == SDL_SCANCODE_D || key.scancode == SDL_SCANCODE_KP_6) {
            for(int x=0; x<CELLS_HORIZ; x++)
                for(int y=0; y<CELLS_VERT; y++)
                    putchar(x, y, rng::i_max_inc(255),
                            Color(rng::f(), rng::f(), rng::f()),
                            ColorByName["BLACK"]);
        }

        else if (key.scancode == SDL_SCANCODE_L)
            player_move(Direction::EAST, dungeon.get());
        else if (key.scancode == SDL_SCANCODE_H)
            player_move(Direction::WEST, dungeon.get());
        else if (key.scancode == SDL_SCANCODE_J)
            player_move(Direction::SOUTH, dungeon.get());
        else if (key.scancode == SDL_SCANCODE_K)
            player_move(Direction::NORTH, dungeon.get());
    }

    Point pos = Point{p->x, p->y} - PlayerScreenPosition;
    dView.setView(pos.x, pos.y);

    shadowMap.clear();
    ShadowFOV(p->x, p->y, 15, *fovResponse.get());
}

void PlayState::update()
{
    if (key_held(SDL_SCANCODE_D)) {
        for(int x=0; x<CELLS_HORIZ; x++)
            for(int y=0; y<CELLS_VERT; y++)
                putchar(x, y, rng::i_max_inc(255),
                        Color(rng::f(), rng::f(), rng::f()),
                        ColorByName["BLACK"]);
    } else if (key_held(SDL_SCANCODE_L)) {
        player_move(Direction::EAST, dungeon.get());
    } else if (key_held(SDL_SCANCODE_H)) {
        player_move(Direction::WEST, dungeon.get());
    } else if (key_held(SDL_SCANCODE_J)) {
        player_move(Direction::SOUTH, dungeon.get());
    } else if (key_held(SDL_SCANCODE_K)) {
        player_move(Direction::NORTH, dungeon.get());
    }

    Point pos = Point{p->x, p->y} - PlayerScreenPosition;
    dView.setView(pos.x, pos.y);

    shadowMap.clear();
    ShadowFOV(p->x, p->y, 15, *fovResponse.get());
}

void PlayState::render() {
    for(int x=0; x<CELLS_HORIZ; x++) {
        for(int y=0; y<CELLS_VERT; y++) {

            const Point p = dView.screenToDungeon(x, y);
            if (shadowMap.find(p) == shadowMap.end() &&
                exploredMap.find(p) == exploredMap.end())
            {
                putchar(x, y, ' ', ColorByName["BLACK"], ColorByName["BLACK"]);
                continue;
            }
            
            float intensity = 0;

            if (shadowMap.find(p) != shadowMap.end())
                intensity = shadowMap[p];

            /*
            putchar(x, y, rng::i_max_inc(255),
                    Color(rng::f(), rng::f(), rng::f()),
                    ColorByName["BLACK"]);
            */
            Color color = ColorByName["GRAY1"];
            auto cell = dView.getCell(x, y);
            if (cell) {
                switch (cell->cellType) {
                case CellType::BEDROCK:
                    if (intensity > 0)
                        color = Color(0, 0, 1.0*intensity);
                    putchar(x, y, '#',
                            color,
                            ColorByName["BLACK"]);
                    break;
                case CellType::ROOM:
                case CellType::HALL:
                    if (intensity > 0)
                        color = Color(.7*intensity, .7*intensity, .7*intensity);
                    putchar(x, y, '.',
                            color,
                            ColorByName["BLACK"]);
                    break;
                }
            } else {
                putchar(x, y, '#',
                        Color(0, 0.6, 0),
                        ColorByName["BLACK"]);
            }
        }
    }

    auto pos = dView.dungeonToScreen(p->x, p->y);

    putchar(pos.x, pos.y, '@',
            Color(1, 1, 1),
            ColorByName["BLACK"]);

    draw_messages();
}

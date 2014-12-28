#pragma once

#ifndef __DUNGEON_HPP__
#define __DUNGEON_HPP__

#include <vector>
#include <memory>
#include <unordered_map>

#include "rng.hpp"

#define MAX_MAP_HEIGHT 100000

enum class CellType {
    BEDROCK,
    HALL,
    ROOM
};

struct CellTypeInfo {
    bool enterable;
    const char *desc;
};

struct MapEntity;

struct Point {
    int x, y;

    Point operator+(const Point& p) {
        return Point{x+p.x, y+p.y};
    }

    Point operator-(const Point& p) {
        return Point{x-p.x, y-p.y};
    }

    bool operator==(const Point& p) const {
        return p.x == x && p.y == y;
    }
};

struct PointHash {
    typedef std::size_t result_type;
 
    result_type operator()(const Point& s) const
    {
        return s.x * MAX_MAP_HEIGHT + s.y;
    }
};

class Room {
public:
    int startX, startY, width, height;

    bool intersects(Room *r2);
    Point randomPointInRoom();
};

struct MapCell {
    CellType cellType;
    std::vector<MapEntity*> entities;

    MapCell();
};

enum class Direction {
    NORTH, SOUTH, EAST, WEST
};

struct DirectionInfo {
    const char *desc;
};

class Dungeon {
public:
    Dungeon(int width, int height);

    Room &getRandomRoom();
    MapCell *getCell(int x, int y);

    /* adds a room to the dungeon, fills cells with room type and adds
     * a room to the room list */
    Room& addRoom(int x, int y, int width, int height);

    /* makes a corridor in a random orientation between the start and
     * end points by filling relevant cells with hallway type */
    void makeLCorridor(Point p1, Point p2);

    void debugPrint();

    /* returns a random spot in a random room */
    Point randomOpenSpot();

private:
    std::vector<Room> rooms;
    std::vector<MapCell> cells;
    int width, height;

    void makeCorridor(Point p1, Direction dir, int length);
};

class DungeonView {
public:
    void setDungeon(Dungeon *d);
    MapCell *getCell(int x, int y);
    void setView(int x, int y);
    void incView(int x, int y);

    Point dungeonToScreen(int x, int y);
    Point screenToDungeon(int x, int y);

private:
    int mXOffset, mYOffset;
    Dungeon *mDungeon;
};

std::unique_ptr<Dungeon> makeRecursiveSplitDungeon(int width, int height,
                                                   int num_rooms,
                                                   int min_room_size,
                                                   int max_room_size);


#endif  // __DUNGEON_HPP

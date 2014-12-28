#include <cstdlib>
#include <vector>
#include <algorithm>
#include <cassert>
#include <map>

#include "dungeon.hpp"
#include "globals.hpp"

#ifdef DEBUG_GRAPH
# include <iostream>
#endif

using namespace std;

enum class SplitDirection {
    X, Y
};

MapCell::MapCell() : cellType(CellType::BEDROCK) {}

Dungeon::Dungeon(int width, int height)
    : width(width), height(height)
{
    for(int i=0; i<width*height; i++)
        cells.push_back(MapCell());
}

MapCell *Dungeon::getCell(int x, int y)
{
    if (x < 0 || x >= width ||
        y < 0 || y >= height)
    {
        return NULL;
    }

    return &cells[width*y + x];
}

bool Room::intersects(Room *r2)
{
    int r1EndX = startX + width - 1;
	int r1EndY = startY + height - 1;
	int r2EndX = r2->startX + r2->width - 1;
	int r2EndY = r2->startY + r2->height - 1;

	if (r1EndX < r2->startX ||
		r1EndY < r2->startY ||
		startX > r2EndX ||
		startY > r2EndY)
    {
		return false;
	}

	return true;
}

Point Room::randomPointInRoom() {
    return Point{rng::i_min_max_inc(startX, startX+width-1),
            rng::i_min_max_inc(startY, startY+height-1)};
}

Room &Dungeon::getRandomRoom()
{
    return rooms[rng::i_min_max_inc(0, rooms.size()-1)];
}

struct SplitResult {
    std::vector<Point> left, right;
};

struct ConnectingNode {
    Point p1, p2;
};

static vector<Point> make_random_points(int n, int maxx, int maxy) {
    vector<Point> points;
    for(int i=0; i<n; i++) {
        points.push_back(move(Point{rng::i_min_max_inc(0, maxx-1), rng::i_min_max_inc(0, maxy-1)}));
    }
    return points;
}

static SplitResult split(vector<Point> &points, SplitDirection dir) {
#ifdef DEBUG_GRAPH
    cout << "spliting........... dir=" << dir << endl;
#endif
    assert(points.size() > 3);
    sort(points.begin(), points.end(), [dir] (const Point& p1, const Point& p2) {
            switch (dir) {
            case SplitDirection::X:
                return p1.x < p2.x;
            case SplitDirection::Y:
                return p1.y < p2.y;
            }
            return false;
        });

// pick random point in middle
    int splittingSpot = rng::i_min_max_inc(0, points.size()-2-1) + 2; // size will be at lDirection::EAST 4

#ifdef DEBUG_GRAPH
    cout << "splitting: " << splittingSpot << endl;
#endif

    vector<Point> left(points.begin(), points.begin()+splittingSpot);
    vector<Point> right(points.begin()+splittingSpot, points.end());

#ifdef DEBUG_GRAPH
    cout << "left" << endl;
    for_each(left.begin(), left.end(), [] (const Point &p) {
            cout << p.x << ", " << p.y << endl;
        });
    cout << "right" << endl;
    for_each(right.begin(), right.end(), [] (const Point &p) {
            cout << p.x << ", " << p.y << endl;
        });

    cout << "end spliting..........." << endl;
#endif

    return move(SplitResult{move(left), move(right)});
}

/**
 * recursively builds connections
 */
static void buildConnectingGraph(vector<Point> &points, vector<ConnectingNode>& connections, SplitDirection dir) {
    SplitResult splitResults = split(points, dir);

    SplitDirection nextDir;
    switch (dir) {
    case SplitDirection::X:
        nextDir = SplitDirection::Y;
        break;
    default:
        nextDir = SplitDirection::X;
    }

    connections.push_back(ConnectingNode{splitResults.left[splitResults.left.size()-1], splitResults.right[0]});

    if (splitResults.left.size() <= 3) {
        for(auto i=1u; i<splitResults.left.size(); i++)
            connections.push_back(ConnectingNode{splitResults.left[0], splitResults.left[i]});
    } else {
        buildConnectingGraph(splitResults.left, connections, nextDir);
    }

    if (splitResults.right.size() <= 3) {
        for(auto i=1u; i<splitResults.right.size(); i++)
            connections.push_back(ConnectingNode{splitResults.right[0], splitResults.right[i]});
    } else {
        buildConnectingGraph(splitResults.right, connections, nextDir);
    }
}

Room&
Dungeon::addRoom(int x, int y, int width, int height) {
    for(int yy = y; yy < y + height; yy++) {
        for(int xx = x; xx < x + width; xx++) {
            MapCell *cell = getCell(xx, yy);
            cell->cellType = CellType::ROOM;
        }
    }

    rooms.push_back(Room({x, y, width, height}));
    return rooms.back();
}

void Dungeon::makeCorridor(Point p1, Direction dir, int length) {
#ifdef DEBUG_DUNGEON_CORR
    printf("  making hall from %d,%d in dir %d of length %d\n",
           p1.x, p1.y, dir, length);
#endif  // DEBUG_DUNGEON_CORR

    for (int i=0; i<length; i++) {
        switch(dir) {
        case Direction::NORTH:
            getCell(p1.x, p1.y - i)->cellType = CellType::HALL;
            break;
        case Direction::SOUTH:
            getCell(p1.x, p1.y + i)->cellType = CellType::HALL;
            break;
        case Direction::EAST:
            getCell(p1.x + i, p1.y)->cellType = CellType::HALL;
            break;
        case Direction::WEST:
            getCell(p1.x - i, p1.y)->cellType = CellType::HALL;
            break;
        }
    }
}

void Dungeon::makeLCorridor(Point p1, Point p2)
{

#ifdef DEBUG_DUNGEON_CORR
    printf("making corridor from %d,%d to %d,%d\n",
           p1.x, p1.y, p2.x, p2.y);
#endif  // DEBUG_DUNGEON_CORR

    // straight corridors in x and y directions
    if (p1.x == p2.x) {
        if (p1.y > p2.y) {
            makeCorridor(p1, Direction::NORTH, p1.y - p2.y + 1);
        } else {
            makeCorridor(p1, Direction::SOUTH, p2.y - p1.y + 1);
        }
    } else if (p1.y == p2.y) {
        if (p1.x > p2.x) {
            makeCorridor(p1, Direction::WEST, p1.x - p2.x + 1);
        } else {
            makeCorridor(p1, Direction::EAST, p2.x - p1.x + 1);
        }
    } else {
        // diagonal corridor, choose random direction first

        SplitDirection dir;

        switch(rng::i_min_max_inc(0, 1)) {
        case 0:
            dir = SplitDirection::X;
            break;
        default:
            dir = SplitDirection::Y;
        }

        if (p1.x > p2.x && p1.y > p2.y) {
            if (dir == SplitDirection::X) {
                makeCorridor(p2, Direction::EAST, p1.x - p2.x + 1);
                makeCorridor(p1, Direction::NORTH, p1.y - p2.y + 1);
            } else {
                makeCorridor(p2, Direction::SOUTH, p1.y - p2.y + 1);
                makeCorridor(p1, Direction::WEST, p1.x - p2.x + 1);
            }
        } else if (p1.x > p2.x && p1.y < p2.y) {
            if (dir == SplitDirection::X) {
                makeCorridor(p2, Direction::EAST, p1.x - p2.x + 1);
                makeCorridor(p1, Direction::SOUTH, p2.y - p1.y + 1);
            } else {
                makeCorridor(p2, Direction::NORTH, p2.y - p1.y + 1);
                makeCorridor(p1, Direction::WEST, p1.x - p2.x + 1);
            }
        }  else if (p1.x < p2.x && p1.y > p2.y) {
            if (dir == SplitDirection::X) {
                makeCorridor(p1, Direction::EAST, p2.x - p1.x + 1);
                makeCorridor(p2, Direction::SOUTH, p1.y - p2.y + 1);
            } else {
                makeCorridor(p1, Direction::NORTH, p1.y - p2.y + 1);
                makeCorridor(p2, Direction::WEST, p2.x - p1.x + 1);
            }
        } else {
            if (dir == SplitDirection::X) {
                makeCorridor(p1, Direction::EAST, p2.x - p1.x + 1);
                makeCorridor(p2, Direction::NORTH, p2.y - p1.y + 1);
            } else {
                makeCorridor(p1, Direction::SOUTH, p2.y - p1.y + 1);
                makeCorridor(p2, Direction::WEST, p2.x - p1.x + 1);
            }
        }
    }
}

void Dungeon::debugPrint() {
    for (int y = 0; y < height; y++) {
        for (int x = 0; x < width; x++) {
            if(getCell(x, y)->cellType == CellType::BEDROCK)
                printf("#");
            else if(getCell(x, y)->cellType == CellType::HALL)
                printf(".");
            else
                printf(" ");
        }
        printf("\n");
    }
}

Point
Dungeon::randomOpenSpot()
{
    return getRandomRoom().randomPointInRoom();
}

std::unique_ptr<Dungeon> makeRecursiveSplitDungeon(int width, int height, int num_rooms,
                                                   int min_room_size,
                                                   int max_room_size)
{
    std::unique_ptr<Dungeon> dungeon(new Dungeon(width, height));

    std::vector<Point> points = make_random_points(num_rooms, width-max_room_size, height-max_room_size);
    std::vector<ConnectingNode> connections;
    buildConnectingGraph(points, connections, SplitDirection::X);

    // make rooms

    // map of point hash (y*width+x) to inserted room
    map<int, Room> roomMap;

    for (auto iter = points.begin(); iter != points.end(); iter++) {
        const Point &p = *iter;
        int hash = p.y * width + p.x;

        // insert room
        int roomSizeX = rng::i_min_max_inc(min_room_size, max_room_size);
        int roomSizeY = rng::i_min_max_inc(min_room_size, max_room_size);
        Room &room = dungeon->addRoom(p.x, p.y, roomSizeX, roomSizeY);
        roomMap[hash] = room;
    }

    // make corridors

    for (auto iter = connections.begin(); iter != connections.end(); iter++) {
        const ConnectingNode &n = *iter;

        int hash = n.p1.y * width + n.p1.x;
        Room& room1 = roomMap[hash];
        hash = n.p2.y * width + n.p2.x;
        Room& room2 = roomMap[hash];

        Point p1 = room1.randomPointInRoom();
        Point p2 = room2.randomPointInRoom();

        dungeon->makeLCorridor(p1, p2);
    }

    return dungeon;
}

void DungeonView::setDungeon(Dungeon *d) {
    mDungeon = d;
}

void DungeonView::setView(int x, int y) {
    mXOffset = x;
    mYOffset = y;
}

void DungeonView::incView(int x, int y) {
    mXOffset += x;
    mYOffset += y;
}

Point DungeonView::dungeonToScreen(int x, int y) {
    return Point{x - mXOffset, y - mYOffset};
}

Point DungeonView::screenToDungeon(int x, int y) {
    return Point{x + mXOffset, y + mYOffset};
}

MapCell *
DungeonView::getCell(int x, int y)
{
    return mDungeon->getCell(x + mXOffset,
                             y + mYOffset);
}

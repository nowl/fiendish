#pragma once

#include "dungeon.hpp"
#include "state.hpp"

class FOVResponse;
class Monster;

class PlayState : public State {
public:
	PlayState();

    void handle_events();
    void update();
    void render();

private:
	std::unique_ptr<Dungeon> dungeon;
	DungeonView dView;
    std::unique_ptr<FOVResponse> fovResponse;
    std::unordered_map<Point, float, PointHash> shadowMap;
    std::unordered_map<Point, bool, PointHash> exploredMap;
    std::unique_ptr<State> message_state;
    std::vector<Monster> monsters;
};

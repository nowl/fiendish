#pragma once

class MessageState : public State {
public:
	MessageState(State *parent) : parentState(parent) {}

    void handle_events();
    void update();
    void render();

private:
    State *parentState;
};

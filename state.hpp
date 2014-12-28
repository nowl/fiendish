#pragma once

class State {
public:
    virtual ~State() {};

    virtual void handle_events() = 0;
    virtual void update() = 0;
    virtual void render() = 0;
};

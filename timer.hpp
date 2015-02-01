#pragma once

class Timer {
public:
    Timer() { Reset(); }

    void Reset() { gettimeofday(&Start, NULL); }

    // time in seconds
    float Elapsed() const {
        struct timeval now;
        gettimeofday(&now, NULL);
        float elapsed = (now.tv_sec - Start.tv_sec) + (now.tv_usec - Start.tv_usec) / 1e6;
        return elapsed;
    }
    
private:
    struct timeval Start;
};

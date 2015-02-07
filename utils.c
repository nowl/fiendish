#include "fiendish.h"

struct mem_buf*
mem_buf_size(struct mem_buf *buf,
             size_t size,
             char update_usage)
{
    if (!buf) {
        buf = malloc(sizeof(*buf));
        buf->mem = malloc(size);
        buf->usage = update_usage ? size : 0;
        buf->cap = size;
    } else {
        size_t new_cap = buf->cap;
        while (size > new_cap) new_cap *= 2;

        if (new_cap != buf->cap) {
            buf->cap = new_cap;
            buf->mem = realloc(buf->mem, buf->cap);
        }
        
        if (update_usage)
            buf->usage = size;
    }

    return buf;
}

void
mem_buf_free(struct mem_buf *buf)
{
    free(buf->mem);
    free(buf);
}

char substr_compare(char *s, int start, int end, char *t) {
    assert(strlen(t) == end-start);
    
    if(end > strlen(s) || start >= strlen(s))
        return 0;

    int i;
    for(i=0; i<end-start; i++)
        if(s[start+i] != t[i])
            return 0;
    
    return 1;
}

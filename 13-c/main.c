#define _GNU_SOURCE

#include <stdbool.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <assert.h>

size_t count_bits(uint64_t i) {
    size_t count = 0;
    while (i != 0) {
        count += i & 1;
        i >>= 1;
    }
    return count;
}

#define INPUT 1364
#define IS_WALL(x, y, number) (count_bits(x*x + 3*x + 2*x*y + y + y*y + number) % 2 != 0)

void dump(size_t w, size_t h, int num) {
    for (size_t y = 0; y < h; y++) {
        for (size_t x = 0; x < w; x++) {
            printf("%c", IS_WALL(x, y, num)?'#':'.');
        }
        printf("\n");
    }
}

#define BUF_CAP 1000
typedef struct {
    size_t x;
    size_t y;
} pos;

bool contains(pos *arr, size_t len, pos val) {
    for (size_t i = 0; i < len; i++) {
        if (arr[i].x == val.x && arr[i].y == val.y) {
            return true;
        }
    }
    return false;
}

size_t solve(size_t target_x, size_t target_y, int num, bool part2, size_t max_steps) {
    pos start = {
        .x = 1,
        .y = 1,
    };
    pos positions_[BUF_CAP*sizeof(pos)] = {0};
    pos *positions = positions_;
    size_t index = 0;
    size_t old_index = 0;
    positions[index++] = start;

    size_t steps = 0;

    while (true) {
        size_t tmp_index = index;
        for (size_t i = old_index; i < tmp_index; i++) {
            assert(index < BUF_CAP);
            pos position = positions[i];
            if (!part2 && (position.x == target_x && position.y == target_y)) {
                return steps;
            }
            
            if (position.x > 0) {
                pos new_pos =  {
                    .x = position.x-1,
                    .y = position.y,
                };
                if (!IS_WALL(new_pos.x, new_pos.y, num) && !contains(positions, BUF_CAP, new_pos))
                    positions[index++] = new_pos;
            }
            
            if (position.y > 0) {
                pos new_pos =  {
                    .x = position.x,
                    .y = position.y-1,
                };
                if (!IS_WALL(new_pos.x, new_pos.y, num) && !contains(positions, BUF_CAP, new_pos))
                    positions[index++] = new_pos;
            }
            
            pos new_pos =  {
                .x = position.x+1,
                .y = position.y,
            };
            if (!IS_WALL(new_pos.x, new_pos.y, num) && !contains(positions, BUF_CAP, new_pos))
                positions[index++] = new_pos;
            
            pos new_pos_ = {
                .x = position.x,
                .y = position.y+1,
            };
            if (!IS_WALL(new_pos_.x, new_pos_.y, num) && !contains(positions, BUF_CAP, new_pos_))
                positions[index++] = new_pos_;
        }
        if (index == 0) {
            fprintf(stderr, "ERROR: detected infinte loop (no possible solutions)\n");
            exit(1);
        }

        old_index = tmp_index;

        steps++;

        if (part2 && steps >= max_steps) {
            break;
        }
    }

    assert(part2);
    return index;
}

int main() {
    printf("PART 1: %ld\n", solve(31, 39, INPUT, false, 0));
    printf("PART 2: %ld\n", solve(0, 0, INPUT, true, 50));
    return 0;
}

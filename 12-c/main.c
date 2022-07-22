#define _GNU_SOURCE

#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>
#include <assert.h>
#include <stdbool.h>

typedef enum {
    TYPE_CPY = 0,
    TYPE_INC,
    TYPE_DEC,
    TYPE_JNZ,
} inst_type;

typedef enum {
    TYPE_REG,
    TYPE_INT,
} operand_type;

typedef struct {
    operand_type type;
    int value;
} operand;

typedef struct {
    inst_type type;
    operand operands[2];
} inst;


inst *parse_line(char *line) {
    char *word = strtok(line, " ");
    char *tokens[3];
    for (size_t i = 0; word != NULL; i++) {
        tokens[i] = word;
        word = strtok(0, " ");
    }

    inst *ret_inst = malloc(sizeof(inst));
    if (strcmp(tokens[0], "cpy") == 0) {
        ret_inst->type = TYPE_CPY;
    } else if (strcmp(tokens[0], "inc") == 0) {
        ret_inst->type = TYPE_INC;
    } else if (strcmp(tokens[0], "dec") == 0) {
        ret_inst->type = TYPE_DEC;
    } else if (strcmp(tokens[0], "jnz") == 0) {
        ret_inst->type = TYPE_JNZ;
    } else {
        fprintf(stderr, "ERROR: Unexpected instruction: %s", tokens[0]);
        exit(1);
    }

    for (size_t op_index = 0; op_index < 2; op_index++) {
        char *end = NULL;
        int result = strtol(tokens[1+op_index], &end, 10);
        if (*end != '\0') {
            size_t reg_index;
            if (strcmp(tokens[1+op_index], "a") == 0) {
                reg_index = 0;
            } else if (strcmp(tokens[1+op_index], "b") == 0) {
                reg_index = 1;
            } else if (strcmp(tokens[1+op_index], "c") == 0) {
                reg_index = 2;
            } else if (strcmp(tokens[1+op_index], "d") == 0) {
                reg_index = 3;
            } else {
                fprintf(stderr, "ERROR: Invalid register: '%s'", tokens[1+op_index]);
                exit(1);
            }
            ret_inst->operands[op_index].type = TYPE_REG;
            ret_inst->operands[op_index].value = reg_index;
        } else {
            ret_inst->operands[op_index].type = TYPE_INT;
            ret_inst->operands[op_index].value = result;
        }
    }
    return ret_inst;
}

inst **parse(const char *file_path) {
    FILE *file;
    char *line = 0;

    file = fopen(file_path, "r");
    if (file == NULL) {
        fprintf(stderr, "ERROR reading file: %s\n", strerror(errno));
        exit(1);
    }

    size_t lines = 0;
    while (!feof(file)) {
        char ch = fgetc(file);
        if (ch == '\n') {
            lines++;
        }
    }
    rewind(file);

    inst **insts = malloc((lines+1) * sizeof(inst));
    const size_t INST_LEN = lines * sizeof(inst);
    size_t pos = 0;

    int read;
    size_t len;
    while ((read = getline(&line, &len, file) != -1)) {
        char *tmp = line;
        tmp[strlen(tmp)-1] = '\0';
        insts[pos++] = parse_line(tmp);
    }
    if (errno != 0) {
        fprintf(stderr, "ERROR reading lines: %s\n", strerror(errno));
        exit(1);
    }

    insts[INST_LEN] = 0;

    fclose(file);
    if (line) free(line);
    return insts;
}

void dump(inst **insts, FILE *file) {
    for (size_t i = 0; insts[i] != 0; i++) {
        fprintf(file, "INST: {type = ");
        inst *cur_inst = insts[i];
        switch (cur_inst->type) {
        case TYPE_CPY:
            fprintf(file, "cpy");
            break;
        case TYPE_INC:
            fprintf(file, "inc");
            break;
        case TYPE_DEC:
            fprintf(file, "dec");
            break;
        case TYPE_JNZ:
            fprintf(file, "jnz");
            break;
        }
        fprintf(file, ", operands = {");
        for (size_t i = 0; i < 2; i++) {
            operand op = cur_inst->operands[i];
            fprintf(file, "type = ");
            switch (op.type) {
            case TYPE_REG:
                fprintf(file, "register");
                break;
            case TYPE_INT:
                fprintf(file, "integer");
                break;
            }
            fprintf(file, ", value = %d", op.value);
            if (i == 0)
                fprintf(file, ", ");
        }
        fprintf(file, "}}\n");
    }
    fprintf(file, "\n");
}

int execute(inst **insts, bool part2) {
    size_t ip = 0;
    int regs[4] = {0};

    if (part2) {
        regs[2] = 1;
    }
    
    while (insts[ip] != NULL) {
        inst *current_inst = insts[ip];
        switch (current_inst->type) {
        case TYPE_CPY:
            switch (current_inst->operands[0].type) {
            case TYPE_INT:
                regs[current_inst->operands[1].value] = current_inst->operands[0].value;
                break;
            case TYPE_REG:
                regs[current_inst->operands[1].value] = regs[current_inst->operands[0].value];
                break;
            }
            break;
        case TYPE_DEC:
            assert(current_inst->operands[0].type == TYPE_REG);
            regs[current_inst->operands[0].value]--;
            break;
        case TYPE_INC:
            assert(current_inst->operands[0].type == TYPE_REG);
            regs[current_inst->operands[0].value]++;
            break;
        case TYPE_JNZ:
            switch (current_inst->operands[0].type) {
            case TYPE_REG:
                if (regs[current_inst->operands[0].value]) {
                    ip += current_inst->operands[1].value;
                    continue;
                }
                break;
            case TYPE_INT:
                if (current_inst->operands[0].value) {
                    ip += current_inst->operands[1].value;
                    continue;
                }
                break;
            }
            break;
        }
        ip++;
    }

    return regs[0];
}

int main(void) {
    inst **insts = parse("./input.txt");
    printf("PART 1: %d\n", execute(insts, false));
    printf("PART 2: %d\n", execute(insts, true));

    return 0;
}

#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <flam3.h>

void flam3_free_genomes(void* num_genomes_as_ptr, flam3_genome *cp) {
    int i;
    intptr_t num_genomes = (intptr_t) num_genomes_as_ptr;
    for (i = 0; i < num_genomes; i++) {
        // printf("%s", flam3_print_to_string(cp + i));
        clear_cp(cp+i, 0);
    }
    free(cp);
}


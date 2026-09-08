#ifndef RUMOCA_HEAD_TO_HEAD_INPUTS_H
#define RUMOCA_HEAD_TO_HEAD_INPUTS_H

typedef struct {
    float X0[10];
    float l[9];
    float r[9];
    float B[2][2];
} ExpMixedInput;

/* Volatile reads prevent LTO from specializing the generated kernels to the
 * checked points. Case zero alone is measured; the remaining cases exercise
 * distinct branch and layout semantics outside the marked interval. */
static volatile const ExpMixedInput INPUT_CASES[4] = {
    {
        {12.5f, -3.25f, 41.0f, 1.5f, -0.75f, 2.25f,
         0.8047378541f, 0.3113554956f, -0.4670332434f, 0.1868132974f},
        {0.011f, -0.007f, 0.0981f, 0.12f, -0.31f, 9.72f,
         0.31f, -0.42f, 0.29f},
        {0.002f, 0.001f, -0.0049f, 0.0f, 0.0f, -9.81f,
         0.21f, 0.14f, -0.33f},
        {{0.0f, 1.0f}, {0.0f, 0.0f}},
    },
    {
        {12.5f, -3.25f, 41.0f, 1.5f, -0.75f, 2.25f,
         0.8047378541f, 0.3113554956f, -0.4670332434f, 0.1868132974f},
        {0.011f, -0.007f, 0.0981f, 0.12f, -0.31f, 9.72f,
         0.00001f, -0.00002f, 0.00001f},
        {0.002f, 0.001f, -0.0049f, 0.0f, 0.0f, -9.81f,
         -0.00002f, 0.00001f, 0.00001f},
        {{0.0f, 1.0f}, {0.0f, 0.0f}},
    },
    {
        {12.5f, -3.25f, 41.0f, 1.5f, -0.75f, 2.25f,
         0.8047378541f, 0.3113554956f, -0.4670332434f, 0.1868132974f},
        {0.018f, -0.014f, 0.087f, 0.15f, -0.27f, 9.68f,
         0.01f, -0.02f, 0.015f},
        {-0.006f, 0.004f, -0.003f, 0.03f, -0.02f, -9.76f,
         -0.018f, 0.012f, 0.021f},
        {{0.0f, 1.0f}, {0.0f, 0.0f}},
    },
    {
        {-7.25f, 6.5f, 2.75f, -1.125f, 3.75f, -4.5f,
         0.65f, -0.2f, 0.35f, 0.62f},
        {1.2f, -0.4f, 0.75f, -2.0f, 1.25f, 0.5f,
         0.51f, -0.27f, 0.33f},
        {-0.8f, 1.1f, -1.4f, 0.6f, -0.9f, 2.2f,
         -0.44f, 0.38f, -0.29f},
        {{0.2f, -0.3f}, {0.4f, 0.1f}},
    },
};

#endif

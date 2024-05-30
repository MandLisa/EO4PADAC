# -*- coding: utf-8 -*-
"""
Created on Thu May 15 19:39:36 2024

@author: lmandl
"""

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <gsl/gsl_errno.h>
#include <gsl/gsl_spline.h>
#include <gsl/gsl_interp.h>

#define MAX_ROWS 10000
#define MAX_COLS 10
#define MAX_LINE_LENGTH 1024

// Function to read a CSV file
int read_csv(const char *filename, double data[MAX_ROWS][MAX_COLS], int *rows, int *cols) {
    FILE *file = fopen(filename, "r");
    if (!file) {
        perror("Failed to open file");
        return -1;
    }

    char line[MAX_LINE_LENGTH];
    *rows = 0;
    *cols = 0;

    while (fgets(line, MAX_LINE_LENGTH, file) && *rows < MAX_ROWS) {
        char *token = strtok(line, ",");
        int col = 0;

        while (token && col < MAX_COLS) {
            data[*rows][col] = atof(token);
            token = strtok(NULL, ",");
            col++;
        }

        if (col > *cols) {
            *cols = col;
        }
        (*rows)++;
    }

    fclose(file);
    return 0;
}

// Function to write a CSV file
void write_csv(const char *filename, double data[MAX_ROWS][MAX_COLS], int rows, int cols) {
    FILE *file = fopen(filename, "w");
    if (!file) {
        perror("Failed to open file");
        return;
    }

    for (int i = 0; i < rows; i++) {
        for (int j = 0; j < cols; j++) {
            fprintf(file, "%lf", data[i][j]);
            if (j < cols - 1) {
                fprintf(file, ",");
            }
        }
        fprintf(file, "\n");
    }

    fclose(file);
}

// Function to apply spline smoothing using GSL
void spline_smoothing(double data[MAX_ROWS][MAX_COLS], int rows

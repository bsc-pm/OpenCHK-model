/**
 *  @file   heatdis-fti.c
 *  @author Leonardo A. Bautista Gomez and Sheng Di
 *  @date   January, 2014
 *  @brief  Heat distribution code to test FTI.
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define ITER_TIMES  200
//#define ITER_OUT    500
#define PRECISION   0.001
#define WORKTAG     26
//#define GRIDSIZE    128
#define GRIDSIZE    8192
#define CRASH_ITER  1750

void handle_error( char *msg ) {
    printf("error: coud not %s\n", msg);
    MPI_Abort(MPI_COMM_WORLD, -1);
}

void initData(int nbLines, int M, int rank, double *h)
{
    int i, j;
    for (i = 0; i < nbLines; i++) {
        for (j = 0; j < M; j++) {
            h[(i*M)+j] = 0;
        }
    }
    if (rank == 0) {
        for (j = (M*0.1); j < (M*0.9); j++) {
            h[j] = 100;
        }
    }
}


void print_solution (char *filename, double *grid)
{
    int i, j;
    FILE *outfile;
    outfile = fopen(filename,"w");
    if (outfile == NULL) {
        printf("Can't open output file.\n");
        exit(-1);
    }
    for (i = 0; i < GRIDSIZE; i++) {
        for (j = 0; j < GRIDSIZE; j++) {
            fprintf (outfile, "%6.2f\n", grid[(i*GRIDSIZE)+j]);
        }
        fprintf(outfile, "\n");
    }
    fclose(outfile);
}


double doWork(int numprocs, int rank, int M, int nbLines, double *g, double *h)
{
    int i,j;
    MPI_Request req1[2], req2[2];
    MPI_Status status1[2], status2[2];
    double localerror, globalerror;
    localerror = 0;
    for(i = 0; i < nbLines; i++) {
        for(j = 0; j < M; j++) {
            h[(i*M)+j] = g[(i*M)+j];
        }
    }
    if (rank > 0) {
        MPI_Isend(g+M, M, MPI_DOUBLE, rank-1, WORKTAG, MPI_COMM_WORLD, &req1[0]);
        MPI_Irecv(h,   M, MPI_DOUBLE, rank-1, WORKTAG, MPI_COMM_WORLD, &req1[1]);
    }
    if (rank < numprocs-1) {
        MPI_Isend(g+((nbLines-2)*M), M, MPI_DOUBLE, rank+1, WORKTAG, MPI_COMM_WORLD, &req2[0]);
        MPI_Irecv(h+((nbLines-1)*M), M, MPI_DOUBLE, rank+1, WORKTAG, MPI_COMM_WORLD, &req2[1]);
    }
    if (rank > 0) {
        MPI_Waitall(2,req1,status1);
    }
    if (rank < numprocs-1) {
        MPI_Waitall(2,req2,status2);
    }
    for (i = 1; i < (nbLines-1); i++) {
        for (j = 0; j < M; j++) {
            g[(i*M)+j] = 0.25*(h[((i-1)*M)+j]+h[((i+1)*M)+j]+h[(i*M)+j-1]+h[(i*M)+j+1]);
            if (localerror < fabs(g[(i*M)+j] - h[(i*M)+j])) {
                localerror = fabs(g[(i*M)+j] - h[(i*M)+j]);
            }
        }
    }
    if (rank == (numprocs-1)) {
        for(j = 0; j < M; j++) {
            g[((nbLines-1)*M)+j] = g[((nbLines-2)*M)+j];
        }
    }
    MPI_Allreduce(&localerror, &globalerror, 1, MPI_DOUBLE, MPI_MAX, MPI_COMM_WORLD);
    return globalerror;
}

bool doCkpt( int i, int *id, int *level );

int main(int argc, char *argv[])
{
    int rank, nbProcs, nbLines, i, j, N, M, res;
    double wtime, *h, *g, *grid, globalerror = 1;
    char fn[32];
    int CRASH = -1;

    if(argc == 2) 
        CRASH = atoi(argv[1]);
    else {
        printf("Usage: %s crash\n", argv[0]);
        exit(-1);
    }

    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &nbProcs);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    MPI_Comm comm = MPI_COMM_WORLD;

    M = GRIDSIZE;
    N = GRIDSIZE;
    nbLines = (N / nbProcs)+3;
    h = (double *) malloc(sizeof(double *) * M * nbLines);
    g = (double *) malloc(sizeof(double *) * M * nbLines);
    grid = (double *) malloc(sizeof(double *) * M * (nbLines-2) * nbProcs);
    initData(nbLines, M, rank, g);
    if (rank == 0) {
        printf("Data initialized. Global grid size is %d x %d. Iters to do %d. Crash at %d.\n", M, (nbLines-2)*nbProcs, ITER_TIMES, CRASH);
    }

    // start with level/id = 1 (init with 0)
    int level = 0;
    int id = 0;

    MPI_Barrier(MPI_COMM_WORLD);
    wtime = MPI_Wtime();
    i = 0;
  
    #pragma chk init comm(comm)

    #pragma chk load(i, id, level, h[0:M*nbLines-1], g[0:M*nbLines-1])
    for(; i < ITER_TIMES; i++) { // Check execution status
        #pragma chk store(i, id, level, h[0:M*nbLines-1], g[0:M*nbLines-1]) id(id) level(level) if(doCkpt(i, &id, &level))
        if ( CRASH == i ){
            printf("CRASH\n");
            MPI_Abort(MPI_COMM_WORLD, 1);
        }
        globalerror = doWork(nbProcs, rank, M, nbLines, g, h);
        if (i%(ITER_TIMES/10) == 0 && i != 0) {
            if (rank == 0) {
                printf("Step : %d, current error = %f; target = %f\n", i, globalerror, PRECISION);
            }
            MPI_Gather(g+M, (nbLines-2)*M, MPI_DOUBLE, grid, (nbLines-2)*M, MPI_DOUBLE, 0, MPI_COMM_WORLD);
            sprintf(fn, "results/vis-%d.dat", i);
            if (rank == 0) {
                print_solution(fn, grid);
            }
        }
        //if (globalerror < PRECISION) {
        //    break;
        //}
    }
    #pragma chk shutdown

    if (rank == 0) {
        printf("Execution finished in %lf seconds.\n", MPI_Wtime() - wtime);
    }

    free(h);
    free(g);
    free(grid);

    MPI_Finalize();
    return 0;
}

bool doCkpt( int i, int *id, int *level ) {
    if (i%(ITER_TIMES/10) == 0 && i != 0) {
        (*id)++;
        *level = (*level)%4+1;
        return true;
    } else {
        return false;
    }
}    
